//===- RISCV32ArgumentRaiser.cpp - Binary raiser utility llvm-mctoll ----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of RISCV32ArgumentRaiser class for use by
// llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#include "RISCV32ArgumentRaiser.h"
#include "RISCVSubtarget.h"
#include "llvm/ADT/DepthFirstIterator.h"
#include <vector>

#define DEBUG_TYPE "mctoll"

using namespace llvm;

char RISCV32ArgumentRaiser::ID = 0;

RISCV32ArgumentRaiser::RISCV32ArgumentRaiser(RISCV32ModuleRaiser &mr)
    : RISCV32RaiserBase(ID, mr) {}

RISCV32ArgumentRaiser::~RISCV32ArgumentRaiser() {}

void RISCV32ArgumentRaiser::init(MachineFunction *mf, Function *rf) {
  RISCV32RaiserBase::init(mf, rf);
  MFI = &MF->getFrameInfo();
  TII = MF->getSubtarget<RISCVSubtarget>().getInstrInfo();
}

/// Change all return relative register operands to stack 0.
void RISCV32ArgumentRaiser::updateReturnRegister(MachineFunction &mf) {
  // for (MachineBasicBlock &mbb : mf) {
  //   if (mbb.succ_empty()) {
  //     bool loop = true;
  //     for (MachineBasicBlock::reverse_iterator ii = mbb.rbegin(),
  //                                              ie = mbb.rend();
  //          (ii != ie) && loop; ++ii) {
  //       MachineInstr &mi = *ii;
  //       for (MachineInstr::mop_iterator oi = mi.operands_begin(),
  //                                       oe = mi.operands_end();
  //            oi != oe; oi++) {
  //         MachineOperand &mo = *oi;
  //         if (mo.isReg() && (mo.getReg() == RISCV32::R0)) {
  //           if (mo.isDef()) {
  //             mo.ChangeToFrameIndex(0);
  //             loop = false;
  //             break;
  //           }
  //         }
  //       }
  //     }
  //   }
  // }
}

/// Change all function arguments of registers into stack elements with same
/// indexes of arguments.
void RISCV32ArgumentRaiser::updateParameterRegister(unsigned reg,
                                                MachineBasicBlock &mbb) {
  // for (MachineBasicBlock::iterator ii = mbb.begin(), ie = mbb.end(); ii != ie;
  //      ++ii) {
  //   MachineInstr &mi = *ii;
  //   for (MachineInstr::mop_iterator oi = mi.operands_begin(),
  //                                   oe = mi.operands_end();
  //        oi != oe; oi++) {
  //     MachineOperand &mo = *oi;
  //     if (mo.isReg() && (mo.getReg() == reg)) {
  //       if (mo.isUse()) {
  //         // The argument's index on frame starts from 1.
  //         // Such as R0 = 1, R1 = 2, R2 = 3, R3 = 4
  //         // For instance: R3 - R0 + 1 = 4
  //         mo.ChangeToFrameIndex(reg - RISCV32::R0 + 1);
  //       } else
  //         return;
  //     }
  //   }
  // }
}

/// Change rest of function arguments on stack frame into stack elements.
void RISCV32ArgumentRaiser::updateParameterFrame(MachineFunction &mf) {

  // for (MachineFunction::iterator mbbi = mf.begin(), mbbe = mf.end();
  //      mbbi != mbbe; ++mbbi) {
  //   MachineBasicBlock &mbb = *mbbi;

  //   for (MachineBasicBlock::iterator mii = mbb.begin(), mie = mbb.end();
  //        mii != mie; ++mii) {
  //     MachineInstr &mi = *mii;
  //     // Match pattern like ldr r1, [fp, #8].
  //     if (mi.getOpcode() == RISCV32::LDRi12 && mi.getNumOperands() > 2) {
  //       MachineOperand &mo = mi.getOperand(1);
  //       MachineOperand &mc = mi.getOperand(2);
  //       if (mo.isReg() && mo.getReg() == RISCV32::R11 && mc.isImm()) {
  //         // TODO: Need to check the imm is larger than 0 and it is align by
  //         // 4(32 bit).
  //         int imm = mc.getImm();
  //         if (imm >= 0) {
  //           int idx = imm / 4 - 2 + 5; // The index 0 is reserved to return
  //                                      // value. From 1 to 4 are the register
  //                                      // argument indices. Plus 5 to the index.
  //           mi.getOperand(1).ChangeToFrameIndex(idx);
  //           mi.RemoveOperand(2);
  //         }
  //       }
  //     }
  //   }
  // }
}

/// Move arguments which are passed by RISCV32 registers(R0 - R3) from function
/// arg.x to corresponding registers in entry block.
void RISCV32ArgumentRaiser::moveArgumentToRegister(unsigned Reg,
                                               MachineBasicBlock &PMBB) {
  const MCInstrDesc &mcInstrDesc = TII->get(RISCV::ADDI);
  MachineInstrBuilder builder = BuildMI(*MF, *(new DebugLoc()), mcInstrDesc);
  builder.addDef(Reg);
  builder.addFrameIndex(Reg - RISCV::X10 + 1);
  builder.addImm(0);
  PMBB.insert(PMBB.begin(), builder.getInstr());
}

/// updateParameterInstr - Using newly created stack elements replace relative
/// operands in MachineInstr.
void RISCV32ArgumentRaiser::updateParameterInstr(MachineFunction &mf) {
  Function *fn = getCRF();
  // Move arguments to corresponding registers.
  MachineBasicBlock &EntryMBB = mf.front();
  switch (fn->arg_size()) {
  default:
    // To do : implement argument larger than 8
    updateParameterFrame(mf);
    LLVM_FALLTHROUGH;
  case 8:
    moveArgumentToRegister(RISCV::X17, EntryMBB);
    LLVM_FALLTHROUGH;
  case 7:
    moveArgumentToRegister(RISCV::X16, EntryMBB);
    LLVM_FALLTHROUGH;
  case 6:
    moveArgumentToRegister(RISCV::X15, EntryMBB);
    LLVM_FALLTHROUGH;
  case 5:
    moveArgumentToRegister(RISCV::X14, EntryMBB);
    LLVM_FALLTHROUGH;
  case 4:
    moveArgumentToRegister(RISCV::X13, EntryMBB);
    LLVM_FALLTHROUGH;
  case 3:
    moveArgumentToRegister(RISCV::X12, EntryMBB);
    LLVM_FALLTHROUGH;
  case 2:
    moveArgumentToRegister(RISCV::X11, EntryMBB);
    LLVM_FALLTHROUGH;
  case 1:
    moveArgumentToRegister(RISCV::X10, EntryMBB);
    LLVM_FALLTHROUGH;
  case 0:
    break;
  }
}

bool RISCV32ArgumentRaiser::raiseArgs() {
  LLVM_DEBUG(dbgs() << "RISCV32ArgumentRaiser start.\n");

  Function *fn = getCRF();

  int argidx = 1;
  for (Function::arg_iterator argi = fn->arg_begin(), arge = fn->arg_end();
       argi != arge; ++argi)
    argi->setName("arg." + std::to_string(argidx++));

  for (unsigned i = 0, e = fn->arg_size() + 1; i < e; ++i) {
    Align ALG(32);
    MFI->CreateStackObject(32, ALG, false);
  }

  updateParameterInstr(*MF);

  // For debugging.
  // LLVM_DEBUG(MF->dump());
  // LLVM_DEBUG(getCRF()->dump());
  LLVM_DEBUG(dbgs() << "RISCV32ArgumentRaiser end.\n");

  return true;
}

bool RISCV32ArgumentRaiser::runOnMachineFunction(MachineFunction &mf) {
  bool rtn = false;
  init();
  rtn = raiseArgs();
  return rtn;
}

#undef DEBUG_TYPE

#ifdef __cplusplus
extern "C" {
#endif

FunctionPass *InitializeRISCV32ArgumentRaiser(RISCV32ModuleRaiser &mr) {
  return new RISCV32ArgumentRaiser(mr);
}

#ifdef __cplusplus
}
#endif
