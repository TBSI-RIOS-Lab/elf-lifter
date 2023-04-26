//===- RISCV32EliminatePrologEpilog.cpp - Binary raiser utility llvm-mctoll ---===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of RISCV32EliminatePrologEpilog class
// for use by llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#include "RISCV32EliminatePrologEpilog.h"
#include "RISCVSubtarget.h"
#include "llvm/Support/Debug.h"

#define DEBUG_TYPE "mctoll"

using namespace llvm;

char RISCV32EliminatePrologEpilog::ID = 0;

RISCV32EliminatePrologEpilog::RISCV32EliminatePrologEpilog(RISCV32ModuleRaiser &mr)
    : RISCV32RaiserBase(ID, mr) {}

RISCV32EliminatePrologEpilog::~RISCV32EliminatePrologEpilog() {}

void RISCV32EliminatePrologEpilog::init(MachineFunction *mf, Function *rf) {
  RISCV32RaiserBase::init(mf, rf);
}

/// Return true if an operand in the instrs vector matches the passed register
/// number, otherwise false.
bool RISCV32EliminatePrologEpilog::checkRegister(
    unsigned Reg, std::vector<MachineInstr *> &instrs) const {
  std::vector<MachineInstr *>::iterator it = instrs.begin();
  for (; it < instrs.end(); ++it) {
    MachineInstr *mi = *it;
    if (mi->mayStore()) {
      for (unsigned i = 0; i < mi->getNumOperands(); i++) {
        MachineOperand MO = mi->getOperand(i);

        // Compare the register number.
        if (MO.isReg() && MO.getReg() == Reg)
          return true;
      }
    }
  }
  return false;
}
/*
// Raise the function prolog.
//
// Look for the following instructions and eliminate them:
//       addi    sp,sp,-48
//       sw      ra,44(sp) # optional
//       sw      s0,40(sp) 
//       addi    s0,sp,48
//
//        
//       lw      ra, 44(sp) # optional
//       lw      s0, 40(sp)
//       addi    sp,sp,48
*/
bool RISCV32EliminatePrologEpilog::eliminateProlog(MachineFunction &MF) const {

  std::vector<MachineInstr *> prologInstrs;
  MachineBasicBlock &frontMBB = MF.front();

  const RISCVSubtarget &STI = MF.getSubtarget<RISCVSubtarget>();
  const RISCVRegisterInfo *RegInfo = STI.getRegisterInfo();
  // unsigned FramePtr = RegInfo->getFrameRegister(MF);

  for (MachineBasicBlock::iterator frontMBBIter = frontMBB.begin();
       frontMBBIter != frontMBB.end(); frontMBBIter++) {
    MachineInstr &curMachInstr = (*frontMBBIter);

    // Push the ADDI instruction
    // addi sp, sp, #imm ; This kind of patten ought to be eliminated.
    if (curMachInstr.getOpcode() == RISCV::ADDI &&
        curMachInstr.getOperand(0).getReg() == RISCV::X2 &&
        curMachInstr.getOperand(1).getReg() == RISCV::X2) {
      prologInstrs.push_back(&curMachInstr);
    }

    // Need to avoid the case of replacing global variables
    if(curMachInstr.getNumOperands() > 2 && curMachInstr.getOperand(1).isReg()){
      // sw s0/ra,#imm(sp), To do: need to restrict operand(0) ?
      if (curMachInstr.getOpcode() == RISCV::SW &&
          curMachInstr.getOperand(1).getReg() == RISCV::X2) {
          prologInstrs.push_back(&curMachInstr);
      }
    }
    // addi s0,sp,#imm
    if (curMachInstr.getOpcode() == RISCV::ADDI &&
        curMachInstr.getOperand(0).getReg() == RISCV::X8 &&
        curMachInstr.getOperand(1).getReg() == RISCV::X2) {
      prologInstrs.push_back(&curMachInstr);
    }
  }

  // Create the stack frame
  const TargetRegisterInfo *TRI = MF.getRegInfo().getTargetRegisterInfo();
  const MCPhysReg *CSRegs = TRI->getCalleeSavedRegs(&MF);

  std::vector<CalleeSavedInfo> CSI;
  for (unsigned i = 0; CSRegs[i]; ++i) {
    unsigned Reg = CSRegs[i];

    // Save register.
    if (checkRegister(Reg, prologInstrs)) {
      CSI.push_back(CalleeSavedInfo(Reg));
    }
  }

  const TargetFrameLowering *TFI = MF.getSubtarget().getFrameLowering();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  if (!TFI->assignCalleeSavedSpillSlots(MF, RegInfo, CSI)) {
    // If target doesn't implement this, use generic code.
    if (CSI.empty())
      return true; // Early exit if no callee saved registers are modified!

    unsigned NumFixedSpillSlots;
    const TargetFrameLowering::SpillSlot *FixedSpillSlots =
        TFI->getCalleeSavedSpillSlots(NumFixedSpillSlots);

    // Allocate stack slots for the registers that need to be saved and restored
    unsigned Offset = 0;
    for (auto &CS : CSI) {
      unsigned Reg = CS.getReg();
      const TargetRegisterClass *RC = RegInfo->getMinimalPhysRegClass(Reg);

      int FrameIdx;
      if (RegInfo->hasReservedSpillSlot(MF, Reg, FrameIdx)) {
        CS.setFrameIdx(FrameIdx);
        continue;
      }

      // Check if this physreg must be spilled to a particular stack slot for
      // this target
      const TargetFrameLowering::SpillSlot *FixedSlot = FixedSpillSlots;
      while (FixedSlot != FixedSpillSlots + NumFixedSpillSlots &&
             FixedSlot->Reg != Reg)
        ++FixedSlot;

      unsigned Size = RegInfo->getSpillSize(*RC);
      if (FixedSlot == FixedSpillSlots + NumFixedSpillSlots) {
        // Nope, just spill it anywhere convenient.
        Align Alignment(RegInfo->getSpillAlignment(*RC));

        // The alignment is the minimum of the desired alignment of the
        // TargetRegisterClass and the stack alignment, whichever is smaller.
        Alignment = std::min(Alignment, TFI->getStackAlign());
        FrameIdx = MFI.CreateStackObject(Size, Alignment, true);
        Offset += Size;

        // Set the object offset
        MFI.setObjectOffset(FrameIdx, MFI.getObjectOffset(FrameIdx) - Offset);
      } else {
        // Spill to the stack.
        FrameIdx = MFI.CreateFixedSpillStackObject(Size, FixedSlot->Offset);
      }

      // Set the frame index
      CS.setFrameIdx(FrameIdx);
    }
    MFI.setCalleeSavedInfo(CSI);
  }

  // Eliminate the instructions identified in function prologue
  unsigned int delInstSz = prologInstrs.size();
  for (unsigned int i = 0; i < delInstSz; i++) {
    frontMBB.erase(prologInstrs[i]);
  }

  return true;
}

bool RISCV32EliminatePrologEpilog::eliminateEpilog(MachineFunction &MF) const {
  // const RISCVSubtarget &STI = MF.getSubtarget<RISCV32Subtarget>();
  // const RISCVRegisterInfo *RegInfo = STI.getRegisterInfo();
  // const RISCVInstrInfo *TII = STI.getInstrInfo();
  //unsigned FramePtr = RegInfo->getFrameRegister(MF);

  for (MachineBasicBlock &MBB : MF) {
    std::vector<MachineInstr *> epilogInstrs;
    // MBBI may be invalidated by the raising operation.
    for (MachineBasicBlock::iterator backMBBIter = MBB.begin();
         backMBBIter != MBB.end(); backMBBIter++) {
      MachineInstr &curMachInstr = (*backMBBIter);

      
      // Need to avoid the case of replacing global variables -Xi
      if(curMachInstr.getNumOperands() > 2){
        // lw s0/ra,#imm(sp) To do: need to restrict operand(0) ?
        if (curMachInstr.getOpcode() == RISCV::LW &&
            curMachInstr.getOperand(1).isReg()) {
            if(curMachInstr.getOperand(1).getReg() == RISCV::X2)
                  epilogInstrs.push_back(&curMachInstr);
        }
      }

      // addi    sp,sp,#imm
      if (curMachInstr.getOpcode() == RISCV::ADDI &&
          curMachInstr.getOperand(0).getReg() == RISCV::X2 && 
          curMachInstr.getOperand(0).getReg() == RISCV::X2) {
        epilogInstrs.push_back(&curMachInstr);
      }
    }

    // Eliminate the instructions identified in function epilogue
    unsigned int delInstSz = epilogInstrs.size();
    for (unsigned int i = 0; i < delInstSz; i++) {
      MBB.erase(epilogInstrs[i]);
    }
  }

  return true;
}

/// Analyze stack size base on moving sp.
/// Patterns like:
/// addi sp, sp, -48
void RISCV32EliminatePrologEpilog::analyzeStackSize(MachineFunction &mf) {
  if (mf.size() < 1)
    return;

  const MachineBasicBlock &mbb = mf.front();

  for (const MachineInstr &mi : mbb.instrs()) {
    if (mi.getOpcode() == RISCV::ADDI && mi.getNumOperands() >= 3 &&
        mi.getOperand(0).isReg() && mi.getOperand(0).getReg() == RISCV::X2 &&
        mi.getOperand(1).isReg() && mi.getOperand(1).getReg() == RISCV::X2 &&
        mi.getOperand(2).isImm() && mi.getOperand(2).getImm() < 0) {
      mf.getFrameInfo().setStackSize(-mi.getOperand(2).getImm());
      break;
    }
  }
}

/// Analyze frame adjustment base on the offset between fp and base sp.
/// Patterns like:
/// addi s0, sp, 48
void RISCV32EliminatePrologEpilog::analyzeFrameAdjustment(MachineFunction &mf) {
  if (mf.size() < 1)
    return;

  const MachineBasicBlock &mbb = mf.front();

  for (const MachineInstr &mi : mbb.instrs()) {
    if (mi.getOpcode() == RISCV::ADDI && mi.getNumOperands() >= 3 &&
        mi.getOperand(0).isReg() && mi.getOperand(0).getReg() == RISCV::X8 &&
        mi.getOperand(1).isReg() && mi.getOperand(1).getReg() == RISCV::X2 &&
        mi.getOperand(2).isImm() && mi.getOperand(2).getImm() > 0) {
      mf.getFrameInfo().setOffsetAdjustment(mf.getFrameInfo().getStackSize() - mi.getOperand(2).getImm());
      LLVM_DEBUG(dbgs()<<"offsetadjustment="<<mf.getFrameInfo().getStackSize() - mi.getOperand(2).getImm()<<"\n");
      break;
    }
  }
}

bool RISCV32EliminatePrologEpilog::eliminate() {
  LLVM_DEBUG(dbgs() << "RISCV32EliminatePrologEpilog start\n");

  analyzeStackSize(*MF);
  LLVM_DEBUG(dbgs() << "DEBUG:: AnalyzeStackSize Completes\n");

  analyzeFrameAdjustment(*MF);
  LLVM_DEBUG(dbgs() << "DEBUG:: AnalyzeFrameAdjustment Completes\n");

  bool success = eliminateProlog(*MF);

  LLVM_DEBUG(dbgs() << "DEBUG:: eliminateProlog Completes\n");


  if (success) {
    success = eliminateEpilog(*MF);
  }
  LLVM_DEBUG(dbgs() << "DEBUG:: EliminateEpilog Completes\n");
  // For debugging.
  // LLVM_DEBUG(MF->dump());
  // LLVM_DEBUG(getCRF()->dump());
  LLVM_DEBUG(dbgs() << "RISCV32EliminatePrologEpilog end.\n");

  return !success;
}

bool RISCV32EliminatePrologEpilog::runOnMachineFunction(MachineFunction &mf) {
  bool rtn = false;
  init();
  rtn = eliminate();
  return rtn;
}

#undef DEBUG_TYPE

#ifdef __cplusplus
extern "C" {
#endif

FunctionPass *InitializeRISCV32EliminatePrologEpilog(RISCV32ModuleRaiser &mr) {
  return new RISCV32EliminatePrologEpilog(mr);
}

#ifdef __cplusplus
}
#endif
