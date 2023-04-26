//===- RISCV32CreateJumpTable.cpp - Binary raiser utility llvm-mctoll ---------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of RISCV32CreateJumpTable class
// for use by llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#include "RISCV32CreateJumpTable.h"
#include "RISCVInstrInfo.h"
#include "RISCVMachineFunctionInfo.h"
#include "RISCVSubtarget.h"
#include "llvm/CodeGen/ISDOpcodes.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineJumpTableInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Object/ELFObjectFile.h"
#include "llvm/Support/Debug.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

#define DEBUG_TYPE "mctoll"

using namespace llvm;

char RISCV32CreateJumpTable::ID = 0;

RISCV32CreateJumpTable::RISCV32CreateJumpTable(RISCV32ModuleRaiser &mr)
    : RISCV32RaiserBase(ID, mr) {}

RISCV32CreateJumpTable::~RISCV32CreateJumpTable() {}

void RISCV32CreateJumpTable::init(MachineFunction *mf, Function *rf) {
  RISCV32RaiserBase::init(mf, rf);
}

void RISCV32CreateJumpTable::setMCInstRaiser(MCInstRaiser *PMCIR) { MCIR = PMCIR; }

/// Get the MachineBasicBlock to add the jumptable instruction.
MachineBasicBlock *RISCV32CreateJumpTable::checkJumptableBB(MachineFunction &MF) {
  MachineBasicBlock *jumptableBB = nullptr;
  // for (MachineBasicBlock &MBB : MF) {
  //   for (MachineBasicBlock::iterator backMBBIter = MBB.begin();
  //        backMBBIter != MBB.end(); backMBBIter++) {
  //     MachineInstr &curMachInstr = (*backMBBIter);

  //     // Find the MI: %r0 = ADDri %pc
  //     if (curMachInstr.getOpcode() == RISCV32::ADDri &&
  //         curMachInstr.getOperand(1).getReg() == RISCV32::PC) {
  //       jumptableBB = curMachInstr.getParent();
  //     }
  //   }
  // }

  // // Remove the machine instructions which are no sense after building the
  // // machine jump table.
  // std::vector<MachineInstr *> Instrs;
  // if (jumptableBB && jumptableBB->pred_size() == 1) {
  //   MachineBasicBlock *mbb = jumptableBB->pred_begin()[0];
  //   for (MachineBasicBlock::iterator MIIter = mbb->begin();
  //        MIIter != mbb->end(); MIIter++) {
  //     MachineInstr &curMI = (*MIIter);

  //     if (curMI.getOpcode() == RISCV32::CMPri &&
  //         curMI.getNextNode()->getOpcode() == RISCV32::STRi12) {
  //       Instrs.push_back(curMI.getNextNode());
  //     }

  //     if (curMI.getOpcode() == RISCV32::STRi12 &&
  //         curMI.getNextNode()->getOpcode() == RISCV32::Bcc) {
  //       Instrs.push_back(curMI.getNextNode());
  //     }
  //   }

  //   for (unsigned int i = 0; i < Instrs.size(); i++) {
  //     mbb->erase(Instrs[i]);
  //   }
  // }
  return jumptableBB;
}

bool RISCV32CreateJumpTable::UpdatetheBranchInst(MachineBasicBlock &MBB) {
  // MachineFunction *MF = MBB.getParent();
  // const RISCV32Subtarget &STI = MF->getSubtarget<RISCV32Subtarget>();
  // const RISCV32BaseInstrInfo *TII = STI.getInstrInfo();

  // std::vector<MachineInstr *> Instrs;
  // for (MachineBasicBlock::iterator MIIter = MBB.begin(); MIIter != MBB.end();
  //      MIIter++) {
  //   MachineInstr &curMI = (*MIIter);

  //   if (curMI.getOpcode() == RISCV32::Bcc) {
  //     for (unsigned int i = 0; i < curMI.getNumOperands(); i++) {
  //       LLVM_DEBUG(curMI.getOperand(i).dump());
  //     }
  //     BuildMI(&MBB, DebugLoc(), TII->get(RISCV32::B)).add(curMI.getOperand(0));
  //     Instrs.push_back(&curMI);
  //   }
  // }

  // for (unsigned int i = 0; i < Instrs.size(); i++) {
  //   MBB.erase(Instrs[i]);
  // }
  return true;
}

/// Raise the machine jumptable according to the CFG.
bool RISCV32CreateJumpTable::raiseMaichineJumpTable(MachineFunction &MF) {
  // // A vector to record MBBs that need to be erased upon jump table creation.
  // std::vector<MachineBasicBlock *> MBBsToBeErased;

  // std::map<uint64_t, MCInstOrData> mcInstMapData;
  // MCInstRaiser::const_mcinst_iter iter_in;

  // // Save the ADDri and Calculate the start address of data.
  // for (MachineBasicBlock &JmpTblBaseCalcMBB : MF) {
  //   for (MachineBasicBlock::iterator CurMBBIter = JmpTblBaseCalcMBB.begin();
  //        CurMBBIter != JmpTblBaseCalcMBB.end(); CurMBBIter++) {
  //     MachineInstr &JmpTblOffsetCalcMI = *CurMBBIter;
  //     // Find the MI: %r0 = ADDri %pc, #8
  //     // add     r0, pc, #8
  //     // ldr     r1, [sp]
  //     // ldr     r2, [r0, r1, lsl #2]
  //     // add     pc, r0, r2

  //     To do: For arm, only find jump table when "add   r0, pc", #imm pattern exists, what is the pattern mean? in which case this pattern appears?
  //            and how to modify it for risc-v ? 

  //     if (JmpTblOffsetCalcMI.getOpcode() == RISCV32::ADDri &&
  //         JmpTblOffsetCalcMI.getOperand(1).getReg() == RISCV32::PC &&
  //         JmpTblOffsetCalcMI.getOperand(2).getImm() == 8) {
  //       // If the fourth instruction in swith block is "add pc, rm, rn",
  //       // this library should be built with "-fPIC".
  //       bool IsFPIC = false;
  //       MachineBasicBlock::iterator FourthInstr = CurMBBIter;
  //       std::advance(FourthInstr, 3);
  //       if (FourthInstr != JmpTblBaseCalcMBB.end()) {
  //         MachineInstr &JGPC = *FourthInstr;
  //         if (JGPC.getOpcode() == RISCV32::ADDrr &&
  //             JGPC.getOperand(0).getReg() == RISCV32::PC) {
  //           IsFPIC = true;
  //         }
  //       }

  //       // A vector of switch target MBBs
  //       std::vector<MachineBasicBlock *> JmpTgtMBBvec;
  //       assert(
  //           MCIR != nullptr &&
  //           "Current function machine instruction raiser wasn't initialized!");
  //       for (iter_in = MCIR->const_mcinstr_begin();
  //            iter_in != MCIR->const_mcinstr_end(); iter_in++) {
  //         MCInstOrData mcInstorData = iter_in->second;
  //         if (mcInstorData.isData() && mcInstorData.getData() > 0) {
  //           // The 16 is 8 + 8. The first 8 is the PC offset, the second 8 is
  //           // the immediate of current instruction.
  //           // If the current library is position-independent, the offset should
  //           // be CASE VALUE + PC + 8.
  //           // If the current library is not position-independent, the offset
  //           // should be CASE VALUE - text section address.
  //           uint64_t Offset =
  //               IsFPIC ? (mcInstorData.getData() +
  //                         MCIR->getMCInstIndex(JmpTblOffsetCalcMI) + 16)
  //                      : (mcInstorData.getData() - MR->getTextSectionAddress());
  //           auto MBBNo = MCIR->getMBBNumberOfMCInstOffset(Offset, MF);
  //           if (MBBNo != -1) {
  //             MachineBasicBlock *MBB = MF.getBlockNumbered(MBBNo);
  //             JmpTgtMBBvec.push_back(MBB);
  //           }
  //         }
  //       }

  //       // If no potential jump target addresses were found the current
  //       // instruction does not compute jump table base.
  //       if (JmpTgtMBBvec.size() == 0) {
  //         continue;
  //       }
  //       // Construct jump table. Current block is the block which would
  //       // potentially contain the start of jump targets. If current block has
  //       // multiple predecessors this may not be a jump table. For now assert
  //       // this to discover potential situations in binaries. Change the assert
  //       // to and continue if the assumption is correct.
  //       assert((JmpTblBaseCalcMBB.pred_size() == 1) &&
  //              "Expect a single predecessor during jump table discovery");
  //       MachineBasicBlock *JmpTblPredMBB = *(JmpTblBaseCalcMBB.pred_begin());
  //       // Predecessor block of current block (MBB) - which is jump table block
  //       // - is expected to have exactly two successors; one the current block
  //       // and the other which should become the default MBB for the switch.
  //       assert((JmpTblPredMBB->succ_size() == 2) &&
  //              "Unexpected number of successors of switch block");
  //       JumpTableInfo JmpTblInfo;
  //       // Set predecessor of current block as condition block of jump table
  //       // info
  //       JmpTblInfo.conditionMBB = JmpTblPredMBB;
  //       // Set default basic block in jump table info
  //       for (auto Succ : JmpTblPredMBB->successors()) {
  //         if (Succ != &JmpTblBaseCalcMBB) {
  //           JmpTblInfo.df_MBB = Succ;
  //           break;
  //         }
  //       }
  //       MachineJumpTableInfo *JTI =
  //           MF.getOrCreateJumpTableInfo(llvm::MachineJumpTableInfo::EK_Inline);
  //       JmpTblInfo.jtIdx = JTI->createJumpTableIndex(JmpTgtMBBvec);
  //       // Verify the branch instruction of JmpTblPredMBB is a conditional jmp
  //       // that uses eflags. Go to the most recent instruction that defines
  //       // eflags. Remove that instruction as well as any subsequent instruction
  //       // that uses the register defined by that instruction.
  //       MachineInstr &BranchInstr = JmpTblPredMBB->instr_back();
  //       std::vector<MachineInstr *> MBBInstrsToErase;
  //       if (BranchInstr.isConditionalBranch()) {
  //         // Walk the basic block backwards to find the most recent instruction
  //         // that implicitly defines eflags.
  //         bool EflagsModifierFound = false;
  //         MachineBasicBlock::reverse_instr_iterator CurInstrIter =
  //             JmpTblPredMBB->instr_rbegin();
  //         for (auto LastInstIter = JmpTblPredMBB->instr_rend();
  //              ((CurInstrIter != LastInstIter) && (!EflagsModifierFound));
  //              ++CurInstrIter) {
  //           MachineInstr &curInst = *CurInstrIter;
  //           if (curInst.getDesc().hasImplicitDefOfPhysReg(RISCV32::CPSR)) {
  //             EflagsModifierFound = true;
  //           }
  //         }
  //         assert(EflagsModifierFound &&
  //                "Failed to find eflags defining instruction during jump table "
  //                "extraction.");
  //         // Note: decrement CurInstrIter to point to the eflags modifying
  //         // instruction.
  //         CurInstrIter--;
  //         // Find the registers that the eflags modifying instruction defines.
  //         // Delete all instructions that uses them since we will be deleting
  //         // the eflags modifying instruction.
  //         MachineInstr &EflagsModInstr = *CurInstrIter;
  //         std::set<unsigned int> EflagsDefRegs;
  //         for (auto MO : EflagsModInstr.defs()) {
  //           // Create a set of all physical registers this instruction defines.
  //           if (MO.isReg()) {
  //             unsigned int DefReg = MO.getReg();
  //             if (Register::isPhysicalRegister(DefReg)) {
  //               EflagsDefRegs.insert(getRISCV32CPSR(DefReg));
  //             }
  //           }
  //         }
  //         // Add EflagsModInstr to the list of instructions to delete
  //         MBBInstrsToErase.push_back(&EflagsModInstr);

  //         MachineBasicBlock::iterator InstrEndIter = JmpTblPredMBB->instr_end();
  //         // Start walking the block instructions forward to identify
  //         // instructions that need be deleted.
  //         MachineBasicBlock::iterator InstrFwdIter =
  //             MachineBasicBlock::instr_iterator(CurInstrIter);
  //         // Find instructions that use any of the register in the set
  //         // EflagsDefRegs. Add it to a list of instructions that can be
  //         // deleted.
  //         while (InstrFwdIter != InstrEndIter) {
  //           MachineInstr &CurInstr = *InstrFwdIter;
  //           for (auto MO : CurInstr.uses()) {
  //             // Check if this use register is defined by EflagsModInstr
  //             if (MO.isReg()) {
  //               unsigned int UseReg = MO.getReg();
  //               if (Register::isPhysicalRegister(UseReg)) {
  //                 if (EflagsDefRegs.find(getRISCV32CPSR(UseReg)) !=
  //                     EflagsDefRegs.end()) {
  //                   MBBInstrsToErase.push_back(&CurInstr);
  //                   // No need to look for other register uses.
  //                   break;
  //                 }
  //               }
  //             }
  //           }
  //           // If this instruction redefines any of the registers, remove that
  //           // register from EflagsDefRegs. Any instruction that uses this
  //           // redefined register and follows the current instruction, should
  //           // not be deleted.
  //           for (auto MO : CurInstr.defs()) {
  //             if (MO.isReg()) {
  //               unsigned int DefReg = MO.getReg();
  //               if (Register::isPhysicalRegister(DefReg)) {
  //                 if (EflagsDefRegs.find(getRISCV32CPSR(DefReg)) !=
  //                     EflagsDefRegs.end()) {
  //                   EflagsDefRegs.erase(DefReg);
  //                 }
  //               }
  //             }
  //           }
  //           InstrFwdIter++;
  //         }
  //         // Finally add BranchInstr to the list of instructions to be
  //         // deleted
  //         MBBInstrsToErase.push_back(&BranchInstr);
  //         // BranchInstr.dump();
  //         // Now delete the instructions
  //         for (auto MI : MBBInstrsToErase) {
  //           JmpTblPredMBB->erase(MI);
  //         }
  //       }

  //       const RISCV32Subtarget &STI = MF.getSubtarget<RISCV32Subtarget>();
  //       const RISCV32BaseInstrInfo *TII = STI.getInstrInfo();
  //       MBBsToBeErased.push_back(&JmpTblBaseCalcMBB);
  //       MachineInstrBuilder MIB =
  //           BuildMI(JmpTblPredMBB, DebugLoc(), TII->get(RISCV32::BR_JTr))
  //               .addJumpTableIndex(JmpTblInfo.jtIdx);

  //       // The new machine instrucion should contain the metadata.
  //       // Create the metadata and add it to the machine instrucion.
  //       LLVMContext *CTX = &M->getContext();
  //       ConstantAsMetadata *CAM = ConstantAsMetadata::get(
  //           ConstantInt::get(*CTX, llvm::APInt(64, 0, false)));
  //       MDNode *MDnode = MDNode::get(*CTX, CAM);
  //       MIB.addMetadata(MDnode);
  //       jtList.push_back(JmpTblInfo);
  //     }
  //   }
  // }

  // // Delete MBBs
  // for (auto MBB : MBBsToBeErased) {
  //   MBB->eraseFromParent();
  // }
  return true;
}

unsigned int RISCV32CreateJumpTable::getRISCV32CPSR(unsigned int PhysReg) {
  // Get the RISCV32 CPSR.
  // if (PhysReg == RISCV32::CPSR) {
  //   return PhysReg;
  // }
  return -1;
}

bool RISCV32CreateJumpTable::getJTlist(std::vector<JumpTableInfo> &List) {
  List = jtList;
  return true;
}

bool RISCV32CreateJumpTable::create() {
  LLVM_DEBUG(dbgs() << "RISCV32CreateJumpTable start.\n");

  raiseMaichineJumpTable(*MF);

  // For debugging.
  // LLVM_DEBUG(MF->dump());
  // LLVM_DEBUG(getCRF()->dump());
  LLVM_DEBUG(dbgs() << "RISCV32CreateJumpTable end.\n");

  return false;
}

bool RISCV32CreateJumpTable::runOnMachineFunction(MachineFunction &mf) {
  bool rtn = false;
  init();
  rtn = create();
  return rtn;
}

#undef DEBUG_TYPE

#ifdef __cplusplus
extern "C" {
#endif

FunctionPass *InitializeRISCV32CreateJumpTable(RISCV32ModuleRaiser &mr) {
  return new RISCV32CreateJumpTable(mr);
}

#ifdef __cplusplus
}
#endif
