//===- RISCV32InstructionSplitting.cpp - Binary raiser utility llvm-mctoll ----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of RISCV32InstructionSplitting class
// for use by llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#include "RISCV32InstructionSplitting.h"
#include "RISCVInstrInfo.h"
#include "RISCVSubtarget.h"
// #include "MCTargetDesc/RISCVAddressingModes.h"
#include "llvm/CodeGen/MachineOperand.h"

#define DEBUG_TYPE "mctoll"

using namespace llvm;

char RISCV32InstructionSplitting::ID = 0;

RISCV32InstructionSplitting::RISCV32InstructionSplitting(RISCV32ModuleRaiser &mr)
    : RISCV32RaiserBase(ID, mr) {}

RISCV32InstructionSplitting::~RISCV32InstructionSplitting() {}

void RISCV32InstructionSplitting::init(MachineFunction *mf, Function *rf) {
  RISCV32RaiserBase::init(mf, rf);
  TII = MF->getSubtarget<RISCVSubtarget>().getInstrInfo();
  MRI = &MF->getRegInfo();
  CTX = &M->getContext();
}

// /// Check if the MI has shift pattern.
// unsigned RISCV32InstructionSplitting::checkisShifter(unsigned Opcode) {
//   switch (Opcode) {
//   case RISCV32::MOVsr:
//   case RISCV32::MOVsi:
//     return RISCV32::MOVr;
//   case RISCV32::ADCrsi:
//   case RISCV32::ADCrsr:
//     return RISCV32::ADCrr;
//   case RISCV32::ADDrsi:
//   case RISCV32::ADDrsr:
//     return RISCV32::ADDrr;
//   case RISCV32::ANDrsi:
//   case RISCV32::ANDrsr:
//     return RISCV32::ANDrr;
//   case RISCV32::BICrsr:
//   case RISCV32::BICrsi:
//     return RISCV32::BICrr;
//   case RISCV32::CMNzrsi:
//   case RISCV32::CMNzrsr:
//     return RISCV32::CMNzrr;
//   case RISCV32::CMPrsi:
//   case RISCV32::CMPrsr:
//     return RISCV32::CMPrr;
//   case RISCV32::EORrsr:
//   case RISCV32::EORrsi:
//     return RISCV32::EORrr;
//   case RISCV32::MVNsr:
//   case RISCV32::MVNsi:
//     return RISCV32::MVNr;
//   case RISCV32::ORRrsi:
//   case RISCV32::ORRrsr:
//     return RISCV32::ORRrr;
//   case RISCV32::RSBrsi:
//   case RISCV32::RSBrsr:
//     return RISCV32::RSBrr;
//   case RISCV32::SUBrsi:
//   case RISCV32::SUBrsr:
//     return RISCV32::SUBrr;
//   case RISCV32::TEQrsr:
//   case RISCV32::TEQrsi:
//     return RISCV32::TEQrr;
//   case RISCV32::TSTrsr:
//   case RISCV32::TSTrsi:
//     return RISCV32::TSTrr;
//   default:
//     return 0;
//   }
// }

// /// If the MI is load/store which needs wback, it will return true.
// bool RISCV32InstructionSplitting::isLDRSTRPre(unsigned Opcode) {
//   switch (Opcode) {
//   case RISCV32::LDR_PRE_REG:
//   case RISCV32::LDR_PRE_IMM:
//   case RISCV32::LDRB_PRE_REG:
//   case RISCV32::LDRB_PRE_IMM:
//   case RISCV32::STR_PRE_REG:
//   case RISCV32::STR_PRE_IMM:
//   case RISCV32::STRB_PRE_REG:
//   case RISCV32::STRB_PRE_IMM:
//     return true;
//   default:
//     return false;
//   }
// }

// /// No matter what pattern of Load/Store is, change the Opcode to xxxi12.
// unsigned RISCV32InstructionSplitting::getLoadStoreOpcode(unsigned Opcode) {
//   // switch (Opcode) {
//   // case RISCV32::LDRrs:
//   // case RISCV32::LDRi12:
//   // case RISCV32::LDR_PRE_REG:
//   // case RISCV32::LDR_PRE_IMM:
//   //   return RISCV32::LDRi12;
//   // case RISCV32::LDRBrs:
//   // case RISCV32::LDRBi12:
//   // case RISCV32::LDRB_PRE_REG:
//   // case RISCV32::LDRB_PRE_IMM:
//   //   return RISCV32::LDRBi12;
//   // case RISCV32::STRrs:
//   // case RISCV32::STRi12:
//   // case RISCV32::STR_PRE_REG:
//   // case RISCV32::STR_PRE_IMM:
//   //   return RISCV32::STRi12;
//   // case RISCV32::STRBrs:
//   // case RISCV32::STRBi12:
//   // case RISCV32::STRB_PRE_REG:
//   // case RISCV32::STRB_PRE_IMM:
//   //   return RISCV32::STRBi12;
//   // default:
//   //   return 0;
//   // }

//   switch (Opcode) {
//   case RISCV::LW:
//     return RISCV::LW;
//   case RISCV::SW:
//     return RISCV::SW;
//   default:
//     return 0;
//   }
// }

// /// True if the RISCV32 instruction performs Shift_C().
// bool RISCV32InstructionSplitting::isShift_C(unsigned Opcode) {
//   switch (Opcode) {
//   case RISCV32::ANDrsr:
//   case RISCV32::ANDrsi:
//   case RISCV32::BICrsr:
//   case RISCV32::BICrsi:
//   case RISCV32::EORrsr:
//   case RISCV32::EORrsi:
//   case RISCV32::MVNsr:
//   case RISCV32::MVNsi:
//   case RISCV32::ORRrsr:
//   case RISCV32::ORRrsi:
//   case RISCV32::TEQrsr:
//   case RISCV32::TEQrsi:
//   case RISCV32::TSTrsr:
//   case RISCV32::TSTrsi:
//     return true;
//   default:
//     return false;
//   }
// }

// /// Get the shift opcode in MI.
// unsigned RISCV32InstructionSplitting::getShiftOpcode(RISCV32_AM::ShiftOpc SOpc,
//                                                  unsigned OffSet) {
//   switch (SOpc) {
//   case RISCV32_AM::asr: {
//     if (OffSet != 0)
//       return RISCV32::ASRi;
//     else
//       return RISCV32::ASRr;
//   }
//   case RISCV32_AM::lsl: {
//     if (OffSet != 0)
//       return RISCV32::LSLi;
//     else
//       return RISCV32::LSLr;
//   }
//   case RISCV32_AM::lsr: {
//     if (OffSet != 0)
//       return RISCV32::LSRi;
//     else
//       return RISCV32::LSRr;
//   }
//   case RISCV32_AM::ror: {
//     if (OffSet != 0)
//       return RISCV32::RORi;
//     else
//       return RISCV32::RORr;
//   }
//   case RISCV32_AM::rrx:
//     return RISCV32::RRX;
//   case RISCV32_AM::no_shift:
//   default:
//     return 0;
//   }
// }

// MachineInstrBuilder &
// RISCV32InstructionSplitting::addOperand(MachineInstrBuilder &mib,
//                                     MachineOperand &mo, bool isDef) {
//   switch (mo.getType()) {
//   default:
//     assert(false && "Unsupported MachineOperand type!");
//     break;
//   case MachineOperand::MO_Register: {
//     if (isDef)
//       mib.addDef(mo.getReg());
//     else
//       mib.addUse(mo.getReg());
//   } break;
//   case MachineOperand::MO_FrameIndex: {
//     mib.addFrameIndex(mo.getIndex());
//   } break;
//   }

//   return mib;
// }

// /// Split LDRxxx/STRxxx<c><q> <Rt>, [<Rn>, #+/-<imm>]! to:
// /// ADD Rn, Rn, #imm
// /// LDRxxx/STRxxx Rt, [Rn]
// MachineInstr *RISCV32InstructionSplitting::splitLDRSTRPreImm(MachineBasicBlock &MBB,
//                                                          MachineInstr &MI) {
//   MachineOperand &Rd = MI.getOperand(0);
//   MachineOperand &Rn = MI.getOperand(1);
//   MachineOperand &Rm = MI.getOperand(2);
//   MachineOperand &Rs = MI.getOperand(3);

//   // MI is splitted into 2 instructions.
//   // So get Metadata for the first instruction.
//   ConstantAsMetadata *CMD_fst = ConstantAsMetadata::get(
//       ConstantInt::get(*CTX, llvm::APInt(64, 0, false)));
//   MDNode *N_fst = MDNode::get(*CTX, CMD_fst);

//   // Get Metadata for the second instruction.
//   ConstantAsMetadata *CMD_sec = ConstantAsMetadata::get(
//       ConstantInt::get(*CTX, llvm::APInt(64, 1, false)));
//   MDNode *N_sec = MDNode::get(*CTX, CMD_sec);

//   unsigned newOpc = getLoadStoreOpcode(MI.getOpcode());
//   // Add Rm,[Rm, #imm]!
//   MachineInstrBuilder fst =
//       BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(RISCV32::ADDrr));
//   addOperand(fst, Rm, true);
//   addOperand(fst, Rm);
//   fst.addImm(Rs.getImm());

//   MachineInstrBuilder sec =
//       BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//   if (MI.mayStore())
//     // STRxxx Rn, [Rm]
//     addOperand(sec, Rn);
//   else if (MI.mayLoad())
//     // LDRxxx Rd, [Rm]
//     addOperand(sec, Rd, true);
//   addOperand(sec, Rm);

//   int idx = MI.findRegisterUseOperandIdx(RISCV32::CPSR);
//   // Add CPSR if the MI has.
//   if (idx != -1) {
//     fst.addImm(MI.getOperand(idx - 1).getImm());
//     addOperand(fst, MI.getOperand(idx));
//     sec.addImm(MI.getOperand(idx - 1).getImm());
//     addOperand(sec, MI.getOperand(idx));
//   }
//   fst.addMetadata(N_fst);
//   sec.addMetadata(N_sec);
//   return &MI;
// }

// /// Split LDRxxx/STRxxx<c><q> <Rt>, [<Rn>, +/-<Rm>{, <shift>}]! to:
// /// Rm shift #imm, but write result to VReg.
// /// Add Rn, Rm
// /// LDRxxx/STRxxx Rt, [Rn]
// MachineInstr *RISCV32InstructionSplitting::splitLDRSTRPre(MachineBasicBlock &MBB,
//                                                       MachineInstr &MI) {
//   unsigned Simm = MI.getOperand(4).getImm();
//   unsigned SOffSet = RISCV32_AM::getAM2Offset(Simm);
//   RISCV32_AM::ShiftOpc SOpc = RISCV32_AM::getAM2ShiftOpc(Simm);
//   unsigned SVReg = MRI->createVirtualRegister(&RISCV32::GPRnopcRegClass);

//   MachineOperand &Rd = MI.getOperand(0);
//   MachineOperand &Rn = MI.getOperand(1);
//   MachineOperand &Rm = MI.getOperand(2);
//   MachineOperand &Rs = MI.getOperand(3);
//   unsigned ShiftOpc = getShiftOpcode(SOpc, SOffSet);

//   // Get Metadata for the first instruction.
//   ConstantAsMetadata *CMD_fst = ConstantAsMetadata::get(
//       ConstantInt::get(*CTX, llvm::APInt(64, 0, false)));
//   MDNode *N_fst = MDNode::get(*CTX, CMD_fst);

//   // Get Metadata for the second instruction.
//   ConstantAsMetadata *CMD_sec = ConstantAsMetadata::get(
//       ConstantInt::get(*CTX, llvm::APInt(64, 1, false)));
//   MDNode *N_sec = MDNode::get(*CTX, CMD_sec);

//   // Get Metadata for the third instruction.
//   ConstantAsMetadata *CMD_thd = ConstantAsMetadata::get(
//       ConstantInt::get(*CTX, llvm::APInt(64, 2, false)));
//   MDNode *N_thd = MDNode::get(*CTX, CMD_thd);

//   unsigned newOpc = getLoadStoreOpcode(MI.getOpcode());
//   int idx = MI.findRegisterUseOperandIdx(RISCV32::CPSR);
//   if (SOffSet > 0) {
//     // LDRxxx/STRxxx<c><q> <Rt>, [<Rn>, +/-<Rm>{, <shift>}]!

//     // Rs shift #imm and write result to VReg.
//     MachineInstrBuilder fst =
//         BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), SVReg);
//     addOperand(fst, Rs);
//     fst.addImm(SOffSet);

//     // Add Rm, VReg
//     MachineInstrBuilder sec =
//         BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(RISCV32::ADDrr));
//     addOperand(sec, Rm, true);
//     addOperand(sec, Rm);
//     sec.addReg(SVReg);

//     MachineInstrBuilder thd =
//         BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//     if (MI.mayStore())
//       // STRxxx Rn, [Rm]
//       addOperand(thd, Rn);
//     else if (MI.mayLoad())
//       // LDRxxx Rd, [Rm]
//       addOperand(thd, Rd, true);
//     addOperand(thd, Rm);

//     // Add CPSR if the MI has.
//     if (idx != -1) {
//       fst.addImm(MI.getOperand(idx - 1).getImm());
//       addOperand(fst, MI.getOperand(idx));
//       sec.addImm(MI.getOperand(idx - 1).getImm());
//       addOperand(sec, MI.getOperand(idx));
//       thd.addImm(MI.getOperand(idx - 1).getImm());
//       addOperand(thd, MI.getOperand(idx));
//     }
//     fst.addMetadata(N_fst);
//     sec.addMetadata(N_sec);
//     thd.addMetadata(N_thd);
//   } else if (ShiftOpc == RISCV32::RRX) {
//     // Split LDRxxx/STRxxx<c><q> <Rt>, [<Rn>, +/-<Rm>, RRX]!
//     MachineInstrBuilder fst =
//         BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), SVReg);
//     addOperand(fst, Rs);

//     MachineInstrBuilder sec =
//         BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(RISCV32::ADDrr));
//     addOperand(sec, Rm, true);
//     addOperand(sec, Rm);
//     sec.addReg(SVReg);

//     MachineInstrBuilder thd =
//         BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//     if (MI.mayStore())
//       addOperand(thd, Rn);
//     else if (MI.mayLoad())
//       addOperand(thd, Rd, true);
//     addOperand(thd, Rm);

//     // Add CPSR if the MI has.
//     if (idx != -1) {
//       sec.addImm(MI.getOperand(idx - 1).getImm());
//       addOperand(sec, MI.getOperand(idx));
//       thd.addImm(MI.getOperand(idx - 1).getImm());
//       addOperand(thd, MI.getOperand(idx));
//     }
//     fst.addMetadata(N_fst);
//     sec.addMetadata(N_sec);
//     thd.addMetadata(N_thd);
//   } else {
//     // Split LDRxxx/STRxxx<c><q> <Rt>, [<Rn>, +/-<Rm>]!
//     MachineInstrBuilder fst =
//         BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(RISCV32::ADDrr));
//     addOperand(fst, Rm, true);
//     addOperand(fst, Rm);
//     addOperand(fst, Rs);

//     MachineInstrBuilder sec =
//         BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//     if (MI.mayStore())
//       addOperand(sec, Rn);
//     else if (MI.mayLoad())
//       addOperand(sec, Rd, true);
//     addOperand(sec, Rm);

//     // Add CPSR if the MI has.
//     if (idx != -1) {
//       fst.addImm(MI.getOperand(idx - 1).getImm());
//       addOperand(fst, MI.getOperand(idx));
//       sec.addImm(MI.getOperand(idx - 1).getImm());
//       addOperand(sec, MI.getOperand(idx));
//     }
//     fst.addMetadata(N_fst);
//     sec.addMetadata(N_sec);
//   }
//   return &MI;
// }

// /// Split LDRxxx/STRxxx<c><q> <Rd>, [<Rn>, +/-<#imm>] to:
// /// Add VReg, Rn, #imm
// /// LDRxxx/STRxxx Rd, [VReg]
// MachineInstr *RISCV32InstructionSplitting::splitLDRSTRImm(MachineBasicBlock &MBB,
//                                                       MachineInstr &MI) {
//   unsigned VReg = MRI->createVirtualRegister(&RISCV32::GPRnopcRegClass);
//   MachineOperand &Rd = MI.getOperand(0);
//   MachineOperand &Rn = MI.getOperand(1);
//   MachineOperand &Rm = MI.getOperand(2);

//   // The MI is splitted into 2 instructions.
//   // Get Metadata for the first instruction.
//   ConstantAsMetadata *CMD_fst = ConstantAsMetadata::get(
//       ConstantInt::get(*CTX, llvm::APInt(64, 0, false)));
//   MDNode *N_fst = MDNode::get(*CTX, CMD_fst);

//   // Get Metadata for the first instruction.
//   ConstantAsMetadata *CMD_sec = ConstantAsMetadata::get(
//       ConstantInt::get(*CTX, llvm::APInt(64, 1, false)));
//   MDNode *N_sec = MDNode::get(*CTX, CMD_sec);

//   unsigned newOpc = getLoadStoreOpcode(MI.getOpcode());
//   // Add VReg, Rn, #imm
//   MachineInstrBuilder fst =
//       BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(RISCV32::ADDrr), VReg);
//   addOperand(fst, Rn);
//   fst.addImm(Rm.getImm());

//   // LDRxxx/STRxxx Rd, [VReg]
//   MachineInstrBuilder sec =
//       BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//   if (MI.mayStore())
//     addOperand(sec, Rd);
//   else
//     addOperand(sec, Rd, true);
//   sec.addReg(VReg);

//   int idx = MI.findRegisterUseOperandIdx(RISCV32::CPSR);
//   // Add CPSR if the MI has.
//   if (idx != -1) {
//     fst.addImm(MI.getOperand(idx - 1).getImm());
//     addOperand(fst, MI.getOperand(idx));
//     sec.addImm(MI.getOperand(idx - 1).getImm());
//     addOperand(sec, MI.getOperand(idx));
//   }
//   fst.addMetadata(N_fst);
//   sec.addMetadata(N_sec);
//   return &MI;
// }

// /// Split LDRxxx/STRxxx<c><q> <Rd>, [<Rn>, +/-<Rm>{, <shift>}] to:
// /// Rm shift #imm, but write result to VReg.
// /// Add VReg, Rn, Rm
// /// LDRxxx/STRxxx Rd, [VReg]
// MachineInstr *RISCV32InstructionSplitting::splitLDRSTR(MachineBasicBlock &MBB,
//                                                    MachineInstr &MI) {
//   unsigned Simm = MI.getOperand(3).getImm();
//   unsigned SOffSet = RISCV32_AM::getAM2Offset(Simm);
//   RISCV32_AM::ShiftOpc SOpc = RISCV32_AM::getAM2ShiftOpc(Simm);
//   unsigned SVReg = MRI->createVirtualRegister(&RISCV32::GPRnopcRegClass);
//   unsigned AVReg = MRI->createVirtualRegister(&RISCV32::GPRnopcRegClass);

//   MachineOperand &Rd = MI.getOperand(0);
//   MachineOperand &Rn = MI.getOperand(1);
//   MachineOperand &Rm = MI.getOperand(2);
//   unsigned ShiftOpc = getShiftOpcode(SOpc, SOffSet);

//   // Get Metadata for the fisrt insturction.
//   ConstantAsMetadata *CMD_fst = ConstantAsMetadata::get(
//       ConstantInt::get(*CTX, llvm::APInt(64, 0, false)));
//   MDNode *N_fst = MDNode::get(*CTX, CMD_fst);

//   // Get Metadata for the second insturction.
//   ConstantAsMetadata *CMD_sec = ConstantAsMetadata::get(
//       ConstantInt::get(*CTX, llvm::APInt(64, 1, false)));
//   MDNode *N_sec = MDNode::get(*CTX, CMD_sec);

//   // Get Metadata for the third insturction.
//   ConstantAsMetadata *CMD_thd = ConstantAsMetadata::get(
//       ConstantInt::get(*CTX, llvm::APInt(64, 2, false)));
//   MDNode *N_thd = MDNode::get(*CTX, CMD_thd);

//   unsigned newOpc = getLoadStoreOpcode(MI.getOpcode());
//   int idx = MI.findRegisterUseOperandIdx(RISCV32::CPSR);
//   if (SOffSet > 0) {
//     // Split LDRxxx/STRxxx Rd, [Rn, Rm, shift]
//     MachineInstrBuilder fst =
//         BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), SVReg);
//     addOperand(fst, Rm);
//     fst.addImm(SOffSet);

//     MachineInstrBuilder sec =
//         BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(RISCV32::ADDrr), AVReg);
//     addOperand(sec, Rn);
//     sec.addReg(SVReg);

//     MachineInstrBuilder thd =
//         BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//     if (MI.mayStore())
//       addOperand(thd, Rd);
//     else
//       addOperand(thd, Rd, true);
//     thd.addReg(AVReg);
//     // Add CPSR if the MI has.
//     if (idx != -1) {
//       fst.addImm(MI.getOperand(idx - 1).getImm());
//       addOperand(fst, MI.getOperand(idx));
//       sec.addImm(MI.getOperand(idx - 1).getImm());
//       addOperand(sec, MI.getOperand(idx));
//       thd.addImm(MI.getOperand(idx - 1).getImm());
//       addOperand(thd, MI.getOperand(idx));
//     }
//     fst.addMetadata(N_fst);
//     sec.addMetadata(N_sec);
//     thd.addMetadata(N_thd);
//   } else if (ShiftOpc == RISCV32::RRX) {
//     // Split LDRxxx/STRxxx Rd, [Rn, Rm, rrx]
//     MachineInstrBuilder fst =
//         BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), SVReg);
//     addOperand(fst, Rm);

//     MachineInstrBuilder sec =
//         BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(RISCV32::ADDrr), AVReg);
//     addOperand(sec, Rn);
//     sec.addReg(SVReg);

//     MachineInstrBuilder thd =
//         BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//     if (MI.mayStore())
//       addOperand(thd, Rd);
//     else
//       addOperand(thd, Rd, true);
//     thd.addReg(AVReg);
//     // Add CPSR if the MI has.
//     if (idx != -1) {
//       sec.addImm(MI.getOperand(idx - 1).getImm());
//       addOperand(sec, MI.getOperand(idx));
//       thd.addImm(MI.getOperand(idx - 1).getImm());
//       addOperand(thd, MI.getOperand(idx));
//     }
//     fst.addMetadata(N_fst);
//     sec.addMetadata(N_sec);
//     thd.addMetadata(N_thd);
//   } else {
//     // Split LDRxxx/STRxxx Rd, [Rn, Rm]
//     MachineInstrBuilder fst =
//         BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(RISCV32::ADDrr), AVReg);
//     addOperand(fst, Rn);
//     addOperand(fst, Rm);

//     MachineInstrBuilder sec =
//         BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//     if (MI.mayStore())
//       addOperand(sec, Rd);
//     else
//       addOperand(sec, Rd, true);
//     sec.addReg(AVReg);
//     // Add CPSR if the MI has.
//     if (idx != -1) {
//       fst.addImm(MI.getOperand(idx - 1).getImm());
//       addOperand(fst, MI.getOperand(idx));
//       sec.addImm(MI.getOperand(idx - 1).getImm());
//       addOperand(sec, MI.getOperand(idx));
//     }
//     fst.addMetadata(N_fst);
//     sec.addMetadata(N_sec);
//   }
//   return &MI;
// }

// /// Split 'Opcode Rd, Rn, Rm, shift' except LDRxxx/STRxxx.
// MachineInstr *RISCV32InstructionSplitting::splitCommon(MachineBasicBlock &MBB,
//                                                    MachineInstr &MI,
//                                                    unsigned newOpc) {
//   MachineInstr *mi = nullptr;
//   for (unsigned i = 0; i < MI.getNumOperands(); i++) {
//     if (MI.getOperand(i).isImm()) {
//       unsigned Simm = MI.getOperand(i).getImm();
//       unsigned SOffSet = RISCV32_AM::getSORegOffset(Simm);
//       RISCV32_AM::ShiftOpc SOpc = RISCV32_AM::getSORegShOp(Simm);
//       unsigned ShiftOpc = getShiftOpcode(SOpc, SOffSet);

//       unsigned VReg = MRI->createVirtualRegister(&RISCV32::GPRnopcRegClass);
//       if (ShiftOpc) {
//         MachineOperand &Rd = MI.getOperand(0);
//         MachineOperand &Rn = MI.getOperand(i - 2);
//         MachineOperand &Rm = MI.getOperand(i - 1);

//         ConstantAsMetadata *CMD_fst = ConstantAsMetadata::get(
//             ConstantInt::get(*CTX, llvm::APInt(64, 0, false)));
//         MDNode *N_fst = MDNode::get(*CTX, CMD_fst);

//         ConstantAsMetadata *CMD_sec = ConstantAsMetadata::get(
//             ConstantInt::get(*CTX, llvm::APInt(64, 1, false)));
//         MDNode *N_sec = MDNode::get(*CTX, CMD_sec);

//         if (SOffSet) {
//           // Split Opcode Rd, Rn, Rm, shift #imm

//           // Rm shifts SOffset and writes result to VReg.
//           MachineInstrBuilder fst =
//               BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), VReg);
//           addOperand(fst, Rm);
//           fst.addImm(SOffSet);
//           fst.addMetadata(N_fst);

//           // Build 'opcode Rd, Rn, VReg'
//           MachineInstrBuilder sec =
//               BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//           addOperand(sec, Rd, true);
//           for (unsigned n = 1; n < (i - 1); n++) {
//             addOperand(sec, MI.getOperand(n));
//           }
//           sec.addReg(VReg);
//           sec.addMetadata(N_sec);
//         } else {
//           if (ShiftOpc == RISCV32::RRX) {
//             // Split 'opcode Rd, Rn, Rm, RRX'
//             MachineInstrBuilder fst =
//                 BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), VReg);
//             addOperand(fst, Rm);
//             fst.addMetadata(N_fst);

//             MachineInstrBuilder sec =
//                 BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//             addOperand(sec, Rd, true);

//             for (unsigned n = 1; n < i - 1; n++) {
//               addOperand(sec, MI.getOperand(n));
//             }
//             sec.addReg(VReg);
//             sec.addMetadata(N_sec);
//           } else {
//             // Split 'opcode Rd, Rn, Rm, shift Rs'

//             // Build 'ShiftOpc VReg, Rn, Rm'
//             MachineInstrBuilder fst =
//                 BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), VReg);
//             addOperand(fst, Rn);
//             addOperand(fst, Rm);
//             fst.addMetadata(N_fst);

//             // Build 'opcode Rd, Rn, VReg'
//             MachineInstrBuilder sec =
//                 BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//             addOperand(sec, Rd, true);

//             for (unsigned n = 1; n < (i - 2); n++) {
//               addOperand(sec, MI.getOperand(n));
//             }
//             sec.addReg(VReg);
//             sec.addMetadata(N_sec);
//           }
//         }
//         mi = &MI;
//         break;
//       }
//     }
//   }

//   return mi;
// }

// /// Split 'opcode<s> Rd, Rn, Rm, shift' except LDRxxx/STRxxx.
// MachineInstr *RISCV32InstructionSplitting::splitS(MachineBasicBlock &MBB,
//                                               MachineInstr &MI, unsigned newOpc,
//                                               int idx) {
//   MachineInstr *mi = nullptr;
//   for (unsigned i = 0; i < MI.getNumOperands(); i++) {
//     if (MI.getOperand(i).isImm()) {
//       unsigned Simm = MI.getOperand(i).getImm();
//       unsigned SOffSet = RISCV32_AM::getSORegOffset(Simm);
//       RISCV32_AM::ShiftOpc SOpc = RISCV32_AM::getSORegShOp(Simm);
//       unsigned ShiftOpc = getShiftOpcode(SOpc, SOffSet);
//       unsigned VReg = MRI->createVirtualRegister(&RISCV32::GPRnopcRegClass);

//       if (ShiftOpc) {
//         ConstantAsMetadata *CMD_fst = ConstantAsMetadata::get(
//             ConstantInt::get(*CTX, llvm::APInt(64, 0, false)));
//         MDNode *N_fst = MDNode::get(*CTX, CMD_fst);

//         ConstantAsMetadata *CMD_sec = ConstantAsMetadata::get(
//             ConstantInt::get(*CTX, llvm::APInt(64, 1, false)));
//         MDNode *N_sec = MDNode::get(*CTX, CMD_sec);

//         MachineOperand &Rd = MI.getOperand(0);
//         MachineOperand &Rn = MI.getOperand(i - 2);
//         MachineOperand &Rm = MI.getOperand(i - 1);

//         // C flag is affected by Shift_c() if isShift_C is true.
//         if (isShift_C(MI.getOpcode())) {
//           if (SOffSet) {
//             // Split opcode<s> Rd, Rn, Rm, shift #imm.

//             // Rm shift #imm and  the new MI updates CPSR.
//             MachineInstrBuilder fst =
//                 BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), VReg);
//             addOperand(fst, Rm);
//             fst.addImm(SOffSet);
//             fst.addImm(RISCV32CC::AL);
//             addOperand(fst, MI.getOperand(idx));
//             fst.addMetadata(N_fst);

//             // Build 'opcode<s> Rd, Rn, VReg'
//             // The new MI updates CPSR.
//             MachineInstrBuilder sec =
//                 BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//             addOperand(sec, Rd, true);
//             for (unsigned n = 1; n < (i - 1); n++) {
//               addOperand(sec, MI.getOperand(n));
//             }
//             sec.addReg(VReg);
//             sec.addImm(RISCV32CC::AL);
//             addOperand(sec, MI.getOperand(idx));
//             sec.addMetadata(N_sec);
//           } else {
//             if (ShiftOpc == RISCV32::RRX) {
//               // Split opcode<s> Rd, Rn, Rm, RRX.
//               MachineInstrBuilder fst =
//                   BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), VReg);
//               addOperand(fst, Rm);
//               fst.addMetadata(N_fst);
//               // XXX: RRX implicit CPSR, how to add cpsr?

//               // Build base instructions
//               MachineInstrBuilder sec =
//                   BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//               addOperand(sec, Rd, true);

//               for (unsigned n = 1; n < (i - 1); n++) {
//                 addOperand(sec, MI.getOperand(n));
//               }
//               sec.addReg(VReg);
//               sec.addImm(RISCV32CC::AL);
//               addOperand(sec, MI.getOperand(idx));
//               sec.addMetadata(N_sec);
//             } else {
//               // Split opcode<s> Rd, Rn, Rm, shift Rs.
//               // The new MI updates CPSR.
//               MachineInstrBuilder fst =
//                   BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), VReg);
//               addOperand(fst, Rn);
//               addOperand(fst, Rm);
//               fst.addImm(RISCV32CC::AL);
//               addOperand(fst, MI.getOperand(idx));
//               fst.addMetadata(N_fst);

//               MachineInstrBuilder sec =
//                   BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//               addOperand(sec, Rd, true);

//               for (unsigned n = 1; n < (i - 2); n++) {
//                 addOperand(sec, MI.getOperand(n));
//               }
//               sec.addReg(VReg);
//               sec.addImm(RISCV32CC::AL);
//               addOperand(sec, MI.getOperand(idx));
//               sec.addMetadata(N_sec);
//             }
//           }
//         } else {
//           if (SOffSet) {
//             // Split opcode<s> Rd, Rn, Rm, shift #imm.

//             // Rm shift #imm,  and the new MI doesn't update CPSR.
//             MachineInstrBuilder fst =
//                 BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), VReg);
//             addOperand(fst, Rm);
//             fst.addImm(SOffSet);
//             fst.addMetadata(N_fst);

//             // Build 'opcode<s> Rd, Rn, VReg'
//             // The new MI updates CPSR.
//             MachineInstrBuilder sec =
//                 BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//             addOperand(sec, Rd, true);
//             for (unsigned n = 1; n < (i - 1); n++) {
//               addOperand(sec, MI.getOperand(n));
//             }
//             sec.addReg(VReg);
//             sec.addImm(RISCV32CC::AL);
//             addOperand(sec, MI.getOperand(idx));
//             sec.addMetadata(N_sec);
//           } else {
//             if (ShiftOpc == RISCV32::RRX) {
//               // Split opcode<s> Rd, Rn, Rm, rrx.
//               MachineInstrBuilder fst =
//                   BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), VReg);
//               addOperand(fst, Rm);
//               fst.addMetadata(N_fst);
//               // RRX implicit CPSR, how to add cpsr?

//               MachineInstrBuilder sec =
//                   BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//               addOperand(sec, Rd, true);

//               for (unsigned n = 1; n < (i - 1); n++) {
//                 addOperand(sec, MI.getOperand(n));
//               }
//               sec.addReg(VReg);
//               sec.addImm(RISCV32CC::AL);
//               addOperand(sec, MI.getOperand(idx));
//               sec.addMetadata(N_sec);
//             } else {
//               // Split opcode<s> Rd, Rn, Rm, shift Rs.

//               // Rm shift reg,  and the new MI doesn't update CPSR.
//               MachineInstrBuilder fst =
//                   BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), VReg);
//               addOperand(fst, Rn);
//               addOperand(fst, Rm);
//               fst.addMetadata(N_fst);

//               // Build 'opcode<s> Rd, Rn, VReg'
//               // The new MI updates CPSR.
//               MachineInstrBuilder sec =
//                   BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//               addOperand(sec, Rd, true);

//               for (unsigned n = 1; n < (i - 2); n++) {
//                 addOperand(sec, MI.getOperand(n));
//               }
//               sec.addReg(VReg);
//               sec.addImm(RISCV32CC::AL);
//               addOperand(sec, MI.getOperand(idx));
//               sec.addMetadata(N_sec);
//             }
//           }
//         }
//         mi = &MI;
//         break;
//       }
//     }
//   }

//   return mi;
// }

// /// Split 'opcode<c> Rd, Rn, Rm, shift' except LDRxxx/STRxxx.
// MachineInstr *RISCV32InstructionSplitting::splitC(MachineBasicBlock &MBB,
//                                               MachineInstr &MI, unsigned newOpc,
//                                               int idx) {
//   MachineInstr *mi = nullptr;
//   for (unsigned i = 0; i < MI.getNumOperands(); i++) {
//     if (MI.getOperand(i).isImm()) {
//       unsigned Simm = MI.getOperand(i).getImm();
//       unsigned SOffSet = RISCV32_AM::getSORegOffset(Simm);
//       RISCV32_AM::ShiftOpc SOpc = RISCV32_AM::getSORegShOp(Simm);
//       unsigned ShiftOpc = getShiftOpcode(SOpc, SOffSet);
//       unsigned VReg = MRI->createVirtualRegister(&RISCV32::GPRnopcRegClass);

//       if (ShiftOpc) {
//         MachineOperand &Rd = MI.getOperand(0);
//         MachineOperand &Rn = MI.getOperand(i - 2);
//         MachineOperand &Rm = MI.getOperand(i - 1);

//         ConstantAsMetadata *CMD_fst = ConstantAsMetadata::get(
//             ConstantInt::get(*CTX, llvm::APInt(64, 0, false)));
//         MDNode *N_fst = MDNode::get(*CTX, CMD_fst);

//         ConstantAsMetadata *CMD_sec = ConstantAsMetadata::get(
//             ConstantInt::get(*CTX, llvm::APInt(64, 1, false)));
//         MDNode *N_sec = MDNode::get(*CTX, CMD_sec);

//         if (SOffSet) {
//           // Split opcode<c> Rd, Rn, Rm, shift #imm
//           // The new MI checks CondCode.

//           MachineInstrBuilder fst =
//               BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), VReg);
//           addOperand(fst, Rm);
//           fst.addImm(SOffSet);
//           fst.addImm(MI.getOperand(idx - 1).getImm());
//           addOperand(fst, MI.getOperand(idx));
//           fst.addMetadata(N_fst);

//           MachineInstrBuilder sec =
//               BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//           addOperand(sec, Rd, true);
//           for (unsigned n = 1; n < (i - 1); n++) {
//             addOperand(sec, MI.getOperand(n));
//           }
//           sec.addReg(VReg);
//           sec.addImm(MI.getOperand(idx - 1).getImm());
//           addOperand(sec, MI.getOperand(idx));
//           sec.addMetadata(N_sec);
//         } else {
//           if (ShiftOpc == RISCV32::RRX) {
//             // Split opcode<c> Rd, Rn, Rm, RRX
//             MachineInstrBuilder fst =
//                 BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), VReg);
//             addOperand(fst, Rm);
//             fst.addMetadata(N_fst);
//             // XXX: RRX implicit CPSR, how to add cpsr?

//             // Build base instructions
//             MachineInstrBuilder sec =
//                 BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//             addOperand(sec, Rd, true);

//             for (unsigned n = 1; n < (i - 1); n++) {
//               addOperand(sec, MI.getOperand(n));
//             }
//             sec.addReg(VReg);
//             sec.addImm(MI.getOperand(idx - 1).getImm());
//             addOperand(sec, MI.getOperand(idx));
//             sec.addMetadata(N_sec);
//           } else {
//             // Split opcode<c> Rd, Rn, Rm, shift Rs
//             // The new MI checks CondCode.

//             MachineInstrBuilder fst =
//                 BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), VReg);
//             addOperand(fst, Rn);
//             addOperand(fst, Rm);
//             fst.addImm(MI.getOperand(idx - 1).getImm());
//             addOperand(fst, MI.getOperand(idx));
//             fst.addMetadata(N_fst);

//             MachineInstrBuilder sec =
//                 BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//             addOperand(sec, Rd, true);

//             for (unsigned n = 1; n < (i - 2); n++) {
//               addOperand(sec, MI.getOperand(n));
//             }
//             sec.addReg(VReg);
//             sec.addImm(MI.getOperand(idx - 1).getImm());
//             addOperand(sec, MI.getOperand(idx));
//             sec.addMetadata(N_sec);
//           }
//         }
//         mi = &MI;
//         break;
//       }
//     }
//   }

//   return mi;
// }

// /// Split 'opcode<s><c> Rd, Rn, Rm, shift' except LDRxxx/STRxxx.
// MachineInstr *RISCV32InstructionSplitting::splitCS(MachineBasicBlock &MBB,
//                                                MachineInstr &MI,
//                                                unsigned newOpc, int idx) {
//   MachineInstr *mi = nullptr;
//   for (unsigned i = 0; i < MI.getNumOperands(); i++) {
//     if (MI.getOperand(i).isImm()) {
//       unsigned Simm = MI.getOperand(i).getImm();
//       unsigned SOffSet = RISCV32_AM::getSORegOffset(Simm);
//       RISCV32_AM::ShiftOpc SOpc = RISCV32_AM::getSORegShOp(Simm);
//       unsigned ShiftOpc = getShiftOpcode(SOpc, SOffSet);
//       unsigned VReg = MRI->createVirtualRegister(&RISCV32::GPRnopcRegClass);

//       if (ShiftOpc) {
//         MachineOperand &Rd = MI.getOperand(0);
//         MachineOperand &Rn = MI.getOperand(i - 2);
//         MachineOperand &Rm = MI.getOperand(i - 1);

//         ConstantAsMetadata *CMD_fst = ConstantAsMetadata::get(
//             ConstantInt::get(*CTX, llvm::APInt(64, 0, false)));
//         MDNode *N_fst = MDNode::get(*CTX, CMD_fst);

//         ConstantAsMetadata *CMD_sec = ConstantAsMetadata::get(
//             ConstantInt::get(*CTX, llvm::APInt(64, 1, false)));
//         MDNode *N_sec = MDNode::get(*CTX, CMD_sec);

//         // C flag is affected by Shift_c() if isShift_C is true.
//         if (isShift_C(MI.getOpcode())) {
//           if (SOffSet) {
//             // Split opcode<s><c> Rd, Rn, Rm, shift #imm

//             // The new MI both updates CPSR and checks CondCode.
//             MachineInstrBuilder fst =
//                 BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), VReg);
//             addOperand(fst, Rm);
//             fst.addImm(SOffSet);
//             fst.addImm(MI.getOperand(idx - 1).getImm());
//             addOperand(fst, MI.getOperand(idx));
//             addOperand(fst, MI.getOperand(idx + 1));
//             fst.addMetadata(N_fst);

//             MachineInstrBuilder sec =
//                 BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//             addOperand(sec, Rd, true);
//             for (unsigned n = 1; n < (i - 1); n++) {
//               addOperand(sec, MI.getOperand(n));
//             }
//             sec.addReg(VReg);
//             sec.addImm(MI.getOperand(idx - 1).getImm());
//             addOperand(sec, MI.getOperand(idx));
//             addOperand(sec, MI.getOperand(idx + 1));
//             sec.addMetadata(N_sec);
//           } else {
//             if (ShiftOpc == RISCV32::RRX) {
//               // Split opcode<s><c> Rd, Rn, Rm, RRX
//               MachineInstrBuilder fst =
//                   BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), VReg);
//               addOperand(fst, Rm);
//               fst.addMetadata(N_fst);
//               // RRX implicit CPSR, how to add cpsr?

//               MachineInstrBuilder sec =
//                   BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//               addOperand(sec, Rd, true);

//               for (unsigned n = 1; n < (i - 1); n++) {
//                 addOperand(sec, MI.getOperand(n));
//               }
//               sec.addReg(VReg);
//               sec.addImm(MI.getOperand(idx - 1).getImm());
//               addOperand(sec, MI.getOperand(idx));
//               addOperand(sec, MI.getOperand(idx + 1));
//               sec.addMetadata(N_sec);
//             } else {
//               // Split opcode<s><c> Rd, Rn, Rm, shift Rs

//               // The new MI both updates CPSR and checks CondCode.
//               MachineInstrBuilder fst =
//                   BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), VReg);
//               addOperand(fst, Rn);
//               addOperand(fst, Rm);
//               fst.addImm(MI.getOperand(idx - 1).getImm());
//               addOperand(fst, MI.getOperand(idx));
//               addOperand(fst, MI.getOperand(idx + 1));
//               fst.addMetadata(N_fst);

//               MachineInstrBuilder sec =
//                   BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//               addOperand(sec, Rd, true);

//               for (unsigned n = 1; n < (i - 2); n++) {
//                 addOperand(sec, MI.getOperand(n));
//               }
//               sec.addReg(VReg);
//               sec.addImm(MI.getOperand(idx - 1).getImm());
//               addOperand(sec, MI.getOperand(idx));
//               addOperand(sec, MI.getOperand(idx + 1));
//               sec.addMetadata(N_sec);
//             }
//           }
//         } else {
//           // Shifter doesn't update cpsr
//           if (SOffSet) {
//             // Split 'opcode<s><c> Rd, Rn, Rm, shift #imm'

//             // Rm shifts #imm
//             // The new MI checks CondCode, doesn't update CPSR.
//             MachineInstrBuilder fst =
//                 BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), VReg);
//             addOperand(fst, Rm);
//             fst.addImm(SOffSet);
//             fst.addImm(MI.getOperand(idx - 1).getImm());
//             addOperand(fst, MI.getOperand(idx));
//             fst.addMetadata(N_fst);

//             // Build 'newOpc<s><c> Rd, Rn, VReg'
//             // The new MI both updates CPSR and checks CondCode.
//             MachineInstrBuilder sec =
//                 BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//             addOperand(sec, Rd, true);
//             for (unsigned n = 1; n < (i - 1); n++) {
//               addOperand(sec, MI.getOperand(n));
//             }
//             sec.addReg(VReg);
//             sec.addImm(MI.getOperand(idx - 1).getImm());
//             addOperand(sec, MI.getOperand(idx));
//             addOperand(sec, MI.getOperand(idx + 1));
//             sec.addMetadata(N_sec);
//           } else {
//             if (ShiftOpc == RISCV32::RRX) {
//               // Split opcode<s><c> Rd, Rn, Rm, RRX
//               MachineInstrBuilder fst =
//                   BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), VReg);
//               addOperand(fst, Rm);
//               fst.addMetadata(N_fst);
//               // RRX implicit CPSR, how to add cpsr?

//               MachineInstrBuilder sec =
//                   BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//               addOperand(sec, Rd, true);

//               for (unsigned n = 1; n < (i - 1); n++) {
//                 addOperand(sec, MI.getOperand(n));
//               }
//               sec.addReg(VReg);
//               sec.addImm(MI.getOperand(idx - 1).getImm());
//               addOperand(sec, MI.getOperand(idx));
//               addOperand(sec, MI.getOperand(idx + 1));
//               sec.addMetadata(N_sec);
//             } else {
//               // Split opcode<s><c> Rd, Rn, Rm, shift Rs

//               // Rm shift #imm.
//               // The new MI checks CondCode, doesn't update CPSR.
//               MachineInstrBuilder fst =
//                   BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(ShiftOpc), VReg);
//               addOperand(fst, Rn);
//               addOperand(fst, Rm);
//               fst.addImm(MI.getOperand(idx - 1).getImm());
//               addOperand(fst, MI.getOperand(idx));
//               fst.addMetadata(N_fst);

//               // Build 'newOpc<s><c> Rd, Rn, VReg'
//               // The new MI both updates CPSR and checks CondCode.
//               MachineInstrBuilder sec =
//                   BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(newOpc));
//               addOperand(sec, Rd, true);

//               for (unsigned n = 1; n < (i - 2); n++) {
//                 addOperand(sec, MI.getOperand(n));
//               }
//               sec.addReg(VReg);
//               sec.addImm(MI.getOperand(idx - 1).getImm());
//               addOperand(sec, MI.getOperand(idx));
//               addOperand(sec, MI.getOperand(idx + 1));
//               sec.addMetadata(N_sec);
//             }
//           }
//         }
//         mi = &MI;
//         break;
//       }
//     }
//   }

//   return mi;
// }

bool RISCV32InstructionSplitting::split() {
  LLVM_DEBUG(dbgs() << "RISCV32InstructionSplitting start.\n");

  // std::vector<MachineInstr *> removelist;
  // for (MachineBasicBlock &MBB : *MF) {
  //   for (MachineBasicBlock::iterator I = MBB.begin(), E = MBB.end(); I != E;
  //        ++I) {
  //     MachineInstr &MI = *I;
  //     MachineInstr *removeMI = nullptr;

  //     unsigned Opcode, newOpc;
  //     Opcode = MI.getOpcode();
  //     newOpc = checkisShifter(Opcode);

  //     // Need to split
  //     if (getLoadStoreOpcode(Opcode)) {
  //       // Split the MI about Load and Store.

  //       // TODO: LDRSH/LDRSB/LDRH/LDRD split.
  //       if (isLDRSTRPre(Opcode)) {
  //         if (MI.getOperand(3).isReg())
  //           removeMI = splitLDRSTRPre(MBB, MI);
  //         else if (MI.getOperand(3).isImm() && MI.getOperand(3).getImm() != 0)
  //           removeMI = splitLDRSTRPreImm(MBB, MI);
  //         if (removeMI)
  //           removelist.push_back(removeMI);
  //       } else if (MI.getOperand(1).isReg() &&
  //                  MI.getOperand(1).getReg() != RISCV32::SP &&
  //                  MI.getOperand(1).getReg() != RISCV32::PC) {
  //         if (MI.getOperand(2).isReg())
  //           removeMI = splitLDRSTR(MBB, MI);
  //         else if (MI.getOperand(2).isImm() && MI.getOperand(2).getImm() != 0)
  //           removeMI = splitLDRSTRImm(MBB, MI);
  //         if (removeMI)
  //           removelist.push_back(removeMI);
  //       }
  //     } else if (newOpc) {
  //       // Split the MI except Load and Store.

  //       bool UpdateCPSR = false;
  //       bool CondCode = false;
  //       int idx = MI.findRegisterUseOperandIdx(RISCV32::CPSR);

  //       // Check if MI contains CPSR
  //       if (idx != -1) {
  //         if (MI.getOperand(idx + 1).isReg() &&
  //             MI.getOperand(idx + 1).getReg() == RISCV32::CPSR) {
  //           UpdateCPSR = true;
  //           CondCode = true;
  //         } else if (MI.getOperand(idx - 1).isImm() &&
  //                    MI.getOperand(idx - 1).getImm() != RISCV32CC::AL) {
  //           CondCode = true;
  //         } else
  //           UpdateCPSR = true;
  //       }

  //       if (!UpdateCPSR && !CondCode)
  //         // Split the MI has no cpsr.
  //         removeMI = splitCommon(MBB, MI, newOpc);
  //       else if (UpdateCPSR && !CondCode)
  //         // Split the MI updates cpsr.
  //         removeMI = splitS(MBB, MI, newOpc, idx);
  //       else if (!UpdateCPSR && CondCode)
  //         // Split the MI checks CondCode.
  //         removeMI = splitC(MBB, MI, newOpc, idx);
  //       else
  //         // Split the MI both updates cpsr and check CondCode
  //         removeMI = splitCS(MBB, MI, newOpc, idx);

  //       if (removeMI)
  //         removelist.push_back(removeMI);
  //     }
  //   }
  // }

  // // Remove old MI.
  // for (MachineInstr *mi : removelist)
  //   mi->removeFromParent();

  // For debugging.
  // LLVM_DEBUG(MF->dump());
  // LLVM_DEBUG(getCRF()->dump());
  LLVM_DEBUG(dbgs() << "RISCV32InstructionSplitting end.\n");

  return true;
}

bool RISCV32InstructionSplitting::runOnMachineFunction(MachineFunction &mf) {
  bool rtn = false;
  init();
  rtn = true;
  return rtn;
}

#undef DEBUG_TYPE

#ifdef __cplusplus
extern "C" {
#endif

FunctionPass *InitializeRISCV32InstructionSplitting(RISCV32ModuleRaiser &mr) {
  return new RISCV32InstructionSplitting(mr);
}

#ifdef __cplusplus
}
#endif
