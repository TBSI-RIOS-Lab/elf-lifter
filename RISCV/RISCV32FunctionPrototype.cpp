//===- RISCV32FunctionPrototype.cpp - Binary raiser utility llvm-mctoll -------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of RISCV32FunctionPrototype class
// for use by llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#include "RISCV32FunctionPrototype.h"
#include "RISCVSubtarget.h"
#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/LivePhysRegs.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/Support/Debug.h"
#include <iostream>
#define DEBUG_TYPE "mctoll"

using namespace llvm;

char RISCV32FunctionPrototype::ID = 0;

RISCV32FunctionPrototype::RISCV32FunctionPrototype() : MachineFunctionPass(ID) {}

RISCV32FunctionPrototype::~RISCV32FunctionPrototype() {}

/// Check the first reference of the reg is USE.
bool RISCV32FunctionPrototype::isUsedRegiser(unsigned reg,
                                         const MachineBasicBlock &mbb) {
  // for (MachineBasicBlock::const_iterator ii = mbb.begin(), ie = mbb.end();
  //      ii != ie; ++ii) {
  //   const MachineInstr &mi = *ii;
  //   for (MachineInstr::const_mop_iterator oi = mi.operands_begin(),
  //                                         oe = mi.operands_end();
  //        oi != oe; oi++) {
  //     const MachineOperand &mo = *oi;
  //     if (mo.isReg() && (mo.getReg() == reg))
  //       return mo.isUse();
  //   }
  // }

  return false;
}

/// Check the first reference of the reg is DEF.
void RISCV32FunctionPrototype::genParameterTypes(std::vector<Type *> &paramTypes) {
  assert(!MF->empty() && "The function body is empty!!!");
  MF->getRegInfo().freezeReservedRegs(*MF);
  LivePhysRegs liveInPhysRegs;
  for (MachineBasicBlock &EMBB : *MF)
    computeAndAddLiveIns(liveInPhysRegs, EMBB);
  // Walk the CFG DFS to discover first register usage
  df_iterator_default_set<const MachineBasicBlock *, 16> Visited;
  DenseMap<unsigned, bool> ArgObtain;
  ArgObtain[RISCV::X10] = false;
  ArgObtain[RISCV::X11] = false;
  ArgObtain[RISCV::X12] = false;
  ArgObtain[RISCV::X13] = false;
  ArgObtain[RISCV::X14] = false;
  ArgObtain[RISCV::X15] = false;
  ArgObtain[RISCV::X16] = false;
  ArgObtain[RISCV::X17] = false;
  const MachineBasicBlock &fmbb = MF->front();
  DenseMap<int, Type *> tarr;
  int maxidx = -1; // When the maxidx is -1, means there is nro argument.
  // Track register liveness on CFG.
  for (const MachineBasicBlock *Mbb : depth_first_ext(&fmbb, Visited)) {  // 对于每个寄存器，在该Fcuntion的所有BB里查找是否有Use，若有则认为是argument
    std::cout << "Mbb: " << Mbb->getFullName() << " " << Mbb->getNumber() << "\n";
    for (unsigned IReg = RISCV::X10; IReg < RISCV::X18; IReg++) {
      if (!ArgObtain[IReg] && Mbb->isLiveIn(IReg)) {
        for (MachineBasicBlock::const_iterator ii = Mbb->begin(),
                                               ie = Mbb->end();
             ii != ie; ++ii) {
          const MachineInstr &LMI = *ii;
          auto RUses = LMI.uses();
          auto ResIter =
              std::find_if(RUses.begin(), RUses.end(),
                           [IReg](const MachineOperand &OP) -> bool {
                             return OP.isReg() && (OP.getReg() == IReg);
                           });
          if (ResIter != RUses.end()) {
            maxidx = IReg - RISCV::X10;
            std::cout << "find IReg " << maxidx + 10 << " use in LMI.uses()\n";
            tarr[maxidx] = getDefaultType();
            break;
          }
        }
        ArgObtain[IReg] = true;
      }
    }
  }

  // // The rest of function arguments are from stack.
  // for (MachineFunction::const_iterator mbbi = MF->begin(), mbbe = MF->end();
  //      mbbi != mbbe; ++mbbi) {
  //   const MachineBasicBlock &mbb = *mbbi;
  //   for (MachineBasicBlock::const_iterator mii = mbb.begin(), mie = mbb.end();
  //        mii != mie; ++mii) {
  //     const MachineInstr &mi = *mii;
  //     // Match pattern like ldr r1, [fp, #8].
  //     if (mi.getOpcode() == RISCV32::LDRi12 && mi.getNumOperands() > 2) {
  //       const MachineOperand &mo = mi.getOperand(1);
  //       const MachineOperand &mc = mi.getOperand(2);
  //       if (mo.isReg() && mo.getReg() == RISCV32::R11 && mc.isImm()) {
  //         // TODO: Need to check the imm is larger than 0 and it is align
  //         // by 4(32 bit).
  //         int imm = mc.getImm();
  //         if (imm >= 0) {
  //           // The start index of arguments on stack. If the library was
  //           // compiled by clang, it starts from 2. If the library was compiled
  //           // by GNU cross compiler, it starts from 1.
  //           // FIXME: For now, we only treat that the library was complied by
  //           // clang. We will enable the 'if condition' after we are able to
  //           // identify the library was compiled by which compiler.
  //           int idxoff = 2;
  //           if (true /* clang */)
  //             idxoff = 2;
  //           else /* gnu */
  //             idxoff = 1;

  //           int idx = imm / 4 - idxoff + 4; // Plus 4 is to guarantee the first
  //                                           // stack argument index is after all
  //                                           // of register arguments' indices.
  //           if (maxidx < idx)
  //             maxidx = idx;
  //           tarr[idx] = getDefaultType();
  //         }
  //       }
  //     }
  //   }
  // }
  for (int i = 0; i <= maxidx; ++i) {
    if (tarr[i] == nullptr)
      paramTypes.push_back(getDefaultType());
    else
      paramTypes.push_back(tarr[i]);
  }
}

/// Get all arguments types of current MachineFunction.
bool RISCV32FunctionPrototype::isDefinedRegiser(unsigned reg,
                                            const MachineBasicBlock &mbb) {
   for (MachineBasicBlock::const_reverse_iterator ii = mbb.rbegin(),
                                                  ie = mbb.rend();
        ii != ie; ++ii) {
     const MachineInstr &mi = *ii;
     for (MachineInstr::const_mop_iterator oi = mi.operands_begin(),
                                           oe = mi.operands_end();
          oi != oe; oi++) {
       const MachineOperand &mo = *oi;
       if (mo.isReg() && (mo.getReg() == reg)) {
         // The return register must not be tied to another register.
         // If it was, it should not be return register.
         if (mo.isTied())
           return false;

         return mo.isDef();
       }
     }
  }

  return false;
}

/// Get return type of current MachineFunction.
Type *RISCV32FunctionPrototype::genReturnType() {
  // TODO: Need to track register liveness on CFG.
  Type *retTy;
  retTy = Type::getVoidTy(*CTX);
  for (const MachineBasicBlock &mbb : *MF) {
    LLVM_DEBUG(dbgs()<<"genReturnType 1\n");
    if (mbb.succ_empty()) {
      LLVM_DEBUG(dbgs()<<"genReturnType 2\n");
      if (isDefinedRegiser(RISCV::X10, mbb) || isDefinedRegiser(RISCV::X11, mbb)) {
        // TODO: Need to identify data type, int, long, float or double.
        LLVM_DEBUG(dbgs()<<"genReturnType 3\n");
        retTy = getDefaultType();
        break;
      }
    }
  }
  LLVM_DEBUG(dbgs()<<*retTy<<"\n");
  return retTy;
}

Function *RISCV32FunctionPrototype::discover(MachineFunction &mf) {
  LLVM_DEBUG(dbgs() << "RISCV32FunctionPrototype start.\n");
  
  MF = &mf;
  Function &fn = const_cast<Function &>(mf.getFunction());
  CTX = &fn.getContext();

  std::vector<Type *> paramTys;
  genParameterTypes(paramTys);

  std::cout << "Function " << fn.getName().str() << " 's Arguments: ";
  for(int i = 0; i < paramTys.size(); i++) {
    std::cout << paramTys[i] << "\t";
  }
  std::cout << "\n";

  std::cout << "Function " << fn.getName().str() << " 's Return Type: ";
  Type *retTy = genReturnType();
  std::cout << &retTy << "\n";
  FunctionType *fnTy = FunctionType::get(retTy, paramTys, false);

  MachineModuleInfo &mmi = mf.getMMI();
  Module *mdl = const_cast<Module *>(mmi.getModule());
  mdl->getFunctionList().remove(&fn);
  Function *pnfn =
      Function::Create(fnTy, GlobalValue::ExternalLinkage, fn.getName(), mdl);
  // When run as FunctionPass, the Function must not be empty, so add
  // EntryBlock at here.
  BasicBlock::Create(pnfn->getContext(), "EntryBlock", pnfn);

  // LLVM_DEBUG(MF->dump());
  // LLVM_DEBUG(dbgs() << "Finish machine Function dump.\n");
  // LLVM_DEBUG(pnfn->dump());
  // LLVM_DEBUG(dbgs() << "RISCV32FunctionPrototype end.\n");

  return pnfn;
}

bool RISCV32FunctionPrototype::runOnMachineFunction(MachineFunction &mf) {
  discover(mf);
  return true;
}

#undef DEBUG_TYPE

#ifdef __cplusplus
extern "C" {
#endif

MachineFunctionPass *InitializeRISCV32FunctionPrototype() {
  return new RISCV32FunctionPrototype();
}

#ifdef __cplusplus
}
#endif
