#include "ExternalFunctions.h"
#include "MachineFunctionRaiser.h"
#include "RISCV32ModuleRaiser.h"
#include "RISCV32FunctionPrototype.h"
#include "RISCV32MachineInstructionRaiser.h"
#include "RISCV32MIRevising.h"
#include "RISCV32EliminatePrologEpilog.h"
#include "RISCV32CreateJumpTable.h"
#include "RISCV32ArgumentRaiser.h"
#include "RISCV32MachineInstructionRaiser.h"
#include "RISCV32FrameBuilder.h"
#include "RISCV32InstructionSplitting.h"
#include "RISCV32SelectionDAGISel.h"
#include "MachineInstructionRaiser.h"
#include "llvm-mctoll.h"

// NOTE : The following RISCV32ModuleRaiser class function is defined here as
// they reference MachineFunctionRaiser class that has a forward declaration
// in ModuleRaiser.h.

//===-- RISCV32EliminatePrologEpilog.cpp ----------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of RISCV32MachineInstructionRaiser class
// for use by llvm-mctoll.
//
//===----------------------------------------------------------------------===//

using namespace llvm;

RISCV32MachineInstructionRaiser::RISCV32MachineInstructionRaiser(
    MachineFunction &machFunc, const ModuleRaiser *mr, MCInstRaiser *mcir)
    : MachineInstructionRaiser(machFunc, mr, mcir),
      machRegInfo(MF.getRegInfo()) {}

bool RISCV32MachineInstructionRaiser::raiseMachineFunction() {
  const RISCV32ModuleRaiser *amr = dyn_cast<RISCV32ModuleRaiser>(MR);

  assert(amr != nullptr && "The RISCV32 module raiser is not initialized!");
  RISCV32ModuleRaiser &rmr = const_cast<RISCV32ModuleRaiser &>(*amr);

  RISCV32MIRevising mir(rmr);
  mir.init(&MF, raisedFunction);
  mir.setMCInstRaiser(mcInstRaiser);
  mir.revise();
  
  RISCV32EliminatePrologEpilog epe(rmr);
  epe.init(&MF, raisedFunction);
  epe.eliminate();

  RISCV32CreateJumpTable cjt(rmr);
  cjt.init(&MF, raisedFunction);
  cjt.setMCInstRaiser(mcInstRaiser);
  cjt.create();
  cjt.getJTlist(jtList);

  RISCV32ArgumentRaiser ar(rmr);
  ar.init(&MF, raisedFunction);
  ar.raiseArgs();

  RISCV32FrameBuilder fb(rmr);
  fb.init(&MF, raisedFunction);
  fb.build();

  RISCV32InstructionSplitting ispl(rmr);
  ispl.init(&MF, raisedFunction);
  ispl.split();

  RISCV32SelectionDAGISel sdis(rmr);
  sdis.init(&MF, raisedFunction);
  sdis.setjtList(jtList);
  sdis.doSelection();

  return true;
}

bool RISCV32MachineInstructionRaiser::raise() {
  raiseMachineFunction();
  return true;
}

int RISCV32MachineInstructionRaiser::getArgumentNumber(unsigned PReg) {
  // NYI
  assert(false &&
         "Unimplemented RISCV32MachineInstructionRaiser::getArgumentNumber()");
  return -1;
}

bool RISCV32MachineInstructionRaiser::buildFuncArgTypeVector(
    const std::set<MCPhysReg> &PhysRegs, std::vector<Type *> &ArgTyVec) {
  // NYI
  assert(false &&
         "Unimplemented RISCV32MachineInstructionRaiser::buildFuncArgTypeVector()");
  return false;
}

Value *RISCV32MachineInstructionRaiser::getRegOrArgValue(unsigned PReg, int MBBNo) {
  assert(false &&
         "Unimplemented RISCV32MachineInstructionRaiser::getRegOrArgValue()");
  return nullptr;
}

FunctionType *RISCV32MachineInstructionRaiser::getRaisedFunctionPrototype() {
  RISCV32FunctionPrototype AFP;
  raisedFunction = AFP.discover(MF);

  Function *ori = const_cast<Function *>(&MF.getFunction());
  // Insert the map of raised function to tempFunctionPointer.
  const_cast<ModuleRaiser *>(MR)->insertPlaceholderRaisedFunctionMap(raisedFunction, ori);

  return raisedFunction->getFunctionType();
}

// Create a new MachineFunctionRaiser object and add it to the list of
// MachineFunction raiser objects of this module.
MachineFunctionRaiser *RISCV32ModuleRaiser::CreateAndAddMachineFunctionRaiser(
    Function *f, const ModuleRaiser *mr, uint64_t start, uint64_t end) {
  MachineFunctionRaiser *mfRaiser = new MachineFunctionRaiser(
      *M, mr->getMachineModuleInfo()->getOrCreateMachineFunction(*f), mr, start,
      end);
  mfRaiser->setMachineInstrRaiser(new RISCV32MachineInstructionRaiser(
      mfRaiser->getMachineFunction(), mr, mfRaiser->getMCInstRaiser()));
  mfRaiserVector.push_back(mfRaiser);
  return mfRaiser;
}

