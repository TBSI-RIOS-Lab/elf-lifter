//===-- RISCV32ModuleRaiser.cpp -------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of RISCV32ModuleRaiser class for use by
// llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#include "RISCV32ModuleRaiser.h"
#include "llvm/Object/ELFObjectFile.h"
#include "llvm/Support/Debug.h"
#define DEBUG_TYPE "mctoll"
using namespace llvm;

namespace RaiserContext {
extern SmallVector<ModuleRaiser *, 4> ModuleRaiserRegistry;
}

bool RISCV32ModuleRaiser::collectDynamicRelocations() {
  if (!Obj->isELF())
    return false;

  const ELF32LEObjectFile *Elf32LEObjFile = dyn_cast<ELF32LEObjectFile>(Obj);
  if (!Elf32LEObjFile)
    return false;

  // Collect all relocation records from various relocation sections
  std::vector<SectionRef> DynRelSec = Obj->dynamic_relocation_sections();
  for (const SectionRef &Section : DynRelSec)
    for (const RelocationRef &Reloc : Section.relocations())
      DynRelocs.push_back(Reloc);

  return true;
}
// Get rodata instruction addr.
uint64_t RISCV32ModuleRaiser::getArgNumInstrAddr(uint64_t callAddr) {
  uint64_t InstArgCount = InstArgCollect.size();
  if (InstArgCount == 0)
    return InstArgCount;
  for (uint64_t i = 0; i < InstArgCount; i++) {
    if (InstArgCollect[i] > callAddr) {
      return InstArgCollect[i - 1];
    }
  }

  return InstArgCollect[InstArgCount - 1];
}

uint64_t RISCV32ModuleRaiser::getFunctionArgNum(uint64_t callAddr) {
  uint64_t rodataAddr = getArgNumInstrAddr(callAddr);

  if (rodataAddr == 0)
    return rodataAddr;
  return InstArgNumMap[rodataAddr];
}

const Value *RISCV32ModuleRaiser::getRODataValueAt(uint64_t Offset) const {
  auto Iter = GlobalRODataValues.find(Offset);
  if (Iter != GlobalRODataValues.end())
    return Iter->second;

  return nullptr;
}

void RISCV32ModuleRaiser::addRODataValueAt(Value *V, uint64_t Offset) const {
  assert((GlobalRODataValues.find(Offset) == GlobalRODataValues.end()) &&
         "Attempt to insert value for already existing rodata location");
  GlobalRODataValues.emplace(Offset, V);
}




#ifdef __cplusplus
extern "C" {
#endif

void InitializeRISCV32ModuleRaiser() {
  ModuleRaiser *m = new RISCV32ModuleRaiser();
  RaiserContext::ModuleRaiserRegistry.push_back(m);
}

#ifdef __cplusplus
}
#endif
