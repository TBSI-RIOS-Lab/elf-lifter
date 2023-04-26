//===-- PHI.cpp ------------------------------------------*- C++
//-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//


#include "AssemblyBasicBlock.h"
#include "AssemblyInstruction.h"
#include "PHI.h"
#include <iostream>
#include <set>
#include <vector>
using namespace llvm;
using namespace std;

PHI::PHI(AssemblyBasicBlock* BB){this->MyBB = BB;}

int PHI::getSize(){ return PhiNode.size(); }

int PHI::AddPair(AssemblyInstruction* BB, AssemblyInstruction* Inst){
  PhiNode.push_back(pair<AssemblyInstruction*, AssemblyInstruction*>{ BB, Inst });
  return PhiNode.size();
}

AssemblyInstruction* PHI::getValue(AssemblyInstruction* key) {
  for (auto pair : PhiNode) if (pair.first == key) return pair.second;
}

void PHI::clearPhi() { PhiNode.clear(); }

vector<pair<AssemblyInstruction*, AssemblyInstruction*>>::iterator PHI::begin() {
  return PhiNode.begin();
}
vector<pair<AssemblyInstruction*, AssemblyInstruction*>>::iterator PHI::end() {
  return PhiNode.end();
}