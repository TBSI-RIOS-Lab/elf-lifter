//===-- PHI.h ------------------------------------------*- C++
//-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//


#ifndef LLVM_TOOLS_LLVM_MCTOLL_PHY_H
#define LLVM_TOOLS_LLVM_MCTOLL_PHY_H
#include <iostream>
#include <set>
#include <vector>
#include <unordered_map>
using namespace llvm;
using namespace std;

class AssemblyInstruction;
class AssemblyBasicBlock;


// The PHI node should be added at the convergence of 2 or more BB control flow
// The placement of PHI node should be at the convergence BB rather than the 
// BB that uses the respective registers to comply with LLVM IR
// Eg 1:
//      [BB 0]      [BB 1]
//       Def A;    Def A;
//          \      /
//           \    /
//           [BB 2]
//            Loop 
//      (A unused at BB 2)
//              |
//              |
//           [BB 3]
//           Use  A; 
//  
// In this example,  PHI node of Var A should be placed at the beginning of 
// BB 2, rather than BB 3;
// 
//
// Eg 2:
//      [BB 0]      [BB 1]
//       Def A;    Def A;
//       |   \      /
//       |    \    /
//       |    [BB 2]       
//       |     Loop         [BB 3]
//       |  (A unused)      Def A;
//       |         \        /
//       |          \      /
//       |           \    /
//       |           [BB 4]
//        一一一一一一 Use  A; 
//  
// In this example,  we need to have 2 PHI nodes for Var A;
// The first node should be placed at BB 2
// The second one is supposed to reside in BB 4：  Phi [A0, BB 0], [A2, BB 2], [A3, BB 3] 
// 
//
//
// Eg 3:
//      [BB 0]      [BB 1]
//       Def A;    Def A;
//       |          /
//       |         /
//       |    [BB 2]       
//       |     Loop         [BB 3]
//       |  (A unused)      Def A;
//       |         \        /
//       |          \      /
//       |           \    /
//       |           [BB 4]
//        一一一一一一 Use  A; 
//  
// In this example,  we only have 1 PHI node in BB 4 for Var A:
// Phi [A0, BB 0], [A1, BB 2], [A3, BB 3] 
// 
//
class PHI {
private:

  AssemblyBasicBlock* MyBB;

  // We can simply derive the id of the BB and associated rd of source instructions
  vector<pair<AssemblyInstruction*, AssemblyInstruction*>> PhiNode = vector<pair<AssemblyInstruction*, AssemblyInstruction*>>(0);

public:
  PHI(AssemblyBasicBlock* BB);
  ~PHI() {}
  int AddPair(AssemblyInstruction*, AssemblyInstruction*);
  AssemblyInstruction* getValue(AssemblyInstruction* key);
  int getSize();
  void clearPhi();
  void dump();
  vector<pair<AssemblyInstruction*, AssemblyInstruction*>>::iterator begin();
  vector<pair<AssemblyInstruction*, AssemblyInstruction*>>::iterator end();

};

#endif // LLVM_TOOLS_LLVM_MCTOLL_PHY_H
