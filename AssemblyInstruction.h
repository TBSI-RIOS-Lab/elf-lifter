//===-- AssemblyInstruction.h ------------------------------------------*- C++
//-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_LLVM_MCTOLL_ASSEMBLYINSTRUCTION_H
#define LLVM_TOOLS_LLVM_MCTOLL_ASSEMBLYINSTRUCTION_H
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/ADT/ArrayRef.h"
#include "AssemblyFunction.h"
#include "ASMUtils.h"
/*
#define GET_REGINFO_ENUM
#include "build/lib/Target/RISCV/RISCVGenRegisterInfo.inc"
#undef GET_REGINFO_ENUM
*/
#include "llvm/MC/MCRegisterInfo.h"
#include "macro.h"
#include <iostream>
#include <string>
#include <utility>

using namespace llvm;
using namespace std;

class AssemblyCFG;
//class AssemblyBasicBlock;

extern int ISA_type;


class AssemblyInstruction {
private:
  ArrayRef<uint8_t> Bytes;
  int Size_Byte;
  uint64_t Address;
  int Opcode;
  int Funct3;
  int Funct7;
  int Funct2;
  int Funct6;
  int Funct4;
  
  int MyBB;
  // Use Reg Array to avoid redundancy
  int32_t Reg[MAX_OPERAND];
  // int32_t Rs1;
  // int32_t Rs2;
  // int32_t Rs3;
  // int32_t Rd;
  int Imm;
  bool hasImm;
  char Type; // Define: SB == 'B', UJ == 'J'
  bool IsBranch;
  bool IsCall;
  AssemblyFunction* CallTarget;
  bool IsCompressed;
  string Mnemonic;
  uint64_t BranchTarget;

  //  LocalCnt[1]:RS1
  //  LocalCnt[2]:RS2
  //  LocalCnt[3]:RS3
  bool LocalCnt[MAX_OPERAND];
  int  GlobalCnt[MAX_OPERAND];

	uint32_t NumOperands;
  // It is also important to track # of Rs registers
  // It helps the dependency graph building process
  // Currently, we don't link immediate operands.
  uint32_t NumRs;
  //  We set a 2D Vector to handle the case that one operand can 
  //  have multiple cross-BB edges (Data PHI)
  //  Init 2D Vector of instruction pointers based on the operands:
  //  GlobalEdge[0]:RD
  //  GlobalEdge[1]:RS1
  //  GlobalEdge[2]:RS2
  //  GlobalEdge[3]:RS3
  vector<vector<AssemblyInstruction*>> GlobalEdge = vector<vector<AssemblyInstruction*>>(4,vector<AssemblyInstruction*>(0));
  
  //  Each operand can have at most 1 local edge!
  //  LocalEdge[0]:RD
  //  LocalEdge[1]:RS1
  //  LocalEdge[2]:RS2
  //  LocalEdge[3]:RS3
  vector<AssemblyInstruction*> LocalEdge = vector<AssemblyInstruction*>(4);
  set<pair<int, int>> colors; // color, type(0 data computing 1 addressing 3 control flow)



  //  Indicating the instruction is load or store
  bool Load = false;
  bool Store = false;


  //  ==================
  //  Operand Attribute:
  //  ==================
  //
  //  Indicating each reg is data/address/both
  //  Used to build "Slice" for addressing and data operations
  int32_t DataOrAddr[MAX_OPERAND];

  //  Indicating the width of data operand
  //  The indices follow the order of : RD, RS1, RS2, RS3, etc.
  //  The unit of data width is "byte":
  //  1:  8-bit/
  //  2:  16-bit
  //  4:  32-bit
  //  8:  64-bit
  int32_t DataWidth[MAX_OPERAND];


  bool Prologue = false;
  bool Epilogue = false;


  // // The source is traversed all the way back to the address of source  
  // // data and serves as metadata to help distinguish instructions
  // // It only records LOAD or STORE instructions
  // std::vector<AssemblyInstruction*> DataSource;


  // Data source can only be one of the following 3
  // Stack (SP)
  // Heap: a0
  // Returned pointer from a function can be hard to track the source,
  // since the function can reside in lib, no dependency info is exposed
  // to the BT interface. But we can bind such heap info with function calls
  // -- linking load, store addresses with 
  std::string DataRoot = "NULL";
  

  AssemblyInstruction(const AssemblyInstruction&) = delete;
  

  GlobalData GlobalSymbol;


protected:
  AssemblyInstruction();

public:
  ~AssemblyInstruction();
  AssemblyInstruction(const MCRegisterInfo &mri, MCInstPrinter &IP, const MCInst *MI, ArrayRef<uint8_t> Bytes, uint64_t Address);
  void setBranch(uint64_t Target);
  void setCall(uint64_t Target, AssemblyFunction* targetFunc);
  uint64_t getAddress() const;
  uint64_t getTarget() const;
  bool getIsBranch() const;
  bool getIsCall() const;
  AssemblyFunction* getCallTarget() const;
  bool getIsCompressed() const;
  char getType() const;
  string getFullMnemonic() const;
  void dump() const;
  int getSizeByte() const;
  uint32_t getOpcode() const;
  int32_t getRs(int index);
	int32_t getRs1();
	int32_t getRs2();
  int32_t getRs3();
	int32_t getRd();
  int32_t setRd(int32_t rd);
	int getImm();
  uint32_t getOpcode();
  string getMnemonic();
  int BuildEdge(AssemblyInstruction* inst, int rs, int LocalOrGlobal);
  
  uint64_t hashCode();

  //AssemblyInstruction* getEdge(int index);
  AssemblyInstruction* getLocalEdge(int rs);
  vector<AssemblyInstruction*>& getGlobalEdge(int rs);
  
  vector<AssemblyInstruction*>::iterator global_edge_begin(int rs);
  vector<AssemblyInstruction*>::iterator global_edge_end(int rs);

  void FoundLocalEdge(int rs);
  bool HasLocalEdge(int rs);
  bool hasGlobalEdgeToDealWith();
  bool hasGlobalEdge(int rs);
  int HasGlobalEdge(int rs);
  int FoundGlobalEdge(int rs);
  bool IsLoad();
  bool IsStore();
  void SetIsCompressed(bool b);
  void SetLoad();
  void SetStore();
  void SetOpAttr(int reg, int DataOrAddr);
  void SetDataWidth(int reg, int width);
  int getDataWidth(int reg);

  void setBB(int BB);
  int getBB();


  void setPrologue();
  void setEpilogue();
  bool ifPrologueEpilogue();

  void printArrays();
  void printEdges();

  set<pair<int, int>>& getColors();
  void addColor(int c, int t);
  bool hasColor(int c);


  void addDataSource(AssemblyInstruction* inst);
  std::vector<AssemblyInstruction*>& getDataSource();
  void setDataRoot(string source);
  std::string getDataRoot();
  std::string getGlobalSymbolName();

  GlobalData getGlobalData();
  void setGlobalData(GlobalData ptr);

};

#endif // LLVM_TOOLS_LLVM_MCTOLL_MCINSTRAISER_H
