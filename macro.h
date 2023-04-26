//===-- Macro.h ------------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include <vector>
#include <string>
#include <set>

using namespace std;



extern bool RISCV_ISA;

extern const set<int> RV_R_TYPE;
extern const set<int> RV_I_TYPE;
extern const set<int> RV_S_TYPE;
extern const set<int> RV_B_TYPE;
extern const set<int> RV_U_TYPE;
extern const set<int> RV_J_TYPE;
extern const set<int> RV_R4_TYPE;
// Need to be fixed at some point for CSRs
extern const set<int> RV_UNKNOWN;  // ECALL, EBREAK, CSR, fence


// Maximum Operand of an instruction
// Sequence would be rd, rs1, rs2, rs3 (if exists)
#define MAX_OPERAND 4

#define RD   0
#define RS1  1
#define RS2  2
#define RS3  3


// The attribute of Rd
#define IsData  1
#define IsAddr  2
#define BOTH    3


// RISC-V LOAD/STORE OPCODE
#define RVLOAD    0b0000011
#define RVSTORE   0b0100011



#define LOCALEDGE  0
#define GLOBALEDGE 1


//RISC-V Inst Type
// Bitmap: rd,rs1,rs2,rs3,imm
// R type: rd,rs1,rs2  =>   0b11100 


#define RTYPE          0b11100
#define R4TYPE         0b11110
#define ITYPE          0b11001
#define UTYPE          0b10001
#define STYPE          0b01101
#define BTYPE          0b01101
#define JTYPE          0b10001
#define UNKOWNTYPE     0b00000



