//===-- AssemblyInstruction.cpp ------------------------------------------*- C++
//-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "AssemblyInstruction.h"
#include "AssemblyFunction.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/MC/MCInstrAnalysis.h"
#include "llvm/MC/MCInstrInfo.h"
#include "../../../../llvm/lib/Target/X86/Disassembler/X86DisassemblerDecoder.h"
#include <iostream>
#include <utility>
#include <string>
#include <functional>
#include "macro.h"
using namespace llvm;
using namespace std;



uint64_t GetField(int l, int r, uint64_t value) {
  return (value >> l) & ((r >> l-1)-1); 
}


int GetInstType(int opcode){
    //cout<< "Entering GetInstType()\n";
    if(RV_R_TYPE.find(opcode) != RV_R_TYPE.end())
        return RTYPE;
    if(RV_I_TYPE.find(opcode) != RV_I_TYPE.end())
        return ITYPE;
    if(RV_S_TYPE.find(opcode) != RV_S_TYPE.end())
        return STYPE;
    if(RV_B_TYPE.find(opcode) != RV_B_TYPE.end())
        return BTYPE;   
    if(RV_J_TYPE.find(opcode) != RV_J_TYPE.end())
        return JTYPE;   
    if(RV_R4_TYPE.find(opcode)!= RV_R4_TYPE.end())
        return R4TYPE;
    if(RV_U_TYPE.find(opcode) != RV_U_TYPE.end())
        return UTYPE;
    if(RV_UNKNOWN.find(opcode)!= RV_UNKNOWN.end())
        return UNKOWNTYPE;
    return -1;
}




AssemblyInstruction::AssemblyInstruction(const MCRegisterInfo &mri, MCInstPrinter &IP, const MCInst *MI, ArrayRef<uint8_t> Bytes, uint64_t Address) {

  //cout<< "Enter AssemblyInstruction::AssemblyInstruction\n";
  auto BytesArray = Bytes.data(); // 4elements, first element is [7:0] of instruction
  int Opcode_ = -1;
  if(Bytes.size() == 2) {
    this->IsCompressed = true;
    Opcode_ = BytesArray[0] & 0b00000011;
    Funct3 = (BytesArray[1] & 0b11100000)>>5;
    Funct4 = (BytesArray[1] & 0b11110000)>>4;
    Funct6 = (BytesArray[1] & 0b11111100)>>2;
    Funct2 = (BytesArray[0] & 0b01100000)>>5;
  }else {
    this->IsCompressed = false;
    Opcode_ = BytesArray[0] & 0b01111111;
  }
  this->Opcode = Opcode_;
  this->Funct3 = -1;
  this->Funct7 = -1;
  this->Type = '-';
  this->MyBB = -1;
  int num_operands = MI->getNumOperands();
	this->NumOperands = num_operands;
  this->Bytes = Bytes;
  this->Size_Byte = Bytes.size();
  this->Address = Address;
  this->IsBranch = false;
  this->IsCall = false;
  this->CallTarget = nullptr;
  this->BranchTarget = 0;
  this->hasImm = false;
  this->Imm = -1;
  pair<const char *, uint64_t> mnemonicPair = IP.getMnemonic(MI);
  string mnemonicString = string(mnemonicPair.first);
  mnemonicString.erase(mnemonicString.find_last_not_of("\t") + 1);
  this->Mnemonic = mnemonicString;
  if(mnemonicString == "sb" || mnemonicString == "sh" || mnemonicString == "sw" || mnemonicString == "sd") { this->Store = true;}
  else if(mnemonicString == "lb" || mnemonicString == "lh" || mnemonicString == "lw" || mnemonicString == "ld") this->Load = true;
  else {this->Store = false; this->Load = false;}

  // Init the Reg Index
  // this->Rd = -1;
  // this->Rs1 = -1;
  // this->Rs2 = -1;
  // this->Rs3 = -1;

  //Init LocalEdge
  for (int i = 0; i < MAX_OPERAND; i++){
    Reg[i]        = -1;
    LocalCnt[i]   = false;
    GlobalCnt[i]  = 0;
    LocalEdge[i]  = NULL;
    DataOrAddr[i] = 0;
  }
  // rd, rs1, rs2(or imm)
  // R-type no imm;
  // I-type no rs2;
  // S-type, B-type no rd;
  // U-type, J-type no rs1, rs;
	//  ---------------------------------------------
  //  R-4 type has rs3! 
  //  TODO: Go fix this function
  //  ---------------------------------------------
  MCOperand operand;

  int type    = GetInstType(Opcode);
  int flag    = 0b10000;

  switch(type) {
    case RTYPE: {
      this->Funct3 = (BytesArray[1] & 0b01110000)>>4;
      this->Funct7 = (BytesArray[3] & 0b11111110)>>1;
      this->Type = 'R';
      break;
    }
    case ITYPE: {
      this->Funct3 = (BytesArray[1] & 0b01110000)>>4;
      this->Type = 'I';
      break;
    }
    case STYPE: {
      this->Funct3 = (BytesArray[1] & 0b01110000)>>4;
      this->Type = 'S';
      break;
    }
    // case BTYPE: { // duplicate with STYPE
    //   this->Funct3 = BytesArray[1] & 0b01110000;
    //   break;
    // }
    case JTYPE: {
      this->Type = 'J';

      break;
    }
    case R4TYPE: {
      this->Type = '4';
      break;
    }
    // case UTYPE: { // duplicate with JTYPE

    //   break;
    // }
    case UNKOWNTYPE: {
      this->Type = 'X';
      break;
    }
  }
  


  // cout<< "Opcodes = " << Opcode << "\n";
  //cout<< "GetInstType() Completes\n";
  //cout<< "Enter AssemblyInstruction::AssemblyInstruction\n";

  // if(type != -1 && RISCV_ISA ){
  //   int i = 0;
  //   while(i < MAX_OPERAND){
  //     //cout<< "Enter Loop\n";
  //     //cout<< "type&flag = "<<(type&flag) << ", type = " << type <<"\n";
  //     //cout<< "i = "<< i <<"\n";

  //     if(type&flag){
  //       //cout<< "Geting Operand" <<"\n";
  //       operand = MI->getOperand(i);
  //       if(operand.isReg()) {
  //         this->Reg[i] = operand.getReg() - 37;
  //       }
  //       //cout<< "Get Operand Completes!" <<"\n";

  //     }
  //     flag>>=1;
  //     i++;
  //   }
    
  //   //cout<< "IMM type&flag = "<<(type&flag) << ", type = " << type <<"\n";

  //   if((type&flag) && operand.isImm())
  //     this->Imm = int(operand.getImm());
  //   std::cout << "Loop1: This->reg[0, 1], imm:" << this->Reg[0] << ", " << this->Reg[1] << ", " << this->Imm << endl;
    
  //   return;
  // }

  for (int i = 0; i < num_operands; i++) {
    MCOperand operand = MI->getOperand(i);
    if(operand.isReg()) {
      int reg_isa_id = operand.getReg();
      // x86
      if(ISA_type == 1) {
        if(X86Disassembler::Reg::MODRM_REG_AL<=reg_isa_id && 
          reg_isa_id<=X86Disassembler::Reg::MODRM_REG_DIL) {
            SetDataWidth(i,8);
        } else if(X86Disassembler::Reg::MODRM_REG_AX <= reg_isa_id &&
                  reg_isa_id<=X86Disassembler::Reg::MODRM_REG_R15W){
            SetDataWidth(i,16);
        } else if(X86Disassembler::Reg::MODRM_REG_EAX<=reg_isa_id &&
                reg_isa_id<=X86Disassembler::Reg::MODRM_REG_R15D) {
            SetDataWidth(i,32);
        } else if(X86Disassembler::Reg::MODRM_REG_RAX<=reg_isa_id &&
                reg_isa_id<=X86Disassembler::Reg::MODRM_REG_R15) {
            SetDataWidth(i,64);
        } else if(X86Disassembler::Reg::MODRM_REG_XMM0<=reg_isa_id &&
                reg_isa_id<=X86Disassembler::Reg::MODRM_REG_XMM31) {
            SetDataWidth(i,128);
        } else if(X86Disassembler::Reg::MODRM_REG_YMM0<=reg_isa_id &&
                reg_isa_id<=X86Disassembler::Reg::MODRM_REG_YMM31) {
            SetDataWidth(i,256);
        }  else if(X86Disassembler::Reg::MODRM_REG_ZMM0<=reg_isa_id &&
                reg_isa_id<=X86Disassembler::Reg::MODRM_REG_ZMM31) {
            SetDataWidth(i,512);
        } 

      } else if(ISA_type==4) {
        SetDataWidth(i,64);
        if(Type == 'I') {
          if(Opcode == 0b0010011 && i<=1) {
              SetDataWidth(i,32);
          } else if(Opcode == 0b0000011 && i==0) {
            if(Funct3 == 0b0) 
              SetDataWidth(i,8);
            else if(Funct3 == 0b1) 
              SetDataWidth(i,16);
            else if(Funct3 == 0b10)
              SetDataWidth(i,32);
            else if(Funct3 == 0b11) 
              SetDataWidth(i,64);
            else if(Funct3 == 0b100)
              SetDataWidth(i,8);
            else if(Funct3 == 0b101)
              SetDataWidth(i,16);
            else if(Funct3 == 0b110)
              SetDataWidth(i,32);
          } else if(Opcode == 0b11011) {
            SetDataWidth(i,32);
          }
        } else if(Type == 'S'){
          if(Opcode == 0b0100011 && i==1) {
            if(Funct3 == 0b000) 
              SetDataWidth(i,8);
            else if(Funct3 == 0b001)
              SetDataWidth(i,16);
            else if(Funct3 == 0b010)
              SetDataWidth(i,32);
            else if(Funct3 == 0b011)
              SetDataWidth(i,64);
          }
        } else if(Type == 'R') {
          if(Opcode == 0b11101) {
            SetDataWidth(i,32);
          }
        }  
        if(IsCompressed) {
          if(Opcode == 0b00 && i==0) {
            switch(Funct3){
              case 0b001 :{
                SetDataWidth(i,64);
                break;
              }
              case 0b010 :{
                SetDataWidth(i,32);
                break;
              }
              case 0b011 :{
                SetDataWidth(i,64);
                break;
              }
              
            }
          } else if(Opcode == 0b00 && i==1) {
            switch(Funct3){
              case 0b101 :{
                SetDataWidth(i,64);
                break;
              }
              case 0b110 :{
                SetDataWidth(i,32);
                break;
              }
              case 0b111 :{
                SetDataWidth(i,64);
                break;
              }
              
            }
          } else if(Opcode == 0b01) {
            switch(Funct3){
              case 0b001 :{
                SetDataWidth(i,32);
                break;
              }
              case 0b100 :{
                SetDataWidth(i,32);
                break;
              }
            }
          } else if(Opcode == 0b10 && i==0) {
            switch(Funct3){
              case 0b010 :{
                SetDataWidth(i,32);
                break;
              }
              case 0b011 :{
                SetDataWidth(i,32);
                break;
              }
            }
          } else if(Opcode == 0b10 && i==1) {
            switch(Funct3){
              case 0b110 :{
                SetDataWidth(i,32);
                break;
              }
              case 0b111 :{
                SetDataWidth(i,32);
                break;
              }
            }
          }
        } 

      }
    }
    // 1 For store instruction, rd is data to be stored, rs1 is base address, imm is offset. In order to do dependency analysis, put data to rs2 instead of rd
    // 2 For branch instruction, rd ans rs1 are two reg to compare. In order to do dependency analysis, put rd to rs2
    if (i == 0) {
      if (operand.isReg()) {
        if (!this->Store && !this->IsBranch) {
          this->Reg[0] = operand.getReg() - 37;
        }else {
          this->Reg[0] = -1;
          this->Reg[2] = operand.getReg() - 37;
        }
      } else if (operand.isImm()) {
        this->hasImm = true;
        this->Imm = int(operand.getImm());
      } else {
        std::cout << "Unrecoginzed operand" << endl;
      }
    } else if (i == 1) {
      if (operand.isReg()) {
        this->Reg[1] = operand.getReg() - 37;
      } else if (operand.isImm()) {
        this->hasImm = true;
        this->Imm = int(operand.getImm());
      } else {
        std::cout << "Unrecoginzed operand" << endl;
      }
    } else if (i == 2) {
      if (operand.isReg()) {
        this->Reg[2] = operand.getReg() - 37;
      } else if (operand.isImm()) {
        this->hasImm = true;
        this->Imm = int(operand.getImm());
      } else {
        std::cout << "Unrecoginzed operand" << endl;
      }
    }
  }
}
// AssemblyInstruction::AssemblyInstruction(const AssemblyInstruction * inst) {
//   this->Size_Byte = inst->Size_Byte;
//   assert(inst->Address);
//   this->Address = inst->Address;
//   this->Opcode = inst->Opcode;
//   this->Funct3 = inst->Funct3;
//   this->Funct7 = inst->Funct7;
//   this->Funct2 = inst->Funct2;
//   this->Funct6 = inst->Funct6;
//   this->Funct4 = inst->Funct4;
//   this->MyBB = inst->MyBB;

//   this->Imm = inst->Imm;
//   this->hasImm = inst->hasImm;
//   this->Type = inst->Type; // Define: SB == 'B', UJ == 'J'
//   this->IsBranch = inst->IsBranch;
//   this->IsCall = inst->IsCall;
//   this->CallTarget = inst->CallTarget ? new AssemblyFunction(*(inst->CallTarget)) : nullptr;
//   this->IsCompressed = inst->IsCompressed;
//   this->Mnemonic = inst->Mnemonic;
//   this->BranchTarget = inst->BranchTarget;

//   this->NumOperands = inst->NumOperands;
//   this->NumRs = inst->NumRs;

//   this->Load = inst->Load;
//   this->Store = inst->Store;

//   this->Prologue = inst->Prologue;
//   this->Epilogue = inst->Epilogue;

//   for (int i = 0; i < MAX_OPERAND; ++i) {
//     this->Reg[i] = inst->Reg[i];
//     this->LocalCnt[i] = inst->LocalCnt[i];
//     this->GlobalCnt[i] = inst->GlobalCnt[i];
//     this->DataOrAddr[i] = inst->DataOrAddr[i];
//     this->DataWidth[i] = inst->DataWidth[i];

//     this->LocalEdge[i] = inst.LocalEdge[i];
//     for (int j = 0; j < inst.GlobalEdge[i].size(); ++j) {
//       this->GlobalEdge[i][j] = new AssemblyInstruction(*(inst.GlobalEdge[i][j]));
//     }
//   }

//   // printf("Warn: Instruction 发生拷贝, asm adress: %08x\n", this->Address);
// }

AssemblyInstruction::AssemblyInstruction() { }

AssemblyInstruction::~AssemblyInstruction() { }

void AssemblyInstruction::setBranch(uint64_t Target) {
  this->IsBranch = true;
  this->BranchTarget = Target;
  this->Reg[2] = this->Reg[0];
  this->Reg[0] = -1;
}

void AssemblyInstruction::setCall(uint64_t Target, AssemblyFunction* targetFunc) {
  this->IsCall = true;
  this->BranchTarget = Target;
  this->CallTarget = targetFunc;
}

uint64_t AssemblyInstruction::getAddress() const { return this->Address; }

uint64_t AssemblyInstruction::getTarget() const { return this->BranchTarget; } 

string AssemblyInstruction::getFullMnemonic() const {
  string result = this->Mnemonic;
  result += " ";
  if(this->Reg[0] >= 0) {
    result += to_string(this->Reg[0]);
  }
  if(this->Reg[2] >= 0) {
    result += ", ";
    result += to_string(this->Reg[2]);
  }
  if(this->Reg[1] >= 0) {
    result += ", ";
    result += to_string(this->Reg[1]);
  }
  if(this->Imm != -1) {
    result += ", ";
    result += to_string(this->Imm);
  }
  // cout << "getFullMnemonic: " << result << endl;
  return result;
  // For store instruction, sw rd rs1 imm means rd->imm(rs1)
  // For load instruction, lw rd, rs1, imm means imm(rs1)->rd
}

void AssemblyInstruction::dump() const {
  // printf("address: 0x%x\topcode: 0x%x\ttype: %c\n", this->Address, this->Opcode, this->Type);
  printf("address: 0x%x\topcode: 0x%x\ttype: %c\tfunct3: %x\tfunct7: %x\t \
          rd: %d wid:%d\trs1: %d wid:%d\trs2:%d wid:%d\tcnt1: %d, cnt2: %d, cnt3: %d\t" ,
         this->Address, this->Opcode, this->Type, this->Funct3, this->Funct7, this->Reg[0],this->DataWidth[0], this->Reg[1],this->DataWidth[1], this->Reg[2],this->DataWidth[2], this->LocalCnt[1], this->LocalCnt[2], this->LocalCnt[3]);
  if(this->hasImm) {
    printf("imm: %d\t", this->Imm);
  }
  if(this->IsBranch) {
    printf("branchTarget: %d\t", this->BranchTarget);
  }
  if(this->IsCall) {
    printf("callTarget: %s\t", this->CallTarget->getName().c_str());
  }
  printf("Mnemonic: %s\t", this->getFullMnemonic().c_str());
  if(this->Prologue) {
    printf("IsPrologue\t");
  }
  if(this->Epilogue) {
    printf("IsEpilogue\t");
  }
  if(this->Load||this->Store){
    cout << "Data Root = " << this->DataRoot << "\t";
    if(DataRoot == "RISCV_GLOBAL"){
      cout << "Symbol Name = " << GlobalSymbol.name << "\t" ;
      if(GlobalSymbol.offset)
        cout << "Offset = " << GlobalSymbol.offset << "\t";
    }
    
  }

  printf("Binary: ");
  const uint8_t *data = this->Bytes.data();
  int size = this->Bytes.size();
  for (int i = size - 1; i >= 0; i--) {
    printf("%x ", data[i]);
  }
  printf("\n\n");

  //printf("\nAssemblyInstruction::dump() starts \n\n");
  auto n = const_cast<AssemblyInstruction*>(this);
  if(n->getRs1() != -1){
    //printf("AssemblyInstruction::dump() Checking Rs1..... \n\n");
    // printf("Edge.size() = %d\n",Edge.size() );
    // printf("Edge[1].size() = %d\n",Edge[1].size() );
    if(n->LocalEdge[RS1] != NULL)
          printf("\tLocal Edge of RS1: Inst address: 0x%x  %s\n ", LocalEdge[RS1]->getAddress(), LocalEdge[RS1]->getFullMnemonic().c_str());
       
    for(int i = 0; i < n->GlobalEdge[RS1].size(); i++){
        if(n->GlobalEdge[RS1][i] != NULL)
          printf("\tGlobal Edge of RS1: Inst address: 0x%x  %s\n ", GlobalEdge[RS1][i]->getAddress(), GlobalEdge[RS1][i]->getFullMnemonic().c_str());
   
    }
    printf("\n");
  }

  if(n->getRs2() != -1){
    //printf("AssemblyInstruction::dump() Checking Rs2..... \n\n");
    if(n->LocalEdge[RS2] != NULL)
          printf("\tLocal Edge of RS2: Inst address: 0x%x  %s\n ", LocalEdge[RS2]->getAddress(), LocalEdge[RS2]->getFullMnemonic().c_str()); 
    for(int i = 0; i < n->GlobalEdge[RS2].size(); i++){
        if(n->GlobalEdge[RS2][i] != NULL)
          printf("\tGlobal Edge of RS2: Inst address: 0x%x  %s\n ", GlobalEdge[RS2][i]->getAddress(), GlobalEdge[RS2][i]->getFullMnemonic().c_str());
    }
    printf("\n");
    //printf("AssemblyInstruction::dump() Check Rs2 Passed! \n\n");

  }
  if(n->getRs3() != -1){
    //printf("AssemblyInstruction::dump() Checking Rs3..... \n\n");


    if(n->LocalEdge[RS3] != NULL)
          printf("\tLocal Edge of RS3: Inst address: 0x%x  %s\n ", LocalEdge[RS3]->getAddress(), LocalEdge[RS3]->getFullMnemonic().c_str());
       
    for(int i = 0; i < n->GlobalEdge[RS3].size(); i++){
        if(n->GlobalEdge[RS3][i] != NULL)
          printf("\tGlobal Edge of RS3: Inst address: 0x%x  %s\n ", GlobalEdge[RS3][i]->getAddress(), GlobalEdge[RS3][i]->getFullMnemonic().c_str());
   
    }

    printf("\n");
    //printf("AssemblyInstruction::dump() Check Rs3 Passed! \n\n");


  }
  printf("\n");
}

bool AssemblyInstruction::getIsBranch() const { return this->IsBranch; }

bool AssemblyInstruction::getIsCall() const { return this->IsCall; }

AssemblyFunction* AssemblyInstruction::getCallTarget() const { return this->CallTarget; }

char AssemblyInstruction::getType() const { return this->Type; }

int AssemblyInstruction::getSizeByte() const { return this->Size_Byte; }

uint32_t AssemblyInstruction::getOpcode() const {return this->Opcode;}

int32_t AssemblyInstruction::getRs(int index){ return this->Reg[index]; }

int32_t AssemblyInstruction::getRs1(){return this->Reg[1];}

int32_t AssemblyInstruction::getRs2(){return this->Reg[2];}

int32_t AssemblyInstruction::getRs3(){return this->Reg[3];}

int32_t AssemblyInstruction::getRd(){return this->Reg[0];}

int32_t AssemblyInstruction::setRd(int32_t rd){this->Reg[0] = rd ;}

int AssemblyInstruction::getImm(){return this->Imm;}

uint32_t AssemblyInstruction::getOpcode(){return this->Opcode;}

int AssemblyInstruction::BuildEdge(AssemblyInstruction* inst, int rs, int LocalOrGlobal){
  //printf("\n\nBuild Edge Starts!, RS = %d\n\n", rs);

  if(LocalOrGlobal == LOCALEDGE)
    this->LocalEdge[rs] = inst;
  else{
    this->GlobalEdge[rs].push_back(inst);
    //printf("\n\nBuild Global Edge from inst 0x%x to 0x%x!\n\n",this->getAddress(), inst->getAddress() );
    //printf("Build Global Edge for inst 0x%x!\n\n",  this->GlobalEdge[rs][GlobalEdge[rs].size()-1]->getAddress());

  }
  

  //printf("\n\nBuild Edge Done!\n\n");

  return 0;

}

vector<AssemblyInstruction*>& AssemblyInstruction::getGlobalEdge(int rs){

  // if(this->GlobalEdge[index].size() < index)
  //   return NULL;

  // else 
    return this->GlobalEdge[rs];

}

vector<AssemblyInstruction*>::iterator AssemblyInstruction::global_edge_begin(int rs) {
  return (*this).GlobalEdge[rs].begin();

}

vector<AssemblyInstruction*>::iterator AssemblyInstruction::global_edge_end(int rs) {
  return (*this).GlobalEdge[rs].end();
}

AssemblyInstruction* AssemblyInstruction::getLocalEdge(int index){

  if(this->LocalEdge.size() < index)
    return NULL;

  else 
    return ((this->LocalEdge[index]));

}

void AssemblyInstruction::FoundLocalEdge(int rs){
    //printf("FoundLocalEdge Starts!\n\n");

    this->LocalCnt[rs] = true;

    //printf("FoundLocalEdge Done!\n\n");

}


int AssemblyInstruction::FoundGlobalEdge(int rs){
    
    //printf("FoundGlobalEdge Starts!\n\n");
    
    // Count global edges of rs
    return (++(this->GlobalCnt[rs]));

    //printf("FoundGlobalEdge Done!\n\n");

}


void AssemblyInstruction::printArrays() {
  std::cout << "\ncontent of local edge count: \n";
  for (int i = 0; i < MAX_OPERAND; ++i) std::cout << LocalCnt[i] << " .. ";
  std::cout << endl;
  std::cout << "\ncontent of global edge count: \n";
  for (int i = 0; i < MAX_OPERAND; ++i) std::cout << GlobalCnt[i] << " .. ";
  std::cout << endl;
  std::cout << "\ncontent of local edge : \n";
  for (int i = 0; i < MAX_OPERAND; ++i) std::cout << LocalEdge[i] << " .. ";
  std::cout << endl;
}

void AssemblyInstruction::printEdges() {
  for (int i = 1; i < MAX_OPERAND; ++i) {
    if (!hasGlobalEdge(i)) continue;
    std::cout << "[ ";
    for (int j = 0; j < GlobalEdge[i].size(); ++j) {
      std::cout << GlobalEdge[i][j]->hashCode() << ", ";
    }
    std::cout << " ]" << std::endl;
  }
}

bool AssemblyInstruction::HasLocalEdge(int rs){
  //  std::cout << "\ncontent of local edge count: \n";
  //  for (int i = 0; i < MAX_OPERAND; ++i) std::cout << LocalCnt[i] << " .. ";
  //  std::cout << endl;
   return(this->LocalCnt[rs]);

}

bool AssemblyInstruction::hasGlobalEdgeToDealWith() {
  for (int i = 1; i < MAX_OPERAND; ++i) if (GlobalEdge[i].size() >= 2) return true;
  return false;
}

bool AssemblyInstruction::hasGlobalEdge(int index){
  assert(GlobalCnt[index] == GlobalEdge[index].size());
  return GlobalEdge[index].size();
}

int AssemblyInstruction::HasGlobalEdge(int rs){

  return this->GlobalCnt[rs];
}

uint64_t AssemblyInstruction::hashCode() {
  return Address;
}

void AssemblyInstruction::SetOpAttr(int reg, int flag){

  if(this->DataOrAddr[reg] == 0){
      this->DataOrAddr[reg] = flag;
      return;
  }
  else if(flag != this->DataOrAddr[reg] && this->DataOrAddr[reg]<BOTH)
      this->DataOrAddr[reg] = BOTH;

}


bool AssemblyInstruction::IsLoad() { return this->Load; }

bool AssemblyInstruction::IsStore() { return this->Store; }

void AssemblyInstruction::SetIsCompressed(bool b) { this->IsCompressed = b; }

void AssemblyInstruction::SetLoad() { this->Load = true; }

void AssemblyInstruction::SetStore() { this->Store = true; }

void AssemblyInstruction::SetDataWidth(int reg, int width) { DataWidth[reg] = width; }

int AssemblyInstruction::getDataWidth(int reg) { return DataWidth[reg]; }

void AssemblyInstruction::setBB(int BB){MyBB = BB; }

int AssemblyInstruction::getBB(){return MyBB; }

void AssemblyInstruction::setPrologue(){ Prologue = true; }

void AssemblyInstruction::setEpilogue(){ Epilogue = true; }

bool AssemblyInstruction::ifPrologueEpilogue(){
  if(this->Epilogue || this->Prologue)
    return true;
  else 
    return false;
}


string AssemblyInstruction::getMnemonic(){return this->Mnemonic;}


set<pair<int, int>>& AssemblyInstruction::getColors() {return this->colors;}
void AssemblyInstruction::addColor(int color, int type) {this->colors.insert({color, type});}
bool AssemblyInstruction::hasColor(int c) {
  for (auto const &p: colors) {
    if (c == p.first)  return true;
  }
  return false;
}





// void AssemblyInstruction::addDataSource(AssemblyInstruction* inst){
  
//   DataSource.push_back(inst);

// }

// std::vector<AssemblyInstruction*>& AssemblyInstruction::getDataSource(){
  
//   return DataSource;
// }


void AssemblyInstruction::setDataRoot(string source){
  DataRoot = source;
}


std::string AssemblyInstruction::getDataRoot(){
  return DataRoot;
}

GlobalData AssemblyInstruction::getGlobalData(){
  return GlobalSymbol;
}

std::string AssemblyInstruction::getGlobalSymbolName(){
  return GlobalSymbol.name;
}

void AssemblyInstruction::setGlobalData(GlobalData g){
   GlobalSymbol = g;
}
