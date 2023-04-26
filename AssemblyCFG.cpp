//===-- AssemblyCFG.cpp ------------------------------------------*- C++
//-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "AssemblyBasicBlock.h"
#include "AssemblyCFG.h"
#include "PHI.h"
#include "AssemblyFunction.h"
#include "ASMUtils.h"
#include <unordered_set>

extern int64_t  GP_BASE;


using namespace llvm;
using namespace std;

AssemblyCFG::AssemblyCFG(AssemblyFunction* function, uint64_t start_address, int start_offset, uint64_t end_address) {
  Size = 0;
  FunctionName = function->getName();
  startAddress = start_address;
  endAddress = end_address;
  startOffset = start_offset;
  this->Function = function;
}

// AssemblyCFG::AssemblyCFG(const AssemblyCFG& cfg) {
//   this->Size = cfg.Size;
//   this->FunctionName = cfg.FunctionName;
//   this->startAddress = cfg.startAddress;
//   this->endAddress = cfg.endAddress;
//   this->startOffset = cfg.startOffset;
//   this->Function = new AssemblyFunction(*Function);

//   for (AssemblyBasicBlock* b : cfg.BasicBlocks) {
//     this->BasicBlocks.push_back(new AssemblyBasicBlock(*b));
//   }
//   printf("Error: CFG 发生拷贝...\n");
// }


int AssemblyCFG::addBasicBlock(AssemblyBasicBlock *ass_bb) {
  this->BasicBlocks.push_back(ass_bb);
  this->Size++;
  return this->Size;
}
int AssemblyCFG::getSize() const { return this->Size; }

string AssemblyCFG::getName() const { return this->FunctionName; }

AssemblyFunction* AssemblyCFG::getFunction(){return this->Function;}

// void AssemblyCFG::AddFunction(){
//     this->Function(this->function_name, uint64_t start_address, uint64_t end_address)

// }

vector<AssemblyBasicBlock*> &AssemblyCFG::getBasicBlocks() {
  return this->BasicBlocks;
}

vector<AssemblyBasicBlock*>::iterator AssemblyCFG::begin() {
  return (*this)
            .BasicBlocks
            .begin();
}

vector<AssemblyBasicBlock*>::iterator AssemblyCFG::end() {
  return (*this)
            .BasicBlocks
            .end();
}

void AssemblyCFG::dump() {
  cout << "CFG dump: funcName = " << this->FunctionName
       << ", Size = " << this->Size << ".\nPhony Stack Size = "
       << this->getPhonyStack() << "\nReal Stack Size = "
       << this->getRealStack()
       << ".\n";



  this->Function->dump();
  cout << "\n\n";     
  for (vector<AssemblyBasicBlock*>::iterator it = this->BasicBlocks.begin();
       it != this->BasicBlocks.end(); it++) {
    vector<AssemblyInstruction*> instrs = *((*it)->getInstructions());
    set<AssemblyBasicBlock*> pre = (*it)->getPredecessors();
    set<AssemblyBasicBlock*> suc = (*it)->getSuccessors();
    cout << "AssemblyBasicBlock." << (*it)->getId() << ":\n";
    cout << ";\t predecessors: ";

    for (set<AssemblyBasicBlock*>::iterator it = pre.begin(); it != pre.end(); it++) {
      cout << (*it)->getId() << ", ";
    }
    cout << "\n\t successors: ";
    for (set<AssemblyBasicBlock*>::iterator it = suc.begin(); it != suc.end(); it++) {
      cout << (*it)->getId() << ", ";
    }
    cout << "\n\n";
    (*it)->dump();
    cout << "\n\n";
  //   cout << "instructions.size = " << instrs.size() << endl;
  //   for (int i = 0; i < instrs.size(); i++) {
  //     cout << "\t";
  //     instrs[i].dump();
  //   }
  }
  cout << "CFG dump: funcName = " << this->FunctionName << ", End." << endl << endl;
}

uint64_t AssemblyCFG::getStartAddress() const{ return startAddress; }

uint64_t AssemblyCFG::getEndAddress() const{ return endAddress; }

int AssemblyCFG::getStartOffset() { return startOffset; }

AssemblyCFG::~AssemblyCFG() {free(this->Function);}


bool IsSavedReg(int reg){

    // Reg s2 - s11
    if(reg >= 18 && reg <= 27)
      return true;
    
    // Reg s0, s1
    if(reg == 8 || reg == 9)
      return true;

    if(reg == 1)
      return true;

    // Reg fs0 - fs1
    if(reg == 40 || reg == 41)
      return true;

    // Reg fs2 - fs11
    if(reg == 50 || reg == 59)
      return true;    



    return false;

}


int AssemblyCFG::FindPrologue(){

  int   size    = 0;
  auto  InstVec = (*(this->BasicBlocks.begin()))->getInstructions();
  
  vector<AssemblyInstruction*>::iterator it; 

  for(it = InstVec->begin(); it!= InstVec->end();it++){
      // cout << "DEBUG:: FindPrologue():: getMnemonic() = "<< it->getMnemonic() 
      //      << ", string width = "<< it->getMnemonic().length() << "\n";

      if((*it)->getMnemonic() == "addi" || (*it)->getMnemonic() == "addiw")
        // SP reg for RISCV
        if((*it)->getRd() == 2 && (*it)->getRs1() == 2){
          this->setPhonyStack(abs((*it)->getImm()));
          break;
        }
  }

  if(it != InstVec->end() ){
    it++;
    for(;it != InstVec->end() ; it++){
        // Integer or floating store
        if((*it)->getOpcode() ==  0b0100111 || (*it)->getOpcode() == 0b0100011){
          if((*it)->getRs1() == 2 && IsSavedReg((*it)->getRs2())){
            //cout << "DEBUG:: FindPrologue():: FOUND Prologue!!!!\n";

            if(FindMatchedEpilogue((*it)->getRs2(), size)){
              this->Prologue.push_back(*it);
              (*it)->setPrologue();
            }
            else 
              cout << "Epilogue ERROR:: Mismatched Prologue!!! at Inst Addr " 
                   << (*it)->getAddress()
                   << ", Saved Reg Index = " 
                   << (*it)->getRs2() << endl;
          }
        }
    }
  }


  // Get PhonyStack Size
  // vector<AssemblyInstruction*>::reverse_iterator rit;
  // for(rit = InstVec->rbegin(); rit!= InstVec->rend();rit++){

  //     if(rit->getMnemonic() == "addi\t" || rit->getMnemonic() == "addiw\t")
  //       // SP reg for RISCV
  //       if(rit->getRd() == 2 && rit->getRs1() == 2){
  //           cout << "DEBUG:: rit->getImm() = " << rit->getImm() << endl;
  //           this->setPhonyStack(rit->getImm());
  //           break;
  //       }
          
  // }

  // if(rit == InstVec->rend())
  //   return 0;

  // Get Real Size
  if(this->getPhonyStack() != 0){
    this->setRealStack(getPhonyStack() - size/8);

    cout << "Phony Stack Size = " << this->getPhonyStack() << ", Real Stack Size = " 
       << this->getRealStack() << endl;
  }
       

  return 0;
}


int AssemblyCFG::FindMatchedEpilogue(int reg, int& size){

  auto  InstVec = (*(this->BasicBlocks.rbegin()))->getInstructions();

  vector<AssemblyInstruction*>::reverse_iterator it;
  for(it = InstVec->rbegin();it != InstVec->rend() ; it++){
        // Integer or floating load
    if((*it)->getOpcode() ==  0b0000111 || (*it)->getOpcode() == 0b0000011){
        // (*it)->getRd() are save registers 
        if((*it)->getRs1() == 2 && (*it)->getRd() == reg){
              this->Epilogue.push_back(*it);
              (*it)->setEpilogue();
              size += (*it)->getDataWidth(RD);
              return 1;
        }
    }
  }
  return 0;

}

int AssemblyCFG::FindEpilogue(){


  auto  InstVec = (*(this->BasicBlocks.rbegin()))->getInstructions();
  int   size    = 0;

  vector<AssemblyInstruction*>::reverse_iterator it;
  for(it = InstVec->rbegin(); it!= InstVec->rend();it++){

      if((*it)->getMnemonic() == "addi" || (*it)->getMnemonic() == "addiw")
        // SP reg for RISCV
        if((*it)->getRd() == 2 && (*it)->getRs1() == 2){
            this->setPhonyStack((*it)->getImm());
            break;
        } 
  }


  if(it != InstVec->rend() ){
    it++;
    for(;it != InstVec->rend() ; it++){
        // Integer or floating load
        if((*it)->getOpcode() ==  0b0000111 || (*it)->getOpcode() == 0b0000011){
          if((*it)->getRs1() == 2 && IsSavedReg((*it)->getRd())){
              this->Epilogue.push_back(*it);
              (*it)->setEpilogue();
              size += (*it)->getDataWidth(RD);
          }
        }
    }

  }

  this->setRealStack(getPhonyStack() - size);

  return 0;

}




  void AssemblyCFG::setPhonyStack(int size ){this->phonystacksize = size;}
  int  AssemblyCFG::getPhonyStack(){return this->phonystacksize;}

  void AssemblyCFG::setRealStack(int size ){this->stacksize = size;}
  int  AssemblyCFG::getRealStack(){return this->stacksize;}




int AssemblyCFG::FindRet(){


  //auto  InstVec = (*(this->BasicBlocks.rbegin()))->getInstructions();
  int   size    = 0;

  vector<AssemblyInstruction*>::reverse_iterator it;

  // Go through all the BBs of current Function, in case there are branches before
  // the return value assignment to a0 or fa0, leading to return value assignment not in last BB.
  for(auto bb = this->BasicBlocks.rbegin(); bb!=this->BasicBlocks.rend(); bb++){
    auto InstVec = (*bb)->getInstructions();
    for(it = InstVec->rbegin(); it!= InstVec->rend();it++){
        // rd = a0 or fa0
        if((*it)->getRd() == 10 || (*it)->getRd() == 42 ){
            this->getFunction()->setReturn((*it)->getDataWidth(RD)); 
            // We set the return type to data by default.
            // The addressing analysis in TraverseLoadStore will reset 
            // return type to IsAddr if the function returns a pointer
            if(this->getFunction()->getReturnType() == 0)
              this->getFunction()->setReturnType(IsData);
            return 1;
        } 

    }
  }

  return 0;

}



void AssemblyCFG::TraverseLoadStore(){

  //cout << "\tDEBUG:: Entering TraverseLoadStore()\n";
  AssemblyInstruction* inst;

  for (vector<AssemblyBasicBlock*>::iterator it = this->BasicBlocks.begin();
       it != this->BasicBlocks.end(); it++) {
    vector<AssemblyInstruction*> instrs = *((*it)->getInstructions());

    for (auto it = instrs.begin(); it != instrs.end(); it++) {
        if((*it)->IsLoad() || (*it)->IsStore()){
          //cout<< "DEBUG::  TraverseLoadStore() Found Load/Store instructions\n";

          if((*it)->getRs1() == 2)
              (*it)->setDataRoot("RISCV_SP");
              
          // Need double check.....
          // if((*it)->getRs1() == 10)
          //     (*it)->setDataRoot("RISCV_HEAP");
          else{
            std::unordered_set<AssemblyInstruction*> visited;
            inst = FindDataSource(*it, visited);
            if(inst){
       
              (*it)->setDataRoot(inst->getDataRoot());
              if((*it)->getDataRoot() == "RISCV_GLOBAL")
                (*it)->setGlobalData(inst->getGlobalData());

              //  cout  << "\tDEBUG::TraverseLoadStore()::  Inst: " << (*it)->getMnemonic() 
              //        << ", Address: 0x" << std::hex << (*it)->getAddress() 
              //        << ", Data Root: "
              //        << (*it)->getDataRoot() << endl;
             
            }
          }
        }
    } 
  }
}


GlobalData* AssemblyCFG::ComputeGlobalAddr(AssemblyInstruction* inst){

    cout << "DEBUG:: Entering ComputeGlobalAddr()\n";

    GlobalData* G = NULL;
    int64_t addr; 

    if(inst->getMnemonic()== "addi"){
      addr = GP_BASE + inst->getImm();
      //printf ("\tDEBUG:: ComputeGlobalAddr:: Addr  = %d\n", addr );
    }
    else{
    
      AssemblyInstruction* lui = inst;

      addr = lui->getImm();
      addr <<= 12;
      AssemblyInstruction* IfAdd = lui->getLocalEdge(RD);

      //cout << "\t DEBUG::ComputeGlobalAddr:: LUI Followed by Inst: " << IfAdd->getMnemonic() <<endl;
      //cout << "\t DEBUG::ComputeGlobalAddr:: LUI Imm =             " << addr <<endl;


      if(!IfAdd){
        cout << "\t DEBUG::ComputeGlobalAddr:: No Instruction Depends on LUI: 0x"
             << std::hex << lui->getAddress() <<endl; 
        return NULL;
      }
      else if(IfAdd->getMnemonic()=="addi" || IfAdd->getMnemonic()=="addiw"){
          cout << "\t DEBUG::ComputeGlobalAddr:: Found ADDI operation after LUI, Imm= 0x"
               << std::hex << IfAdd->getImm(); 
          addr += IfAdd->getImm();
      }
      else if(IfAdd->IsLoad()||IfAdd->IsStore()){
          cout << "\t DEBUG::ComputeGlobalAddr:: Found LOAD/STORE operation after LUI, Imm= 0x"
               << std::hex << IfAdd->getImm(); 
          addr += IfAdd->getImm();
      }
      else
        return NULL;

    }

    G=MatchGlobalData(addr);
    if(!G){
      G=MatchGlobalSection(addr);
      if(G)
        G->offset = addr - G->addr;
    }

    if(G)
      cout <<"\tDEBUG:: MatchGlobalData: "<< G->name << endl;

    //cout << "DEBUG:: Exiting ComputeGlobalAddr()\n";

    return G;
 

}


AssemblyInstruction* AssemblyCFG::FindDataSource(AssemblyInstruction* inst, std::unordered_set<AssemblyInstruction*> visited){

  if(!inst || visited.count(inst)) return NULL;
  visited.insert(inst);

  //cout << "DEBUG:: Entering FindDataSource()\n";

  AssemblyInstruction*            ret     =     NULL;
  GlobalData*                     G       =     NULL;
  int                             i       =     0;
  string                          name;


  //cout << "\tDEBUG::FindDataSource():: Found instruction " << inst->getMnemonic()
  //       << "\t ,Addr = 0x" << std::hex<< inst->getAddress() << endl;

  // Check if the address points to stack SP
  if(inst->getRd() == 2){
    inst->setDataRoot("RISCV_SP");
    return inst;
  }
  else if(inst->getMnemonic() == "jal" && inst->getRd() == 10){
    // cout << "\tDEBUG::FindDataSource():: Found JAL !\n";
    
    //name = MatchPLTFunction(inst->getImm());
    if(inst->getCallTarget()){
      inst->getCallTarget()->setReturnType(IsAddr);
      // All the addresses data width is 64 bit
      inst->getCallTarget()->setReturnWidth(64);
      name = inst->getCallTarget()->getName();
      inst->setDataRoot(name);
      return inst;
    }

  }
  // addi rd, gp, imm
  else if(inst->getMnemonic() == "addi" && inst->getRs1() == 3 && inst->getRd() != 3){
    // cout << "\tDEBUG::FindDataSource():: Found addi " << std::dec << inst->getRd()
    //      << ", gp, " << std::dec << inst->getImm() << endl;

    G = ComputeGlobalAddr(inst);
    if(G){
      inst->setGlobalData(*G);
      inst->setDataRoot("RISCV_GLOBAL");
      return inst;
    }
    return NULL;
  }
  // Check if the address points to global data
  // COMMNTED Due to the GP instructions used by RISC-V compiler toolchain
  else if(inst->getMnemonic() == "lui"){
    G = ComputeGlobalAddr(inst);
    if(G){
      inst->setGlobalData(*G);
      inst->setDataRoot("RISCV_GLOBAL");
      return inst;
    }
    return NULL;
  }


  for(i = RS1; i<=RS3; i++){
    if(inst->HasLocalEdge(i)){
      ret = FindDataSource(inst->getLocalEdge(i), visited);
      if(ret)
        return ret;
    }
  }
  
  for(i = RS1; i<=RS3; i++){
    if(inst->HasGlobalEdge(i)){
      for(auto it = inst->getGlobalEdge(i).begin(); it != inst->getGlobalEdge(i).end();it++){
        // skip the inst itself for loops to avoid infinite recursive calls
        if(inst == *it) 
          continue;   
        ret = FindDataSource((*it), visited);
        if(ret)
          return ret;
      }
    }
  }
  
  return NULL;


}




void AssemblyCFG::ProcessFuncCall(){

  //cout << "\tEntering ProcessFuncCall()\n";
  
  int                   CallFlag   =    0;
  AssemblyInstruction*  Call       =    NULL;


  for (vector<AssemblyBasicBlock*>::iterator it = this->BasicBlocks.begin();
       it != this->BasicBlocks.end(); it++) {
    vector<AssemblyInstruction*> instrs = *((*it)->getInstructions());
    
    // BY DEFAULT,  we assume the instruction next to call uses the a0
    // Double check if corner cases exist
    for (auto it = instrs.begin(); it != instrs.end(); it++) {
        if(CallFlag){
            CallFlag = 0;
            for(int i = RS1; i<=RS3; i++){
              if((*it)->getRs(i) == 10 ||  (*it)->getRs(i) == 42)
                  // cout << "\t Debug:: ProcessFuncCall():: Returned Inst "
                  //      << (*it)->getMnemonic() << ", Rs[" << i << "] = "
                  //      << (*it)->getRs(i) << "\n";

                  Call->setRd((*it)->getRs(i));

                  // cout << "\t Debug:: ProcessFuncCall():: Set Inst "
                  //      << Call->getMnemonic() << ", Rd = "
                  //      << Call->getRd() << "\n";

                  Call = NULL;
                  break;
            }
        }

        // TODO: jalr might be used and addresses need to be calculated
        if((*it)->getIsCall()){ 
          // RISC-V Call Process          
          if(ISA_type >=3 && (*it)->getMnemonic() == "jal"){
            CallFlag  =  1;
            Call = (*it);
            // cout << "\t Debug::\t Processing Function Call "
            //      << (*it)->getMnemonic() << "\n";
          }
        } 
    }
  }

  // cout << "\tLeavinging ProcessFuncCall()\n";


}


void AssemblyCFG::ProcessRISCVGP(){

  uint64_t addr = 0;

  // cout << "\tDEBUG:: ProcessRISCVGP():: Function Name is " 
  //      << this->getFunction().getName() << endl;
       
  if(this->getFunction()->getName() == "_start"){
     //cout << "\tDEBUG:: ProcessRISCVGP():: Entering _start\n "; 

    for (vector<AssemblyBasicBlock*>::iterator it = this->BasicBlocks.begin();
        it != this->BasicBlocks.end(); it++) {
      
        vector<AssemblyInstruction*> instrs = *((*it)->getInstructions());
        for (auto it = instrs.begin(); it != instrs.end(); it++) {
            // cout << "\tDEBUG:: ProcessRISCVGP():: Checking instruction : "
            //      << (*it)->getMnemonic() << ", \tSize = " <<  (*it)->getMnemonic().size() << endl;

            // auipc gp, imm
            if((*it)->getMnemonic() == "auipc" && (*it)->getRd()== 3){
                //cout << "\tDEBUG:: ProcessRISCVGP():: Find auipc!\n "; 

                auto next = it + 1;
                // addi gp, gp, imm
                if((*next)->getMnemonic() == "addi" && (*next)->getRd()==3 && (*next)->getRs1()==3){
                     GP_BASE   =     (*it)->getAddress();
                     GP_BASE   +=    (*it)->getImm() << 12;
                     GP_BASE   +=    (*next)->getImm();
                     cout << "\tDEBUG:: GP_BASE = 0x" << std::hex <<GP_BASE << endl;
                     return;
                }
            }
        }
    }
  }

}
