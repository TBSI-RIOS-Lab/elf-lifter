//===-- AssemblyBasicBlock.cpp ------------------------------------------*- C++
//-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "AssemblyBasicBlock.h"
#include "AssemblyInstruction.h"
#include "AssemblyFunction.h"
#include "PHI.h"
#include <iostream>
using namespace std;


int SelfPtr = 0;

AssemblyBasicBlock::AssemblyBasicBlock(int id, uint64_t start_address,
                                       uint64_t end_address) {
  this->Id = id;
  this->StartAddress = start_address;
  this->EndAddress = end_address;
  this->phi.clear();
  this->CallTarget = nullptr;
}

int AssemblyBasicBlock::addInstruction(AssemblyInstruction *ins) {
  ins->setBB(this->getId());
  this->Instructions.push_back(ins);
  //cout << "Adding Instruction " << ins->getMnemonic() << ", Opcode :: " << ins->getOpcode() << endl;

  if(ins->getOpcode() == RVLOAD){
    //cout<< "DEBUG::  Adding load instructions\n";
    ins->SetLoad();
    ins->SetOpAttr(RD, IsData);
    ins->SetOpAttr(RS1, IsAddr);
  }
  else if (ins->getOpcode() == RVSTORE){
    //cout<< "DEBUG::  Adding store instructions\n";
    ins->SetStore();
    ins->SetOpAttr(RS1, IsAddr);
    ins->SetOpAttr(RS2, IsData);
  }
  this->Size = this->Size + 1;
  return this->Size;
}

// AssemblyBasicBlock::AssemblyBasicBlock(const AssemblyBasicBlock& bb) {
//   std::cout << "Warn: Basic Block 发生拷贝..." << std::endl;
//   this->Id = bb.Id;
//   this->StartAddress = bb.StartAddress;
//   this->EndAddress = bb.EndAddress;
//   for (AssemblyInstruction* i : bb.Instructions) {
//     this->Instructions.push_back(new AssemblyInstruction(i));
//   }
//   for (auto b : bb.Predecessors) {
//     this->Predecessors.insert(b);
//   }
//   for (auto b : bb.Successors) {
//     this->Successors.insert(b);
//   }
// }

int AssemblyBasicBlock::addPredecessor(AssemblyBasicBlock* bb) {
  this->Predecessors.insert(bb);
}

int AssemblyBasicBlock::addSuccessor(AssemblyBasicBlock* bb) {
  this->Successors.insert(bb);
}

void AssemblyBasicBlock::setEndAddress(uint64_t end_address) {
  this->EndAddress = end_address;
}

void AssemblyBasicBlock::setCallTarget(AssemblyFunction* f) {
  this->CallTarget = f;
}

void AssemblyBasicBlock::dump() {
  cout << "\tBB dump: id = " << this->Id << ", start = " << this->StartAddress
       << ", end = " << this->EndAddress << ", Size = " << this->Size << ".\n";
  int cnt = 1;
  //cout << "now dump Instructions of BB"<<endl;
  for(int i=0; i<Instructions.size();i++) {
    Instructions[i]->dump();
  }
  if(this->CallTarget) {
    cout<<"Has Call Target: "<<this->CallTarget->getName()<<endl;
  }
  //cout << "done dump Instructions of BB\n";
  if( this->phi.size() == 0){
    cout<<"\tNo Phi nodes in current BB\n";
    return;
  }
  cout << "\tBB PHI dump: ";
  for(auto it = this->phi.begin(); it!= this->phi.end();it++){
      cout<< "\n\tPHI node [" << cnt << "]:\n";
      for(auto p = it->begin(); p!=it->end();p++){
        cout<< "\tSrc Inst Addr: 0x" << std::hex << p->first->getAddress()
            << " Pair = < Addr: 0x" << p->second->getAddress()
            <<", BB ID: " << std::dec <<p->second->getBB()<<">\n";
      }
      cnt++;
      cout<<endl;
  }
  //cout << "\t AssemblyBasicBlock::dump() completes\n";


}

AssemblyBasicBlock::~AssemblyBasicBlock() {}

int AssemblyBasicBlock::getId() const{ return this->Id; }

int AssemblyBasicBlock::getSize() const{ return this->Size; }

AssemblyFunction* AssemblyBasicBlock::getCallTarget() const{ return this->CallTarget; }

uint64_t AssemblyBasicBlock::getStartAddress() const{
  return this->StartAddress;
}

uint64_t AssemblyBasicBlock::getEndAddress() const{ return this->EndAddress; }

vector<AssemblyInstruction*>* AssemblyBasicBlock::getInstructions() {
  return &(this->Instructions);
}

vector<AssemblyInstruction*>::iterator AssemblyBasicBlock::begin() {
  return (*this)
            .Instructions
            .begin();
}

vector<AssemblyInstruction*>::iterator AssemblyBasicBlock::end() {
  return (*this)
            .Instructions
            .end();
}

vector<PHI>::iterator AssemblyBasicBlock::phi_begin() {
  return (*this)
            .phi
            .begin();
}

vector<PHI>::iterator AssemblyBasicBlock::phi_end() {
  return (*this)
            .phi
            .end();
}

set<AssemblyBasicBlock*>::iterator AssemblyBasicBlock::pre_begin() {
  return (*this)
            .Predecessors
            .begin();
}

set<AssemblyBasicBlock*>::iterator AssemblyBasicBlock::pre_end() {
  return (*this)
            .Predecessors
            .end();
}

set<AssemblyBasicBlock*>::iterator AssemblyBasicBlock::suc_begin() {
  return (*this)
            .Successors
            .begin();
}

set<AssemblyBasicBlock*>::iterator AssemblyBasicBlock::suc_end() {
  return (*this)
            .Successors
            .end();
}

set<AssemblyBasicBlock*> AssemblyBasicBlock::getPredecessors() {
  return this->Predecessors;
}

set<AssemblyBasicBlock*> AssemblyBasicBlock::getSuccessors() {
  return this->Successors;
}


void AssemblyBasicBlock::addPhi(PHI p){this->phi.push_back(p);}



uint64_t AssemblyBasicBlock::hashCode() {
  return StartAddress ^ (EndAddress << Id);
}


int AssemblyBasicBlock::BuildLocalEdge(){
    //printf("Local Edge Build Starts!\n\n");

    
    AssemblyInstruction* Def                  = NULL;
    std::vector<AssemblyInstruction*>* InstVec = this->getInstructions();
    //printf("Entering Loop \n\n");
    for(int i = InstVec->size()-1; i >= 0; i--){
        if((*InstVec)[i]->getRs1() != -1 ){
          Def = FindLocalDef((*InstVec)[i]->getRs1(), InstVec, i);
          if(Def){
            // Link from Def to Use
            (*InstVec)[i]->BuildEdge(Def, RS1, LOCALEDGE);
            (*InstVec)[i]->FoundLocalEdge(RS1);
            // Reverse Link from Def to Use
            Def->BuildEdge((*InstVec)[i], RD, LOCALEDGE);
            Def = NULL; 
          }
        }
        //printf("Check RS1 passed \n\n");

        if((*InstVec)[i]->getRs2()!= -1){ 
          Def = FindLocalDef((*InstVec)[i]->getRs2(), InstVec, i);
          if(Def){
       // Link from Def to Use
            (*InstVec)[i]->BuildEdge(Def, RS2, LOCALEDGE);
            (*InstVec)[i]->FoundLocalEdge(RS2);
            // Reverse Link from Def to Use
            Def->BuildEdge((*InstVec)[i], RD, LOCALEDGE);
            Def = NULL; 
          }
        }
        //printf("Check RS2 passed \n\n");
        if((*InstVec)[i]->getRs3()!= -1){ 
          Def = FindLocalDef((*InstVec)[i]->getRs3(), InstVec, i);
          if(Def){
            // Link from Def to Use
            (*InstVec)[i]->BuildEdge(Def, RS3, LOCALEDGE);
            (*InstVec)[i]->FoundLocalEdge(RS3);
            // Reverse Link from Def to Use
            Def->BuildEdge((*InstVec)[i], RD, LOCALEDGE);
            Def = NULL; 
          }
        }
    //    printf("Check RS3 passed \n\n");
    }
    //printf("Local Edge Build Done!\n\n");


}

// Args: StartColor: First color in this BB
// Return: Last color in this BB
int AssemblyBasicBlock::PaintColor(int StartColor){
  cout << "Start PaintColor()" << endl;
  // There are three types of path need to color
  // 1. From bottom `store` data path to first `load` or `phi`
  // 2. From bottom `store` address path to first `load` or `phi`
  // 3. From bottom `branch` to first `load` or `phi`
  string t = "";
  // First step: Collect every store and branch
  std::vector<AssemblyInstruction*>* InstVec = this->getInstructions();
  for(int i = InstVec->size()-1; i >= 0; i--){
    if((*InstVec)[i]->IsStore()){
      cout << "IsStore" << endl;
      // int32_t addressReg = (*InstVec)[i]->Reg[1];
      // int32_t dataReg = (*InstVec)[i]->Reg[2];
      printf("Color: %d, Type: Addressing\n", StartColor);
      paintInsColorRecursive((*InstVec)[i], 1, StartColor, 1, 0); // addressReg in LocalEdge[1/RS1]
      StartColor++;
      printf("Color: %d, Type: Data Compute\n", StartColor);
      paintInsColorRecursive((*InstVec)[i], 2, StartColor, 0, 0); // dataReg in LocalEdge[2/RS2]
      StartColor++;
    }else if((*InstVec)[i]->getIsBranch()){
      cout << "IsSBranch" << endl;
      printf("Color: %d, Type: Control Flow\n", StartColor);
      paintInsColorRecursive((*InstVec)[i], 2, StartColor, 2, 0);
      StartColor++;
      printf("Color: %d, Type: Control Flow\n", StartColor);
      paintInsColorRecursive((*InstVec)[i], 1, StartColor, 2, 0);
      StartColor++;
    }
  }
  return StartColor;
}

void AssemblyBasicBlock::paintInsColorRecursive(AssemblyInstruction* ins, int tracedReg, int color, int type, int depth) {
  ins->addColor(color, type);
  for(int i = 0; i < depth; i++) {cout << "  ";}
  printf("%x(%d) %s ->\n", ins->getAddress(), color, ins->getFullMnemonic().c_str());
  if(!ins->IsLoad() && ins->getLocalEdge(tracedReg) != NULL) { // Paint until `load` or `phi` or no upstream instruction
    paintInsColorRecursive(ins->getLocalEdge(tracedReg), 1, color, type, depth + 1); // search rs1 and rs2
    paintInsColorRecursive(ins->getLocalEdge(tracedReg), 2, color, type, depth + 1);
  }else {
    return;
  }
}

AssemblyInstruction* AssemblyBasicBlock::FindLocalDef(int32_t reg, std::vector<AssemblyInstruction*>* InstVec, int index){

 printf("FindLocalDef Starts!\n\n");

  for(int i = index-1; i>=0; i--){
      if((*InstVec)[i]->getRd() != -1 && (*InstVec)[i]->getRd() == reg && !(*InstVec)[i]->ifPrologueEpilogue()){
        //printf("FindLocalDef Done!, reg = %d \n\n", reg);
        return (*InstVec)[i];
      }
  }
  printf("FindLocalDef Not Found!\n\n");

  return NULL;
}


AssemblyInstruction* AssemblyBasicBlock::FindGlobalDef(int32_t reg, std::vector<AssemblyInstruction*>* InstVec){

  //printf("Entering FindGlobalDef()\n");

  for(auto it = InstVec->rbegin(); 
      it!= InstVec->rend(); it++){
      if((*it)->getRd() == reg && (*it)->getRd() != -1 && !(*it)->ifPrologueEpilogue())
        return (*it);
  }
  //printf("Def Not Found! Leaving FindGlobalDef()\n");

  return NULL;

}



int AssemblyBasicBlock::GlobalDefUse(vector<AssemblyInstruction*>::iterator it, 
                                    int RegID, 
                                    vector<AssemblyInstruction*>* PreInstVec,
                                    int Rs){
    
    //PHI                   phi     = PHI(this);
    AssemblyInstruction*  Def     = NULL;
    int                   EdgeCnt = 0;


    //printf("Entering GlobalDefUse()\n");
    Def  =  FindGlobalDef(RegID, PreInstVec);
    
    if(Def){
        //printf("GlobalDefUse()::Found GlobalDef!\n");

        // Link from Def to Use
        (*it)->BuildEdge(Def, Rs, GLOBALEDGE);
        EdgeCnt = (*it)->FoundGlobalEdge(Rs);
        // Reverse Link from Def to Use
        Def->BuildEdge((*it), RD, GLOBALEDGE);  
        //printf("Build Edge done, Leaving GlobalDefUse()\n");

        return 1;
    }
    //printf("Def Not Found! Leaving GlobalDefUse()\n");

    return 0;

}


int AssemblyBasicBlock::RecursiveTraverse(AssemblyBasicBlock* BB,
                                          vector<AssemblyInstruction*>::iterator it,
                                          int RegID,
                                          int Rs,
                                          vector<set<int>>& visited){

  
  //printf("Entering RecursiveTraverse()\n");
  // Skip the visited BB
  if(visited[Rs].find(BB->getId())!= visited[Rs].end()){
    //printf("Leaving RecursiveTraverse()\n");
    return 0;
  }


  

  int                          ret         =   0;
  set<AssemblyBasicBlock*>     myset       =   BB->getPredecessors();
  vector<AssemblyInstruction*>* PreInstVec =   BB->getInstructions();



  // cout << "Instruction Addr:0x" << std::hex <<it->getAddress();
  // cout << ", Reg ID: " << RegID << ", RS ID: " << Rs
  //      << ", Visiting BB ID: " << BB->getId() << endl;



  // if(myset.find(*BB)== myset.end())
  //   visited->insert(BB->getId());
  // else {
  //   ret = GlobalDefUse(it, RegID, PreInstVec, Rs);
  // }


  // Find the def in the current BB
  // If found, no more deeper predecessor visiting
  // Otherwise, keep diving.
  ret = GlobalDefUse(it, RegID, PreInstVec, Rs);

  visited[Rs].insert(BB->getId());
  //cout << "Visited BB inserted: " <<BB->getId() << endl;



  if(myset.empty()){
    //printf("Leaving RecursiveTraverse()\n");
    return 0;
  }


  if(!ret){
    for (std::set<AssemblyBasicBlock*>::iterator b = myset.begin(); b != myset.end(); b++) {
        RecursiveTraverse(const_cast<AssemblyBasicBlock*>((*b)), it, RegID, Rs, visited);
    }
  }

  //printf("Leaving RecursiveTraverse()\n");



  return ret;

}



int AssemblyBasicBlock::BuildGlobalEdge(){



  set<AssemblyBasicBlock*>      myset         = this->getPredecessors();
  vector<AssemblyInstruction*>* InstVec       = this->getInstructions();
  vector<AssemblyInstruction*>* PreInstVec    = NULL;
  AssemblyInstruction*          Def           = NULL;
  vector<set<int>>              visited(MAX_OPERAND);
  // PHI                               phi           = PHI(this);


  //printf("Entering BuildGlobalEdge()\n");

  if(myset.empty()){
   // printf("myset is empty, leaving BuildGlobalEdge()\n");
    return 0;
  }
  


  for(vector<AssemblyInstruction*>::iterator it = InstVec->begin(); 
      it!= InstVec->end(); it++){
    // it->setBB(this);
    // cout <<  "Inst belongs to BB ID: " << it->getBB()->getId() << endl;

    for(int i = 0; i < MAX_OPERAND; i++)
        visited[i].clear();
    //cout << "Visited BB cleared !!!!!" << endl;

    for (std::set<AssemblyBasicBlock*>::iterator bb = myset.begin(); bb != myset.end(); bb++) {
      auto n = *bb;

      PreInstVec = n->getInstructions();
    
  
          if((*it)->getRs1() != -1 ){
              if(!(*it)->HasLocalEdge(RS1)){
                  RecursiveTraverse(n, it, (*it)->getRs1(), RS1,  visited);
                  //visited.clear();
                  //GlobalDefUse(it, it->getRs1(), PreInstVec, RS1);
                
              }
              //printf("BuildGlobalEdge():: RS1 check completes\n");

          }

          if((*it)->getRs2() != -1 ){
              if(!(*it)->HasLocalEdge(RS2)){
                RecursiveTraverse(n, it, (*it)->getRs2(), RS2,  visited);
                //visited.clear();
                //GlobalDefUse(it, it->getRs2(), PreInstVec, RS2);

              }
              //printf("BuildGlobalEdge():: RS2 check completes\n");

          }

          if((*it)->getRs3() != -1 ){
              if(!(*it)->HasLocalEdge(RS3)){
                RecursiveTraverse(n, it, (*it)->getRs3(), RS3,  visited);
                //visited.clear();
                //GlobalDefUse(it, it->getRs3(), PreInstVec, RS3);
              }
              //printf("BuildGlobalEdge():: RS3 check completes\n");

          }


      }
  }
  //printf("Leaving BuildGlobalEdge()\n");

  return 0;
}


// int AssemblyBasicBlock::PhiCheck(PHI phi, AssemblyInstruction* inst, int Rs){

//                   phi.AddPair(this, inst);
//                   this->addPhi(phi);


// }
       

void AssemblyBasicBlock::BuildPhiNodes(){

    vector<AssemblyInstruction*>*     InstVec     = this->getInstructions();
    vector<AssemblyInstruction*>*     EdgeVec     = NULL;
    PHI                               phi(this);
    set<AssemblyBasicBlock*>          myset       = this->getPredecessors();


    if(myset.empty())
      return;
  



    //printf("Entering Loop \n\n");
    for(auto it = InstVec->begin(); it!=InstVec->end(); it++){
          if((*it)->getRs1() != -1 ){
            if((*it)->HasGlobalEdge(RS1) > 1){
              EdgeVec = &((*it)->getGlobalEdge(RS1));
              for(auto edge = EdgeVec->begin(); edge!= EdgeVec->end(); edge++){
                if(*edge == NULL)
                  cout<<"\t NULL INST POINTER!!!!!!" << endl;
                phi.AddPair((*it), *edge);
              }
              // for(int i = 0; i< EdgeVec->size(); i++){
              //   phi.AddPair(this, (*EdgeVec)[i]);
              // }
              this->addPhi(phi);
              phi.clearPhi();
            }
          }

          if((*it)->getRs2() != -1 ){
            if((*it)->HasGlobalEdge(RS2) > 1){
              EdgeVec = &((*it)->getGlobalEdge(RS2));
              for(auto edge = EdgeVec->begin(); edge!=EdgeVec->end(); edge++){
                if(*edge == NULL)
                  cout<<"\t NULL INST POINTER!!!!!!" << endl;
                phi.AddPair((*it), *edge);
              }
              this->addPhi(phi);
              phi.clearPhi();
            }
          }  

         if((*it)->getRs3() != -1 ){
            if((*it)->HasGlobalEdge(RS3) > 1){
              EdgeVec = &((*it)->getGlobalEdge(RS3));
              for(auto edge = EdgeVec->begin(); edge!=EdgeVec->end(); edge++){
                if(*edge == NULL)
                  cout<<"\t NULL INST POINTER!!!!!!" << endl;
                phi.AddPair((*it), *edge);
              }
              this->addPhi(phi);
              phi.clearPhi();
            }
          }    
      

    }


}




