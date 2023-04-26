//===- RISCV32SelectionDAGISel.cpp - Binary raiser utility llvm-mctoll --------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of RISCV32SelectionDAGISel class
// for use by llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#include "RISCV32SelectionDAGISel.h"

using namespace llvm;

char RISCV32SelectionDAGISel::ID = 0;

#define DEBUG_TYPE "mctoll"

RISCV32SelectionDAGISel::RISCV32SelectionDAGISel(RISCV32ModuleRaiser &mr)
    : RISCV32RaiserBase(ID, mr) {}

RISCV32SelectionDAGISel::~RISCV32SelectionDAGISel() {
  delete SLT;
  delete SDB;
  delete DAGInfo;
  delete CurDAG;
  delete FuncInfo;
}

void RISCV32SelectionDAGISel::init(MachineFunction *mf, Function *rf) {
  RISCV32RaiserBase::init(mf, rf);

  ORE = make_unique<OptimizationRemarkEmitter>(getCRF());
  FuncInfo = new RISCVFunctionRaisingInfo();
  CurDAG = new SelectionDAG(*MR->getTargetMachine(), CodeGenOpt::None);
  DAGInfo = new RISCVDAGRaisingInfo(*CurDAG);
  SDB = new RISCVDAGBuilder(*DAGInfo, *FuncInfo);
  SLT = new RISCVInstSelector(*DAGInfo, *FuncInfo);
}

void RISCV32SelectionDAGISel::selectBasicBlock() {

  for (MachineBasicBlock::const_iterator I = MBB->begin(), E = MBB->end();
       I != E; ++I) {
    SDB->visit(*I);
  }

  doInstructionSelection();
  emitDAG();

  // If the current function has return value, records relationship between
  // BasicBlock and each Value which is mapped with R0. In order to record
  // the return Value of each exit BasicBlock.
  Type *RTy = FuncInfo->Fn->getReturnType();
  LLVM_DEBUG(dbgs()<<"\nReturnType=="<<*RTy<<" MBB->SuccSize()=="<<MBB->succ_size()<<"\n");
  if (RTy != nullptr && !RTy->isVoidTy() && MBB->succ_size() == 0) {
    LLVM_DEBUG(dbgs() << "attempt to assign RetValMap\n");
    Instruction *TInst = dyn_cast<Instruction>(
        DAGInfo->getRealValue(FuncInfo->RegValMap[RISCV::X10]));
    assert(TInst && "A def R0 was pointed to a non-instruction!!!");
    BasicBlock *TBB = TInst->getParent();
    FuncInfo->RetValMap[TBB] = TInst;
  }

  // Free the SelectionDAG state, now that we're finished with it.
  //DAGInfo->clear();          CYC ANNOTATION: in order to process live through register in O3,
  //                                           this is disabled
  CurDAG->clear();
}

void RISCV32SelectionDAGISel::doInstructionSelection() {
  LLVM_DEBUG(dbgs()<< "\n--- inside RISCV32SelectionDAGISel----\n");
  SelectionDAG::allnodes_iterator ISelPosition = CurDAG->allnodes_begin();
  while (ISelPosition != CurDAG->allnodes_end()) {
    SDNode *Node = &*ISelPosition++;
    //LLVM_DEBUG(Node->print_details(dbgs(), CurDAG));
    LLVM_DEBUG(dbgs() << "\n---" << Node->getOperationName() << "----\n");
    SLT->select(Node);
  }
  LLVM_DEBUG(dbgs()<< "\n--- end of RISCV32SelectionDAGISel----\n");
}

void RISCV32SelectionDAGISel::emitDAG() {
  LLVM_DEBUG(dbgs() << "\n--- EmitDAG ----\n");
  RISCVIREmitter imt(BB, DAGInfo, FuncInfo);
  imt.setjtList(jtList);
  SelectionDAG::allnodes_iterator ISelPosition = CurDAG->allnodes_begin();
  while (ISelPosition != CurDAG->allnodes_end()) {
    SDNode *Node = &*ISelPosition++;
    //Node->print_details(dbgs(), CurDAG);
    LLVM_DEBUG(dbgs() << "\n---" << Node->getOperationName() << "nodeid=" << Node->PersistentId <<"nodetype" <<Node->getOpcode() <<"----\n");
    //LLVM_DEBUG(dbgs() << "\n---" << Node->getOperationName() << "----\n");
    imt.emitNode(Node);
  }
}

void RISCV32SelectionDAGISel::initEntryBasicBlock() {
  BasicBlock *bb = &RF->getEntryBlock();
  for (unsigned i = 0; i < 4; i++) {
    Align MALG(32);
    AllocaInst *Alloc = new AllocaInst(Type::getInt1Ty(RF->getContext()), 0,
                                       nullptr, MALG, "", bb);
    FuncInfo->AllocaMap[i] = Alloc;
    new StoreInst(ConstantInt::getFalse(RF->getContext()), Alloc, bb);
  }
}

bool RISCV32SelectionDAGISel::doSelection() {
  LLVM_DEBUG(dbgs() << "RISCV32SelectionDAGISel start.\n");

  MachineFunction &mf = *MF;
  CurDAG->init(mf, *ORE.get(), this, nullptr, nullptr, nullptr, nullptr);
  FuncInfo->set(*MR, *getCRF(), mf, CurDAG);

  initEntryBasicBlock();
  for (MachineBasicBlock &mbb : mf) {
    MBB = &mbb;
    BB = FuncInfo->getOrCreateBasicBlock(MBB);
    selectBasicBlock();
  }

  // Add an additional exit BasicBlock, all of original return BasicBlocks
  // will branch to this exit BasicBlock. This will lead to the function has
  // one and only exit. If the function has return value, this help return
  // R0.
  Function *CurFn = const_cast<Function *>(FuncInfo->Fn);
  BasicBlock *LBB = FuncInfo->getOrCreateBasicBlock();

  if (CurFn->getReturnType()) {
    PHINode *LPHI = PHINode::Create(FuncInfo->getCRF()->getReturnType(),
                                    FuncInfo->RetValMap.size(), "", LBB);
    LLVM_DEBUG(dbgs()<<"RetValMap.size()=="<<FuncInfo->RetValMap.size()<<"\n");
    for (auto Pair : FuncInfo->RetValMap) {
      // LLVM_DEBUG(dbgs() << "\nAt auto Pair: FuncInfo->RetValMap\nfirst(BB):");
      // LLVM_DEBUG(Pair.first->dump());
      // LLVM_DEBUG(dbgs() << "\nSecond(Value):");
      // LLVM_DEBUG(Pair.second->dump());
      LPHI->addIncoming(Pair.second, Pair.first);
    }

    ReturnInst::Create(CurFn->getContext(), LPHI, LBB);
  } else
    ReturnInst::Create(CurFn->getContext(), LBB);

  for (auto &FBB : CurFn->getBasicBlockList())
    if (FBB.getTerminator() == nullptr)
      BranchInst::Create(LBB, &FBB);

  FuncInfo->clear();

  LLVM_DEBUG(dbgs() << "RISCV32SelectionDAGISel end.\n");

  return true;
}

bool RISCV32SelectionDAGISel::setjtList(std::vector<JumpTableInfo> &List) {
  jtList = List;
  return true;
}

bool RISCV32SelectionDAGISel::runOnMachineFunction(MachineFunction &mf) {
  bool rtn = false;
  init();
  rtn = doSelection();
  return rtn;
}
#undef DEBUG_TYPE

#ifdef __cplusplus
extern "C" {
#endif

FunctionPass *InitializeRISCV32SelectionDAGISel(RISCV32ModuleRaiser &mr) {
  return new RISCV32SelectionDAGISel(mr);
}

#ifdef __cplusplus
}
#endif
