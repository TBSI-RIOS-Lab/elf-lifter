//===- RISCVInstSelector.cpp - Binary raiser utility llvm-mctoll ---------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementaion of RISCVInstSelector class for use
// by llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#include "RISCVInstSelector.h"
#include "RISCV.h"
#include "ARM.h"
#include "ARMSubtarget.h"
#include "RISCVSubtarget.h"
#include "RISCVSelectionCommon.h"

using namespace llvm;
#define DEBUG_TYPE "mctoll"
/// Replace all uses of F with T, then remove F from the DAG.
void RISCVInstSelector::replaceNode(SDNode *F, SDNode *T) {
  if (MachineSDNode::classof(F)) {
    NodePropertyInfo *np = DAGInfo->NPMap[F];
    DAGInfo->NPMap.erase(F);
    DAGInfo->NPMap[T] = np;
  }
  LLVM_DEBUG(dbgs()<<"detecter replaceNode 1.0\n");
  CurDAG->ReplaceAllUsesWith(F, T);
  LLVM_DEBUG(dbgs()<<"detecter replaceNode 2.0\n");
  CurDAG->RemoveDeadNode(F);
  LLVM_DEBUG(dbgs()<<"detecter replaceNode 3.0\n");
}

/// Checks the SDNode is a function argument or not.
bool RISCVInstSelector::isArgumentNode(SDNode *node) {
  if (!FrameIndexSDNode::classof(node))
    return false;

  return FuncInfo->isArgumentIndex(
      dyn_cast<FrameIndexSDNode>(node)->getIndex());
}

/// Checks the SDNode is a function return or not.
bool RISCVInstSelector::isReturnNode(SDNode *node) {
  if (!FrameIndexSDNode::classof(node))
    return false;

  return FuncInfo->isReturnIndex(dyn_cast<FrameIndexSDNode>(node)->getIndex());
}

/// Record the new defined Node, it uses to map the register number to Node.
/// In DAG emitter, emitter get a value of use base on this defined Node.
void RISCVInstSelector::recordDefinition(SDNode *oldNode, SDNode *newNode) {
  assert(newNode != nullptr &&
         "The new SDNode ptr is null when record define!");

  if (oldNode == nullptr) {
    outs() << "Warning: RecordDefine error, the SDNode ptr is null!\n";
    return;
  }

  if (RegisterSDNode::classof(oldNode)) {
    unsigned opReg = static_cast<RegisterSDNode *>(oldNode)->getReg();
    FuncInfo->setValueByRegister(opReg, SDValue(newNode, 0));
    FuncInfo->NodeRegMap[newNode] = opReg;
  }

  if (isReturnNode(oldNode)) {
    FuncInfo->setRetValue(SDValue(newNode, 0));
    FuncInfo->setValueByRegister(RISCV::X10, SDValue(newNode, 0));
    FuncInfo->NodeRegMap[newNode] = RISCV::X10;
  }
}

/// Gets the Metadata of given SDNode.
SDValue RISCVInstSelector::getMDOperand(SDNode *N) {
  for (auto &sdv : N->ops()) {
    if (MDNodeSDNode::classof(sdv.getNode())) {
      return sdv.get();
    }
  }
  assert(false && "Should not run at here!");
  return SDValue();
}

/// Instruction opcode selection.
void RISCVInstSelector::selectCode(SDNode *N) {
  SDLoc dl(N);
  LLVM_DEBUG(dbgs() <<"machineopcode at selectcode:"<< N->getMachineOpcode()<<"\n");
  switch (N->getMachineOpcode()) {
  default:
    break;
  /* ADDI */
  case RISCV::ADDI:{
      LLVM_DEBUG(dbgs() << "addi\n");
      SDValue R1 = N->getOperand(0);
      SDValue R2 = N->getOperand(1);
      SDValue R3 = N->getOperand(2);
      LLVM_DEBUG(R1.getNode()->print(dbgs(),CurDAG));
      LLVM_DEBUG(R2.getNode()->print(dbgs(),CurDAG));
      LLVM_DEBUG(R3.getNode()->print(dbgs(),CurDAG));

      LLVM_DEBUG(dbgs()<<"\nR1 pid="<<R1.getNode()->PersistentId
                       <<"\nR2 pid="<<R2.getNode()->PersistentId
                       <<"\nR3 pid="<<R3.getNode()->PersistentId<<"\n");
      bool flag = 0;

      if (FrameIndexSDNode::classof(N->getOperand(1).getNode())) {
        if(FuncInfo->isArgumentIndex(static_cast<FrameIndexSDNode*>(R2.getNode())->getIndex())) {
          LLVM_DEBUG(dbgs()<<"113.01.0\n");
          SDNode *Node = CurDAG
                       ->getNode(EXT_RISCV32ISD::CMOV, dl, getDefaultEVT(), R2,
                                 CurDAG->getConstant(0, dl, getDefaultEVT()))
                       .getNode();
          recordDefinition(R1.getNode(), Node);
          replaceNode(N, Node); 
        } else {
          LLVM_DEBUG(dbgs()<<"113.01\n");
          SDNode *Node = CurDAG
                    ->getNode(EXT_RISCV32ISD::LOAD, dl, getDefaultEVT(), R2/*,
                              getMDOperand(N)*/)
                    .getNode();
          recordDefinition(R1.getNode(), Node);
          replaceNode(N, Node);
        }
        flag = 1;
      }       


      ConstantSDNode *N2C = dyn_cast<ConstantSDNode>(R3);
       //if (RegisterSDNode::classof(Rn.getNode()))
        //   Rn = FuncInfo->getValFromRegMap(Rn);
      LLVM_DEBUG(dbgs() << ISD::ADD << "\n");
      // if(R3.imm()){
      if(R2->getOpcode() == ISD::Register && flag == 0) {
        const RegisterSDNode *RR = dyn_cast<RegisterSDNode>(R2);
        LLVM_DEBUG(dbgs() << "regno display here"<<RR->getReg());
        if(RR->getReg() == 37) { // $x0 special case
              SDNode *Node = CurDAG
                       ->getNode(ISD::ADD, dl, getDefaultEVT(), R3,
                                 CurDAG->getConstant(0, dl, getDefaultEVT()),getMDOperand(N))
                       .getNode();
              recordDefinition(R1.getNode(), Node);
              replaceNode(N, Node);
              LLVM_DEBUG(dbgs()<<"at reg == 37 ::\n");
              LLVM_DEBUG(Node->print(dbgs(),CurDAG));
              flag = 1;
        }
      }
      
      if(!flag) {
        SDValue op2 = N->getOperand(2);
        if (RegisterSDNode::classof(op2.getNode()))
          op2 = FuncInfo->getValFromRegMap(op2);

        SDValue Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
        LLVM_DEBUG(dbgs()<<"113.3\n");
        
        LLVM_DEBUG(dbgs() <<"\nR2 pid="<<Rn.getNode()->PersistentId<<"phyregnum="<<
                  static_cast<RegisterSDNode*>(Rn.getNode())->getReg()
                  
                       <<"\nR3 pid="<<op2.getNode()->PersistentId<<"\n");
        SDNode *Node = CurDAG
                   ->getNode(ISD::ADD /*EXT_RISCV32ISD::CMOV*/, dl, getDefaultEVT(), Rn, op2
                             ,getMDOperand(N))
                   .getNode();

       // Node = CurDAG->
       //   getNode(ISD::ADD, dl, getDefaultEVT(), R2, R3 /*getMDOperand(N) getDefaultEVT()*/)
       //  .getNode();
       // Node = CurDAG->getNode(ARMISD::CMOV, dl, getDefaultEVT(), R2, 
       //        CurDAG->getConstant(N2C->getSExtValue(), dl, getDefaultEVT())
       //     ).getNode();
        LLVM_DEBUG(dbgs() << Node->getOpcode() << "\n");
        recordDefinition(R1.getNode(), Node);
        replaceNode(N, Node);
        //LLVM_DEBUG(dbgs()<<"at reg <> 37 normalcase ::\n");
        //LLVM_DEBUG(Node->print(dbgs(),CurDAG));
      }
    }
    break;
    /* STR */
  case RISCV::SW: {
    LLVM_DEBUG(dbgs() << "sw detecter\n" );
    LLVM_DEBUG(N->print(dbgs(),CurDAG));
    SDValue Val = N->getOperand(0);
    SDValue Ptr = N->getOperand(1); // This is a pointer.

    if (RegisterSDNode::classof(Val.getNode())) {
      Val = FuncInfo->getValFromRegMap(Val);
      LLVM_DEBUG(dbgs()<<"sw.1\n");
    }

    if (RegisterSDNode::classof(Ptr.getNode())){
      Ptr = FuncInfo->getValFromRegMap(Ptr);
      LLVM_DEBUG(dbgs()<<"sw.2\n");
    }
    /*
    LLVM_DEBUG(dbgs()<<"\nval:");
    LLVM_DEBUG(Val->print(dbgs(),CurDAG));
    LLVM_DEBUG(dbgs()<<"\n");

    LLVM_DEBUG(dbgs()<<"ptr:");
    LLVM_DEBUG(Ptr->print(dbgs(),CurDAG));
    LLVM_DEBUG(dbgs()<<"\n");
    */
    SDNode *Node;
    if (RegisterSDNode::classof(Val.getNode())) {
      const RegisterSDNode *RR = dyn_cast<RegisterSDNode>(Val);
      if(RR->getReg() == 37) {
        /*
        *Node = CurDAG ->getNode(ISD::ADD, dl, getDefaultEVT(), R3,
                                 CurDAG->getConstant(0, dl, getDefaultEVT()),getMDOperand(N))
                       .getNode();
                       */
        Node = CurDAG->getNode(EXT_RISCV32ISD::STORE, 
                dl, getDefaultEVT(),
                CurDAG->getConstant(0,dl,getDefaultEVT()),
                Ptr,
                getMDOperand(N)).getNode();
      }
    } else {
      Node = CurDAG->getNode(EXT_RISCV32ISD::STORE, dl, getDefaultEVT(), Val, Ptr, getMDOperand(N)).getNode();
    }

    replaceNode(N, Node);
    } 
  break;
  case RISCV::LW: {
    EVT InstTy = EVT::getEVT(Type::getInt16Ty(*CurDAG->getContext()));
    SDValue Rd = N->getOperand(0);
    SDValue Rn = N->getOperand(1);
    SDNode *Node = nullptr;
    LLVM_DEBUG(dbgs()<<"\nLW::Rd=");
    LLVM_DEBUG(Rd.getNode()->print(dbgs(),CurDAG));
    LLVM_DEBUG(dbgs()<<"\nLW::Rn=");
    LLVM_DEBUG(Rn.getNode()->print(dbgs(),CurDAG));
    LLVM_DEBUG(dbgs()<<"\n");

    if (RegisterSDNode::classof(Rn.getNode()))
      Rn = FuncInfo->getValFromRegMap(Rn);
    Node = CurDAG->getNode(EXT_RISCV32ISD::LOAD, dl, InstTy, Rn, getMDOperand(N))
               .getNode();

    recordDefinition(Rd.getNode(), Node);
    replaceNode(N, Node);
  }
  break;
  case RISCV::LUI: {
    LLVM_DEBUG(dbgs()<< "LUI detected\n");
    SDValue Rd = N->getOperand(0);
    SDValue Rn = N->getOperand(1);
    LLVM_DEBUG(dbgs()<<"\nLUI::Rd=");
    LLVM_DEBUG(Rd.getNode()->print(dbgs(),CurDAG));
    LLVM_DEBUG(dbgs()<<"\nLUI::Rn=");
    LLVM_DEBUG(Rn.getNode()->print(dbgs(),CurDAG));
    LLVM_DEBUG(dbgs()<<"\n");
    if(ConstantSDNode::classof(Rn.getNode())) {
      ConstantSDNode* CstNode = static_cast<ConstantSDNode*>(Rn.getNode());
      APInt tmp = CstNode->getAPIntValue().operator<<(12);
      SDValue TmpSdvalue = CurDAG->getConstant(tmp,dl,getDefaultEVT());
      LLVM_DEBUG(dbgs()<<"\nLUI::Rn_new=");
      LLVM_DEBUG(TmpSdvalue.getNode()->print(dbgs(),CurDAG));
      LLVM_DEBUG(dbgs()<<"\n");
      SDNode* Node = CurDAG->getNode(ISD::ADD,dl,getDefaultEVT(),
                    CurDAG->getConstant(0,dl,getDefaultEVT()),TmpSdvalue,
                    getMDOperand(N))
                    .getNode();
      LLVM_DEBUG(dbgs()<<"\nNode=");
      LLVM_DEBUG(Node->print(dbgs(),CurDAG));
      LLVM_DEBUG(dbgs()<<"\n");
      recordDefinition(Rd.getNode(),Node);
      replaceNode(N,Node);


    } else {
      assert(0 && "error format in LUI!\n");
    }
    
  } break;
  case RISCV::JAL: {
    LLVM_DEBUG(dbgs()<< "JAL inst sel\n");
    SDValue FuncOrBB = N->getOperand(1);
    SDValue LinkReg = N->getOperand(0);
    RegisterSDNode* LinkRegPtr = static_cast<RegisterSDNode*>(LinkReg.getNode());
    assert(LinkRegPtr->getReg() == RISCV::X1 && "JAL Link ret Addr not eq to X1, case uncovered!\n");
    //LLVM_DEBUG(Func.getNode()->dump());
    SDNode *Node = nullptr;
    /*
    if (RegisterSDNode::classof(Func.getNode())) {
      LLVM_DEBUG(dbgs()<< "JAL inst sel 1,0\n");
      Func = FuncInfo->getValFromRegMap(Func);
      Node =
          CurDAG
              ->getNode(ISD::BRIND, dl, getDefaultEVT(), Func, getMDOperand(N))
              .getNode();
    } else {      */
    LLVM_DEBUG(dbgs()<< "JAL inst sel 2,0\n");
    if(LinkRegPtr->getReg() == RISCV::X1) {
      Node = CurDAG
                  ->getNode(EXT_RISCV32ISD::BRD, dl, getDefaultEVT(), FuncOrBB,
                            getMDOperand(N))
                  .getNode();
      //}

      FuncInfo->setValueByRegister(RISCV::X10, SDValue(Node, 0));
      FuncInfo->NodeRegMap[Node] = RISCV::X10;
    } else { // RISCV:: X0
      Node = 
        CurDAG->getNode(ISD::BR, dl, getDefaultEVT(), FuncOrBB, getMDOperand(N)).getNode();
    }
    replaceNode(N, Node);
  } break;
  case RISCV::ADD :{ // ADDrr of riscv
    N->print(dbgs(),CurDAG);
    SDValue Rd = N->getOperand(0);
    SDValue Rn = N->getOperand(1);
    SDNode *Node = nullptr;
    SDValue Rs = N->getOperand(2);
    if(RegisterSDNode::classof(Rs.getNode()))
      Rs = FuncInfo->getValFromRegMap(Rs);
    Rn = FuncInfo->getValFromRegMap(Rn);
    Node = CurDAG->getNode(ISD::ADD, dl, getDefaultEVT(), Rn, Rs, getMDOperand(N)).getNode();
    recordDefinition(Rd.getNode(), Node);
    replaceNode(N, Node);
  } break;
  case RISCV::BGE : {
    SDValue Left = N->getOperand(0);
    SDValue Right = N->getOperand(1);
    SDValue Target = N->getOperand(2);
    LLVM_DEBUG(dbgs() << "left:\n");
    LLVM_DEBUG(Left.getNode()->print(dbgs(),CurDAG));
    LLVM_DEBUG(dbgs() << "right:\n");
    LLVM_DEBUG(Right.getNode()->print(dbgs(),CurDAG));
    LLVM_DEBUG(dbgs() << "Target:\n");
    LLVM_DEBUG(Target.getNode()->print(dbgs(),CurDAG));
    if(RegisterSDNode::classof(Left.getNode())) 
      Left = FuncInfo->getValFromRegMap(Left);
    
    if(RegisterSDNode::classof(Right.getNode())) 
      Right = FuncInfo->getValFromRegMap(Right);
    SDNode *Node = CurDAG->getNode(EXT_RISCV32ISD::BGE,dl,getDefaultEVT(),Left,Right,getMDOperand(N)).getNode();
    replaceNode(N,Node);
    //SDNode *Node = CurDAG->getNode(ISD::SGE)
  }break;
  case RISCV::BNE: {
    SDValue Left = N->getOperand(0);
    SDValue Right = N->getOperand(1);
    SDValue Target = N->getOperand(2);
    LLVM_DEBUG(dbgs() << "left:\n");
    LLVM_DEBUG(Left.getNode()->print(dbgs(),CurDAG));
    LLVM_DEBUG(dbgs() << "right:\n");
    LLVM_DEBUG(Right.getNode()->print(dbgs(),CurDAG));
    LLVM_DEBUG(dbgs() << "Target:\n");
    LLVM_DEBUG(Target.getNode()->print(dbgs(),CurDAG));
    if(RegisterSDNode::classof(Left.getNode())) 
      Left = FuncInfo->getValFromRegMap(Left);
    
    if(RegisterSDNode::classof(Right.getNode()))  {
      Right = FuncInfo->getValFromRegMap(Right);
      RegisterSDNode* RSDnode = static_cast<RegisterSDNode*>(Right.getNode());
      if(RSDnode->getReg() == 37) { // x0
        Right = CurDAG->getConstant(0,dl,getDefaultEVT()); 
      }
    }
    SDNode *Node = CurDAG->getNode(EXT_RISCV32ISD::BNE,dl,getDefaultEVT(),Left,Right,getMDOperand(N)).getNode();
    replaceNode(N,Node);

  }break;
  /* ADC */
  // case RISCV32::ADCrr:
  // case RISCV32::ADCri:
  // case RISCV32::ADCrsr:
  // case RISCV32::ADCrsi:
  // case RISCV32::tADC:
  // case RISCV32::t2ADCrr:
  // case RISCV32::t2ADCri:
  // case RISCV32::t2ADCrs: {
  //   SDValue Rd = N->getOperand(0);
  //   SDValue Rn = N->getOperand(1);
  //   SDNode *Node = nullptr;
  //   if (isTwoAddressMode(Rd.getNode())) {
  //     // ADCS <Rdn>,<Rm>
  //     // ADC<c> <Rdn>,<Rm>
  //     if (RegisterSDNode::classof(N->getOperand(1).getNode()))
  //       Rn = FuncInfo->getValFromRegMap(N->getOperand(1));

  //     SDValue Rd = FuncInfo->getValFromRegMap(N->getOperand(0));
  //     Node =
  //         CurDAG
  //             ->getNode(ISD::ADDC, dl, getDefaultEVT(), Rd, Rn, getMDOperand(N))
  //             .getNode();
  //   } else {
  //     // ADC{S}<c> <Rd>,<Rn>,#<const>
  //     SDValue op2 = N->getOperand(2);
  //     if (RegisterSDNode::classof(op2.getNode()))
  //       op2 = FuncInfo->getValFromRegMap(op2);
  //     Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
  //     Node = CurDAG
  //                ->getNode(ISD::ADDC, dl, getDefaultEVT(), Rn, op2,
  //                          getMDOperand(N))
  //                .getNode();
  //   }

  //   recordDefinition(Rd.getNode(), Node);
  //   replaceNode(N, Node);
  // } break;
  // /* SUB */
  // case RISCV32::SUBri:
  // case RISCV32::SUBrr:
  // case RISCV32::SUBrsi:
  // case RISCV32::SUBrsr:
  // case RISCV32::tSUBi3:
  // case RISCV32::tSUBi8:
  // case RISCV32::tSUBrr:
  // case RISCV32::tSUBspi:
  // case RISCV32::t2SUBri:
  // case RISCV32::t2SUBri12:
  // case RISCV32::t2SUBrr:
  // case RISCV32::t2SUBrs:
  // case RISCV32::t2SUBS_PC_LR: {
  //   SDValue Rd = N->getOperand(0);
  //   SDValue Rn = N->getOperand(1);
  //   SDNode *Node = nullptr;
  //   if (isTwoAddressMode(Rd.getNode())) {
  //     if (RegisterSDNode::classof(N->getOperand(1).getNode()))
  //       Rn = FuncInfo->getValFromRegMap(N->getOperand(1));

  //     SDValue Rd = FuncInfo->getValFromRegMap(N->getOperand(0));
  //     Node =
  //         CurDAG
  //             ->getNode(ISD::SUB, dl, getDefaultEVT(), Rd, Rn, getMDOperand(N))
  //             .getNode();
  //   } else {
  //     SDValue op2 = N->getOperand(2);
  //     if (RegisterSDNode::classof(op2.getNode()))
  //       op2 = FuncInfo->getValFromRegMap(op2);

  //     Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
  //     Node =
  //         CurDAG
  //             ->getNode(ISD::SUB, dl, getDefaultEVT(), Rn, op2, getMDOperand(N))
  //             .getNode();
  //   }
  //   recordDefinition(Rd.getNode(), Node);
  //   replaceNode(N, Node);
  // } break;
  // /* MOV */
  // case RISCV32::MOVi16:
  // case RISCV32::t2MOVi16:
  // case RISCV32::MOVi32imm:
  // case RISCV32::tMOVr:
  // case RISCV32::MOVr:
  // case RISCV32::t2MOVi:
  // case RISCV32::t2MOVr:
  // case RISCV32::MOVCCr:
  // case RISCV32::t2MOVCCr:
  // case RISCV32::t2MOVi32imm:
  // case RISCV32::MOVTi16:
  // case RISCV32::MOVi: {
  //   // Dispalcement operation need do.
  //   SDValue Rd = N->getOperand(0);
  //   SDValue Rn = N->getOperand(1);
  //   if (RegisterSDNode::classof(Rn.getNode()))
  //     Rn = FuncInfo->getValFromRegMap(Rn);

  //   SDNode *Node = CurDAG
  //                      ->getNode(RISCV32ISD::CMOV, dl, getDefaultEVT(), Rn,
  //                                CurDAG->getConstant(0, dl, getDefaultEVT()))
  //                      .getNode();

  //   recordDefinition(Rd.getNode(), Node);
  //   replaceNode(N, Node);
  // } break;
  // case RISCV32::STRH:
  // case RISCV32::STRH_PRE:
  // case RISCV32::STRH_POST: {
  //   EVT InstTy = EVT::getEVT(Type::getInt16Ty(*CurDAG->getContext()));
  //   SDValue Val = N->getOperand(0);
  //   SDValue Op1 = N->getOperand(1);
  //   SDNode *Node = nullptr;

  //   if (RegisterSDNode::classof(Val.getNode()))
  //     Val = FuncInfo->getValFromRegMap(Val);

  //   if (RegisterSDNode::classof(Op1.getNode()))
  //     Op1 = FuncInfo->getValFromRegMap(Op1);

  //   if (N->getNumOperands() < 5)
  //     Node = CurDAG
  //                ->getNode(EXT_RISCV32ISD::STORE, dl, InstTy, Val, Op1,
  //                          getMDOperand(N))
  //                .getNode();
  //   else {
  //     SDValue Op2 = N->getOperand(2);
  //     Op2 = FuncInfo->getValFromRegMap(Op2);
  //     Node = CurDAG
  //                ->getNode(EXT_RISCV32ISD::STORE, dl, InstTy, Val, Op1, Op2,
  //                          getMDOperand(N))
  //                .getNode();
  //   }

  //   replaceNode(N, Node);
  // } break;
  // case RISCV32::STRBi12:
  // case RISCV32::STRBrs:
  // case RISCV32::STRB_PRE_IMM:
  // case RISCV32::STRB_PRE_REG:
  // case RISCV32::STRB_POST_IMM:
  // case RISCV32::STRB_POST_REG: {
  //   EVT InstTy = EVT::getEVT(Type::getInt8Ty(*CurDAG->getContext()));
  //   SDValue Val = N->getOperand(0);
  //   SDValue Op1 = N->getOperand(1);
  //   SDNode *Node = nullptr;

  //   if (RegisterSDNode::classof(Val.getNode()))
  //     Val = FuncInfo->getValFromRegMap(Val);

  //   if (RegisterSDNode::classof(Op1.getNode()))
  //     Op1 = FuncInfo->getValFromRegMap(Op1);

  //   if (N->getNumOperands() < 5)
  //     Node = CurDAG
  //                ->getNode(EXT_RISCV32ISD::STORE, dl, InstTy, Val, Op1,
  //                          getMDOperand(N))
  //                .getNode();
  //   else {
  //     SDValue Op2 = N->getOperand(2);
  //     Op2 = FuncInfo->getValFromRegMap(Op2);
  //     Node = CurDAG
  //                ->getNode(EXT_RISCV32ISD::STORE, dl, InstTy, Val, Op1, Op2,
  //                          getMDOperand(N))
  //                .getNode();
  //   }

  //   replaceNode(N, Node);
  // } break;
  // 
  /*
  case ARM::LDRi12:
  case ARM::LDRrs:
  case ARM::t2LDR_PRE:
  case ARM::t2LDR_POST:
  case ARM::tLDR_postidx:
  case ARM::LDR_PRE_IMM:
  case ARM::LDR_PRE_REG:
  case ARM::LDR_POST_IMM:
  case ARM::LDR_POST_REG: {
    EVT InstTy = EVT::getEVT(Type::getInt32Ty(*CurDAG->getContext()));
    SDValue Rd = N->getOperand(0);
    SDValue Rn = N->getOperand(1);
    SDNode *Node = nullptr;
    if (RegisterSDNode::classof(Rn.getNode()))
      Rn = FuncInfo->getValFromRegMap(Rn);

    Node = CurDAG->getNode(EXT_RISCV32ISD::LOAD, dl, InstTy, Rn, getMDOperand(N))
               .getNode();

    recordDefinition(Rd.getNode(), Node);
    replaceNode(N, Node);
  } break;
  */
  // case RISCV32::LDRH:
  // case RISCV32::LDRSH:
  // case RISCV32::t2LDRSH_PRE:
  // case RISCV32::t2LDRSH_POST:
  // case RISCV32::t2LDRH_PRE:
  // case RISCV32::t2LDRH_POST:
  // case RISCV32::LDRSH_PRE:
  // case RISCV32::LDRSH_POST: {
  //   EVT InstTy = EVT::getEVT(Type::getInt16Ty(*CurDAG->getContext()));
  //   SDValue Rd = N->getOperand(0);
  //   SDValue Rn = N->getOperand(1);
  //   SDNode *Node = nullptr;

  //   if (RegisterSDNode::classof(Rn.getNode()))
  //     Rn = FuncInfo->getValFromRegMap(Rn);
  //   Node = CurDAG->getNode(EXT_RISCV32ISD::LOAD, dl, InstTy, Rn, getMDOperand(N))
  //              .getNode();

  //   recordDefinition(Rd.getNode(), Node);
  //   replaceNode(N, Node);
  // } break;
  // case RISCV32::LDRBi12:
  // case RISCV32::LDRBrs:
  // case RISCV32::t2LDRSB_PRE:
  // case RISCV32::t2LDRSB_POST:
  // case RISCV32::t2LDRB_PRE:
  // case RISCV32::t2LDRB_POST:
  // case RISCV32::LDRSB_PRE:
  // case RISCV32::LDRSB_POST:
  // case RISCV32::LDRB_PRE_IMM:
  // case RISCV32::LDRB_POST_IMM:
  // case RISCV32::LDRB_PRE_REG:
  // case RISCV32::LDRB_POST_REG: {
  //   EVT InstTy = EVT::getEVT(Type::getInt8Ty(*CurDAG->getContext()));
  //   SDValue Rd = N->getOperand(0);
  //   SDValue Rn = N->getOperand(1);
  //   SDNode *Node = nullptr;

  //   if (RegisterSDNode::classof(Rn.getNode()))
  //     Rn = FuncInfo->getValFromRegMap(Rn);
  //   Node = CurDAG->getNode(EXT_RISCV32ISD::LOAD, dl, InstTy, Rn, getMDOperand(N))
  //              .getNode();

  //   recordDefinition(Rd.getNode(), Node);
  //   replaceNode(N, Node);
  // } break;
  // /* Branch */
  // case RISCV32::Bcc:
  // case RISCV32::tBcc:
  // case RISCV32::t2Bcc: {
  //   SDValue Iftrue = N->getOperand(0);
  //   SDValue Cond = N->getOperand(1);
  //   SDNode *Node = nullptr;

  //   if (DAGInfo->NPMap[N]->HasCPSR)
  //     Node = CurDAG
  //                ->getNode(ISD::BRCOND, dl, getDefaultEVT(), Iftrue, Cond,
  //                          getMDOperand(N))
  //                .getNode();
  //   else
  //     Node =
  //         CurDAG->getNode(ISD::BR, dl, getDefaultEVT(), Iftrue, getMDOperand(N))
  //             .getNode();

  //   const MachineBasicBlock *LMBB = DAGInfo->NPMap[N]->MI->getParent();
  //   if (LMBB->succ_size() == 0) {
  //     FuncInfo->setValueByRegister(RISCV32::R0, SDValue(Node, 0));
  //     FuncInfo->NodeRegMap[Node] = RISCV32::R0;
  //   }
  //   replaceNode(N, Node);
  // } break;
  // case RISCV32::B:
  // case RISCV32::tB:
  // case RISCV32::t2B: {
  //   SDValue BrBlock = N->getOperand(0);
  //   SDNode *Node =
  //       CurDAG->getNode(ISD::BR, dl, getDefaultEVT(), BrBlock, getMDOperand(N))
  //           .getNode();

  //   replaceNode(N, Node);
  // } break;
  // case RISCV32::BLX:
  // case RISCV32::BLXi:
  // case RISCV32::BLX_pred:
  // case RISCV32::tBLXi:
  // case RISCV32::tBLXr: {
  //   outs() << "WARNING: Not yet implemented!\n";
  // } break;
  // case RISCV32::BR_JTr: {
  //   SDNode *Node = nullptr;
  //   SDValue Rd = N->getOperand(0);
  //   Node = CurDAG->getNode(ISD::BR_JT, dl, getDefaultEVT(), Rd, getMDOperand(N))
  //              .getNode();
  //   replaceNode(N, Node);
  // } break;
  // case RISCV32::BX:
  // case RISCV32::BX_CALL:
  // case RISCV32::BX_pred:
  // case RISCV32::tBX:
  // case RISCV32::tBX_CALL: {
  //   SDValue CallReg = N->getOperand(0);
  //   if (RegisterSDNode::classof(CallReg.getNode()))
  //     CallReg = FuncInfo->getValFromRegMap(CallReg);

  //   SDNode *Node =
  //       CurDAG
  //           ->getNode(ISD::BRIND, dl, getDefaultEVT(), CallReg, getMDOperand(N))
  //           .getNode();
  //   replaceNode(N, Node);
  // } break;
  // case RISCV32::BX_RET:
  // case RISCV32::tBX_RET:
  //   // assert(0 && "Branch instructions are removed in previous stage. should
  //   // not get here!");
  //   break;
  // case RISCV32::tCMPhir:
  // case RISCV32::CMPrr:
  // case RISCV32::t2CMPri:
  // case RISCV32::CMPri:
  // case RISCV32::tCMPi8:
  // case RISCV32::t2CMPrr:
  // case RISCV32::tCMPr: {
  //   SDValue cmpl = N->getOperand(0);
  //   SDValue cmph = N->getOperand(1);
  //   if (RegisterSDNode::classof(cmph.getNode()))
  //     cmph = FuncInfo->getValFromRegMap(N->getOperand(1));
  //   cmpl = FuncInfo->getValFromRegMap(cmpl);

  //   // Create condition SDValuleR
  //   // TODO: It should be verified why this type node can not be added Metadata
  //   // Operand.
  //   SDNode *Node = CurDAG
  //                      ->getNode(ISD::SETCC, dl, getDefaultEVT(), cmpl, cmph
  //                                /* , getMDOperand(N) */)
  //                      .getNode();

  //   replaceNode(N, Node);
  // } break;
  // /* AND */
  // case RISCV32::ANDri:
  // case RISCV32::ANDrr:
  // case RISCV32::ANDrsi:
  // case RISCV32::ANDrsr:
  // case RISCV32::tAND:
  // case RISCV32::t2ANDri:
  // case RISCV32::t2ANDrr:
  // case RISCV32::t2ANDrs: {
  //   SDValue Rd = N->getOperand(0);
  //   SDValue Rn = N->getOperand(1);
  //   SDNode *Node = nullptr;

  //   if (isTwoAddressMode(Rd.getNode())) {
  //     // AND<c> <Rdn>,<Rm>
  //     // ANDS <Rdn>,<Rm>
  //     if (RegisterSDNode::classof(N->getOperand(1).getNode()))
  //       Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
  //     SDValue Rd = FuncInfo->getValFromRegMap(N->getOperand(0));
  //     Node =
  //         CurDAG
  //             ->getNode(ISD::AND, dl, getDefaultEVT(), Rd, Rn, getMDOperand(N))
  //             .getNode();
  //   } else {
  //     // AND{S}<c> <Rd>,<Rn>,#<const>
  //     SDValue op2 = N->getOperand(2);
  //     if (RegisterSDNode::classof(op2.getNode()))
  //       op2 = FuncInfo->getValFromRegMap(op2);

  //     Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
  //     Node =
  //         CurDAG
  //             ->getNode(ISD::AND, dl, getDefaultEVT(), Rn, op2, getMDOperand(N))
  //             .getNode();
  //   }

  //   recordDefinition(Rd.getNode(), Node);
  //   replaceNode(N, Node);
  //   // TODO:
  //   // AND{S}<c>.W <Rd>,<Rn>,<Rm>{,<shift>}
  //   // AND{S}<c> <Rd>,<Rn>,<Rm>{,<shift>}
  //   // AND{S}<c> <Rd>,<Rn>,<Rm>,<type> <Rs>
  // } break;
  // /* ASR */
  // case RISCV32::ASRr:
  // case RISCV32::ASRi:
  // case RISCV32::tASRrr:
  // case RISCV32::tASRri:
  // case RISCV32::t2ASRrr:
  // case RISCV32::t2ASRri: {
  //   SDValue Rd = N->getOperand(0);
  //   SDValue Rn = N->getOperand(1);
  //   SDNode *Node = nullptr;
  //   if (isTwoAddressMode(Rd.getNode())) {
  //     if (RegisterSDNode::classof(N->getOperand(1).getNode()))
  //       Rn = FuncInfo->getValFromRegMap(N->getOperand(1));

  //     SDValue Rd = FuncInfo->getValFromRegMap(N->getOperand(0));
  //     Node =
  //         CurDAG
  //             ->getNode(ISD::SRA, dl, getDefaultEVT(), Rd, Rn, getMDOperand(N))
  //             .getNode();
  //   } else {
  //     SDValue op2 = N->getOperand(2);
  //     if (RegisterSDNode::classof(op2.getNode()))
  //       op2 = FuncInfo->getValFromRegMap(op2);

  //     Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
  //     Node =
  //         CurDAG
  //             ->getNode(ISD::SRA, dl, getDefaultEVT(), Rn, op2, getMDOperand(N))
  //             .getNode();
  //   }

  //   recordDefinition(Rd.getNode(), Node);
  //   replaceNode(N, Node);
  // } break;
  // /* CMN */
  // case RISCV32::CMNri:
  // case RISCV32::CMNzrr:
  // case RISCV32::tCMNz:
  // case RISCV32::t2CMNri:
  // case RISCV32::t2CMNzrr:
  // case RISCV32::t2CMNzrs: {
  //   SDValue Rd = N->getOperand(0);
  //   SDValue Rn = N->getOperand(1);
  //   SDNode *Node = nullptr;

  //   if (RegisterSDNode::classof(N->getOperand(1).getNode()))
  //     Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
  //   Rd = FuncInfo->getValFromRegMap(Rd);
  //   Node =
  //       CurDAG
  //           ->getNode(RISCV32ISD::CMN, dl, getDefaultEVT(), Rd, Rn, getMDOperand(N))
  //           .getNode();

  //   recordDefinition(Rd.getNode(), Node);
  //   replaceNode(N, Node);
  // } break;
  // /* EOR */
  // case RISCV32::EORri:
  // case RISCV32::EORrr:
  // case RISCV32::EORrsi:
  // case RISCV32::EORrsr:
  // case RISCV32::tEOR:
  // case RISCV32::t2EORrr:
  // case RISCV32::t2EORrs:
  // case RISCV32::t2EORri: {
  //   SDValue Rd = N->getOperand(0);
  //   SDValue Rn = N->getOperand(1);
  //   SDNode *Node = nullptr;
  //   if (isTwoAddressMode(Rd.getNode())) {
  //     // EORS <Rdn>,<Rm>
  //     // EOR<c> <Rdn>,<Rm>
  //     if (RegisterSDNode::classof(N->getOperand(1).getNode()))
  //       Rn = FuncInfo->getValFromRegMap(N->getOperand(1));

  //     SDValue Rd = FuncInfo->getValFromRegMap(N->getOperand(0));
  //     Node =
  //         CurDAG
  //             ->getNode(ISD::XOR, dl, getDefaultEVT(), Rd, Rn, getMDOperand(N))
  //             .getNode();
  //   } else {
  //     // EOR{S}<c> <Rd>,<Rn>,#<const>
  //     SDValue op2 = N->getOperand(2);
  //     if (RegisterSDNode::classof(op2.getNode()))
  //       op2 = FuncInfo->getValFromRegMap(op2);
  //     Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
  //     Node =
  //         CurDAG
  //             ->getNode(ISD::XOR, dl, getDefaultEVT(), Rn, op2, getMDOperand(N))
  //             .getNode();
  //   }
  //   recordDefinition(Rd.getNode(), Node);
  //   replaceNode(N, Node);
  //   // TODO:
  //   // EOR{S}<c>.W <Rd>,<Rn>,<Rm>{,<shift>}
  //   // EOR{S}<c> <Rd>,<Rn>,<Rm>{,<shift>}
  //   // EOR{S}<c> <Rd>,<Rn>,<Rm>,<type> <Rs>
  // } break;
  // /* MSR */
  // case RISCV32::MSR:
  // case RISCV32::MSRi:
  // case RISCV32::MSRbanked:
  // case RISCV32::t2MSR_M:
  // case RISCV32::t2MSR_AR:
  // case RISCV32::t2MSRbanked: {
  //   // Update the CPSR.
  //   SDValue Cond = N->getOperand(1);
  //   SDNode *Node = nullptr;
  //   if (RegisterSDNode::classof(N->getOperand(1).getNode()))
  //     Cond = FuncInfo->getValFromRegMap(N->getOperand(1));

  //   Node = CurDAG
  //              ->getNode(EXT_RISCV32ISD::MSR, dl, getDefaultEVT(), Cond,
  //                        getMDOperand(N))
  //              .getNode();

  //   replaceNode(N, Node);
  // } break;
  // /* MUL */
  // case RISCV32::MUL:
  // case RISCV32::tMUL:
  // case RISCV32::t2MUL: {
  //   /* MULS <Rd>, <Rn>, <Rm> */
  //   /* MUL<c> <Rd>, <Rn>, <Rm> */
  //   /* MUL{S}<c> <Rd>, <Rn>, <Rm> */
  //   SDValue Rd = N->getOperand(0);
  //   SDValue Rn = N->getOperand(1);
  //   SDNode *Node = nullptr;
  //   SDValue op2 = N->getOperand(2);
  //   op2 = FuncInfo->getValFromRegMap(op2);
  //   Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
  //   Node =
  //       CurDAG->getNode(ISD::MUL, dl, getDefaultEVT(), Rn, op2, getMDOperand(N))
  //           .getNode();

  //   recordDefinition(Rd.getNode(), Node);
  //   replaceNode(N, Node);
  // } break;
  // /* MVN */
  // case RISCV32::MVNi:
  // case RISCV32::MVNr:
  // case RISCV32::MVNsi:
  // case RISCV32::MVNsr:
  // case RISCV32::tMVN:
  // case RISCV32::t2MVNi:
  // case RISCV32::t2MVNr:
  // case RISCV32::t2MVNs: {
  //   SDValue Rd = N->getOperand(0);
  //   SDValue Rn = N->getOperand(1);
  //   SDNode *Node = nullptr;
  //   if (RegisterSDNode::classof(N->getOperand(1).getNode()))
  //     Rn = FuncInfo->getValFromRegMap(N->getOperand(1));

  //   Node = CurDAG
  //              ->getNode(ISD::XOR, dl, getDefaultEVT(), Rn,
  //                        CurDAG->getConstant(-1, dl, getDefaultEVT()))
  //              .getNode();

  //   recordDefinition(Rd.getNode(), Node);
  //   replaceNode(N, Node);
  // } break;
  // /* LSL */
  // case RISCV32::LSLi:
  // case RISCV32::LSLr:
  // case RISCV32::tLSLri:
  // case RISCV32::tLSLrr:
  // case RISCV32::t2LSLri:
  // case RISCV32::t2LSLrr: {
  //   SDValue Rd = N->getOperand(0);
  //   SDValue Rn = N->getOperand(1);
  //   SDNode *Node = nullptr;
  //   if (isTwoAddressMode(Rd.getNode())) {
  //     if (RegisterSDNode::classof(N->getOperand(1).getNode()))
  //       Rn = FuncInfo->getValFromRegMap(N->getOperand(1));

  //     Rd = FuncInfo->getValFromRegMap(N->getOperand(0));
  //     Node =
  //         CurDAG
  //             ->getNode(ISD::SHL, dl, getDefaultEVT(), Rd, Rn, getMDOperand(N))
  //             .getNode();
  //   } else {
  //     SDValue op2 = N->getOperand(2);
  //     if (RegisterSDNode::classof(op2.getNode()))
  //       op2 = FuncInfo->getValFromRegMap(op2);

  //     Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
  //     Node =
  //         CurDAG
  //             ->getNode(ISD::SHL, dl, getDefaultEVT(), Rn, op2, getMDOperand(N))
  //             .getNode();
  //   }
  //   recordDefinition(Rd.getNode(), Node);
  //   replaceNode(N, Node);
  // } break;
  // /* LSR */
  // case RISCV32::LSRi:
  // case RISCV32::LSRr:
  // case RISCV32::tLSRri:
  // case RISCV32::tLSRrr:
  // case RISCV32::t2LSRri:
  // case RISCV32::t2LSRrr: {
  //   SDValue Rd = N->getOperand(0);
  //   SDValue Rn = N->getOperand(1);
  //   SDNode *Node = nullptr;
  //   if (isTwoAddressMode(Rd.getNode())) {
  //     if (RegisterSDNode::classof(N->getOperand(1).getNode()))
  //       Rn = FuncInfo->getValFromRegMap(N->getOperand(1));

  //     SDValue Rd = FuncInfo->getValFromRegMap(N->getOperand(0));
  //     Node =
  //         CurDAG
  //             ->getNode(ISD::SRL, dl, getDefaultEVT(), Rd, Rn, getMDOperand(N))
  //             .getNode();
  //   } else {
  //     SDValue op2 = N->getOperand(2);
  //     if (RegisterSDNode::classof(op2.getNode()))
  //       op2 = FuncInfo->getValFromRegMap(op2);
  //     Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
  //     Node =
  //         CurDAG
  //             ->getNode(ISD::SRL, dl, getDefaultEVT(), Rn, op2, getMDOperand(N))
  //             .getNode();
  //   }
  //   recordDefinition(Rd.getNode(), Node);
  //   replaceNode(N, Node);
  // } break;
  // /* ORR */
  // case RISCV32::ORRri:
  // case RISCV32::ORRrr:
  // case RISCV32::ORRrsi:
  // case RISCV32::ORRrsr:
  // case RISCV32::tORR:
  // case RISCV32::t2ORRri:
  // case RISCV32::t2ORRrr:
  // case RISCV32::t2ORRrs: {
  //   SDValue Rd = N->getOperand(0);
  //   SDValue Rn = N->getOperand(1);
  //   // <opcode>   {<cond>}{s}<Rd>，<Rn>{，<OP2>}
  //   SDNode *Node = nullptr;
  //   if (isTwoAddressMode(Rd.getNode())) {
  //     if (RegisterSDNode::classof(N->getOperand(1).getNode()))
  //       Rn = FuncInfo->getValFromRegMap(N->getOperand(1));

  //     SDValue Rd = FuncInfo->getValFromRegMap(N->getOperand(0));
  //     Node =
  //         CurDAG->getNode(ISD::OR, dl, getDefaultEVT(), Rd, Rn, getMDOperand(N))
  //             .getNode();
  //   } else {
  //     SDValue op2 = N->getOperand(2);
  //     if (RegisterSDNode::classof(op2.getNode()))
  //       op2 = FuncInfo->getValFromRegMap(op2);

  //     Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
  //     Node =
  //         CurDAG
  //             ->getNode(ISD::OR, dl, getDefaultEVT(), Rn, op2, getMDOperand(N))
  //             .getNode();
  //   }
  //   recordDefinition(Rd.getNode(), Node);
  //   replaceNode(N, Node);
  // } break;
  // /* ROR */
  // case RISCV32::RORi:
  // case RISCV32::RORr:
  // case RISCV32::tROR:
  // case RISCV32::t2RORri:
  // case RISCV32::t2RORrr: {
  //   SDValue Rd = N->getOperand(0);
  //   SDValue Rn = N->getOperand(1);
  //   SDNode *Node = nullptr;
  //   if (isTwoAddressMode(Rd.getNode())) {
  //     if (RegisterSDNode::classof(N->getOperand(1).getNode()))
  //       Rn = FuncInfo->getValFromRegMap(N->getOperand(1));

  //     SDValue Rd = FuncInfo->getValFromRegMap(N->getOperand(0));
  //     Node =
  //         CurDAG
  //             ->getNode(ISD::ROTR, dl, getDefaultEVT(), Rd, Rn, getMDOperand(N))
  //             .getNode();
  //   } else {
  //     SDValue op2 = N->getOperand(2);
  //     if (RegisterSDNode::classof(op2.getNode()))
  //       op2 = FuncInfo->getValFromRegMap(op2);
  //     Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
  //     Node = CurDAG
  //                ->getNode(ISD::ROTR, dl, getDefaultEVT(), Rn, op2,
  //                          getMDOperand(N))
  //                .getNode();
  //   }
  //   recordDefinition(Rd.getNode(), Node);
  //   replaceNode(N, Node);
  // } break;
  // /* RRX */
  // case RISCV32::RRX: {
  //   SDValue Rd = N->getOperand(0);
  //   SDValue Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
  //   SDNode *Node = nullptr;
  //   Node =
  //       CurDAG->getNode(RISCV32ISD::RRX, dl, getDefaultEVT(), Rn, getMDOperand(N))
  //           .getNode();

  //   recordDefinition(Rd.getNode(), Node);
  //   replaceNode(N, Node);
  // } break;
  // /* RSB */
  // case RISCV32::RSBri:
  // case RISCV32::RSBrr:
  // case RISCV32::RSBrsi:
  // case RISCV32::RSBrsr:
  // case RISCV32::tRSB:
  // case RISCV32::t2RSBri:
  // case RISCV32::t2RSBrr:
  // case RISCV32::t2RSBrs: {
  //   SDValue Rd = N->getOperand(0);
  //   SDValue Rn = N->getOperand(1);
  //   SDNode *Node = nullptr;
  //   if (isTwoAddressMode(Rd.getNode())) {
  //     if (RegisterSDNode::classof(N->getOperand(1).getNode()))
  //       Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
  //     SDValue Rd = FuncInfo->getValFromRegMap(N->getOperand(0));
  //     Node = CurDAG
  //                ->getNode(EXT_RISCV32ISD::RSB, dl, getDefaultEVT(), Rd, Rn,
  //                          getMDOperand(N))
  //                .getNode();
  //   } else {
  //     SDValue op2 = N->getOperand(2);
  //     if (RegisterSDNode::classof(op2.getNode()))
  //       op2 = FuncInfo->getValFromRegMap(op2);
  //     Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
  //     Node = CurDAG
  //                ->getNode(EXT_RISCV32ISD::RSB, dl, getDefaultEVT(), op2, Rn,
  //                          getMDOperand(N))
  //                .getNode();
  //   }
  //   recordDefinition(Rd.getNode(), Node);
  //   replaceNode(N, Node);
  // } break;
  // /* RSC */
  // case RISCV32::RSCri:
  // case RISCV32::RSCrr:
  // case RISCV32::RSCrsi:
  // case RISCV32::RSCrsr: {
  //   // RSC{S}<c> <Rd>,<Rn>, #0
  //   // RSC{S}<c>.W <Rd>,<Rn>,#<const>
  //   SDValue Rd = N->getOperand(0);
  //   SDValue Rn = N->getOperand(1);
  //   SDNode *Node = nullptr;
  //   if (isTwoAddressMode(Rd.getNode())) {
  //     if (RegisterSDNode::classof(N->getOperand(1).getNode()))
  //       Rn = FuncInfo->getValFromRegMap(N->getOperand(1));

  //     SDValue Rd = FuncInfo->getValFromRegMap(N->getOperand(0));
  //     Node = CurDAG
  //                ->getNode(EXT_RISCV32ISD::RSC, dl, getDefaultEVT(), Rd, Rn,
  //                          getMDOperand(N))
  //                .getNode();
  //   } else {
  //     SDValue op2 = N->getOperand(2);
  //     if (RegisterSDNode::classof(op2.getNode()))
  //       op2 = FuncInfo->getValFromRegMap(op2);
  //     Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
  //     Node = CurDAG
  //                ->getNode(EXT_RISCV32ISD::RSC, dl, getDefaultEVT(), Rn, op2,
  //                          getMDOperand(N))
  //                .getNode();
  //   }
  //   recordDefinition(Rd.getNode(), Node);
  //   replaceNode(N, Node);
  // } break;
  // /* CLZ */
  // case RISCV32::CLZ:
  // case RISCV32::t2CLZ: {
  //   SDValue Rd = N->getOperand(0);
  //   SDValue Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
  //   SDNode *Node = nullptr;
  //   Node = CurDAG->getNode(ISD::CTLZ, dl, getDefaultEVT(), Rn, getMDOperand(N))
  //              .getNode();
  //   recordDefinition(Rd.getNode(), Node);
  //   replaceNode(N, Node);
  // } break;
  // /* SBC */
  // case RISCV32::SBCrr:
  // case RISCV32::SBCri:
  // case RISCV32::tSBC: {
  //   SDValue Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
  //   SDValue Operand2 = FuncInfo->getValFromRegMap(N->getOperand(2));
  //   SDNode *Node = CurDAG
  //                      ->getNode(EXT_RISCV32ISD::SBC, dl, getDefaultEVT(), Rn,
  //                                Operand2, getMDOperand(N))
  //                      .getNode();

  //   recordDefinition(Rn.getNode(), Node);
  //   replaceNode(N, Node);
  // } break;
  // /* TEQ */
  // case RISCV32::TEQri:
  // case RISCV32::TEQrr:
  // case RISCV32::TEQrsi:
  // case RISCV32::TEQrsr:
  // case RISCV32::t2TEQri:
  // case RISCV32::t2TEQrr:
  // case RISCV32::t2TEQrs: {
  //   SDValue Rd = N->getOperand(0);
  //   SDValue Rn = N->getOperand(1);
  //   SDNode *Node = nullptr;

  //   if (RegisterSDNode::classof(N->getOperand(1).getNode()))
  //     Rn = FuncInfo->getValFromRegMap(N->getOperand(1));

  //   Rd = FuncInfo->getValFromRegMap(N->getOperand(0));
  //   Node = CurDAG
  //              ->getNode(EXT_RISCV32ISD::TEQ, dl, getDefaultEVT(), Rd, Rn,
  //                        getMDOperand(N))
  //              .getNode();

  //   recordDefinition(Rd.getNode(), Node);
  //   replaceNode(N, Node);
  // } break;
  // /* TST */
  // case RISCV32::TSTrsi:
  // case RISCV32::TSTrr:
  // case RISCV32::TSTri:
  // case RISCV32::TSTrsr:
  // case RISCV32::tTST:
  // case RISCV32::t2TSTri:
  // case RISCV32::t2TSTrr:
  // case RISCV32::t2TSTrs: {
  //   SDValue Rd = N->getOperand(0);
  //   SDValue Rn = N->getOperand(1);
  //   SDNode *Node = nullptr;

  //   if (RegisterSDNode::classof(N->getOperand(1).getNode()))
  //     Rn = FuncInfo->getValFromRegMap(N->getOperand(1));

  //   Rd = FuncInfo->getValFromRegMap(N->getOperand(0));
  //   Node = CurDAG
  //              ->getNode(EXT_RISCV32ISD::TST, dl, getDefaultEVT(), Rd, Rn,
  //                        getMDOperand(N))
  //              .getNode();

  //   recordDefinition(Rd.getNode(), Node);
  //   replaceNode(N, Node);
  // } break;
  // /* BIC */
  // case RISCV32::BICri:
  // case RISCV32::BICrr:
  // case RISCV32::BICrsi:
  // case RISCV32::BICrsr:
  // case RISCV32::tBIC:
  // case RISCV32::t2BICri:
  // case RISCV32::t2BICrr:
  // case RISCV32::t2BICrs: {
  //   SDValue Rd = N->getOperand(0);
  //   SDValue Rn = N->getOperand(1);
  //   SDNode *Node = nullptr;
  //   if (isTwoAddressMode(Rd.getNode())) {
  //     if (RegisterSDNode::classof(N->getOperand(1).getNode()))
  //       Rn = FuncInfo->getValFromRegMap(N->getOperand(1));

  //     SDValue Rd = FuncInfo->getValFromRegMap(N->getOperand(0));
  //     Node = CurDAG
  //                ->getNode(EXT_RISCV32ISD::BIC, dl, getDefaultEVT(), Rd, Rn,
  //                          getMDOperand(N))
  //                .getNode();
  //   } else {
  //     SDValue op2 = N->getOperand(2);
  //     if (RegisterSDNode::classof(op2.getNode()))
  //       op2 = FuncInfo->getValFromRegMap(op2);

  //     Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
  //     Node = CurDAG
  //                ->getNode(EXT_RISCV32ISD::BIC, dl, getDefaultEVT(), Rn, op2,
  //                          getMDOperand(N))
  //                .getNode();
  //   }

  //   recordDefinition(Rd.getNode(), Node);
  //   replaceNode(N, Node);
  // } break;
  // /* MLA */
  // case RISCV32::MLA:
  // case RISCV32::t2MLA: {
  //   SDValue Rd = N->getOperand(0);
  //   SDValue Rn = FuncInfo->getValFromRegMap(N->getOperand(1));
  //   SDValue Rm = FuncInfo->getValFromRegMap(N->getOperand(2));
  //   SDValue Ra = FuncInfo->getValFromRegMap(N->getOperand(3));
  //   SDNode *Node = nullptr;
  //   Node = CurDAG
  //              ->getNode(EXT_RISCV32ISD::MLA, dl, getDefaultEVT(), Rn, Rm, Ra,
  //                        getMDOperand(N))
  //              .getNode();
  //   recordDefinition(Rd.getNode(), Node);
  //   replaceNode(N, Node);
  // } break;
  // /* UXTB */
  // case RISCV32::UXTB: {
  //   SDValue Rd = N->getOperand(0);
  //   SDValue Rm = N->getOperand(1);
  //   SDValue Rotation = N->getOperand(2);
  //   SDNode *Node = nullptr;

  //   if (RegisterSDNode::classof(N->getOperand(1).getNode()))
  //     Rm = FuncInfo->getValFromRegMap(N->getOperand(1));
  //   Node = CurDAG
  //              ->getNode(EXT_RISCV32ISD::UXTB, dl, getDefaultEVT(), Rd, Rm,
  //                        Rotation, getMDOperand(N))
  //              .getNode();
  //   recordDefinition(Rd.getNode(), Node);
  //   replaceNode(N, Node);
  // } break;
  // case RISCV32::MCR:
  // case RISCV32::MCRR:
  // case RISCV32::t2MCR:
  // case RISCV32::t2MCRR:
  // case RISCV32::VMSR:
  // case RISCV32::VMSR_FPEXC:
  // case RISCV32::VMSR_FPSID:
  // case RISCV32::VMSR_FPINST:
  // case RISCV32::VMSR_FPINST2: {
  //   outs() << "WARNING: Not yet implemented!\n";
  // } break;
  // case RISCV32::MRS:
  // case RISCV32::MRSsys:
  // case RISCV32::t2MRS_AR:
  // case RISCV32::t2MRSsys_AR: {
  //   SDValue Rn = N->getOperand(0);
  //   if (RegisterSDNode::classof(Rn.getNode()))
  //     Rn = FuncInfo->getValFromRegMap(Rn);

  //   SDNode *Node =
  //       CurDAG
  //           ->getNode(EXT_RISCV32ISD::MRS, dl, getDefaultEVT(), Rn, getMDOperand(N))
  //           .getNode();
  //   replaceNode(N, Node);
  // } break;
  // /* ABS */
  // case RISCV32::ABS:
  // case RISCV32::t2ABS: {
  //   outs() << "WARNING: Not yet implemented!\n";
  // } break;
  // case RISCV32::tLDRpci:
  // case RISCV32::LDRcp: {
  //   outs() << "WARNING: Not yet implemented!\n";
  // } break;
  // case RISCV32::t2SBFX:
  // case RISCV32::SBFX:
  // case RISCV32::t2UBFX:
  // case RISCV32::UBFX: {
  //   outs() << "WARNING: Not yet implemented!\n";
  // } break;
  // case RISCV32::t2UMAAL:
  // case RISCV32::UMAAL: {
  //   outs() << "WARNING: Not yet implemented!\n";
  // } break;
  // case RISCV32::t2UMLAL:
  // case RISCV32::UMLAL:
  // case RISCV32::UMLALv5: {
  //   outs() << "WARNING: Not yet implemented!\n";
  // } break;
  // case RISCV32::t2SMLAL:
  // case RISCV32::SMLAL:
  // case RISCV32::SMLALv5: {
  //   outs() << "WARNING: Not yet implemented!\n";
  // } break;
  // case RISCV32::t2SMMLS:
  // case RISCV32::SMMLS: {
  //   outs() << "WARNING: Not yet implemented!\n";
  // } break;
  // case RISCV32::VZIPd8:
  // case RISCV32::VZIPd16:
  // case RISCV32::VZIPq8:
  // case RISCV32::VZIPq16:
  // case RISCV32::VZIPq32: {
  //   outs() << "WARNING: Not yet implemented!\n";
  // } break;
  // case RISCV32::VUZPd8:
  // case RISCV32::VUZPd16:
  // case RISCV32::VUZPq8:
  // case RISCV32::VUZPq16:
  // case RISCV32::VUZPq32: {
  //   outs() << "WARNING: Not yet implemented!\n";
  // } break;
  // case RISCV32::VTRNd8:
  // case RISCV32::VTRNd16:
  // case RISCV32::VTRNd32:
  // case RISCV32::VTRNq8:
  // case RISCV32::VTRNq16:
  // case RISCV32::VTRNq32: {
  //   outs() << "WARNING: Not yet implemented!\n";
  // } break;
  //   // TODO: Need to add other pattern matching here.
  // }
    break;
  }  
}

void RISCVInstSelector::select(SDNode *N) {
  LLVM_DEBUG(dbgs()<<"begin selcode perid="<<N->PersistentId<<"\n");
  if (!N->isMachineOpcode()) {
    //N->setNodeId(-1);
    LLVM_DEBUG(dbgs()<< "#### ###at select SDNode="<<(uint64_t)N <<  " used -1 ###\n");
    return; // Already selected.
  }
  LLVM_DEBUG(dbgs()<< "$$$$$ at select SDNode="<<(uint64_t)N << "unqi id = "<< N->PersistentId << "###\n");

  selectCode(N);
}
