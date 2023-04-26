//===- RISCV32MIRevising.cpp - Binary raiser utility llvm-mctoll --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of RISCV32MIRevising class for use by
// llvm-mctoll.
//
//===----------------------------------------------------------------------===//

#include "RISCV32MIRevising.h"
#include "RISCV32ModuleRaiser.h"
#include "RISCVSubtarget.h"
#include "ExternalFunctions.h"
#include "MCInstRaiser.h"
#include "MachineFunctionRaiser.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/Object/ELF.h"
#include "llvm/Object/ELFObjectFile.h"

#define DEBUG_TYPE "mctoll"

using namespace llvm;
using namespace llvm::object;

char RISCV32MIRevising::ID = 0;

int64_t _GPbase = 0;
MachineInstr *LWInst = NULL;
MachineInstr *LastLW = NULL;


RISCV32MIRevising::RISCV32MIRevising(RISCV32ModuleRaiser &MRsr) : RISCV32RaiserBase(ID, MRsr) {
  MCIR = nullptr;
}

RISCV32MIRevising::~RISCV32MIRevising() {}

void RISCV32MIRevising::init(MachineFunction *mf, Function *rf) {
  RISCV32RaiserBase::init(mf, rf);
}

void RISCV32MIRevising::setMCInstRaiser(MCInstRaiser *PMCIR) { MCIR = PMCIR; }

// Extract the offset of MachineInstr MI from the Metadata operand.
static uint64_t getMCInstIndex(const MachineInstr &MI) {
  unsigned NumExpOps = MI.getNumExplicitOperands();
  const MachineOperand &MO = MI.getOperand(NumExpOps);
  assert(MO.isMetadata() &&
         "Unexpected non-metadata operand in branch instruction!");
  const MDNode *MDN = MO.getMetadata();
  // Unwrap metadata of the instruction to get the MCInstIndex of
  // the MCInst corresponding to this MachineInstr.
  ConstantAsMetadata *CAM = dyn_cast<ConstantAsMetadata>(MDN->getOperand(0));
  assert(CAM != nullptr && "Unexpected metadata type!");
  Constant *CV = CAM->getValue();
  ConstantInt *CI = dyn_cast<ConstantInt>(CV);
  assert(CI != nullptr && "Unexpected metadata constant type!");
  APInt ArbPrecInt = CI->getValue();
  return ArbPrecInt.getSExtValue();
}

template <class ELFT>
uint64_t getLoadAlignProgramHeader(const ELFFile<ELFT> *Obj) {
  typedef ELFFile<ELFT> ELFO;
  auto ProgramHeaderOrError = Obj->program_headers();

  if (!ProgramHeaderOrError)
    report_fatal_error(
        errorToErrorCode(ProgramHeaderOrError.takeError()).message());

  for (const typename ELFO::Elf_Phdr &Phdr : *ProgramHeaderOrError) {
    if (Phdr.p_type == ELF::PT_LOAD)
      return (uint64_t)Phdr.p_align;
  }

  assert(false && "Failed to get Phdr p_align!");
  return 0;
}

/// Create function for external function.
uint64_t RISCV32MIRevising::getCalledFunctionAtPLTOffset(uint64_t PLTEndOff,
                                                     uint64_t CallAddr) {
  LLVM_DEBUG(dbgs() << "\n@getCalledFunctionAtPLTOffset\n<<PltEndOff="<<PLTEndOff<<",calladdr="<<CallAddr<<"\n");
  
  //return 0;
  /*
  To do : fix plt table

  For arm 
     10290:       e28fc600        add     ip, pc, #0, 12
     10294:       e28cca10        add     ip, ip, #16, 20 ; 0x10000
     10298:       e5bcf348        ldr     pc, [ip, #840]! ; 0x348
  */
  const ELF32LEObjectFile *Elf32LEObjFile =
      dyn_cast<ELF32LEObjectFile>(MR->getObjectFile());
  assert(Elf32LEObjFile != nullptr &&
         "Only 32-bit ELF binaries supported at present!");
  unsigned char ExecType = Elf32LEObjFile->getELFFile().getHeader().e_type;

  assert((ExecType == ELF::ET_DYN) || (ExecType == ELF::ET_EXEC));
  // Find the section that contains the offset. That must be the PLT section
  for (section_iterator SecIter : Elf32LEObjFile->sections()) {
    uint64_t SecStart = SecIter->getAddress();
    uint64_t SecEnd = SecStart + SecIter->getSize();
    if ((SecStart <= PLTEndOff) && (SecEnd >= PLTEndOff)) {
      StringRef SecName;
      if (auto NameOrErr = SecIter->getName())
        SecName = *NameOrErr;
      else {
        consumeError(NameOrErr.takeError());
        assert(false && "Failed to get section name with PLT offset");
      }
      if (SecName.compare(".plt") != 0) {
        assert(false && "Unexpected section name of PLT offset");
      }

      auto StrOrErr = SecIter->getContents();
      assert(StrOrErr && "Failed to get the content of section!");
      auto SecData = *StrOrErr;
      ArrayRef<uint8_t> Bytes(reinterpret_cast<const uint8_t *>(SecData.data()),
                              SecData.size());

      MCInst InstAUIPC;
      uint64_t InstAUIPCSz;
      bool Success = MR->getMCDisassembler()->getInstruction(
          InstAUIPC, InstAUIPCSz , Bytes.slice(PLTEndOff - SecStart),
          PLTEndOff , nulls());
      assert(Success && "Failed to disassemble instruction in PLT");
      LLVM_DEBUG(dbgs()<<"InstAUIPC="<<InstAUIPC<<"\n");
      unsigned int OpcAUIPC = InstAUIPC.getOpcode();
     // MCInstrDesc MCIDAddIP = MR->getMCInstrInfo()->get(OpcAddIP);

      if (OpcAUIPC != RISCV::AUIPC ) {
        assert(false && "Failed to find function entry from .plt.");
      }

      MCOperand OpdAUIPC= InstAUIPC.getOperand(1);
      assert(OpdAUIPC.isImm() && "Unexpected immediate for offset.");
      unsigned Bits = OpdAUIPC.getImm() & 0xFFFFF;
      int64_t RipOffset = (Bits << 12) + PLTEndOff;//static_cast<int64_t>(RISCV32_AM::rotr32(Bits, Rot));

      MCInst Inst;
      uint64_t InstSz;
      Success = MR->getMCDisassembler()->getInstruction(
          Inst, InstSz, Bytes.slice(PLTEndOff + 4 - SecStart), PLTEndOff + 4,
          nulls());
      assert(Success && "Failed to disassemble instruction in PLT");
      unsigned int Opcode = Inst.getOpcode();
      MCInstrDesc MCID = MR->getMCInstrInfo()->get(Opcode);
/*
      if (Opcode != RISCV32::LDRi12 && (MCID.getNumOperands() != 6)) {
        assert(false && "Failed to find function entry from .plt.");
      }*/
      LLVM_DEBUG(dbgs()<<"LW Inst="<<Inst << "\n");
      MCOperand Operand = Inst.getOperand(2);
      assert(Operand.isImm() && "Unexpected immediate for offset.");

      uint64_t Index = Operand.getImm();

      uint64_t GotPltRelocOffset = RipOffset+ Index;
      LLVM_DEBUG(dbgs()<<"LW Index="<<Index<< ",gotpltrelocoffset="<<GotPltRelocOffset<<"\n");
      const RelocationRef *GotPltReloc =
          MR->getDynRelocAtOffset(GotPltRelocOffset);
      assert(GotPltReloc != nullptr &&
             "Failed to get dynamic relocation for jmp target of PLT entry");

      //assert((GotPltReloc->getType() == ELF::R_RISCV32_JUMP_SLOT) &&
        //     "Unexpected relocation type for PLT jmp instruction");
      symbol_iterator CalledFuncSym = GotPltReloc->getSymbol();
      assert(CalledFuncSym != Elf32LEObjFile->symbol_end() &&
             "Failed to find relocation symbol for PLT entry");
      Expected<StringRef> CalledFuncSymName = CalledFuncSym->getName();
      assert(CalledFuncSymName &&
             "Failed to find symbol associated with dynamic "
             "relocation of PLT jmp target.");
      Expected<uint64_t> CalledFuncSymAddr = CalledFuncSym->getAddress();
      assert(CalledFuncSymAddr &&
             "Failed to get called function address of PLT entry");

      if (CalledFuncSymAddr.get() == 0) {
        // Set CallTargetIndex for plt offset to map undefined function symbol
        // for emit CallInst use.
        Function *CalledFunc =
            ExternalFunctions::Create(*CalledFuncSymName, *MR);
        // Bail out if function prototype is not available
        if (!CalledFunc)
          exit(-1);
        MR->setSyscallMapping(PLTEndOff, CalledFunc);
        MR->fillInstAddrFuncMap(CallAddr, CalledFunc);
      }
      return CalledFuncSymAddr.get();
    }
  }
  return 0;
}

/// Relocate call branch instructions in object files.
void RISCV32MIRevising::relocateBranch(MachineInstr &MInst) {
  int64_t relCallTargetOffset = MInst.getOperand(1).getImm();
  const ELF32LEObjectFile *Elf32LEObjFile =
      dyn_cast<ELF32LEObjectFile>(MR->getObjectFile());
  assert(Elf32LEObjFile != nullptr &&
         "Only 32-bit ELF binaries supported at present.");

  auto EType = Elf32LEObjFile->getELFFile().getHeader().e_type;
  if ((EType == ELF::ET_DYN) || (EType == ELF::ET_EXEC)) {
    int64_t textSectionAddress = MR->getTextSectionAddress();
    assert(textSectionAddress >= 0 && "Failed to find text section address");

    // Get MCInst offset - the offset of machine instruction in the binary
    // and instruction size
    int64_t MCInstOffset = getMCInstIndex(MInst);
    int64_t CallAddr = MCInstOffset + textSectionAddress;
    int64_t CallTargetIndex = CallAddr + relCallTargetOffset;
    assert(MCIR != nullptr && "MCInstRaiser was not initialized");
    int64_t CallTargetOffset = CallTargetIndex - textSectionAddress;
    if (CallTargetOffset < 0 || !MCIR->isMCInstInRange(CallTargetOffset)) {
      Function *CalledFunc = nullptr;
      uint64_t MCInstSize = MCIR->getMCInstSize(MCInstOffset);
      uint64_t Index = 1;
      CalledFunc = MR->getRaisedFunctionAt(CallTargetIndex);
      if (CalledFunc == nullptr) {
        CalledFunc =
            MR->getCalledFunctionUsingTextReloc(MCInstOffset, MCInstSize);
      }
      // Look up the PLT to find called function.
      if (CalledFunc == nullptr)
        Index = getCalledFunctionAtPLTOffset(CallTargetIndex, CallAddr);

      if (CalledFunc == nullptr) {
        if (Index == 0)
          MInst.getOperand(1).setImm(CallTargetIndex);
        else if (Index != 1)
          MInst.getOperand(1).setImm(Index);
        else
          assert(false && "Failed to get the call function!");
      } else
        MInst.getOperand(1).setImm(CallTargetIndex);
    } else if(MCIR->isMCInstInRange(CallTargetOffset)){
      MInst.getOperand(1).setImm(CallTargetIndex);
    }
  } else {
    uint64_t Offset = getMCInstIndex(MInst);
    const RelocationRef *reloc = MR->getTextRelocAtOffset(Offset, 4);
    auto ImmValOrErr = (*reloc->getSymbol()).getValue();
    assert(ImmValOrErr && "Failed to get immediate value");
    MInst.getOperand(1).setImm(*ImmValOrErr);
  }
}



void RISCV32MIRevising::FindLoadGP(){

  LLVM_DEBUG(dbgs()<< "DEBUG:: Entering FindLoadGP()\n");
  // const ELF32LEObjectFile *Elf32LEObjFile =
  //     dyn_cast<ELF32LEObjectFile>(MR->getObjectFile());
  // assert(Elf32LEObjFile != nullptr &&
  //        "Only 32-bit ELF binaries supported at present!");
  //unsigned char ExecType = Elf32LEObjFile->getELFFile().getHeader().e_type;
  //assert((ExecType == ELF::ET_DYN) || (ExecType == ELF::ET_EXEC));

  StringRef SymName;
  int64_t SymAddr;
  const ObjectFile* Obj = MR->getObjectFile();
  if(!Obj)
      LLVM_DEBUG(dbgs()<< "DEBUG:: ObjectFile Retrieved is NULL\n");

  // section_iterator SecIter;
  for (const SymbolRef &Symbol : Obj->symbols()) {
    //LLVM_DEBUG(dbgs()<< "DEBUG:: Enter Symbol Loop\n");
    auto NameOrErr = Symbol.getName();
    if (!NameOrErr)
        report_error(NameOrErr.takeError(), Obj->getFileName());

    SymName = *(NameOrErr);
    LLVM_DEBUG(dbgs()<< "DEBUG:: SymName Attained is: " << SymName.data()<< "\n");
    auto AddressOrErr = Symbol.getAddress();
    if (!AddressOrErr)
        report_error(AddressOrErr.takeError(), Obj->getFileName());
    SymAddr = *(AddressOrErr);
    

    if(!std::strcmp(SymName.data(), "load_gp")){
        //LLVM_DEBUG(dbgs()<< "DEBUG:: Found load_gp\n");


        //SymAddr = *(Symbol.getAddress());
        auto SecIterOrErr = Symbol.getSection();
        //LLVM_DEBUG(dbgs()<< "DEBUG::  Symbol.getSection() completed\n");

        if (!SecIterOrErr)
          report_error(SecIterOrErr.takeError(), Obj->getFileName());

        auto SecIter  = *(SecIterOrErr);
        //LLVM_DEBUG(dbgs()<< "DEBUG:: Got section_iterator\n");

        int64_t SecStart = SecIter->getAddress();
        //LLVM_DEBUG(dbgs()<< "DEBUG:: Got Sec. base address\n");

        auto StrOrErr = SecIter->getContents();
        assert(StrOrErr && "Failed to get the content of section!");
        auto SecData = *StrOrErr;
        ArrayRef<uint8_t> Bytes(reinterpret_cast<const uint8_t *>(SecData.data()),
                              SecData.size());
        //LLVM_DEBUG(dbgs()<< "DEBUG:: Bytes Attained\n");

        MCInst InstAUIPC;
        uint64_t InstAUIPCSz;
        bool Success = MR->getMCDisassembler()->getInstruction(
          InstAUIPC, InstAUIPCSz , Bytes.slice(SymAddr - SecStart),
          SymAddr , nulls());
        //LLVM_DEBUG(dbgs()<< "DEBUG:: InstAUIPC Attained\n");
        assert(Success && "Failed to disassemble instruction in load_gp");
        //LLVM_DEBUG(dbgs()<<"InstAUIPC="<<InstAUIPC<<"\n");
        unsigned int OpcAUIPC = InstAUIPC.getOpcode();
        if (OpcAUIPC != RISCV::AUIPC ) {
          assert(false && "Failed to find function entry from load_gp.");
        }
        
        MCInst Inst;
        uint64_t InstSz;
        Success = MR->getMCDisassembler()->getInstruction(
          Inst, InstSz, Bytes.slice(SymAddr + 4 - SecStart), SymAddr + 4,
          nulls());
        assert(Success && "Failed to disassemble ADDI instruction in load_gp");
        //LLVM_DEBUG(dbgs()<< "DEBUG:: ADDI X3,X3,IMM Attained\n");

        unsigned int Opcode = Inst.getOpcode();
        MCInstrDesc MCID = MR->getMCInstrInfo()->get(Opcode);

      if (Opcode != RISCV::ADDI && (MCID.getNumOperands() != 3)) {
        assert(false && "Failed to find instruction of ADDI X3,X3,IMM");
      }

 

      DeriveGP(InstAUIPC, Inst, SymAddr);
        // if(MInst.getOperand(0).isReg() 
        //       && MInst.getOperand(0).getReg()  == RISCV::X3
        //       && NInst->getOperand(0).getReg() == RISCV::X3 
        //       && NInst->getOperand(1).getReg() == RISCV::X3 )
        //         DeriveGP(MInst,*NInst);


    }else
        continue;


  }
  LLVM_DEBUG(dbgs()<< "DEBUG:: Leaving FindLoadGP()\n");

}

void RISCV32MIRevising::DeriveGP(MCInst &MInst, MCInst &NInst, int64_t SymAddr){
  LLVM_DEBUG(dbgs() << "DEBUG::RISCV32MIRevising:: Entering DeriveGP()\n");
   const Value *GlobVal = nullptr;
   int64_t Imm = 0;
  // To match the pattern: 
  //    10328: 00002197            auipc gp,0x2
  //    1032c: 4d818193            addi  gp,gp,1240 # 12800 <__global_pointer$>
  //    10330: 00008067            ret
  // if (NInst.getNumOperands() > 2) {
  //   assert(NInst.getOperand(2).isImm() &&
  //          "The third operand must be immediate data!");
    Imm = MInst.getOperand(1).getImm();
  //}

  int64_t ImmGP = NInst.getOperand(2).getImm();

  // Get MCInst offset - the offset of machine instruction in the binary
  // and instruction size  
  //int64_t MCInstOffset = getMCInstIndex(MInst);

  const ELF32LEObjectFile *ObjFile =
      dyn_cast<ELF32LEObjectFile>(MR->getObjectFile());
  assert(ObjFile != nullptr &&
         "Only 32-bit ELF binaries supported at present.");

  // Get the text section address
  int64_t TextSecAddr = MR->getTextSectionAddress();
  assert(TextSecAddr >= 0 && "Failed to find text section address");
  // AUIPC 
  Imm = Imm<<12;
  //int64_t InstAddr = TextSecAddr + MCInstOffset; // PC
  int64_t InstAddr = SymAddr;
  int64_t Offset = InstAddr + Imm;
  int64_t BaseGP = Offset + ImmGP;
  LLVM_DEBUG(dbgs() << "DEBUG::RISCV32MIRevising:: PC = " << InstAddr       
                    << ", AUIPC IMM = "                   << Imm            
                    << ", BaseGP = "                      << BaseGP  << "\n");
  setGPBase(BaseGP);
  LLVM_DEBUG(dbgs() << "DEBUG::RISCV32MIRevising::  getGPBase() = "
                    << getGPBase() << "\n");
  LLVM_DEBUG(dbgs() << "DEBUG::RISCV32MIRevising:: Leaving DeriveGP()\n");
}





/// Find global value by PC offset.
const Value *RISCV32MIRevising::getGlobalValueByOffset( MachineInstr &MInst,
                                                        int64_t GPOffset,
                                                        int64_t flag) {
  const Value *GlobVal = nullptr;
  const ELF32LEObjectFile *ObjFile =
      dyn_cast<ELF32LEObjectFile>(MR->getObjectFile());
  assert(ObjFile != nullptr &&
         "Only 32-bit ELF binaries supported at present.");

  // Get the text section address
  int64_t TextSecAddr = MR->getTextSectionAddress();
  assert(TextSecAddr >= 0 && "Failed to find text section address");
  int64_t MCInstOffset = getMCInstIndex(MInst);
  int64_t InstAddr = TextSecAddr + MCInstOffset;

  int64_t Offset = GPOffset;

  if(flag)
    Offset = getGPBase() + GPOffset;

  LLVM_DEBUG(dbgs() << "DEBUG::getGPBase() = " << getGPBase() 
                    << ", GPOffset = " << GPOffset << "\n");

  // Start to search the corresponding symbol.
  const SymbolRef *Symbol = nullptr;
  const RelocationRef *DynReloc = MR->getDynRelocAtOffset(Offset);
  //if (DynReloc && (DynReloc->getType() == ELF::R_RISCV_GPREL_I ||
  //                 DynReloc->getType() == ELF::R_RISCV_GPREL_S)){
  if (DynReloc){
     LLVM_DEBUG(dbgs() << "DEBUG::getGlobalValueByOffset:: DynReloc->getType() = " 
                       << DynReloc->getType() << "\n");
     LLVM_DEBUG(dbgs() << "DEBUG::getGlobalValueByOffset::Symbol = &*DynReloc->getSymbol()\n");
          
    Symbol = &*DynReloc->getSymbol();
  }
  assert(MCIR != nullptr && "MCInstRaiser was not initialized!");
  if (Symbol == nullptr) {
    // auto Iter = MCIR->getMCInstAt(Offset - TextSecAddr);
    // uint64_t OffVal = static_cast<uint64_t>((*Iter).second.getData());
    LLVM_DEBUG(dbgs() << "DEBUG::getGlobalValueByOffset::Symbol is NULL\n");

    for (auto &Sym : ObjFile->symbols()) {
      // StringRef name = Sym.getName().get();
      // LLVM_DEBUG(dbgs() << "DEBUG::Symbol Name = " << name <<"\n");
      //LLVM_DEBUG(dbgs() << "DEBUG::getGlobalValueByOffset::Entered Loop\n");

      Expected<StringRef> SymName = Sym.getName();
      //LLVM_DEBUG(dbgs() << "DEBUG::getGlobalValueByOffset::Got SymName\n");

      assert(SymName &&
          "Failed to find symbol associated with dynamic relocation.");
      auto name = SymName.get();
      //LLVM_DEBUG(dbgs() << "DEBUG::getGlobalValueByOffset::Got Name\n");

      //LLVM_DEBUG(SymName);
      LLVM_DEBUG(dbgs() << "DEBUG::RISCV32MIRevising:: Found Global Symbol : "
                      << name << "\n");
      if (Sym.getELFType() == ELF::STT_OBJECT) {
        LLVM_DEBUG(dbgs() << "DEBUG::getGlobalValueByOffset::ELF is STT_OBJECT\n");

        auto SymAddr = Sym.getAddress();
        assert(SymAddr && "Failed to lookup symbol for global address!");

        //if (OffVal >= SymAddr.get() &&
        //    OffVal < (SymAddr.get() + Sym.getSize())) {
        LLVM_DEBUG(dbgs() << "DEBUG:: Sybmol Matched! Global Symbol Address = : "
                      << SymAddr.get() 
                      << ", Offset = " << Offset 
                      << "\n");
        if (Offset == SymAddr.get() )
          Symbol = &Sym;
        if (Offset >= SymAddr.get() &&
            Offset < (SymAddr.get() + Sym.getSize())) {
          Symbol = &Sym;
          break;
        }
      }
    }
  }

  LLVMContext &LCTX = M->getContext();
  if (Symbol != nullptr) {
    LLVM_DEBUG(dbgs() << "DEBUG::RISCV32MIRevising::getGlobalValueByOffset::Symbol is Valid\n");

    // If the symbol is found.
    Expected<StringRef> SymNameVal = Symbol->getName();
    assert(SymNameVal &&
           "Failed to find symbol associated with dynamic relocation.");
    auto SymName = SymNameVal.get();
    LLVM_DEBUG(SymName);
    LLVM_DEBUG(dbgs() << "DEBUG::RISCV32MIRevising:: Found Global Symbol : "
                      << SymName << "\n");

    GlobVal = M->getGlobalVariable(SymName);
    if (GlobVal == nullptr) {
      //TODO: FIGURE OUT WHAT IS DataRefImpl 
      DataRefImpl SymImpl = Symbol->getRawDataRefImpl();
      auto SymbOrErr = ObjFile->getSymbol(SymImpl);
      if (!SymbOrErr)
        consumeError(SymbOrErr.takeError());
      else {
        auto Symb = SymbOrErr.get();
        assert((Symb->getType() == ELF::STT_OBJECT) &&
               "Object symbol type is expected. But not found!");
        GlobalValue::LinkageTypes Linkage;
        switch (Symb->getBinding()) {
        case ELF::STB_GLOBAL: // global symbol
          Linkage = GlobalValue::ExternalLinkage;
          break;
        default:
          assert(false && "Unhandled dynamic symbol");
        }
        uint64_t SymSz = Symb->st_size;
        Type *GlobValTy = nullptr;
        switch (SymSz) {
        case 4:
          GlobValTy = Type::getInt32Ty(LCTX);
          break;
        case 2:
          GlobValTy = Type::getInt16Ty(LCTX);
          break;
        case 1:
          GlobValTy = Type::getInt8Ty(LCTX);
          break;
        default:
          GlobValTy = ArrayType::get(Type::getInt8Ty(LCTX), SymSz);
          break;
        }

        auto SymOrErr = Symbol->getValue();
        if (!SymOrErr)
          report_error(SymOrErr.takeError(), "Can not find the symbol!");

        uint64_t SymVirtAddr = *SymOrErr;
        auto SecOrErr = Symbol->getSection();
        if (!SecOrErr)
          report_error(SecOrErr.takeError(),
                       "Can not find the section which is the symbol in!");

        section_iterator SecIter = *SecOrErr;
        Constant *GlobInit = nullptr;
        if (SecIter->isBSS()) {
          Linkage = GlobalValue::CommonLinkage;
          if (ArrayType::classof(GlobValTy))
            GlobInit = ConstantAggregateZero::get(GlobValTy);
          else
            GlobInit = ConstantInt::get(GlobValTy, 0);
        } else {
          auto StrOrErr = SecIter->getContents();
          if (!StrOrErr)
            report_error(StrOrErr.takeError(),
                         "Failed to get the content of section!");
          StringRef SecData = *StrOrErr;
          // Currently, Symbol->getValue() is virtual address.
          unsigned Index = SymVirtAddr - SecIter->getAddress();
          const unsigned char *Beg = SecData.bytes_begin() + Index;
          char Shift = 0;
          uint64_t InitVal = 0;
          while (SymSz-- > 0) {
            // We know this is little-endian
            InitVal = ((*Beg++) << Shift) | InitVal;
            Shift += 8;
          }
          GlobInit = ConstantInt::get(GlobValTy, InitVal);
        }

        auto GlobVar = new GlobalVariable(*M, GlobValTy, false /* isConstant */,
                                          Linkage, GlobInit, SymName);
        uint64_t Align = 32;
        switch (SymSz) {
        default:
        case 4:
          // When the symbol size is bigger than 4 bytes, identify the object as
          // array or struct and set alignment to 32 bits.
          Align = 32;
          break;
        case 2:
          Align = 16;
          break;
        case 1:
          Align = 8;
          break;
        }
        MaybeAlign MA(Align);
        GlobVar->setAlignment(MA);
        GlobVar->setDSOLocal(true);
        GlobVal = GlobVar;
      }
    }
  } 
  else {
    // If can not find the corresponding symbol.
    LLVM_DEBUG(dbgs() << "DEBUG::RISCV32MIRevising::getGlobalValueByOffset:: Entering RODATA SECTION\n");
    
    GlobVal = MR->getRODataValueAt(Offset);
    
    if (GlobVal == nullptr) {
      LLVM_DEBUG(dbgs() << "DEBUG::getGlobalValueByOffset::GlobVal is NULL\n");

      uint64_t Index = Offset - TextSecAddr;
      //if (MCIR->getMCInstAt(Index) != MCIR->const_mcinstr_end()) {
        std::string LocalName("ROConst");
        //LocalName.append(std::to_string(Index));
        LocalName.append(std::to_string(Offset));
        // Find if a global value associated with symbol name is already
        // created
        StringRef LocalNameRef(LocalName);
        GlobVal = M->getGlobalVariable(LocalNameRef);
        if (GlobVal == nullptr) {
          LLVM_DEBUG(dbgs() << "DEBUG::getGlobalVariable::GlobVal is still NULL\n");

          // MCInstOrData MD = MCIR->getMCInstAt(Index)->second;
          // uint32_t Data = MD.getData();
          // uint64_t DataAddr = (uint64_t)Data;
          uint64_t DataAddr = Offset;
          // Check if this is an address in .rodata
          for (section_iterator SecIter : ObjFile->sections()) {
            uint64_t SecStart = SecIter->getAddress();
            uint64_t SecEnd = SecStart + SecIter->getSize();

            if ((SecStart <= DataAddr) && (SecEnd >= DataAddr)) {
              if (SecIter->isData()) {
                auto StrOrErr = SecIter->getContents();
                assert(StrOrErr && "Failed to get the content of section!");
                StringRef SecData = *StrOrErr;
                uint64_t DataOffset = DataAddr - SecStart;
                // ??????????? .rodata begins with such data? It is not an independent section?????
                const unsigned char *RODataBegin =
                    SecData.bytes_begin() + DataOffset;

                unsigned char c;
                uint64_t argNum = 0;
                const unsigned char *str = RODataBegin;
                do {
                  c = (unsigned char)*str++;
                  if (c == '%') {
                    argNum++;
                  }
                } while (c != '\0');
                if (argNum != 0) {
                 MR->collectRodataInstAddr(InstAddr);
                 MR->fillInstArgMap(InstAddr, argNum + 1);
                }
                StringRef ROStringRef(
                    reinterpret_cast<const char *>(RODataBegin));
                Constant *StrConstant =
                    ConstantDataArray::getString(LCTX, ROStringRef);
                auto GlobalStrConstVal = new GlobalVariable(
                    *M, StrConstant->getType(), /* isConstant */ true,
                    GlobalValue::PrivateLinkage, StrConstant, "RO-String");
                // Record the mapping between offset and global value
                MR->addRODataValueAt(GlobalStrConstVal, Offset);
                GlobVal = GlobalStrConstVal;
                break;
              }
            }
          }

          if (GlobVal == nullptr) {
            Type *ty = Type::getInt32Ty(LCTX);
            Constant *GlobInit = ConstantInt::get(ty, DataAddr);
            auto GlobVar = new GlobalVariable(*M, ty, /* isConstant */ true,
                                              GlobalValue::PrivateLinkage,
                                              GlobInit, LocalNameRef);
            MaybeAlign MA(32);
            GlobVar->setAlignment(MA);
            GlobVar->setDSOLocal(true);
            GlobVal = GlobVar;
          }
        }
      //}
    }
  }

  return GlobVal;
}  

void RISCV32MIRevising::setGPBase(int64_t GP){
   _GPbase = GP;
}

int64_t RISCV32MIRevising::getGPBase(){
  return _GPbase;
}




void RISCV32MIRevising::addressPCRelativeConst(MachineInstr &MInst, MachineInstr &NInst) {
  LLVM_DEBUG(dbgs() << "DEBUG::Entering addressPCRelativeConst()\n");

  const Value *GlobVal = nullptr;
  int64_t Imm = 0;
  LLVM_DEBUG(dbgs() << "DEBUG:: MInst.Opcode = "
                    << MInst.getOpcode()
                    << ", Number of Operands = "
                    << MInst.getNumOperands()
                    << "\n");

  // To match the pattern: OPCODE Rx, [PC, #IMM]
  if (MInst.getNumOperands() > 2) {
     assert(MInst.getOperand(1).isImm() &&
            "The third operand must be immediate data!");
     Imm = MInst.getOperand(1).getImm();
   }
  // Get MCInst offset - the offset of machine instruction in the binary
  // and instruction size
  Imm = MInst.getOperand(1).getImm();
  //}

  int64_t ImmConst = NInst.getOperand(2).getImm();
  // LUI 
  Imm = Imm<<12;
  //RODATA Offset
  Imm+= ImmConst;

  LLVM_DEBUG(dbgs() << "DEBUG:: IMM Offset = "<< Imm << "\n");

  // int64_t MCInstOffset = getMCInstIndex(MInst);
  //GlobVal = getGlobalValueByOffset(MInst, MCInstOffset, static_cast<uint64_t>(Imm) + 8);
  GlobVal = getGlobalValueByOffset(NInst, static_cast<int64_t>(Imm),0);
  LLVM_DEBUG(dbgs() << "GlobVal returned by getGlobalValueByOffset = " <<GlobVal << "\n");

  assert(GlobVal && "A not addressed pc-relative data!");

  //Create NewInst with lw a0, 0(a0) to replace NInst or placed after NInst.
  // MInst.getIns
//   if(LWInst == NULL)
//       LLVM_DEBUG(dbgs() << "DEBUG:: LWInst = NULL!!!! \n");

  MCInstrDesc MCID = MR->getMCInstrInfo()->get(RISCV::LW);
  //const MCInstrDesc NDesc = LWInst->getDesc();
  MCInstrDesc NDesc = LastLW->getDesc(); 
  LLVM_DEBUG(dbgs() << "DEBUG:: Complete LWInst->getDesc() \n");

  if(NDesc.Opcode == RISCV::LW)
      LLVM_DEBUG(dbgs() << "DEBUG:: NDesc.Opcode = RISCV::LW \n");
  // else
  // NDesc.Opcode=RISCV::LW;
  // NDesc.Flags=262144; //same as RISCV LW
  const MCInstrDesc Desc = MCID;
  DebugLoc *debugLoc = new DebugLoc();
  LLVM_DEBUG(dbgs() << "DEBUG:: Complete new DebugLoc() \n");

 
 
  //MachineInstr *NewInst= NInst.getMF()->CreateMachineInstr(Desc, *debugLoc);
  MachineInstr *NewInst= LastLW;

  if(!NewInst)
      LLVM_DEBUG(dbgs() << "DEBUG:: CreateMachineInstr() failed \n");
  else
      LLVM_DEBUG(dbgs() << "DEBUG:: Complete CreateMachineInstr() \n");
  // auto NextNode = MInst.getNextNode();


  // auto iter = NInst.getIterator();
  // if(iter->getOpcode() == NInst.getOpcode()){
  //   std::cout << "DEBUG:: iter->getOpcode() == NInst.getOpcode()\n";
  //     // LLVM_DEBUG( dbgs() << "DEBUG:: addressPCRelativeConst:: Iterator attained is correct\n" );
  // }

  // MInst.getParent()->insert(iter, NewInst);
  // if(  NewInst->getNextNode() == &MInst)
  //     LLVM_DEBUG(dbgs() << "DEBUG:: NextNode of NewInst is valid \n");

  LLVM_DEBUG(dbgs() << "DEBUG:: NewInst # of Operand = " << NewInst->getNumOperands() << "\n");

  if(NewInst->getOpcode() == RISCV::LW)
      LLVM_DEBUG(dbgs() << "DEBUG:: NewInst opcode = LW \n");


  NewInst->RemoveOperand(3);
  NewInst->RemoveOperand(2);
  NewInst->RemoveOperand(1);
  NewInst->RemoveOperand(0);




  NewInst->addOperand(NInst.getOperand(0));
  NewInst->addOperand(NInst.getOperand(1));
  NewInst->addOperand(LWInst->getOperand(2));
  LLVM_DEBUG(dbgs() << "DEBUG:: NewInst # of Operand after addOperand() = " << NewInst->getNumOperands() << "\n");


  NewInst->getOperand(1).ChangeToES(GlobVal->getName().data());
  NewInst->getOperand(2).ChangeToImmediate(0);

  if (NewInst->getNumOperands() > 2) {
    NewInst->RemoveOperand(2);
  }
  
  

  // vector<MachineInstr *> RMVec;
  // RMVec.push_back(&MInst);
  // RMVec.push_back(&NInst);
  // for (MachineInstr *PMI : RMVec)
  //   PMI->eraseFromParent();


  // // 
  // MInst.removeFromParent();
  // NInst.removeFromParent();

  //  NInst.eraseFromParentAndMarkDBGValuesForRemoval();
  //  MInst.eraseFromParentAndMarkDBGValuesForRemoval();

  //  NInst.eraseFromParentAndMarkDBGValuesForRemoval();
  //  MInst.eraseFromParentAndMarkDBGValuesForRemoval();

  LLVM_DEBUG(dbgs() << "DEBUG:: RISCV::LW = " << RISCV::LW << "\n");

  LLVM_DEBUG(dbgs() << "DEBUG:: NewInst MCInstrDesc Size = " << NewInst->getDesc().getSize() << "\n");

  LLVM_DEBUG(dbgs() << "DEBUG:: NewInst Opcode = " << NewInst->getOpcode() << "\n");

  LLVM_DEBUG(dbgs() << "DEBUG:: NewInst Flags = " << NewInst->getDesc().getFlags() << "\n");




}


/// Address PC relative data in function, and create corresponding global value.
void RISCV32MIRevising::addressPCRelativeData(MachineInstr &MInst) {
  LLVM_DEBUG(dbgs() << "DEBUG::Entering addressPCRelativeData()\n");

  const Value *GlobVal = nullptr;
  int64_t Imm = 0;
  // To match the pattern: OPCODE Rx, [PC, #IMM]
  if (MInst.getNumOperands() > 2) {
    assert(MInst.getOperand(2).isImm() &&
           "The third operand must be immediate data!");
    Imm = MInst.getOperand(2).getImm();
  }
  // Get MCInst offset - the offset of machine instruction in the binary
  // and instruction size
  int64_t MCInstOffset = getMCInstIndex(MInst);
  //GlobVal = getGlobalValueByOffset(MInst, MCInstOffset, static_cast<uint64_t>(Imm) + 8);
  GlobVal = getGlobalValueByOffset(MInst, static_cast<int64_t>(Imm),1);
  LLVM_DEBUG(dbgs() << "GlobVal returned by getGlobalValueByOffset = " <<GlobVal << "\n");
  // Check the next instruction whether it is also related to PC relative data
  // of global variable.
  // It should like:
  // ldr     r1, [pc, #32]
  // ldr     r1, [pc, r1]
  //
  // This is not the only case!!! -- XI 
  // 153 00010454 <__libc_csu_init>:
  // 154    10454: e92d43f8  push  {r3, r4, r5, r6, r7, r8, r9, lr}
  // 155    10458: e1a07000  mov r7, r0
  // 156    1045c: e59f6048  ldr r6, [pc, #72] ; 104ac <__libc_csu_init+0x58>
  // 157    10460: e59f5048  ldr r5, [pc, #72] ; 104b0 <__libc_csu_init+0x5c>
  // 158    10464: e08f6006  add r6, pc, r6
  // 159    10468: e08f5005  add r5, pc, r5

  // MachineInstr *NInst = MInst.getNextNode();
  // To match the pattern: OPCODE Rx, [PC, Rd], Rd must be the def of previous
  // instruction.
  // if (NInst->getNumOperands() >= 2 && NInst->getOperand(1).isReg() &&
  //     NInst->getOperand(1).getReg() == RISCV::PC &&
  //     NInst->getOperand(2).isReg() &&
  //     NInst->getOperand(2).getReg() == MInst.getOperand(0).getReg()) {
  //   auto GV = dyn_cast<GlobalVariable>(GlobVal);
  //   if (GV != nullptr && GV->isConstant()) {
  //     // Firstly, read the PC relative data according to PC offset.
  //     auto Init = GV->getInitializer();
  //     uint64_t GVData = Init->getUniqueInteger().getZExtValue();
  //     int64_t MCInstOff = getMCInstIndex(*NInst);
  //     // Search the global symbol of object by PC relative data.
  //     GlobVal = getGlobalValueByOffset(MCInstOff, GVData + 8);
  //     // If the global symbol is exist, erase current ldr instruction.
  //     if (GlobVal != nullptr)
  //       NInst->eraseFromParent();
  //   }
  // }

  assert(GlobVal && "A not addressed pc-relative data!");

  // Replace PC relative operands to external symbol (ES) operand.
  // The pattern will be generated.
  // ldr r3, [pc, #20] => ldr r3, @globalvalue
  LLVM_DEBUG(dbgs() << "DEBUG::addressPCRelativeData:: starting ChangeToES() \n");

  // if(MInst.getOpcode() == RISCV::ADDI){
  //     MInst.getOperand(1).ChangeToES(GlobVal->getName().data());
  //     MInst.getOperand(2).ChangeToImmediate(0, MInst.getOperand(2).getTargetFlags());
  // }
  // else{ // load & store
      MInst.getOperand(1).ChangeToES(GlobVal->getName().data());

      if (MInst.getNumOperands() > 2) {
          MInst.RemoveOperand(2);
      }
  // }
  LLVM_DEBUG(dbgs() << "DEBUG:: MInst Flags = " << MInst.getDesc().getFlags() << "\n");
  LLVM_DEBUG(dbgs() << "DEBUG:: MInst MCInstrDesc Size = " << MInst.getDesc().getSize() << "\n");

  LLVM_DEBUG(dbgs() << "DEBUG:: MInst Opcode = " << MInst.getOpcode() << "\n");


}


// /// Decode modified immediate constants in some instructions with immediate
// /// operand.
// void RISCV32MIRevising::decodeModImmOperand(MachineInstr &MInst) {
//   switch (MInst.getOpcode()) {
//   default:
//     break;
//   case RISCV32::ORRri:
//     MachineOperand &mo = MInst.getOperand(2);
//     unsigned Bits = mo.getImm() & 0xFF;
//     unsigned Rot = (mo.getImm() & 0xF00) >> 7;
//     int64_t Rotated = static_cast<int64_t>(RISCV32_AM::rotr32(Bits, Rot));
//     mo.setImm(Rotated);
//     break;
//   }
// }

/// Remove some useless operations of instructions. Some instructions like
/// NOP (mov r0, r0).
bool RISCV32MIRevising::removeNeedlessInst(MachineInstr *MInst) {
  if (MInst->getOpcode() == RISCV::ADDI && MInst->getNumOperands() >= 2 &&
      MInst->getOperand(0).isReg() && MInst->getOperand(1).isImm() &&
      MInst->getOperand(0).getImm() == 0) {
    return true;
  }

  return false;
}

int RISCV32MIRevising::reviseMIforConst(MachineInstr &MInst) {

      LLVM_DEBUG(dbgs() << "DEBUG::Entering reviseMIforConst() \n");

      if(MInst.getOpcode() == RISCV::LW)
         LastLW = &MInst;
      if (MInst.getNumOperands() >= 2 
        && MInst.getOpcode() == RISCV::LUI) {
           MachineInstr *NInst = MInst.getNextNode();
           if(NInst->getOpcode() == RISCV::ADDI && MInst.getOperand(0).isReg() 
              && MInst.getOperand(0).getReg() == NInst->getOperand(1).getReg()
              && NInst->getOperand(0).getReg() >= RISCV::X10 && NInst->getOperand(0).getReg() <= RISCV::X17 )
      
        addressPCRelativeConst(MInst, *NInst);
        return 1;
     }
     
      LLVM_DEBUG(dbgs() << "DEBUG::Leaving reviseMIforConst() \n");

    return 0;
}


/// The entry function of this class.
bool RISCV32MIRevising::reviseMI(MachineInstr &MInst) {
  // no orr in riscv
  // decodeModImmOperand(MInst);
  // Relocate BL target in same section.

  
  LLVM_DEBUG(dbgs() << "DEBUG::Entering reviseMI() \n");
  // LLVM_DEBUG(dbgs() << MInst.getOpcode() << "\n");
  // LLVM_DEBUG(dbgs() << RISCV::JAL << "\n");

  // To do : add other branch target function: jalr, beq, bne, bge, blt, bltu, begu
  // To do : fix plt function

  // jal rd(operand0), target(operand1)
  // j target -> jal x0(operand0), target(operand1)
  if (MInst.getOpcode() == RISCV::JAL) {
    MachineOperand &mo1 = MInst.getOperand(1);
    if (mo1.isImm()){
      LLVM_DEBUG(dbgs() << MInst << "\n");
      relocateBranch(MInst);
    }
  }

  // if (MInst.getOpcode() == RISCV32::LDRi12 || MInst.getOpcode() == RISCV32:: ) {
  //   if (MInst.getNumOperands() >= 2 && MInst.getOperand(1).isReg() &&
  //       MInst.getOperand(1).getReg() == ARM::PC) {
  //     addressPCRelativeData(MInst);
  //   }
  // }

  // Match the pattern: -- XI
  // 00010328 <load_gp>:
  //    10328: 00002197            auipc gp,0x2
  //    1032c: 4d818193            addi  gp,gp,1240 # 12800 <__global_pointer$>
  //    10330: 00008067            ret
  // int64_t GPBase = 0;
  // if (MInst.getOpcode() == RISCV::AUIPC ) {
  //     MachineInstr *NInst = MInst.getNextNode();
  //     if(NInst->getOpcode() == RISCV::ADDI && MInst.getOperand(0).isReg() 
  //       && MInst.getOperand(0).getReg() == RISCV::X3
  //       && NInst->getOperand(0).getReg() == RISCV::X3 && NInst->getOperand(1).getReg() == RISCV::X3 )
  //     DeriveGP(MInst,*NInst);
  // }
    // RISC-V GP can be used by any kinds of load, store, floating load/store, addi, etc.
    // Thus, we only check whether GP is invoked. 
    // However, it will convert static var to the global ones
    // if (MInst.getOpcode() == RISCV32::LDRi12 || MInst.getOpcode() == RISCV32:: ) {
  //else {

 //Constant Pattern: 
 //278    103f8: 000107b7            lui a5,0x10
 //279    103fc: 4b078513            addi  a0,a5,1200 # 104b0 <a>





    if (MInst.getNumOperands() > 2 
         && MInst.getOperand(1).isReg()
         && MInst.getOperand(1).getReg() == RISCV::X3 
         && MInst.getOperand(0).isReg() 
         && MInst.getOperand(0).getReg() != RISCV::X3
         && MInst.getOperand(2).isImm()) {
      //COMMENTED TIL THE BUG FIXED - XI
        if(!LWInst)
            LWInst = &MInst;
        addressPCRelativeData(MInst);

    }
  //}

  return true;
}

void RISCV32MIRevising::setGPFlag(){
  _GP_flag = 1;
}

bool RISCV32MIRevising::GP_Derived(){
  if(_GP_flag)
    return true;
  else
    return false;
}


bool RISCV32MIRevising::revise() {
  bool rtn = false;
  LLVM_DEBUG(dbgs() << "RISCV32MIRevising start.\n");

  vector<MachineInstr *> RMVec;
     
  // Compute GP and remove load_gp
  FindLoadGP();
  // if(GP_Derived()){
  // for (MachineFunction::iterator mbbi = MF->begin(), mbbe = MF->end();
  //      mbbi != mbbe; ++mbbi) {
  //   for (MachineBasicBlock::iterator mii = mbbi->begin(), mie = mbbi->end();
  //        mii != mie; ++mii) {
  //         MachineInstr &MInst   = *mii;
  //         int64_t      GPBase   = 0;
  //         if (MInst.getOpcode() == RISCV::AUIPC ) {
            
  //           MachineInstr *NInst = MInst.getNextNode();
  //           if(NInst->getOpcode() == RISCV::ADDI && MInst.getOperand(0).isReg() 
  //             && MInst.getOperand(0).getReg()  == RISCV::X3
  //             && NInst->getOperand(0).getReg() == RISCV::X3 
  //             && NInst->getOperand(1).getReg() == RISCV::X3 )
  //               DeriveGP(MInst,*NInst);
  //               //MF->erase(mbbi);
  //                RMVec.push_back(&*mii);
  //                RMVec.push_back(&*(++mii));
  //                RMVec.push_back(&*(++mii));
  //               setGPFlag();


  //               break;
  //         }
         
  //     // if (removeNeedlessInst(&*mii)) {
  //     //   RMVec.push_back(&*mii);
  //     //   rtn = true;
  //     // } else
  //     //   rtn = reviseMI(*mii);
  //   }
  // }
  // }


  for (MachineFunction::iterator mbbi = MF->begin(), mbbe = MF->end();
       mbbi != mbbe; ++mbbi) {
    for (MachineBasicBlock::iterator mii = mbbi->begin(), mie = mbbi->end();
         mii != mie; ++mii) {
      if (removeNeedlessInst(&*mii)) {
        RMVec.push_back(&*mii);
        rtn = true;
      } else
        rtn = reviseMI(*mii);
    }
  }




  for (MachineFunction::iterator mbbi = MF->begin(), mbbe = MF->end();
       mbbi != mbbe; ++mbbi) {
    for (MachineBasicBlock::iterator mii = mbbi->begin(), mie = mbbi->end();
         mii != mie; ++mii) {
        if(reviseMIforConst(*mii)){
             RMVec.push_back(&*mii);
             mii++;
             RMVec.push_back(&*mii);
             auto iter = LastLW->getIterator();
             iter++;
             RMVec.push_back(&*iter);
        }
    }
  }


  for (MachineInstr *PMI : RMVec)
    PMI->eraseFromParent();



  // For debugging.
  // LLVM_DEBUG(MF->dump());
  // LLVM_DEBUG(getCRF()->dump());
  LLVM_DEBUG(dbgs() << "RISCV32MIRevising end.\n");

  return rtn;
}

bool RISCV32MIRevising::runOnMachineFunction(MachineFunction &mf) {
  bool rtn = false;
  init();
  rtn = revise();
  return rtn;
}

#undef DEBUG_TYPE

#ifdef __cplusplus
extern "C" {
#endif

FunctionPass *InitializeRISCV32MIRevising(RISCV32ModuleRaiser &mr) {
  return new RISCV32MIRevising(mr);
}

#ifdef __cplusplus
}
#endif
