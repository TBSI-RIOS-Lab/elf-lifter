//===-- llvm-mctoll.cpp -----------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This program is a utility that converts a binary to LLVM IR (.ll file)
//
//===----------------------------------------------------------------------===//

#include "llvm-mctoll.h"
#include "EmitRaisedOutputPass.h"
#include "ExternalFunctions.h"
#include "MCInstOrData.h"
#include "MachineFunctionRaiser.h"
#include "ModuleRaiser.h"
#include "AssemblyInstruction.h"
#include "AssemblyBasicBlock.h"
#include "AssemblyCFG.h"
#include "AssemblyFunction.h"

#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/Bitcode/BitcodeWriterPass.h"
#include "llvm/CodeGen/FaultMaps.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/DebugInfo/DWARF/DWARFContext.h"
#include "llvm/DebugInfo/Symbolize/Symbolize.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCDisassembler/MCDisassembler.h"
#include "llvm/MC/MCDisassembler/MCRelocationInfo.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/MC/MCInstrAnalysis.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCTargetOptions.h"
#include "llvm/Object/Archive.h"
#include "llvm/Object/COFF.h"
#include "llvm/Object/COFFImportFile.h"
#include "llvm/Object/ELFObjectFile.h"
#include "llvm/Object/MachO.h"
#include "llvm/Object/ObjectFile.h"
#include "llvm/Object/Wasm.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Errc.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include <algorithm>
#include <cctype>
#include <cstring>
#include <fstream>
#include <set>
#include <system_error>
#include <unordered_map>
#include <utility>
#include <vector>
#include <string>
#include <iostream>
#include <iomanip>

#include "macro.h"
#include <sstream>
#include <stdexcept>

#include "ASMUtils.h"


using namespace llvm;
using namespace object;
using namespace std;

const set<int> RV_R_TYPE ({0b0110011,0b0111011,0b0101111,0b1010011});
const set<int> RV_I_TYPE ({0b0010011,0b0000011, 0b1100111,0b0011011,0b0000111});
const set<int> RV_S_TYPE ({0b0100011,0b0100111});
const set<int> RV_B_TYPE ({0b1100011});
const set<int> RV_U_TYPE ({0b0110111, 0b0010111});
const set<int> RV_J_TYPE ({0b1101111});
const set<int> RV_R4_TYPE   ({0b1000011,0b1000111,0b1001011,0b1001111});
// Need to be fixed at some point for CSRs
const set<int> RV_UNKNOWN  ({0b0001111, 0b1110011});  // ECALL, EBREAK, CSR, fence
set<string> excludeFunctionSet ({"register_fini", "exit", "__libc_init_array", "memset", "__call_exitprocs", "__libc_fini_array", "atexit", "__register_exitproc", "_exit", "__errno", "_vfprintf_r", "vfprintf", "printf", "_printf_r", "_ldtoa_r", "_ldcheck"});

// Compressed encoding should be analyzed as well.

std::string ELFPATH;


// Data segment addr
uint64_t DATA;
uint64_t SDATA;
uint64_t RODATA;
uint64_t SRODATA;
uint64_t BSS;
uint64_t SBSS;

// Data segment size
int64_t DATA_ADDR_SIZE;
int64_t SDATA_SIZE;
int64_t RODATA_SIZE;
int64_t SRODATA_SIZE;
int64_t BSS_SIZE;
int64_t SBSS_SIZE;



// <ADDR, SIZE>
std::vector<GlobalData> GLOBAL_DATA;
//std::vector<std::pair<string, int64_t>> GLOBAL_SECTION;



// Data Section info 
std::vector<GlobalData> DATA_SECTION;


// Function Symbol info in ELF
std::map<string, AssemblyFunction*> textSectionFunctions;



std::vector<std::pair<string, uint64_t>> PLTFunctions;


// GP init value for RISC-V
int64_t  GP_BASE = 0;



std::string MatchPLTFunction(uint64_t addr){

    for(auto it:PLTFunctions){
      if(it.second == addr)
        return it.first;
    }
    
    return "NULL";
}



GlobalData* MatchGlobalData(uint64_t addr){
  for(auto i = GLOBAL_DATA.begin(); i!=GLOBAL_DATA.end();i++){
    // if(i->addr == addr)
    //   return &(*i);
    if(i->addr <= addr && i->addr + i->size > addr)
        return &(*i);
  }
  return NULL;
}



GlobalData* MatchGlobalSection(uint64_t addr){
  for(auto i = DATA_SECTION.begin(); i!=DATA_SECTION.end();i++){
    if(addr >= i->addr && addr <= i->addr + i->size)
      return &(*i);
  }
  return NULL;
}



void DataSectionDump(){
  
  cout << "===================================================\n";  
  for (auto i = DATA_SECTION.begin();i!=DATA_SECTION.end();i++){
      cout << "\n\t Data Section: "<<(*i).name;
      cout << "\n\t Addr = 0x" << std::hex << (*i).addr;
      cout << "\n\t Size = " << std::dec <<(*i).size << endl;
      cout << "\n";
  }

}


void GlobalDataDump(){
  cout << "===================================================\n"; 

  for (auto i = GLOBAL_DATA.begin();i!=GLOBAL_DATA.end();i++){
      cout << "\n\t Global Data: "<<(*i).name;
      cout << "\n\t Addr = 0x" << std::hex << (*i).addr;
      cout << "\n\t Size = " << std::dec << (*i).size << endl;
      cout << "\n";
  }

}


std::string exec(string cmd) {
    char buffer[4096];
    std::string result = ""; 
  const char * c = cmd.c_str();
    FILE* pipe = popen(c, "r");
    if (!pipe) throw std::runtime_error("popen() failed!");
    while (fgets(buffer, sizeof buffer, pipe) != NULL) {
        result += buffer;
    }   
  
    pclose(pipe);
    return result;
}

int GetDataSymbolSize(uint64_t addr){
    cout << endl;
    int           size      = 0; 
    int           index     = 0; 
    stringstream  ss;
    string        s;

    ss << std::hex << addr;

    // Currently Set for RISC-V, need to be portable to X86 & ARM
    std::string str = "riscv64-unknown-linux-gnu-nm -B -l -r --size-sort --print-size ";
    str += ELFPATH;
    str += " | grep \"";
    str += ss.str();
    str += "\"";
    //cout << "DEBUG:: Retriving Symbol Command: " << str <<endl;

    str = exec(str);
    //cout << "\tDEBUG:: Retriving Symbol size: " << str <<endl;

    if(str.empty()){
      cout << "\tWARNING: Data Symbol Empty!! \n";  
      return 0;
    }

    index  = (str.find(" ", index)) + 1;
    s = str.substr(index, 16);
    //cout << str <<endl;
    //cout << s <<endl;
    cout << "\tSymbol size = " << stoi(s,nullptr,16)<<endl;
    return stoi(s,nullptr,16);


}


void RecordGlobalData(string name,uint64_t addr){

    int size =  GetDataSymbolSize(addr);
    // Skip empty symbol like dso_handle
    if(size == 0)
      return;

    GlobalData data;

    data.addr = addr;
    data.name = name;
    data.size = size;
    GLOBAL_DATA.push_back(data);



}


void RecordGlobalSection(string name,uint64_t addr, int size){

    GlobalData section;
    section.addr = addr;
    section.name = name;
    section.size = size;
    DATA_SECTION.push_back(section);



}












bool RISCV_ISA = false;



static cl::OptionCategory LLVMMCToLLCategory("llvm-mctoll options");

static cl::list<std::string> InputFilenames(cl::Positional,
                                            cl::desc("<input object files>"),
                                            cl::ZeroOrMore);
static cl::opt<std::string> OutputFilename("o", cl::desc("Output filename"),
                                           cl::value_desc("filename"),
                                           cl::cat(LLVMMCToLLCategory),
                                           cl::NotHidden);

cl::opt<std::string>
    MCPU("mcpu",
         cl::desc("Target a specific cpu type (-mcpu=help for details)"),
         cl::value_desc("cpu-name"), cl::init(""));

cl::list<std::string>
    MAttrs("mattr", cl::CommaSeparated,
           cl::desc("Target specific attributes (-mattr=help for details)"),
           cl::value_desc("a1,+a2,-a3,..."));

// Output file type. Default is binary bitcode.
cl::opt<CodeGenFileType> OutputFormat(
    "output-format", cl::init(CGFT_AssemblyFile),
    cl::desc("Output format (default: binary bitcode):"),
    cl::values(clEnumValN(CGFT_AssemblyFile, "ll",
                          "Emit llvm text bitcode ('.ll') file"),
               clEnumValN(CGFT_ObjectFile, "bc",
                          "Emit llvm binary bitcode ('.bc') file"),
               clEnumValN(CGFT_Null, "null",
                          "Emit nothing, for performance testing")),
    cl::cat(LLVMMCToLLCategory), cl::NotHidden);

cl::opt<bool> llvm::Disassemble("raise", cl::desc("Raise machine instruction"),
                                cl::cat(LLVMMCToLLCategory), cl::NotHidden);

cl::alias Disassembled("d", cl::desc("Alias for -raise"),
                       cl::aliasopt(Disassemble), cl::cat(LLVMMCToLLCategory),
                       cl::NotHidden);

static cl::opt<bool>
    MachOOpt("macho", cl::desc("Use MachO specific object file parser"));
static cl::alias MachOm("m", cl::desc("Alias for --macho"),
                        cl::aliasopt(MachOOpt));

static cl::opt<bool> NoVerify("disable-verify", cl::Hidden,
                              cl::desc("Do not verify input module"));

cl::opt<std::string>
    llvm::TripleName("triple", cl::desc("Target triple to disassemble for, "
                                        "see -version for available targets"));

cl::opt<std::string>
    llvm::ArchName("arch-name", cl::desc("Target arch to disassemble for, "
                                         "see -version for available targets"));

cl::opt<std::string> llvm::FilterFunctionSet(
    "filter-functions-file",
    cl::desc("Specify which functions to raise via a configuration file."),
    cl::cat(LLVMMCToLLCategory), cl::NotHidden);

cl::alias static FilterFunctionSetF(
    "f", cl::desc("Alias for --filter-functions-file"),
    cl::aliasopt(llvm::FilterFunctionSet), cl::cat(LLVMMCToLLCategory),
    cl::NotHidden);

cl::list<std::string>
    llvm::FilterSections("section",
                         cl::desc("Operate on the specified sections only. "
                                  "With -macho dump segment,section"));

cl::alias static FilterSectionsj("j", cl::desc("Alias for --section"),
                                 cl::aliasopt(llvm::FilterSections));

cl::opt<bool>
    llvm::PrintImmHex("print-imm-hex",
                      cl::desc("Use hex format for immediate values"));

cl::opt<bool> PrintFaultMaps("fault-map-section",
                             cl::desc("Display contents of faultmap section"));

cl::opt<unsigned long long>
    StartAddress("start-address", cl::desc("Disassemble beginning at address"),
                 cl::value_desc("address"), cl::init(0));
cl::opt<unsigned long long> StopAddress("stop-address",
                                        cl::desc("Stop disassembly at address"),
                                        cl::value_desc("address"),
                                        cl::init(UINT64_MAX));
cl::list<std::string> llvm::IncludeFileNames(
    "include-files", cl::CommaSeparated,
    cl::desc("List of comma-seperated header files with function prototypes "
             "using standard C syntax."),
    cl::cat(LLVMMCToLLCategory), cl::NotHidden);

cl::alias static IncludeFileNamesShort(
    "I", cl::desc("Alias for --include-files=<single-header-file>"),
    cl::aliasopt(llvm::IncludeFileNames), cl::cat(LLVMMCToLLCategory),
    cl::NotHidden);

namespace {
static ManagedStatic<std::vector<std::string>> RunPassNames;

struct RunPassOption {
  void operator=(const std::string &Val) const {
    if (Val.empty())
      return;
    SmallVector<StringRef, 8> PassNames;
    StringRef(Val).split(PassNames, ',', -1, false);
    for (auto PassName : PassNames)
      RunPassNames->push_back(std::string(PassName));
  }
};
} // namespace

#define DEBUG_TYPE "mctoll"

static RunPassOption RunPassOpt;

int ISA_type = -1;

static cl::opt<RunPassOption, true, cl::parser<std::string>> RunPass(
    "run-pass",
    cl::desc("Run compiler only for specified passes (comma separated list)"),
    cl::value_desc("pass-name"), cl::ZeroOrMore, cl::location(RunPassOpt));

static StringRef ToolName;

namespace {
typedef std::function<bool(llvm::object::SectionRef const &)> FilterPredicate;

class SectionFilterIterator {
public:
  SectionFilterIterator(FilterPredicate P,
                        llvm::object::section_iterator const &I,
                        llvm::object::section_iterator const &E)
      : Predicate(std::move(P)), Iterator(I), End(E) {
    ScanPredicate();
  }
  const llvm::object::SectionRef &operator*() const { return *Iterator; }
  SectionFilterIterator &operator++() {
    ++Iterator;
    ScanPredicate();
    return *this;
  }
  bool operator!=(SectionFilterIterator const &Other) const {
    return Iterator != Other.Iterator;
  }

private:
  void ScanPredicate() {
    while (Iterator != End && !Predicate(*Iterator)) {
      ++Iterator;
    }
  }
  FilterPredicate Predicate;
  llvm::object::section_iterator Iterator;
  llvm::object::section_iterator End;
};

class SectionFilter {
public:
  SectionFilter(FilterPredicate P, llvm::object::ObjectFile const &O)
      : Predicate(std::move(P)), Object(O) {}
  SectionFilterIterator begin() {
    return SectionFilterIterator(Predicate, Object.section_begin(),
                                 Object.section_end());
  }
  SectionFilterIterator end() {
    return SectionFilterIterator(Predicate, Object.section_end(),
                                 Object.section_end());
  }

private:
  FilterPredicate Predicate;
  llvm::object::ObjectFile const &Object;
};
SectionFilter ToolSectionFilter(llvm::object::ObjectFile const &O) {
  LLVM_DEBUG(dbgs() << "Entering Function ToolSectionFilter()\n");
  return SectionFilter(
      [](llvm::object::SectionRef const &S) {

        if (FilterSections.empty())
          return true;
        llvm::StringRef String;
        if (auto NameOrErr = S.getName())
          String = *NameOrErr;
        else {
          consumeError(NameOrErr.takeError());
          return false;
        }
        bool ret = is_contained(FilterSections, String);
        LLVM_DEBUG(dbgs() << "Entering Lamda Function, Section String = "
                          << String
                           <<"\n");
        LLVM_DEBUG(dbgs() << "Lamda func return " << ret << "\n");
        return ret;
      },
      O);
}
} // namespace

void llvm::error(std::error_code EC) {
  if (!EC)
    return;

  errs() << ToolName << ": error reading file: " << EC.message() << ".\n";
  errs().flush();
  exit(1);
}

void llvm::error(Error E) {
  if (!E)
    return;
  WithColor::error(errs(), ToolName) << toString(std::move(E));
  exit(1);
}

LLVM_ATTRIBUTE_NORETURN void llvm::error(Twine Message) {
  errs() << ToolName << ": " << Message << ".\n";
  errs().flush();
  exit(1);
}

LLVM_ATTRIBUTE_NORETURN void llvm::report_error(StringRef File, Twine Message) {
  WithColor::error(errs(), ToolName)
      << "'" << File << "': " << Message << ".\n";
  exit(1);
}

LLVM_ATTRIBUTE_NORETURN void llvm::report_error(Error E, StringRef File) {
  assert(E);
  std::string Buf;
  raw_string_ostream OS(Buf);
  logAllUnhandledErrors(std::move(E), OS);
  OS.flush();
  WithColor::error(errs(), ToolName) << "'" << File << "': " << Buf;
  exit(1);
}

LLVM_ATTRIBUTE_NORETURN void llvm::report_error(Error E, StringRef ArchiveName,
                                                StringRef FileName,
                                                StringRef ArchitectureName) {
  assert(E);
  WithColor::error(errs(), ToolName);
  if (ArchiveName != "")
    errs() << ArchiveName << "(" << FileName << ")";
  else
    errs() << "'" << FileName << "'";
  if (!ArchitectureName.empty())
    errs() << " (for architecture " << ArchitectureName << ")";
  std::string Buf;
  raw_string_ostream OS(Buf);
  logAllUnhandledErrors(std::move(E), OS);
  OS.flush();
  errs() << ": " << Buf;
  exit(1);
}

LLVM_ATTRIBUTE_NORETURN void llvm::report_error(Error E, StringRef ArchiveName,
                                                const object::Archive::Child &C,
                                                StringRef ArchitectureName) {
  Expected<StringRef> NameOrErr = C.getName();
  // TODO: if we have a error getting the name then it would be nice to print
  // the index of which archive member this is and or its offset in the
  // archive instead of "???" as the name.
  if (!NameOrErr) {
    consumeError(NameOrErr.takeError());
    report_error(std::move(E), ArchiveName, "???", ArchitectureName);
  } else
    report_error(std::move(E), ArchiveName, NameOrErr.get(), ArchitectureName);
}

static const Target *getTarget(const ObjectFile *Obj = nullptr) {
  // Figure out the target triple.
  llvm::Triple TheTriple("unknown-unknown-unknown");
  if (TripleName.empty()) {
    if (Obj) {
      auto Arch = Obj->getArch();
      TheTriple.setArch(Triple::ArchType(Arch));
      
      // For ARM targets, try to use the build attributes to build determine
      // the build target. Target features are also added, but later during
      // disassembly.
      if (Arch == Triple::arm || Arch == Triple::armeb) {
        Obj->setARMSubArch(TheTriple);
      }

      // if (Arch == Triple::riscv32) {
      //   Obj->setRISCVSubArch(TheTriple);
      // }

      // TheTriple defaults to ELF, and COFF doesn't have an environment:
      // the best we can do here is indicate that it is mach-o.
      if (Obj->isMachO())
        TheTriple.setObjectFormat(Triple::MachO);

      if (Obj->isCOFF()) {
        const auto COFFObj = dyn_cast<COFFObjectFile>(Obj);
        if (COFFObj->getArch() == Triple::thumb)
          TheTriple.setTriple("thumbv7-windows");
      }
    }
  } else {
    TheTriple.setTriple(Triple::normalize(TripleName));
    // Use the triple, but also try to combine with ARM build attributes.
    if (Obj) {
      auto Arch = Obj->getArch();
      if (Arch == Triple::arm || Arch == Triple::armeb) {
        Obj->setARMSubArch(TheTriple);
      }
    }
  }

  // Get the target specific parser.
  // LLVM_DEBUG(dbgs() << "::" << TheTriple.getTriple() << "\n");
  LLVM_DEBUG(dbgs() << "::" << TheTriple.getTriple() << "\n");
  std::string Error;
  const Target *TheTarget =
      TargetRegistry::lookupTarget(ArchName, TheTriple, Error);
  LLVM_DEBUG(dbgs() << "::" << Error << "\n");
  if (!TheTarget) {
    if (Obj)
      report_error(Obj->getFileName(), "Support for raising " +
                                           TheTriple.getArchName() +
                                           " not included");
    else
      error("Unsupported target " + TheTriple.getArchName());
  }

  // A few of opcodes in ARMv4 or ARMv5 are indentified as ARMv6 opcodes,
  // so unify the triple Archs lower then ARMv6 to ARMv6 temporarily.
  if (TheTriple.getArchName() == "armv4t" ||
      TheTriple.getArchName() == "armv5te" ||
      TheTriple.getArchName() == "armv5" || TheTriple.getArchName() == "armv5t")
    TheTriple.setArchName("armv6");

  // Update the triple name and return the found target.
  TripleName = TheTriple.getTriple();
  return TheTarget;
}

static std::unique_ptr<ToolOutputFile> GetOutputStream(const char *TargetName,
                                                       Triple::OSType OS,
                                                       const char *ProgName) {
  // If we don't yet have an output filename, make one.
  if (OutputFilename.empty()) {
    // If InputFilename ends in .o, remove it.
    StringRef IFN = InputFilenames[0];
    if (IFN.endswith(".o"))
      OutputFilename = std::string(IFN.drop_back(2));
    else if (IFN.endswith(".so"))
      OutputFilename = std::string(IFN.drop_back(3));
    else
      OutputFilename = std::string(IFN);

    switch (OutputFormat) {
    case CGFT_AssemblyFile:
      OutputFilename += "-dis.ll";
      break;
    // Just uses enum CGFT_ObjectFile represent llvm bitcode file type
    // provisionally.
    case CGFT_ObjectFile:
      OutputFilename += "-dis.bc";
      break;
    case CGFT_Null:
      OutputFilename += ".null";
      break;
    }
  }

  // Decide if we need "binary" output.
  bool Binary = false;
  switch (OutputFormat) {
  case CGFT_AssemblyFile:
    break;
  case CGFT_ObjectFile:
  case CGFT_Null:
    Binary = true;
    break;
  }

  // Open the file.
  std::error_code EC;
  sys::fs::OpenFlags OpenFlags = sys::fs::F_None;
  if (!Binary)
    OpenFlags |= sys::fs::F_Text;
  auto FDOut = std::make_unique<ToolOutputFile>(OutputFilename, EC, OpenFlags);
  if (EC) {
    errs() << EC.message() << '\n';
    return nullptr;
  }

  return FDOut;
}

static bool addPass(PassManagerBase &PM, StringRef toolname, StringRef PassName,
                    TargetPassConfig &TPC) {
  if (PassName == "none")
    return false;

  const PassRegistry *PR = PassRegistry::getPassRegistry();
  const PassInfo *PI = PR->getPassInfo(PassName);
  if (!PI) {
    errs() << toolname << ": run-pass " << PassName << " is not registered.\n";
    return true;
  }

  Pass *P;
  if (PI->getNormalCtor())
    P = PI->getNormalCtor()();
  else {
    errs() << toolname << ": cannot create pass: " << PI->getPassName() << "\n";
    return true;
  }
  std::string Banner = std::string("After ") + std::string(P->getPassName());
  PM.add(P);
  TPC.printAndVerify(Banner);

  return false;
}

bool llvm::RelocAddressLess(RelocationRef a, RelocationRef b) {
  return a.getOffset() < b.getOffset();
}

namespace {
static bool isArmElf(const ObjectFile *Obj) {
  return (Obj->isELF() &&
          (Obj->getArch() == Triple::aarch64 ||
           Obj->getArch() == Triple::aarch64_be ||
           Obj->getArch() == Triple::arm || Obj->getArch() == Triple::armeb ||
           Obj->getArch() == Triple::thumb ||
           Obj->getArch() == Triple::thumbeb));
}

class PrettyPrinter {
public:
  virtual ~PrettyPrinter() {}
  virtual void printInst(MCInstPrinter &IP, const MCInst *MI,
                         ArrayRef<uint8_t> Bytes, uint64_t Address,
                         raw_ostream &OS, StringRef Annot,
                         MCSubtargetInfo const &STI) {
    OS << format("%8" PRIx64 ":", Address);
    OS << "\t";
    dumpBytes(Bytes, OS);
    if (MI)
      IP.printInst(MI, 0, "", STI, OS);
    else
      OS << " <unknown>";
    OS << "\n";
  }
};
PrettyPrinter PrettyPrinterInst;

PrettyPrinter &selectPrettyPrinter(Triple const &Triple) {
  return PrettyPrinterInst;
}
} // namespace

bool llvm::isRelocAddressLess(RelocationRef A, RelocationRef B) {
  return A.getOffset() < B.getOffset();
}

template <class ELFT>
static std::error_code getRelocationValueString(const ELFObjectFile<ELFT> *Obj,
                                                const RelocationRef &RelRef,
                                                SmallVectorImpl<char> &Result) {
  DataRefImpl Rel = RelRef.getRawDataRefImpl();

  typedef typename ELFObjectFile<ELFT>::Elf_Sym Elf_Sym;
  typedef typename ELFObjectFile<ELFT>::Elf_Shdr Elf_Shdr;
  typedef typename ELFObjectFile<ELFT>::Elf_Rela Elf_Rela;

  const ELFFile<ELFT> &EF = *Obj->getELFFile();

  auto SecOrErr = EF.getSection(Rel.d.a);
  if (!SecOrErr)
    return errorToErrorCode(SecOrErr.takeError());
  const Elf_Shdr *Sec = *SecOrErr;
  auto SymTabOrErr = EF.getSection(Sec->sh_link);
  if (!SymTabOrErr)
    return errorToErrorCode(SymTabOrErr.takeError());
  const Elf_Shdr *SymTab = *SymTabOrErr;
  assert(SymTab->sh_type == ELF::SHT_SYMTAB ||
         SymTab->sh_type == ELF::SHT_DYNSYM);
  auto StrTabSec = EF.getSection(SymTab->sh_link);
  if (!StrTabSec)
    return errorToErrorCode(StrTabSec.takeError());
  auto StrTabOrErr = EF.getStringTable(*StrTabSec);
  if (!StrTabOrErr)
    return errorToErrorCode(StrTabOrErr.takeError());
  StringRef StrTab = *StrTabOrErr;
  uint8_t type = RelRef.getType();
  StringRef res;
  int64_t addend = 0;
  switch (Sec->sh_type) {
  default:
    return object_error::parse_failed;
  case ELF::SHT_REL: {
    // TODO: Read implicit addend from section data.
    break;
  }
  case ELF::SHT_RELA: {
    const Elf_Rela *ERela = Obj->getRela(Rel);
    addend = ERela->r_addend;
    break;
  }
  }
  symbol_iterator SI = RelRef.getSymbol();
  const Elf_Sym *symb = Obj->getSymbol(SI->getRawDataRefImpl());
  StringRef Target;
  if (symb->getType() == ELF::STT_SECTION) {
    Expected<section_iterator> SymSI = SI->getSection();
    if (!SymSI)
      return errorToErrorCode(SymSI.takeError());
    const Elf_Shdr *SymSec = Obj->getSection((*SymSI)->getRawDataRefImpl());
    auto SecName = EF.getSectionName(SymSec);
    if (!SecName)
      return errorToErrorCode(SecName.takeError());
    Target = *SecName;
  } else {
    Expected<StringRef> SymName = symb->getName(StrTab);
    if (!SymName)
      return errorToErrorCode(SymName.takeError());
    Target = *SymName;
  }
  switch (EF.getHeader()->e_machine) {
  case ELF::EM_X86_64:
    switch (type) {
    case ELF::R_X86_64_PC8:
    case ELF::R_X86_64_PC16:
    case ELF::R_X86_64_PC32: {
      std::string fmtbuf;
      raw_string_ostream fmt(fmtbuf);
      fmt << Target << (addend < 0 ? "" : "+") << addend << "-P";
      fmt.flush();
      Result.append(fmtbuf.begin(), fmtbuf.end());
    } break;
    case ELF::R_X86_64_8:
    case ELF::R_X86_64_16:
    case ELF::R_X86_64_32:
    case ELF::R_X86_64_32S:
    case ELF::R_X86_64_64: {
      std::string fmtbuf;
      raw_string_ostream fmt(fmtbuf);
      fmt << Target << (addend < 0 ? "" : "+") << addend;
      fmt.flush();
      Result.append(fmtbuf.begin(), fmtbuf.end());
    } break;
    default:
      res = "Unknown";
    }
    break;
  case ELF::EM_LANAI:
  case ELF::EM_AVR:
  case ELF::EM_AARCH64: {
    std::string fmtbuf;
    raw_string_ostream fmt(fmtbuf);
    fmt << Target;
    if (addend != 0)
      fmt << (addend < 0 ? "" : "+") << addend;
    fmt.flush();
    Result.append(fmtbuf.begin(), fmtbuf.end());
    break;
  }
  case ELF::EM_386:
  case ELF::EM_IAMCU:
  case ELF::EM_ARM:
  case ELF::EM_HEXAGON:
  case ELF::EM_MIPS:
  case ELF::EM_BPF:
  case ELF::EM_RISCV:
    res = Target;
    break;
  default:
    res = "Unknown";
  }
  if (Result.empty())
    Result.append(res.begin(), res.end());
  return std::error_code();
}

static uint8_t getElfSymbolType(const ObjectFile *Obj, const SymbolRef &Sym) {
  assert(Obj->isELF());
  auto SymbImpl = Sym.getRawDataRefImpl();
  if (auto *Elf32LEObj = dyn_cast<ELF32LEObjectFile>(Obj)) {
    auto SymbOrErr = Elf32LEObj->getSymbol(SymbImpl);
    if (!SymbOrErr)
      report_error(SymbOrErr.takeError(), "ELF32 symbol not found");
    return SymbOrErr.get()->getType();
  }
  if (auto *Elf64LEObj = dyn_cast<ELF64LEObjectFile>(Obj)) {
    auto SymbOrErr = Elf64LEObj->getSymbol(SymbImpl);
    if (!SymbOrErr)
      report_error(SymbOrErr.takeError(), "ELF32 symbol not found");
    return SymbOrErr.get()->getType();
  }
  if (auto *Elf32BEObj = dyn_cast<ELF32BEObjectFile>(Obj)) {
    auto SymbOrErr = Elf32BEObj->getSymbol(SymbImpl);
    if (!SymbOrErr)
      report_error(SymbOrErr.takeError(), "ELF32 symbol not found");
    return SymbOrErr.get()->getType();
  }
  if (auto *Elf64BEObj = cast<ELF64BEObjectFile>(Obj)) {
    auto SymbOrErr = Elf64BEObj->getSymbol(SymbImpl);
    if (!SymbOrErr)
      report_error(SymbOrErr.takeError(), "ELF32 symbol not found");
    return SymbOrErr.get()->getType();
  }
  llvm_unreachable("Unsupported binary format");
  // Keep the code analyzer happy
  return ELF::STT_NOTYPE;
}

template <class ELFT>
static void
addDynamicElfSymbols(const ELFObjectFile<ELFT> *Obj,
                     std::map<SectionRef, SectionSymbolsTy> &AllSymbols) {
  for (auto Symbol : Obj->getDynamicSymbolIterators()) {
    uint8_t SymbolType = Symbol.getELFType();
    if (SymbolType != ELF::STT_FUNC || Symbol.getSize() == 0)
      continue;

    Expected<uint64_t> AddressOrErr = Symbol.getAddress();
    if (!AddressOrErr)
      report_error(AddressOrErr.takeError(), Obj->getFileName());
    uint64_t Address = *AddressOrErr;

    Expected<StringRef> Name = Symbol.getName();
    if (!Name)
      report_error(Name.takeError(), Obj->getFileName());
    if (Name->empty())
      continue;

    Expected<section_iterator> SectionOrErr = Symbol.getSection();
    if (!SectionOrErr)
      report_error(SectionOrErr.takeError(), Obj->getFileName());
    section_iterator SecI = *SectionOrErr;
    if (SecI == Obj->section_end())
      continue;

    AllSymbols[*SecI].emplace_back(Address, *Name, SymbolType);
  }
}

static void
addDynamicElfSymbols(const ObjectFile *Obj,
                     std::map<SectionRef, SectionSymbolsTy> &AllSymbols) {

  LLVM_DEBUG(dbgs()<<"DEBUG::Entering addDynamicElfSymbols()\n");
  assert(Obj->isELF());
  if (auto *Elf32LEObj = dyn_cast<ELF32LEObjectFile>(Obj))
    addDynamicElfSymbols(Elf32LEObj, AllSymbols);
  else if (auto *Elf64LEObj = dyn_cast<ELF64LEObjectFile>(Obj))
    addDynamicElfSymbols(Elf64LEObj, AllSymbols);
  else if (auto *Elf32BEObj = dyn_cast<ELF32BEObjectFile>(Obj))
    addDynamicElfSymbols(Elf32BEObj, AllSymbols);
  else if (auto *Elf64BEObj = cast<ELF64BEObjectFile>(Obj))
    addDynamicElfSymbols(Elf64BEObj, AllSymbols);
  else
    llvm_unreachable("Unsupported binary format");
}

/*
   A list of symbol entries corresponding to CRT functions added by
   the linker while creating an ELF executable. It is not necessary to
   disassemble and translate these functions.
*/

static std::set<StringRef> ELFCRTSymbols = {
    "call_weak_fn",
    "deregister_tm_clones",
    "__do_global_dtors_aux",
    "__do_global_dtors_aux_fini_array_entry",
    "_fini",
    "frame_dummy",
    "__frame_dummy_init_array_entry",
    "_init",
    "__init_array_end",
    "__init_array_start",
    "__libc_csu_fini",
    "__libc_csu_init",
    "register_tm_clones",
    // Commented for raising global var of RISC-V ELF
    //"_start",
    "_dl_relocate_static_pie"};

/*
   A list of symbol entries corresponding to CRT functions added by
   the linker while creating an MachO executable. It is not necessary
   to disassemble and translate these functions.
*/

static std::set<StringRef> MachOCRTSymbols = {"__mh_execute_header",
                                              "dyld_stub_binder", "__text",
                                              "__stubs", "__stub_helper"};

/*
   A list of sections whose contents are to be disassembled as code
*/

static std::set<StringRef> ELFSectionsToDisassemble = {".text"};
static std::set<StringRef> MachOSectionsToDisassemble = {};

/* TODO : If it is a C++ binary object symbol, look at the
   signature of the symbol to deduce the return value and return
   type. If the symbol does not include the function signature,
   just create a function that takes no arguments */
/* A non vararg function type with no arguments */
/* TODO: Figure out the symbol linkage type from the symbol
   table. For now assuming global linkage
*/

static bool isAFunctionSymbol(const ObjectFile *Obj, SymbolInfoTy &Symbol) {
  if (Obj->isELF()) {
    //LLVM_DEBUG(dbgs() << "Symbol.Type = " << Symbol.Type << "\n");
    return (Symbol.Type == ELF::STT_FUNC);
  }
  
  if (Obj->isMachO()) {
    // If Symbol is not in the MachOCRTSymbol list return true indicating that
    // this is a symbol of a function we are interested in disassembling and
    // raising.
    return (MachOCRTSymbols.find(Symbol.Name) == MachOCRTSymbols.end());
  }
  return false;
}

namespace RaiserContext {
SmallVector<ModuleRaiser *, 4> ModuleRaiserRegistry;

bool isSupportedArch(Triple::ArchType arch) {
  for (auto m : ModuleRaiserRegistry)
    if (m->getArchType() == arch)
      return true;

  return false;
}

ModuleRaiser *getModuleRaiser(const TargetMachine *tm) {
  ModuleRaiser *mr = nullptr;
  auto arch = tm->getTargetTriple().getArch();
  for (auto m : ModuleRaiserRegistry)
    if (m->getArchType() == arch) {
      mr = m;
      break;
    }
  assert(nullptr != mr && "This arch has not yet supported for raising!\n");
  return mr;
}

} // namespace RaiserContext

static vector<AssemblyCFG*> cfgs;

static void DisassembleObject(const ObjectFile *Obj, bool InlineRelocs) {
  if (StartAddress > StopAddress)
    error("Start address should be less than stop address");

  // const Target *TheTarget = MCContext(Obj);
  const Target *TheTarget = getTarget(Obj);
  // Package up features to be passed to target/subtarget
  SubtargetFeatures Features = Obj->getFeatures();
  if (MAttrs.size()) {
    for (unsigned i = 0; i != MAttrs.size(); ++i)
      Features.AddFeature(MAttrs[i]);
  }

  std::unique_ptr<const MCRegisterInfo> MRI(
      TheTarget->createMCRegInfo(TripleName));
  if (!MRI)
    report_error(Obj->getFileName(),
                 "no register info for target " + TripleName);

  MCTargetOptions MCOptions;
  // Set up disassembler.
  std::unique_ptr<const MCAsmInfo> AsmInfo(
      TheTarget->createMCAsmInfo(*MRI, TripleName, MCOptions));
  if (!AsmInfo)
    report_error(Obj->getFileName(),
                 "no assembly info for target " + TripleName);
  std::unique_ptr<const MCSubtargetInfo> STI(
      // TheTarget->createMCSubtargetInfo(TripleName, MCPU, Features.getString()));
      TheTarget->createMCSubtargetInfo(TripleName, MCPU, "+c,+64bit,-e,+m,+a,+f,+f,+d,+c"));
  if (!STI)
    report_error(Obj->getFileName(),
                 "no subtarget info for target " + TripleName);
  std::unique_ptr<const MCInstrInfo> MII(TheTarget->createMCInstrInfo());
  if (!MII)
    report_error(Obj->getFileName(),
                 "no instruction info for target " + TripleName);
  MCObjectFileInfo MOFI;
  MCContext Ctx(AsmInfo.get(), MRI.get(), &MOFI);
  // FIXME: for now initialize MCObjectFileInfo with default values
  MOFI.InitMCObjectFileInfo(Triple(TripleName), false, Ctx);

  std::unique_ptr<MCDisassembler> DisAsm(
      TheTarget->createMCDisassembler(*STI, Ctx));
  if (!DisAsm)
    report_error(Obj->getFileName(),
                 "no disassembler for target " + TripleName);

  std::unique_ptr<const MCInstrAnalysis> MIA(
      TheTarget->createMCInstrAnalysis(MII.get()));

  int AsmPrinterVariant = AsmInfo->getAssemblerDialect();
  std::unique_ptr<MCInstPrinter> IP(TheTarget->createMCInstPrinter(
      Triple(TripleName), AsmPrinterVariant, *AsmInfo, *MII, *MRI));
  if (!IP)
    report_error(Obj->getFileName(),
                 "no instruction printer for target " + TripleName);
  IP->setPrintImmHex(PrintImmHex);
  PrettyPrinter &PIP = selectPrettyPrinter(Triple(TripleName));

  LLVMContext llvmCtx;
  std::unique_ptr<TargetMachine> Target(
      TheTarget->createTargetMachine(TripleName, MCPU, Features.getString(),
                                     TargetOptions(), /* RelocModel */ None));
  assert(Target && "Could not allocate target machine!");

  LLVMTargetMachine &llvmTgtMach = static_cast<LLVMTargetMachine &>(*Target);
  MachineModuleInfoWrapperPass *machineModuleInfo =
      new MachineModuleInfoWrapperPass(&llvmTgtMach);
  /* New Module instance with file name */
  Module module(Obj->getFileName(), llvmCtx);
  /* Set datalayout of the module to be the same as LLVMTargetMachine */
  module.setDataLayout(Target->createDataLayout());
  machineModuleInfo->doInitialization(module);
  // Initialize all module raisers that are supported and are part of current
  // LLVM build.
  ModuleRaiser::InitializeAllModuleRaisers();
  // Get the module raiser for Target of the binary being raised
  ModuleRaiser *moduleRaiser = RaiserContext::getModuleRaiser(Target.get());
  assert((moduleRaiser != nullptr) && "Failed to build module raiser");
  // Set data of module raiser
  moduleRaiser->setModuleRaiserInfo(&module, Target.get(),
                                    &machineModuleInfo->getMMI(), MIA.get(),
                                    MII.get(), Obj, DisAsm.get());

  // Collect dynamic relocations.
  moduleRaiser->collectDynamicRelocations();

  // Create a mapping, RelocSecs = SectionRelocMap[S], where sections
  // in RelocSecs contain the relocations for section S.
  std::error_code EC;
  std::map<SectionRef, SmallVector<SectionRef, 1>> SectionRelocMap;



  for (const SectionRef &Section : ToolSectionFilter(*Obj)) {
    Expected<section_iterator> SecOrErr = Section.getRelocatedSection();
    if (!SecOrErr) {
      cout << "DEBUG::\t Section.getRelocatedSection() returns NULL\n";
      break;
    }
    section_iterator Sec2 = *SecOrErr;
    
    if(auto errcheck = Sec2->getName()){
      auto name = *errcheck;
      cout << "DEBUG::\t Relocation Section Name = " << name.str() << endl;
    }
    cout << "DEBUG::\t Relocation Section Addr = " << Sec2->getAddress() << endl;


    if (Sec2 != Obj->section_end()){
      cout << "DEBUG::\t Push section iterator to SectionRelocMap\n";
      SectionRelocMap[*Sec2].push_back(Section);
    }
  }

  // Create a mapping from virtual address to symbol name.  This is used to
  // pretty print the symbols while disassembling.
  std::map<SectionRef, SectionSymbolsTy> AllSymbols;
  for (const SymbolRef &Symbol : Obj->symbols()) {
    Expected<uint64_t> AddressOrErr = Symbol.getAddress();
    if (!AddressOrErr)
      report_error(AddressOrErr.takeError(), Obj->getFileName());
    uint64_t Address = *AddressOrErr;
    Expected<StringRef> Name = Symbol.getName();
    if (!Name)
      report_error(Name.takeError(), Obj->getFileName());      
    if (Name->empty())
      continue;

    Expected<section_iterator> SectionOrErr = Symbol.getSection();
    if (!SectionOrErr)
      report_error(SectionOrErr.takeError(), Obj->getFileName());
    section_iterator SecI = *SectionOrErr;
    if (SecI == Obj->section_end())
      continue;

    uint8_t SymbolType = ELF::STT_NOTYPE;
    if (Obj->isELF())
      SymbolType = getElfSymbolType(Obj, Symbol);

    LLVM_DEBUG(dbgs() << "DEBUG::Symbol Address is "  << Address
                      // << ", Symbol type = " << SymbolType
                      << "\n");
    if(SymbolType == ELF::STT_OBJECT){
        LLVM_DEBUG(dbgs() << "DEBUG::Symbol type = Data \n");
    }

    AllSymbols[*SecI].emplace_back(Address, *Name, SymbolType);
  }
  if (AllSymbols.empty() && Obj->isELF())
    addDynamicElfSymbols(Obj, AllSymbols);

  // Create a mapping from virtual address to section.
  std::vector<std::pair<uint64_t, SectionRef>> SectionAddresses;
  for (SectionRef Sec : Obj->sections())
    SectionAddresses.emplace_back(Sec.getAddress(), Sec);
  array_pod_sort(SectionAddresses.begin(), SectionAddresses.end());


  //for (const SectionRef &Section : ToolSectionFilter(*Obj));

  // Linked executables (.exe and .dll files) typically don't include a real
  // symbol table but they might contain an export table.
  if (const auto *COFFObj = dyn_cast<COFFObjectFile>(Obj)) {
    for (const auto &ExportEntry : COFFObj->export_directories()) {
      StringRef Name;
      error(ExportEntry.getSymbolName(Name));
      if (Name.empty())
        continue;

      uint32_t RVA;
      error(ExportEntry.getExportRVA(RVA));

      uint64_t VA = COFFObj->getImageBase() + RVA;
      auto Sec = std::upper_bound(
          SectionAddresses.begin(), SectionAddresses.end(), VA,
          [](uint64_t LHS, const std::pair<uint64_t, SectionRef> &RHS) {
            return LHS < RHS.first;
          });
      if (Sec != SectionAddresses.begin())
        --Sec;
      else
        Sec = SectionAddresses.end();

      if (Sec != SectionAddresses.end())
        AllSymbols[Sec->second].emplace_back(VA, Name, ELF::STT_NOTYPE);
    }
  }

  uint64_t pltStart, pltEnd;
  // std::map<string, AssemblyFunction*> textSectionFunctions;
  // Sort all the symbols, this allows us to use a simple binary search to find
  // a symbol near an address.
  for (std::pair<const SectionRef, SectionSymbolsTy> &SecSyms : AllSymbols) {
    array_pod_sort(SecSyms.second.begin(), SecSyms.second.end());
    SectionRef Section = SecSyms.first;
    StringRef SectionName2;
    if (auto NameOrErr = Section.getName())
      SectionName2 = *NameOrErr;
    cout << "DEBUG::Section Name is " << SectionName2.str() << ",\t Address is " << Section.getAddress() << ",\t Size is " << Section.getSize();
    if(Section.isText()) cout << ",\t Type is Text" << endl;
    else if(Section.isBSS()) cout << ",\t Type is BSS" << endl;
    else if(Section.isData()) cout << ",\t Type is Data" << endl;
    else cout << ",\t Type is Unknown" << endl;

    if(SectionName2.str() == ".bss" || SectionName2.str() == ".sbss" || SectionName2.str() == ".data"
       || SectionName2.str() == ".sdata"||SectionName2.str() == ".rodata"||SectionName2.str() == ".srodata")
      RecordGlobalSection(SectionName2.str(), Section.getAddress() , Section.getSize());



    // Record plt range. Used for judging whether a function is in plt. 
    if(SectionName2.str() == ".plt") {
      pltStart = Section.getAddress();
      pltEnd = pltStart + Section.getSize();
      cout << "pltStart = " << pltStart << ", pltEnd = " << pltEnd << endl;

      // for(auto iter : Section.relocations()){
      //     cout<< "\t PLT Reloc Type = "  << iter.getType() << endl;
      //     cout <<"\t PLT Symbol: " << ((iter).getSymbol())->getName()->str() << endl;;
      // }
      //cout << "\t PLT Reloc Section Name is " << Section.getRelocatedSection().get()->getName()->str() << endl;
    }

    // Retrieve Function Symbols in PLT
    StringRef RelaSymbolName;
    uint64_t  RelaSymbolAddress;
    uint64_t  RelaSymbolValue;

    if(SectionName2.str() == ".rela.plt" || SectionName2.str() == "rel.plt") {

        for(auto iter : Section.relocations()){
          if (auto ErrCheck = ((iter).getSymbol())->getName())
              RelaSymbolName = *ErrCheck;

          if(auto ErrCheck = iter.getSymbol()->getAddress()){
              RelaSymbolAddress = ErrCheck.get();
          }

          // if(auto ErrCheck = iter.getSymbol()->getValue()){
          //     RelaSymbolValue = ErrCheck.get();
          // }
          
          // cout<< "\t PLT Reloc Type = "  << iter.getType() << endl;
          // cout <<"\t PLT Symbol:         \t" << RelaSymbolName.str() << endl;
          // cout <<"\t PLT Symbol Addr:    \t" << RelaSymbolAddress << endl;
          // cout <<"\t PLT Symbol Offset:  \t" << std::hex << iter.getOffset() << endl;

            

          // If ISA = RV32 or RV64
          // if(ISA_type == 3 || ISA_type == 4)
          //   RelaSymbolAddress += pltStart + 32 + counter*8;

          PLTFunctions.push_back(make_pair(RelaSymbolName.str(),RelaSymbolAddress));
          
        }
        // cout  << "\t Dumping PLT Functions < Symbol Name,  Address >";
        // for(auto it:PLTFunctions)
        //   cout<< "\t PLT Symbol: < " << it.first    << ",  "<< it.second << " >" << endl;
    }


  
   

    SectionSymbolsTy Symbols = SecSyms.second;
    for (int i = 0; i < Symbols.size(); i++){
      const auto &Symb = Symbols[i];
      if(Symb.Type == ELF::STT_NOTYPE)cout << "\t" << Symb.Name.str() << "\t" << Symb.Addr << "\t" << "STT_NOTYPE" << endl;
      else if(Symb.Type == ELF::STT_OBJECT){
          if(SectionName2.str() == ".bss" || SectionName2.str() == ".sbss" || SectionName2.str() == ".data"
             || SectionName2.str() == ".sdata"||SectionName2.str() == ".rodata"||SectionName2.str() == ".srodata")
                RecordGlobalData(Symb.Name.str(),Symb.Addr);
          cout << "\t" << Symb.Name.str() << "\t" << Symb.Addr << "\t" << "STT_OBJECT" << endl;

      }
      else if(Symb.Type == ELF::STT_FUNC) {
        cout << "\t" << Symb.Name.str() << "\t" << Symb.Addr << "\t" << "STT_FUNC" << endl;
        if(i < Symbols.size() - 1) textSectionFunctions.insert(make_pair(Symb.Name.str(), new AssemblyFunction(Symb.Name.str(), Symb.Addr, Symbols[i + 1].Addr)));
      }
      else if(Symb.Type == ELF::STT_SECTION) cout << "\t" << Symb.Name.str() << "\t" << Symb.Addr << "\t" << "STT_SECTION" << endl;
      else if(Symb.Type == ELF::STT_FILE) cout << "\t" << Symb.Name.str() << "\t" << Symb.Addr << "\t" << "STT_FILE" << endl;
      else if(Symb.Type == ELF::STT_COMMON) cout << "\t" << Symb.Name.str() << "\t" << Symb.Addr << "\t" << "STT_COMMON" << endl;
      else if(Symb.Type == ELF::STT_TLS) cout << "\t" << Symb.Name.str() << "\t" << Symb.Addr << "\t" << "STT_TLS" << endl;
      else if(Symb.Type == ELF::STT_GNU_IFUNC) cout << "\t" << Symb.Name.str() << "\t" << Symb.Addr << "\t" << "STT_GNU_IFUNC" << endl;
      else if(Symb.Type == ELF::STT_LOOS) cout << "\t" << Symb.Name.str() << "\t" << Symb.Addr << "\t" << "STT_LOOS" << endl;
      else if(Symb.Type == ELF::STT_HIOS) cout << "\t" << Symb.Name.str() << "\t" << Symb.Addr << "\t" << "STT_HIOS" << endl;
      else if(Symb.Type == ELF::STT_LOPROC) cout << "\t" << Symb.Name.str() << "\t" << Symb.Addr << "\t" << "STT_LOPROC" << endl;
      else if(Symb.Type == ELF::STT_HIPROC) cout << "\t" << Symb.Name.str() << "\t" << Symb.Addr << "\t" << "STT_HIPROC" << endl;
      else if(Symb.Type == ELF::STT_AMDGPU_HSA_KERNEL) cout << "\t" << Symb.Name.str() << "\t" << Symb.Addr << "\t" << "STT_AMDGPU_HSA_KERNEL" << endl;
      else cout << "\t" << Symb.Name.str() << "\t" << Symb.Addr << "\t" << "UNKNOWN_SYMBOL_TYPE" << endl;
    }
    // Process last function in .text
    if(SectionName2.str() == ".text") {
      textSectionFunctions.insert(make_pair(Symbols[Symbols.size() - 1].Name.str(), new AssemblyFunction(Symbols[Symbols.size() - 1].Name.str(), Symbols[Symbols.size() - 1].Addr, Section.getAddress() + Section.getSize())));
    }
  }
  


  std::cout << "\n\n\t Dumping PLT Functions < Symbol Name,  Address >\n";
  for(int i = 0; i<PLTFunctions.size();i++){
    // If ISA = RV32 or RV64
    if(ISA_type == 3 || ISA_type == 4)
      PLTFunctions[i].second = pltStart + 32 + i*16;
    cout<< "\t PLT Symbol: < " << PLTFunctions[i].first    << ",  0x"<< std::hex <<PLTFunctions[i].second << " >" << endl;
  }
  std::cout << "\n\n";

  //LLVM_DEBUG(dbgs() << "DEBUG:: Last ToolSectionFilter  \n");
  //for (const SectionRef &Section : ToolSectionFilter(*Obj));

  int times = 0;
  int globalTIRColor = 0;



  unsigned  start   = 0;
  int       found   = 0;
  int       fun     = -1;


  for (const SectionRef &Section : ToolSectionFilter(*Obj)) {
    
    times++;
    LLVM_DEBUG(dbgs() << "DEBUG::Section Looping = " << times << "\n");
    
    // Commented to allow parse the data symbol - Xi
    // if ((!Section.isText() || Section.isVirtual()))
    //   continue;

    StringRef SectionName;
    if (auto NameOrErr = Section.getName())
      SectionName = *NameOrErr;
    else
      consumeError(NameOrErr.takeError());

    uint64_t SectionAddr = Section.getAddress();
    uint64_t SectSize = Section.getSize();
    LLVM_DEBUG(dbgs() << "DEBUG::Section Name is:  " << SectionName 
                      << ", Address is " << SectionAddr 
                      << ", Size is " << SectSize
                      << "\n");
    if (!SectSize)
      continue;
    


    // Get the list of all the symbols in this section.
    SectionSymbolsTy &Symbols = AllSymbols[Section];
    std::vector<uint64_t> DataMappingSymsAddr;
    std::vector<uint64_t> TextMappingSymsAddr;
    if (isArmElf(Obj)) {
      for (const auto &Symb : Symbols) {
        LLVM_DEBUG(dbgs() << Symb.Name << "\n");
        uint64_t Address = Symb.Addr;
        StringRef Name = Symb.Name;
        if (Name.startswith("$d"))
          DataMappingSymsAddr.push_back(Address - SectionAddr);
        if (Name.startswith("$x"))
          TextMappingSymsAddr.push_back(Address - SectionAddr);
        if (Name.startswith("$a"))
          TextMappingSymsAddr.push_back(Address - SectionAddr);
        if (Name.startswith("$t"))
          TextMappingSymsAddr.push_back(Address - SectionAddr);
      }
    }

    std::sort(DataMappingSymsAddr.begin(), DataMappingSymsAddr.end());
    std::sort(TextMappingSymsAddr.begin(), TextMappingSymsAddr.end());

    if (Obj->isELF() && Obj->getArch() == Triple::amdgcn) {
      // AMDGPU disassembler uses symbolizer for printing labels
      std::unique_ptr<MCRelocationInfo> RelInfo(
          TheTarget->createMCRelocationInfo(TripleName, Ctx));
      if (RelInfo) {
        std::unique_ptr<MCSymbolizer> Symbolizer(TheTarget->createMCSymbolizer(
            TripleName, nullptr, nullptr, &Symbols, &Ctx, std::move(RelInfo)));
        DisAsm->setSymbolizer(std::move(Symbolizer));
      }
    }

    // Make a list of all the relocations for this section.
    std::vector<RelocationRef> Rels;
    if (InlineRelocs) {
      for (const SectionRef &RelocSec : SectionRelocMap[Section]) {
        for (const RelocationRef &Reloc : RelocSec.relocations()) {
          if(auto errcheck = Reloc.getSymbol()->getName()){
              StringRef name = *errcheck;
              cout << "DEBUG::\t Relocation Symbol Name =    " << name.str() << endl;
          }
          if(auto errcheck = Reloc.getSymbol()->getAddress()){
              uint64_t addr = *errcheck;
              cout << "DEBUG::\t Relocation Symbol Address = " << addr << endl;
          }
          Rels.push_back(Reloc);
        }
      }
    }

    // Sort relocations by address.
    std::sort(Rels.begin(), Rels.end(), RelocAddressLess);

    // If the section has no symbol at the start, just insert a dummy one.
    StringRef name;
    if (Symbols.empty() || Symbols[0].Addr != 0) {
      Symbols.insert(
          Symbols.begin(),
          SymbolInfoTy(SectionAddr, name,
                       Section.isText() ? ELF::STT_FUNC : ELF::STT_OBJECT));
    }

    SmallString<40> Comments;
    raw_svector_ostream CommentStream(Comments);

    StringRef BytesStr =
        unwrapOrError(Section.getContents(), Obj->getFileName());
    ArrayRef<uint8_t> Bytes(reinterpret_cast<const uint8_t *>(BytesStr.data()),
                            BytesStr.size());

    uint64_t Size;
    uint64_t Index;

    FunctionFilter *FuncFilter = moduleRaiser->getFunctionFilter();
    if (cl::getRegisteredOptions()["filter-functions-file"]
            ->getNumOccurrences() > 0) {
      if (!FuncFilter->readFilterFunctionConfigFile(
              FilterFunctionSet.getValue())) {
        dbgs() << "Unable to read function filter configuration file "
               << FilterFunctionSet.getValue() << ". Ignoring\n";
      }
    }

    // Build a map of relocations (if they exist in the binary) of text
    // section whose instructions are being raised.
    moduleRaiser->collectTextSectionRelocs(Section);

    // Set used to record all branch targets of a function.
    std::set<uint64_t> branchTargetSet;
    MachineFunctionRaiser *curMFRaiser = nullptr;
    std::vector<AssemblyInstruction*> assInstrsOfFunction;

    // Disassemble symbol by symbol.
    LLVM_DEBUG(dbgs() << "BEGIN Disassembly of Functions in Section : "
                      << SectionName.data() << "\n");
    cout << "BEGIN Disassembly of Functions in Section: " << SectionName.data() << endl;
    vector<AssemblyCFG*> allCFGs;

    // Reorder symbols to place the RISC-V _start function to the beginning 
    // _start function handles the global pointer (global variables)



    std::vector<uint64_t> end_addr;
    
    for ( auto si = 0; si != Symbols.size(); si++){ 
     
      end_addr.push_back(Symbols[si + 1].Addr - SectionAddr);
      std::cout<< "\t DEBUG:: Func symbol:  " << Symbols[si].Name.str() 
               << "\t Start = " << Symbols[si].Addr - SectionAddr
               << "\t End   = " << Symbols[si + 1].Addr - SectionAddr <<endl;
      
    }

    if(ISA_type >=3){

    for ( start = 0; start != Symbols.size(); start++) {
        if(Symbols[start].Name.str()== ".text" && fun == -1)
            fun = start+1;

        if(Symbols[start].Name.str()=="_start"){
          found = 1;
          break;
        }
    }


    // if(found){
    //   std::cout << "\tDEBUG:: _start function symbol index in .text is " << start << endl;
    //   std::cout << "\tDEBUG:: function symbol index in .text starts from " << fun << endl;
    //   std::cout << "\tDEBUG:: The first function symbol is " << Symbols[fun].Name.str() << endl;

    // }

    
    if(found && start){
        auto swap       =   Symbols[start];        
        Symbols[start]  =   Symbols[fun];
        Symbols[fun]    =   swap;

        auto end        =   end_addr[start];
        end_addr[start] =   end_addr[fun];
        end_addr[fun]   =   end;
    }
    }



    for (unsigned si = 0, se = Symbols.size(); si != se; ++si) {
      uint64_t Start = Symbols[si].Addr - SectionAddr;
      // The end is either the section end or the beginning of the next
      // symbol.
      // uint64_t End =
      //     (si == se - 1) ? SectSize : Symbols[si + 1].Addr - SectionAddr;
      uint64_t End =
          (si == se - 1) ? SectSize : end_addr[si];
      // Don't try to disassemble beyond the end of section contents.
      if (End > SectSize)
        End = SectSize;
      // If this symbol has the same address as the next symbol, then skip it.

      
      if (Start >= End ){
        std::cout << "\tWARNING:: Function End Address < Start Address!!! Skipping Function:\t"
                  << Symbols[si].Name.str() << endl;
        continue;
      }
      // Check if we need to skip symbol
      // Skip if the symbol's data is not between StartAddress and StopAddress
      if (End + SectionAddr < StartAddress ||
          Start + SectionAddr > StopAddress) {
        continue;
      }

      // Stop disassembly at the stop address specified
      if (End + SectionAddr > StopAddress)
        End = StopAddress - SectionAddr;

      if (Obj->isELF() && Obj->getArch() == Triple::amdgcn) {
        // make size 4 bytes folded
        End = Start + ((End - Start) & ~0x3ull);
        if (Symbols[si].Type == ELF::STT_AMDGPU_HSA_KERNEL) {
          // skip amd_kernel_code_t at the begining of kernel symbol (256 bytes)
          Start += 256;
        }
        if (si == se - 1 ||
            Symbols[si + 1].Type == ELF::STT_AMDGPU_HSA_KERNEL) {
          // cut trailing zeroes at the end of kernel
          // cut up to 256 bytes
          const uint64_t EndAlign = 256;
          const auto Limit = End - (std::min)(EndAlign, End - Start);
          while (End > Limit && *reinterpret_cast<const support::ulittle32_t *>(
                                    &Bytes[End - 4]) == 0)
            End -= 4;
        }
      }


      LLVM_DEBUG(dbgs() << "\n encap1 \n");
      LLVM_DEBUG(dbgs() << Symbols[si].Name << "\n");
      cout << "encap1\t" << Symbols[si].Name.str() << endl;
      // COMMENTED FOR COMPLETE EXECUTION - XI
      // if(Symbols[si].Name == "load_gp")
      //     Symbols[si].Type = ELF::STT_FUNC;
      if(Symbols[si].Type == ELF::STT_FUNC){
          LLVM_DEBUG(dbgs() << "Symbol Type = ELF::STT_FUNC\n");
          cout << "\tSymbol Type = ELF::STT_FUNC\n";
      }
      if(Symbols[si].Type == ELF::STT_OBJECT){
          LLVM_DEBUG(dbgs() << "Symbol "<< Symbols[si].Name << " is a data object (variable, array, etc.)\n");
          cout <<  "\tSymbol "<< Symbols[si].Name.str() << " is a data object (variable, array, etc.)\n";
      }

    
      if (isAFunctionSymbol(Obj, Symbols[si])) {
        LLVM_DEBUG( dbgs() << "Entering function symbol\n");
        auto &SymStr = Symbols[si].Name;
        if(Symbols[si].Name == "load_gp") {
          LLVM_DEBUG( dbgs() << "load_gp is a function symbol\n");
        }
        if ((FilterFunctionSet.getNumOccurrences() != 0)) {
          // Check the symbol name whether it should be excluded or not.
          if (!FuncFilter->isFilterSetEmpty(FunctionFilter::FILTER_EXCLUDE)) {
            FunctionFilter::FuncInfo *FI = FuncFilter->findFuncInfoBySymbol(
                SymStr, FunctionFilter::FILTER_EXCLUDE);
            if (FI != nullptr) {
              // Record the function start index.
              FI->StartIdx = Start;
              LLVM_DEBUG( dbgs() << "DEBUG::Function will be excluded\n");
              cout << "DEBUG::Function will be excluded\n";
              continue;
            }
          }
          if(Symbols[si].Name == "load_gp")
            LLVM_DEBUG( dbgs() << "load_gp pass the function filter\n");
          // Check the symbol name whether it should be included or not.
          if (FuncFilter->findFuncInfoBySymbol(
              SymStr, FunctionFilter::FILTER_INCLUDE) == nullptr){
                  LLVM_DEBUG( dbgs() << "DEBUG::Function will be included\n");
                  cout << "DEBUG::Function will be included\n";
                  continue;
          }
        
        }

        LLVM_DEBUG(dbgs() << "check::" << SymStr << "\n");
        cout << "check if in ELFCRTSymbol::" << SymStr.str() << "\n";
        // If Symbol is in the ELFCRTSymbol list return this is a symbol of a
        // function we are not interested in disassembling and raising.
        if (ELFCRTSymbols.find(SymStr) != ELFCRTSymbols.end())
          continue;
        LLVM_DEBUG(dbgs() << "pass::" << SymStr << "\n");
        cout << "pass not in ELFCRTSymbol::" << SymStr.str() << "\n";
        // Note that since LLVM infrastructure was built to be used to build a
        // conventional compiler pipeline, MachineFunction is built well after
        // Function object was created and populated fully. Hence, creation of
        // a Function object is necessary to build MachineFunction.
        // However, in a raiser, we are conceptually walking the traditional
        // compiler pipeline backwards. So we build MachineFunction from
        // the binary before building Function object. Given the dependency,
        // build a place holder Function object to allow for building the
        // MachineFunction object.
        // This Function object is NOT populated when raising MachineFunction
        // abstraction of the binary function. Instead, a new Function is
        // created using the LLVMContext and name of this Function object.
        FunctionType *FTy = FunctionType::get(Type::getVoidTy(llvmCtx), false);
        StringRef FunctionName(Symbols[si].Name);
        // Strip leading underscore if the binary is MachO
        if (Obj->isMachO()) {
          FunctionName.consume_front("_");
        }
        Function *Func = Function::Create(FTy, GlobalValue::ExternalLinkage,
                                          FunctionName, &module);
        
        // 如果是FunctionSymbol，那么创建一个Func；先把当前的curMFRaiser对应的函数处理完（添加target）。然后clear branchTargetSet
        // 然后后面的for (Index = Start; Index < End; Index += Size)循环去遍历当前函数指令，插入branchTargetSet

        // New function symbol encountered. Record all targets collected to
        // current MachineFunctionRaiser before we start parsing the new
        // function bytes.
        curMFRaiser = moduleRaiser->getCurrentMachineFunctionRaiser();
        if(curMFRaiser != nullptr) 
          cout << "\tTargets of the function: " << curMFRaiser->getMachineFunction().getName().str() << endl;
        for (auto target : branchTargetSet) {
          assert(curMFRaiser != nullptr &&
                 "Encountered unintialized MachineFunction raiser object");
          curMFRaiser->getMCInstRaiser()->addTarget(target);
          cout << "\t\t" << target << endl;
        }

        // Build AssemblyCFG
        if(curMFRaiser != nullptr && excludeFunctionSet.count(curMFRaiser->getMachineFunction().getName().str()) == 0 && curMFRaiser->getMachineFunction().getName().str().rfind("__", 0) != 0) {
          AssemblyCFG* ass_CFG = new AssemblyCFG(textSectionFunctions.find(curMFRaiser->getMachineFunction().getName().str())->second, SectionAddr + curMFRaiser->getMCInstRaiser()->getFuncStart(), curMFRaiser->getMCInstRaiser()->getFuncStart(), SectionAddr + curMFRaiser->getMCInstRaiser()->getFuncEnd());
          
          cfgs.push_back(ass_CFG);
          printf("new AssemblyCFG: %s, At %d ~ %d\n", ass_CFG->getName().c_str(), ass_CFG->getStartAddress(), ass_CFG->getEndAddress());

          int lastInstr = 0;
          vector<AssemblyInstruction*>::iterator assInstrsOfFunctionIt = assInstrsOfFunction.begin();
          int sizeof_assInstrsOfFunction = 0;
          for(; assInstrsOfFunctionIt != assInstrsOfFunction.end(); assInstrsOfFunctionIt++) {
            sizeof_assInstrsOfFunction += (*assInstrsOfFunctionIt)->getSizeByte();
          }
          assInstrsOfFunctionIt = assInstrsOfFunction.begin();
          cout << "assInstrsOfFunction, number & size = " << assInstrsOfFunction.size() << ", "  << sizeof_assInstrsOfFunction << endl;
          // Make BB and fill instructions
          for (set<uint64_t>::iterator it = branchTargetSet.begin(); it != branchTargetSet.end(); ) {
            if(ass_CFG->getSize() == branchTargetSet.size() - 1) {
              break;
            }
            AssemblyBasicBlock* ass_bb = new AssemblyBasicBlock(ass_CFG->getSize(), *it, *(++it));
            while (assInstrsOfFunctionIt != assInstrsOfFunction.end() && (*assInstrsOfFunctionIt)->getAddress() < SectionAddr + *it) {
              ass_bb->addInstruction(*assInstrsOfFunctionIt);
              assInstrsOfFunctionIt++;
            }
            if(ass_CFG->getSize() >= 1) {
              AssemblyBasicBlock *lastBB = ass_CFG->getBasicBlocks().back();
              if(lastBB->getInstructions()->back()->getOpcode() != 0b1101111) {
              // if not j instruction: link lastBB and CurrentBB
                lastBB->addSuccessor(ass_bb);
                ass_bb->addPredecessor(lastBB);
              }
            }
            ass_CFG->addBasicBlock(ass_bb);
          }
          // Process last BB, address range: [branchTargetSet.last ~ branchTargetSet.first + sizeof_assInstrsOfFunction]
          if (branchTargetSet.size() >= 1) {
            auto itBegin = branchTargetSet.begin();
            auto itEnd = branchTargetSet.end();
            --itEnd;
            AssemblyBasicBlock* ass_bb = new AssemblyBasicBlock(ass_CFG->getSize(), *itEnd, sizeof_assInstrsOfFunction + *itBegin);
            while (assInstrsOfFunctionIt != assInstrsOfFunction.end() && (*assInstrsOfFunctionIt)->getAddress() < SectionAddr + ass_bb->getEndAddress()) {
              ass_bb->addInstruction(*assInstrsOfFunctionIt);
              assInstrsOfFunctionIt++;
            }
            AssemblyBasicBlock *lastBB = ass_CFG->getBasicBlocks().back();
            if(lastBB && lastBB->getInstructions()->back()->getOpcode() != 0b1101111) {
            // if not j instruction: link lastBB and CurrentBB
              lastBB->addSuccessor(ass_bb);
              ass_bb->addPredecessor(lastBB);
            }
            ass_CFG->addBasicBlock(ass_bb);
          }
          
          // Assign successor/predecessor
          vector<AssemblyBasicBlock*>& CFGBBs = ass_CFG->getBasicBlocks();
          int CFGBBsSize = CFGBBs.size();
          int curBBIndex = 0;
          // if no branch instruction, only 1 BB
          if (branchTargetSet.size() == 1) {
            // for(vector<AssemblyInstruction*>::iterator it = assInstrsOfFunction.begin(); it != assInstrsOfFunction.end(); it++) {
            //   CFGBBs[curBBIndex].addInstruction(*it);
            // }
          } else {
            for (vector<AssemblyInstruction*>::iterator it = assInstrsOfFunction.begin(); it != assInstrsOfFunction.end(); it++) {
              if ((*it)->getIsBranch()) {
                // update curBBIndex
                printf("branch instr itaddress: %x\n", (*it)->getAddress());
                printf("branch instr cfg address: %x\n", ass_CFG->getStartAddress());
                printf("branch instr cfg offset: %d\n", ass_CFG->getStartOffset());
                uint64_t itAddress = ((*it)->getAddress() - ass_CFG->getStartAddress()) + ass_CFG->getStartOffset();
                for (int i = 0; i < CFGBBsSize; i++) {
                  if (CFGBBs[i]->getEndAddress() > itAddress) {
                    curBBIndex = i;
                    break;
                  }
                }
                // Serch target in which BB
                uint64_t target = (*it)->getTarget();
                for (int i = 0; i < CFGBBsSize; i++) {
                  if (CFGBBs[i]->getEndAddress() > target) {
                    // CurrentBB and targetBB
                    CFGBBs[curBBIndex]->addSuccessor(CFGBBs[i]);
                    CFGBBs[i]->addPredecessor(CFGBBs[curBBIndex]);
                    // TargetBB and TargetBB-1
                    // // TODO: currently only support RISCV j instruction
                    // // if not j instruction: link CurrentBB and nextBB
                    // if(i > 0 && CFGBBs[i - 1]->getInstructions()->back() && CFGBBs[i - 1]->getInstructions()->back()->getOpcode() != 0b1101111){
                    //   CFGBBs[i - 1]->addSuccessor(CFGBBs[i]);
                    //   CFGBBs[i]->addPredecessor(CFGBBs[i - 1]);
                    // }
                    // // if not j instruction: link CurrentBB and nextBB
                    // if(CFGBBs[curBBIndex]->getInstructions()->back() && CFGBBs[curBBIndex]->getInstructions()->back()->getOpcode() != 0b1101111 && curBBIndex < CFGBBsSize - 1 && i != curBBIndex + 1) {
                    //   cout << "addSuccessor: curBBIndex:" << curBBIndex << " succ: " << (curBBIndex+1) << ", instr_addr: " <<  (*it)->getAddress() << endl;
                    //   CFGBBs[curBBIndex]->addSuccessor(CFGBBs[curBBIndex + 1]);
                    //   CFGBBs[curBBIndex + 1]->addPredecessor(CFGBBs[curBBIndex]);
                    // }
                    curBBIndex++;
                    break;
                  }
                }
              }
              if ((*it)->getIsCall()) {
                // update curBBIndex
                uint64_t itAddress = ((*it)->getAddress() - ass_CFG->getStartAddress()) + ass_CFG->getStartOffset();
                for (int i = 0; i < CFGBBsSize; i++) {
                  if (CFGBBs[i]->getEndAddress() > itAddress) {
                    curBBIndex = i;
                    break;
                  }
                }
                // Add callTarget info in this bb
                CFGBBs[curBBIndex]->setCallTarget((*it)->getCallTarget());
                // Link currentBB and nextBB
                // if(curBBIndex < CFGBBsSize - 1) {
                //   cout << "addSuccessor: curBBIndex:" << curBBIndex << " succ: " << (curBBIndex+1) << ", instr_addr: " <<  (*it)->getAddress() << endl;
                //   CFGBBs[curBBIndex]->addSuccessor(CFGBBs[curBBIndex + 1]);
                //   CFGBBs[curBBIndex + 1]->addPredecessor(CFGBBs[curBBIndex]);
                // }
              }
            }
          }

          
          ass_CFG->FindPrologue();
          
          ass_CFG->ProcessFuncCall();


          for(auto i = CFGBBs.begin();i!=CFGBBs.end();i++){
              cout << "BB[" << (*i)->getId() << "] successors size: " << (*i)->getSuccessors().size() << endl;
              (*i)->BuildLocalEdge();
          }
          printf("Local Edge Build for All BB Completes!");

          for(auto i = CFGBBs.begin();i!=CFGBBs.end();i++){
              (*i)->BuildGlobalEdge();
          }

          printf("Global Edge Build done!");
   

          for(auto i = CFGBBs.begin();i!=CFGBBs.end();i++){
              (*i)->BuildPhiNodes();
          }
          printf("Phi Node Building for All BB Completes!");
         
        
          if(found){
            ass_CFG->ProcessRISCVGP();
            found = 0;
          }

          
          ass_CFG->TraverseLoadStore();


          
          ass_CFG->FindRet();
          //ass_CFG->FindEpilogue();


          GlobalDataDump();
          DataSectionDump();
          ass_CFG->dump();
          allCFGs.push_back(ass_CFG);
      
          printf("dumping each BB\n");
          for(auto i = CFGBBs.begin();i!=CFGBBs.end();i++) {
            (*i)->dump();
          } 
          printf("dumping each BB end\n");
          assInstrsOfFunction.clear();
        }
        // Clear the set used to record all branch targets of this function.
        branchTargetSet.clear();
        // Create a new MachineFunction raiser
        curMFRaiser = moduleRaiser->CreateAndAddMachineFunctionRaiser(
            Func, moduleRaiser, Start, End);
        LLVM_DEBUG(dbgs() << "\nFunction " << Symbols[si].Name << ":\n");
        cout << "Function: " << Symbols[si].Name.str() << ":\n";
      } else {
        // Continue using to the most recent MachineFunctionRaiser
        // Get current MachineFunctionRaiser
        curMFRaiser = moduleRaiser->getCurrentMachineFunctionRaiser();
        // assert(curMFRaiser != nullptr && "Current Machine Function Raiser not
        // initialized");
        if (curMFRaiser == nullptr) {
          // At this point in the instruction stream, we do not have a function
          // symbol to which the bytes being parsed can be made part of. So skip
          // parsing the bytes of this symbol.
          continue;
        }

        // Adjust function end to represent the addition of the content of the
        // current symbol. This represents a situation where we have discovered
        // bytes (most likely data bytes) that belong to the most recent
        // function being parsed.
        MCInstRaiser *mcInstRaiser = curMFRaiser->getMCInstRaiser();
        if (mcInstRaiser->getFuncEnd() < End) {
          assert(mcInstRaiser->adjustFuncEnd(End) &&
                 "Unable to adjust function end value");
        }
      }
      LLVM_DEBUG(dbgs() << "\n encap2 \n");
      cout << "encap2\t" << Symbols[si].Name.str() << endl;
      // Get the associated MCInstRaiser
      MCInstRaiser *mcInstRaiser = curMFRaiser->getMCInstRaiser();

      // Start new basic block at the symbol.
      branchTargetSet.insert(Start);

      for (Index = Start; Index < End; Index += Size) {
        MCInst Inst;

        if (Index + SectionAddr < StartAddress || Index + SectionAddr > StopAddress) {
          // skip byte by byte till StartAddress is reached
          Size = 1;
          continue;
        }

        // AArch64 ELF binaries can interleave data and text in the
        // same section. We rely on the markers introduced to
        // understand what we need to dump. If the data marker is within a
        // function, it is denoted as a word/short etc
        if (isArmElf(Obj) && Symbols[si].Type != ELF::STT_OBJECT) {
          uint64_t Stride = 0;

          auto DAI = std::lower_bound(DataMappingSymsAddr.begin(),
                                      DataMappingSymsAddr.end(), Index);
          if (DAI != DataMappingSymsAddr.end() && *DAI == Index) {
            // Switch to data.
            while (Index < End) {
              if (Index + 4 <= End) {
                Stride = 4;
                uint32_t Data = 0;
                if (Obj->isLittleEndian()) {
                  const auto Word =
                      reinterpret_cast<const support::ulittle32_t *>(
                          Bytes.data() + Index);
                  Data = *Word;
                } else {
                  const auto Word = reinterpret_cast<const support::ubig32_t *>(
                      Bytes.data() + Index);
                  Data = *Word;
                }
                mcInstRaiser->addMCInstOrData(Index, Data);
                LLVM_DEBUG(dbgs() << "Data0:" << Data << "\n");
                LLVM_DEBUG(dbgs() << "\nFunction " << curMFRaiser->getMachineFunction().getName() << ":\n");
                cout << "\tData0:" << Data << "\tFunction " << curMFRaiser->getMachineFunction().getName().str() << ":\n";
              } else if (Index + 2 <= End) {
                Stride = 2;
                uint16_t Data = 0;
                if (Obj->isLittleEndian()) {
                  const auto Short =
                      reinterpret_cast<const support::ulittle16_t *>(
                          Bytes.data() + Index);
                  Data = *Short;
                } else {
                  const auto Short =
                      reinterpret_cast<const support::ubig16_t *>(Bytes.data() +
                                                                  Index);
                  Data = *Short;
                }
                mcInstRaiser->addMCInstOrData(Index, Data);
                LLVM_DEBUG(dbgs() << "Data1:" << Data << "\n");
                cout << "Data1:" << Data << "\n";
              } else {
                Stride = 1;
                mcInstRaiser->addMCInstOrData(Index, Bytes.slice(Index, 1)[0]);
                LLVM_DEBUG(dbgs() << "Data2:sdfds" << Bytes.slice(Index, 1)[0] << "\n");
                cout << "Data2:sdfds" << Bytes.slice(Index, 1)[0] << "\n";
              }
              Index += Stride;

              auto TAI = std::lower_bound(TextMappingSymsAddr.begin(),
                                          TextMappingSymsAddr.end(), Index);
              if (TAI != TextMappingSymsAddr.end() && *TAI == Index)
                break;
            }
          }
        }

        // If there is a data symbol inside an ELF text section and we are
        // only disassembling text, we are in a situation where we must print
        // the data and not disassemble it.
        // TODO : Get rid of the following code in the if-block.
        if (Obj->isELF() && Symbols[si].Type == ELF::STT_OBJECT &&
            Section.isText()) {
          // parse data up to 8 bytes at a time
          uint8_t AsciiData[9] = {'\0'};
          uint8_t Byte;
          int NumBytes = 0;

          for (Index = Start; Index < End; Index += 1) {
            if (((SectionAddr + Index) < StartAddress) ||
                ((SectionAddr + Index) > StopAddress))
              continue;
            if (NumBytes == 0) {
              outs() << format("%8" PRIx64 ":", SectionAddr + Index);
              outs() << "\t";
            }
            Byte = Bytes.slice(Index)[0];
            outs() << format(" %02x", Byte);
            AsciiData[NumBytes] = isprint(Byte) ? Byte : '.';

            uint8_t IndentOffset = 0;
            NumBytes++;
            if (Index == End - 1 || NumBytes > 8) {
              // Indent the space for less than 8 bytes data.
              // 2 spaces for byte and one for space between bytes
              IndentOffset = 3 * (8 - NumBytes);
              for (int Excess = 8 - NumBytes; Excess < 8; Excess++)
                AsciiData[Excess] = '\0';
              NumBytes = 8;
            }
            if (NumBytes == 8) {
              AsciiData[8] = '\0';
              outs() << std::string(IndentOffset, ' ') << "         ";
              outs() << reinterpret_cast<char *>(AsciiData);
              outs() << '\n';
              NumBytes = 0;
            }
          }
        }

        if (Index >= End)
          break;
          //continue;

        // Disassemble a real instruction or a data
        bool Disassembled = DisAsm->getInstruction(
            Inst, Size, Bytes.slice(Index), SectionAddr + Index, CommentStream);
        if (Size == 0)
          Size = 1;
        PIP.printInst(*IP, Disassembled ? &Inst : nullptr,
                        Bytes.slice(Index, Size), SectionAddr + Index, outs(),
                        "", *STI);     

        if (!Disassembled) {
          errs() << "**** Warning: Failed to decode last instruction\n";
          outs() << CommentStream.str();
          Comments.clear();
          errs() << "\n";
        }

        // Add MCInst to the list if all instructions were decoded
        // successfully till now. Else, do not bother adding since no attempt
        // will be made to raise this function.
        if (Disassembled) {
          mcInstRaiser->addMCInstOrData(Index, Inst);
          AssemblyInstruction *ass_ins = new AssemblyInstruction(*MRI, *IP, &Inst, Bytes.slice(Index, Size), SectionAddr + Index);

          // Find branch target and record it. Call targets are not
          // recorded as they are not needed to build per-function CFG.
          if (MIA && MIA->isCall(Inst) && !MIA->isReturn(Inst)) {
            // Include user functions call, lib functions call and ret; jal, jalr, ret and j
            uint64_t Target = ass_ins->getAddress() + ass_ins->getImm();
             AssemblyFunction* targetFunc = nullptr;
            if(Target >= pltStart && Target < pltEnd) {
              std::string FuncName = MatchPLTFunction(Target);
              if(FuncName!="NULL")
                // callTarget is in plt
                cout << "\tDEBUG:: Matching PLT Function \t" << FuncName << endl;
                targetFunc =  new AssemblyFunction("plt_function("+FuncName+")" , 0, 0);
            }else {
              for(std::map<string, AssemblyFunction*>::iterator it = textSectionFunctions.begin(); it != textSectionFunctions.end(); it++) {
                if(Target == it->second->getStartAddress()) {
                  targetFunc = it->second;
                  cout << "Found Target Function: " << targetFunc->getName() << endl;
                  break;
                }
              }
            }
            if(targetFunc){
              ass_ins->setCall(Target, targetFunc);
              cout << "\tDEBUG::\tInst "<< ass_ins->getMnemonic() << " Calling Function " 
                   << ass_ins->getCallTarget()->getName() <<endl;

              ass_ins->dump();
              uint64_t fallThruIndex = Index + Size;
              // branchTargetSet.insert(fallThruIndex);    // Because LLVM_IR not divide function call into 2 BBs
              cout << "Mark a target (next instr after Call): " << fallThruIndex << endl;
            }
          }

          if (MIA && MIA->isBranch(Inst)) {
            uint64_t Target;
            if (MIA->evaluateBranch(Inst, Index, Size, Target)) {
              // In a relocatable object, the target's section must reside in
              // the same section as the call instruction or it is accessed
              // through a relocation.
              //
              // In a non-relocatable object, the target may be in any
              // section.
              //
              // N.B. We don't walk the relocations in the relocatable case
              // yet.
              cout << "Evaluate Branch: " << Target << endl;
              ass_ins->setBranch(Target);
              if (!Obj->isRelocatableObject()) {
                auto SectionAddress = std::upper_bound(
                    SectionAddresses.begin(), SectionAddresses.end(), Target,
                    [](uint64_t LHS,
                       const std::pair<uint64_t, SectionRef> &RHS) {
                      return LHS < RHS.first;
                    });
                if (SectionAddress != SectionAddresses.begin()) {
                  --SectionAddress;
                }
              }
              // Add the index Target to target indices set.
              branchTargetSet.insert(Target);
            }

            // Mark the next instruction as a target.
            uint64_t fallThruIndex = Index + Size;
            branchTargetSet.insert(fallThruIndex);
            cout << "Mark a target: " << fallThruIndex << endl;
          }
          // 如果是J指令
          if(ass_ins->getOpcode() == 0b1101111) {
            
          }
          assInstrsOfFunction.push_back(ass_ins);
        }
      }
      FuncFilter->eraseFunctionBySymbol(Symbols[si].Name,
                                        FunctionFilter::FILTER_INCLUDE);
    }
    LLVM_DEBUG(dbgs() << "END Disassembly of Functions in Section : "
                      << SectionName.data() << "\n");


    // Record all targets of the last function parsed
    curMFRaiser = moduleRaiser->getCurrentMachineFunctionRaiser();
    if(curMFRaiser != nullptr) cout << "\tTargets of the last function: " << curMFRaiser->getMachineFunction().getName().str() << endl;
    for (auto target : branchTargetSet){
      curMFRaiser->getMCInstRaiser()->addTarget(target);
      cout << "\t\t" << target << endl;
    }

    if(curMFRaiser != nullptr && excludeFunctionSet.count(curMFRaiser->getMachineFunction().getName().str()) == 0 && curMFRaiser->getMachineFunction().getName().str().rfind("__", 0) != 0) {
      // Build AssemblyCFG of the last function parsed
      AssemblyCFG* ass_CFG = new AssemblyCFG(textSectionFunctions.find(curMFRaiser->getMachineFunction().getName().str())->second, SectionAddr +  curMFRaiser->getMCInstRaiser()->getFuncStart(), curMFRaiser->getMCInstRaiser()->getFuncStart(), SectionAddr + curMFRaiser->getMCInstRaiser()->getFuncEnd());

      cfgs.push_back(ass_CFG);
      printf("new AssemblyCFG: %s, At %d ~ %d\n", ass_CFG->getName().c_str(), ass_CFG->getStartAddress(), ass_CFG->getEndAddress());

      int lastInstr = 0;
      vector<AssemblyInstruction*>::iterator assInstrsOfFunctionIt = assInstrsOfFunction.begin();
      int sizeof_assInstrsOfFunction = 0;
      for(; assInstrsOfFunctionIt != assInstrsOfFunction.end(); assInstrsOfFunctionIt++) {
        sizeof_assInstrsOfFunction += (*assInstrsOfFunctionIt)->getSizeByte();
      }
      assInstrsOfFunctionIt = assInstrsOfFunction.begin();
      cout << "assInstrsOfFunction, number & size = " << assInstrsOfFunction.size() << ", "  << sizeof_assInstrsOfFunction << endl;
      // Make BB
      for (set<uint64_t>::iterator it = branchTargetSet.begin(); it != branchTargetSet.end(); ) {
        if(ass_CFG->getSize() == branchTargetSet.size() - 1) {
          break;
        }
        AssemblyBasicBlock* ass_bb = new AssemblyBasicBlock(ass_CFG->getSize(), *it, *(++it));
        while (assInstrsOfFunctionIt != assInstrsOfFunction.end() && (*assInstrsOfFunctionIt)->getAddress() < SectionAddr + *it) {
          ass_bb->addInstruction(*assInstrsOfFunctionIt);
          assInstrsOfFunctionIt++;
        }
        if(ass_CFG->getSize() >= 1) {
          AssemblyBasicBlock *lastBB = ass_CFG->getBasicBlocks().back();
          if(lastBB->getInstructions()->back()->getOpcode() != 0b1101111) {
          // if not j instruction: link lastBB and CurrentBB
            lastBB->addSuccessor(ass_bb);
            ass_bb->addPredecessor(lastBB);
          }
        }
        ass_CFG->addBasicBlock(ass_bb);
      }
      // Process last BB, addr range: [branchTargetSet.last ~ branchTargetSet.first + sizeof_assInstrsOfFunction]
      if (branchTargetSet.size() >= 1) {
        auto itBegin = branchTargetSet.begin();
        auto itEnd = branchTargetSet.end();
        --itEnd;
        AssemblyBasicBlock* ass_bb = new AssemblyBasicBlock(ass_CFG->getSize(), *itEnd, sizeof_assInstrsOfFunction + *itBegin);
        printf("Last ass_bb: %d, %d ~ %d\n", ass_bb->getId(), ass_bb->getStartAddress(), ass_bb->getEndAddress());
        while (assInstrsOfFunctionIt != assInstrsOfFunction.end() && (*assInstrsOfFunctionIt)->getAddress() < SectionAddr + ass_bb->getEndAddress()) {
          ass_bb->addInstruction(*assInstrsOfFunctionIt);
          assInstrsOfFunctionIt++;
        }
        if(ass_CFG->getSize() >= 1) {
          AssemblyBasicBlock *lastBB = ass_CFG->getBasicBlocks().back();
          if(lastBB->getInstructions()->back()->getOpcode() != 0b1101111) {
          // if not j instruction: link lastBB and CurrentBB
            lastBB->addSuccessor(ass_bb);
            ass_bb->addPredecessor(lastBB);
          }
        }
        ass_CFG->addBasicBlock(ass_bb);
      }
      
      // Assign successor/predecessor
      vector<AssemblyBasicBlock*>& CFGBBs = ass_CFG->getBasicBlocks();
      int CFGBBsSize = CFGBBs.size();
      int curBBIndex = 0;

      // if no branch instruction, only 1 BB
      if (branchTargetSet.size() == 1) {
        // for(vector<AssemblyInstruction*>::iterator it = assInstrsOfFunction.begin(); it != assInstrsOfFunction.end(); it++) {
        //   CFGBBs[curBBIndex].addInstruction(*it);
        // }
      } else {
        for(vector<AssemblyInstruction*>::iterator it = assInstrsOfFunction.begin(); it != assInstrsOfFunction.end(); it++) {
          if ((*it)->getIsBranch()) {
            // updqate curBBIndex
            printf("branch instr itaddress: %x\n", (*it)->getAddress());
            printf("branch instr cfg address: %x\n", ass_CFG->getStartAddress());
            printf("branch instr cfg offset: %d\n", ass_CFG->getStartOffset());
            uint64_t itAddress = ((*it)->getAddress() - ass_CFG->getStartAddress()) + ass_CFG->getStartOffset();
            for (int i = 0; i < CFGBBsSize; i++) {
              // printf("itAddress(hex): %d, endAddress: %d\n", itAddress, CFGBBs[i].getEndAddress());
              if (CFGBBs[i]->getEndAddress() > itAddress) {
                curBBIndex = i;
                break;
              }
            }
            // Serch target in which BB
            uint64_t target = (*it)->getTarget();
            for (int i = 0; i < CFGBBsSize; i++) {
              if(CFGBBs[i]->getEndAddress() > target) {
                CFGBBs[curBBIndex]->addSuccessor(CFGBBs[i]);
                CFGBBs[i]->addPredecessor(CFGBBs[curBBIndex]);
                // TargetBB and TargetBB-1
                // TODO: currently only support RISCV j instruction
                // if not j instruction: link i-1 and i
                if(i > 0 && CFGBBs[i - 1]->getInstructions()->back()->getOpcode() != 0b1101111){
                  CFGBBs[i - 1]->addSuccessor(CFGBBs[i]);
                  CFGBBs[i]->addPredecessor(CFGBBs[i - 1]);
                }
                curBBIndex++;
                break;
              }
            }
          }
          if ((*it)->getIsCall()) {
            // update curBBIndex
            uint64_t itAddress = ((*it)->getAddress() - ass_CFG->getStartAddress()) + ass_CFG->getStartOffset();
            for (int i = 0; i < CFGBBsSize; i++) {
              if (CFGBBs[i]->getEndAddress() > itAddress) {
                curBBIndex = i;
                break;
              }
            }
            // Add callTarget info in this bb
            CFGBBs[curBBIndex]->setCallTarget((*it)->getCallTarget());
          }
        }
      }


      ass_CFG->FindPrologue();
      
      // Process JAL ra, imm => Call a0/fa0, imm
      ass_CFG->ProcessFuncCall();

      for(auto i = CFGBBs.begin();i!=CFGBBs.end();i++){
          (*i)->BuildLocalEdge();
      }
      printf("Local Edge Build done!\n");
      for(auto i = CFGBBs.begin();i!=CFGBBs.end();i++){
          (*i)->BuildGlobalEdge();
      }
      printf("Global Edge Build done!\n");

      //ass_CFG->dump();

      for(auto i = CFGBBs.begin();i!=CFGBBs.end();i++){
          (*i)->BuildPhiNodes();
      }
      printf("Phi Node Building for All BB Completes!\n\n");
      

      //  if(ISA_type>=3)
      //    ass_CFG->ProcessRISCVGP();


      ass_CFG->FindRet();
      ass_CFG->TraverseLoadStore();

      //ass_CFG->FindEpilogue();
      printf("Global Data Dump:\n");

      GlobalDataDump();
      DataSectionDump();
      ass_CFG->dump();
      allCFGs.push_back(ass_CFG);
    }
    for(vector<AssemblyCFG*>::iterator it = allCFGs.begin(); it != allCFGs.end(); it++) {
      cout << (*it)->getName() << ",";
      printf("from %x to %x\n", (*it)->getStartAddress(), (*it)->getEndAddress());
    }
    try {
      // DumpTIR();
    }catch(const char* msg) {
      cout << msg << endl;
    }


    assInstrsOfFunction.clear();


    LLVM_DEBUG(dbgs() << "END Disassembly of Functions in Section : "
                      << SectionName.data() << "\n");
    cout << "END Disassembly of Functions in Section : " << SectionName.data() << "\n";
    moduleRaiser->runMachineFunctionPasses();

    if (!FuncFilter->isFilterSetEmpty(FunctionFilter::FILTER_INCLUDE)) {
      errs() << "***** WARNING: The following include filter symbol(s) are not "
                "found :\n";
      FuncFilter->dump(FunctionFilter::FILTER_INCLUDE);
    }
    LLVM_DEBUG(dbgs() << "DEBUG::One Iteration of the Disassembly Loop Ends \n");
    cout << "DEBUG::One Iteration of the Disassembly Loop Ends \n";
  }
  
  LLVM_DEBUG(dbgs() << "DEBUG::The Entire Disassembly Loop Completes\n");
  cout << "DEBUG::The Entire Disassembly Loop Completes\n";

  // Add the pass manager
  Triple TheTriple = Triple(TripleName);

  // Decide where to send the output.
  std::unique_ptr<ToolOutputFile> Out =
      GetOutputStream(TheTarget->getName(), TheTriple.getOS(), ToolName.data());

  if (!Out)
    return;

  // Keep the file created.
  Out->keep();

  raw_pwrite_stream *OS = &Out->os();

  legacy::PassManager PM;

  LLVMTargetMachine &LLVMTM = static_cast<LLVMTargetMachine &>(*Target);

  if (RunPassNames->empty()) {
    TargetPassConfig &TPC = *LLVMTM.createPassConfig(PM);
    if (TPC.hasLimitedCodeGenPipeline()) {
      errs() << ToolName << ": run-pass cannot be used with "
             << TPC.getLimitedCodeGenPipelineReason(" and ") << ".\n";
      return;
    }

    TPC.setDisableVerify(NoVerify);
    PM.add(&TPC);
    PM.add(machineModuleInfo);

    // Add print pass to emit ouptut file.
    PM.add(new EmitRaisedOutputPass(*OS, OutputFormat));

    TPC.printAndVerify("");
    for (const std::string &RunPassName : *RunPassNames) {
      if (addPass(PM, ToolName, RunPassName, TPC))
        return;
    }

    TPC.setInitialized();
  } else if (Target->addPassesToEmitFile(
                 PM, *OS, nullptr, /* no dwarf output file stream*/
                 OutputFormat, NoVerify, machineModuleInfo)) {
    outs() << ToolName << "run system pass!\n";
  }

  cl::PrintOptionValues();
  PM.run(module);
}



static void DumpObject(ObjectFile *o, const Archive *a = nullptr) {
  // Avoid other output when using a raw option.
  LLVM_DEBUG(dbgs() << '\n');
  if (a)
    LLVM_DEBUG(dbgs() << a->getFileName() << "(" << o->getFileName() << ")");
  else
    LLVM_DEBUG(dbgs() << "; " << o->getFileName());
  LLVM_DEBUG(dbgs() << ":\tfile format " << o->getFileFormatName() << "\n\n");

  assert(Disassemble && "Disassemble not set!");
  DisassembleObject(o, /* InlineRelocations */ false);
}

static void DumpObject(const COFFImportFile *I, const Archive *A) {
  assert(false &&
         "This function needs to be deleted and is not expected to be called.");
}

/// @brief Dump each object file in \a a;
static void DumpArchive(const Archive *a) {
  Error Err = Error::success();
  for (auto &C : a->children(Err)) {
    Expected<std::unique_ptr<Binary>> ChildOrErr = C.getAsBinary();
    if (!ChildOrErr) {
      if (auto E = isNotObjectErrorInvalidFileType(ChildOrErr.takeError()))
        report_error(std::move(E), a->getFileName(), C);
      continue;
    }
    if (ObjectFile *o = dyn_cast<ObjectFile>(&*ChildOrErr.get()))
      DumpObject(o, a);
    else if (COFFImportFile *I = dyn_cast<COFFImportFile>(&*ChildOrErr.get()))
      DumpObject(I, a);
    else
      report_error(errorCodeToError(object_error::invalid_file_type),
                   a->getFileName());
  }
  if (Err)
    report_error(std::move(Err), a->getFileName());
}

/// @brief Open file and figure out how to dump it.
static void DumpInput(StringRef file) {
  // If we are using the Mach-O specific object file parser, then let it parse
  // the file and process the command line options.  So the -arch flags can
  // be used to select specific slices, etc.
  if (MachOOpt) {
    parseInputMachO(file);
    return;
  }

  // Attempt to open the binary.
  Expected<OwningBinary<Binary>> BinaryOrErr = createBinary(file); // cycnote: many llvm-tool all have this setence, Even DumpInput s are similar
  if (!BinaryOrErr)
    report_error(BinaryOrErr.takeError(), file);
  Binary &Binary = *BinaryOrErr.get().getBinary();

  if (Archive *a = dyn_cast<Archive>(&Binary))
    DumpArchive(a);
  else if (ObjectFile *o = dyn_cast<ObjectFile>(&Binary)) {
    if (o->getArch() == Triple::x86_64) {
      ISA_type = 1;
      const ELF64LEObjectFile *Elf64LEObjFile = dyn_cast<ELF64LEObjectFile>(o);
      // Raise x86_64 relocatable binaries (.o files) is not supported.
      auto EType = Elf64LEObjFile->getELFFile().getHeader().e_type;
      if ((EType == ELF::ET_DYN) || (EType == ELF::ET_EXEC))
        DumpObject(o);
      else {
        errs() << "Raising x64 relocatable (.o) x64 binaries not supported\n";
        exit(1);
      }
    } else if (o->getArch() == Triple::arm){
      ISA_type = 2;
      DumpObject(o);
    } else if (o->getArch() == Triple::riscv32){
      ISA_type = 3;
      LLVM_DEBUG(dbgs() << "riscv32 start.\n");
      RISCV_ISA = true;
      DumpObject(o);
    } else if (o->getArch() == Triple::riscv64){
      ISA_type = 4;
      LLVM_DEBUG(dbgs() << "riscv64 start.\n");
      RISCV_ISA = true;
      DumpObject(o);
    } else {
      errs() << o->getArch();
      errs() << "No support to raise Binaries other than x64 and ARM\n";
      exit(1);
    }
  } else
    report_error(errorCodeToError(object_error::invalid_file_type), file);
}



int main(int argc, char **argv) {
  // Print a stack trace if we signal out.
  sys::PrintStackTraceOnErrorSignal(argv[0]);
  PrettyStackTraceProgram X(argc, argv);
  llvm_shutdown_obj Y; // Call llvm_shutdown() on exit.

  ELFPATH = argv[1];

  // Initialize targets and assembly printers/parsers.
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllDisassemblers();

  // Register the target printer for --version.
  cl::AddExtraVersionPrinter(TargetRegistry::printRegisteredTargetsForVersion);

  cl::HideUnrelatedOptions(LLVMMCToLLCategory);

  cl::ParseCommandLineOptions(argc, argv, "MC to LLVM IR dumper\n");

  ToolName = argv[0];

  // Defaults to a.out if no filenames specified.
  if (InputFilenames.size() == 0)
    InputFilenames.push_back("a.out");

  // Read declarations in user-specified include files
  std::set<string> InclFNameSet(IncludeFileNames.begin(),
                                IncludeFileNames.end());
  std::vector<string> InclFNames(InclFNameSet.begin(), InclFNameSet.end());
  if (!InclFNames.empty()) {
    if (!ExternalFunctions::getUserSpecifiedFuncPrototypes(InclFNames)) {
      dbgs() << "Unable to read external function prototype. Ignoring\n";
    }
  }

  // Disassemble contents of .text section.
  Disassemble = true;
  FilterSections.addValue(".text");
  // Add other sections to be disassembled
  FilterSections.addValue(".sdata");
  FilterSections.addValue(".sbss");
  FilterSections.addValue(".bss");
  FilterSections.addValue(".data");
  FilterSections.addValue(".rodata");

  llvm::setCurrentDebugType(DEBUG_TYPE);
  std::for_each(InputFilenames.begin(), InputFilenames.end(), DumpInput);

  return EXIT_SUCCESS;
}
#undef DEBUG_TYPE
