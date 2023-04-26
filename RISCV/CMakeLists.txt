include_directories(
  ${LLVM_MAIN_SRC_DIR}/lib/Target/ARM
  ${LLVM_BINARY_DIR}/lib/Target/ARM
  ${LLVM_MAIN_SRC_DIR}/lib/Target/RISCV
  ${LLVM_BINARY_DIR}/lib/Target/RISCV
  ${CMAKE_CURRENT_SOURCE_DIR}/..
  # ${CMAKE_CURRENT_SOURCE_DIR}/.
  ${CMAKE_CURRENT_SOURCE_DIR}/DAG
)

if(NOT LLVM_MCTOLL_BUILT_STANDALONE)
  set(LLVM_MCTOLL_DEPS intrinsics_gen RISCVCommonTableGen)
endif()

llvm_add_library(RISCVRaiser STATIC
  RISCVModuleRaiser.cpp
  RISCV32ModuleRaiser.cpp
  RISCV32MachineInstructionRaiser.cpp
  RISCV32FunctionPrototype.cpp
  RISCV32MIRevising.cpp
  RISCV32EliminatePrologEpilog.cpp
  RISCV32CreateJumpTable.cpp
  RISCV32ArgumentRaiser.cpp
  RISCV32FrameBuilder.cpp
  RISCV32InstructionSplitting.cpp
  RISCV32SelectionDAGISel.cpp

  DAG/RISCVFunctionRaisingInfo.cpp
  DAG/RISCVDAGRaisingInfo.cpp
  DAG/RISCVDAGBuilder.cpp
  DAG/RISCVInstSelector.cpp
  DAG/RISCVIREmitter.cpp
  
  RISCV64MachineInstructionRaiser.cpp
  RISCV64ModuleRaiser.cpp
  RISCVCallingConvention.cpp

  DEPENDS
    ${LLVM_MCTOLL_DEPS}
  )
