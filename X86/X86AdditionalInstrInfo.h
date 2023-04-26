//===-- X86AdditionalInstrInfo.h --------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// The X86AdditionalInstrInfo class contains information about X86 instructions
// that are not available from tblgen generated tables.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_LLVM_MCTOLL_X86_X86ADDITIONALINSTRINFO_H
#define LLVM_TOOLS_LLVM_MCTOLL_X86_X86ADDITIONALINSTRINFO_H

#include <cassert>
#include <cstdint>
#include <llvm/ADT/DenseMap.h>

namespace mctoll {

// Instruction Kinds
enum InstructionKind : uint8_t {
  Unknown = 0,
  BINARY_OP_RM,
  BINARY_OP_RR,
  BINARY_OP_WITH_IMM,
  BINARY_OP_MRI_OR_MRC, // binary operation instructions with MRI/MRC encoding
  BRANCH_MEM_OP,
  COMPARE,
  CONVERT_BWWDDQ,
  CONVERT_WDDQQO,
  DIVIDE_MEM_OP,
  DIVIDE_REG_OP,
  FPU_REG_OP,
  LEA_OP,
  LEAVE_OP,
  LOAD_FPU_REG,
  MOV_RR,
  MOV_RI,
  MOV_TO_MEM,
  MOV_FROM_MEM,
  NOOP,
  INPLACE_MEM_OP,
  SETCC,
  STORE_FPU_REG,
  SSE_KIND_BEGIN,
  SSE_MOV_TO_MEM,
  SSE_MOV_FROM_MEM,
  SSE_COMPARE,
  SSE_CONVERT_SS2SD,
  SSE_KIND_END,
  INSTR_KIND_END
};

struct X86AdditionalInstrInfo {
  // A vaue of 8 or 4 or 2 or 1 indicates the size of memory an instruction
  // operates on. A value of 0 indicates that the instruction does not have
  // memory operands.
  uint8_t MemOpSize;
  // Instruction kind
  InstructionKind InstKind;
  // Add any necessary additional instruction related data as fields of this
  // structure.
};

using const_addl_instr_info =
    const llvm::DenseMap<uint16_t, X86AdditionalInstrInfo>;

extern const const_addl_instr_info X86AddlInstrInfo;

static inline InstructionKind getInstructionKind(unsigned int Opcode) {
  auto Iter = mctoll::X86AddlInstrInfo.find((uint16_t)Opcode);
  assert(Iter != mctoll::X86AddlInstrInfo.end() && "Unknown opcode");
  return Iter->second.InstKind;
}

static inline unsigned short getInstructionMemOpSize(unsigned int Opcode) {
  auto Iter = mctoll::X86AddlInstrInfo.find((uint16_t)Opcode);
  assert(Iter != mctoll::X86AddlInstrInfo.end() && "Unknown opcode");
  return Iter->second.MemOpSize;
}

static inline bool isNoop(unsigned int Opcode) {
  return (getInstructionKind(Opcode) == mctoll::InstructionKind::NOOP);
}

static inline bool isSSE2Instruction(unsigned int Opcode) {
	auto Kind = getInstructionKind(Opcode);
	return ((Kind > InstructionKind::SSE_KIND_BEGIN) &&
			(Kind < InstructionKind::SSE_KIND_END));
}
} // namespace mctoll

#endif // LLVM_TOOLS_LLVM_MCTOLL_X86_X86ADDITIONALINSTRINFO_H
