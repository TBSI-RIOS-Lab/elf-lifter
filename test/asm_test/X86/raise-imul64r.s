# REQUIRES: x86_64-linux
# RUN: clang -o %t %s
# RUN: llvm-mctoll -d -I /usr/include/stdio.h %t
# RUN: clang -o %t-dis %t-dis.ll
# RUN: %t-dis 2>&1 | FileCheck %s
# CHECK: ret val: 0

	.text
	.file	"test-and.c"
	.globl	call_me                 # -- Begin function call_me
	.p2align	4, 0x90
	.type	call_me,@function
call_me:                                # @call_me
	.cfi_startproc
# %bb.0:                                # %entry
	addq	$5, (%rdi)
	movl	$4, %esi
	andl	(%rdi), %esi
	movl	%esi, (%rdi)
	movzwl	(%rdi), %eax
                                        # kill: def $eax killed $eax killed $rax
	retq
.Lfunc_end0:
	.size	call_me, .Lfunc_end0-call_me
	.cfi_endproc
                                        # -- End function
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$5, 4(%rsp)
	leaq	4(%rsp), %rdi
	callq	call_me
	movl	$.L.str, %edi
        imulq   %rdx
	movl	%eax, %esi
	xorl	%eax, %eax
	callq	printf
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc
                                        # -- End function
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"ret val: %d\n"
	.size	.L.str, 13


	.section	".note.GNU-stack","",@progbits
	.addrsig
