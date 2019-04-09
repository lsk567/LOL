	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 14
	.globl	_main                   ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%r14
	.cfi_def_cfa_offset 16
	pushq	%rbx
	.cfi_def_cfa_offset 24
	pushq	%rax
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -24
	.cfi_offset %r14, -16
	movl	$8, %edi
	callq	_malloc
	movq	$0, (%rax)
	movl	$16, %edi
	callq	_malloc
	leaq	_main(%rip), %rcx
	movq	%rcx, (%rax)
	movq	$0, 8(%rax)
	movl	$4, %edi
	callq	_malloc
	movq	%rax, %r14
	movl	$1, (%rax)
	movl	$4, %edi
	callq	_malloc
	movq	%rax, %rbx
	movl	$2, (%rax)
	movl	$4, %edi
	callq	_malloc
	movl	(%r14), %edi
	addl	(%rbx), %edi
	movl	%edi, (%rax)
	callq	_str_of_int
	movq	%rax, %rdi
	callq	_println
	xorl	%eax, %eax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
	.cfi_endproc
                                        ## -- End function

.subsections_via_symbols
