	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 14
	.globl	_main                   ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	movl	$8, %edi
	callq	_malloc
	movq	$0, (%rax)
	movl	$16, %edi
	callq	_malloc
	leaq	_main(%rip), %rcx
	movq	%rcx, (%rax)
	movq	$0, 8(%rax)
	movl	$16, %edi
	callq	_malloc
	movq	%rax, %rbx
	xorl	%edi, %edi
	callq	_malloc
	leaq	_f0(%rip), %rcx
	movq	%rcx, (%rbx)
	movq	%rax, 8(%rbx)
	movq	%rax, %rdi
	callq	*%rcx
	xorl	%eax, %eax
	popq	%rbx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_f0                     ## -- Begin function f0
	.p2align	4, 0x90
_f0:                                    ## @f0
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movl	$8, %edi
	callq	_malloc
	movq	%rbx, (%rax)
	movl	$16, %edi
	callq	_malloc
	leaq	_f0(%rip), %rcx
	movq	%rbx, 8(%rax)
	movq	%rcx, (%rax)
	leaq	L_str(%rip), %rdi
	callq	_printhw
	popq	%rbx
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__cstring,cstring_literals
L_str:                                  ## @str
	.asciz	"Hello World!"


.subsections_via_symbols
