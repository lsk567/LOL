	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 14
	.globl	_main                   ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$8, %edi
	callq	_malloc
	movq	$0, (%rax)
	movl	$16, %edi
	callq	_malloc
	leaq	_main(%rip), %rcx
	movq	%rcx, (%rax)
	movq	$0, 8(%rax)
	leaq	L_str(%rip), %rdi
	callq	_gsl_test
	leaq	L_str.1(%rip), %rdi
	callq	_println
	xorl	%eax, %eax
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__cstring,cstring_literals
L_str:                                  ## @str
	.asciz	"foo"

L_str.1:                                ## @str.1
	.asciz	"success!"


.subsections_via_symbols
