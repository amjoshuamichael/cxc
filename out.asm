	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 12, 0
	.globl	_main                           ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:                               ## %entry
	movabsq	$85899345920, %rax              ## imm = 0x1400000000
	movq	%rax, -8(%rsp)
	xorl	%eax, %eax
	movl	$20, %edx
	retq
	.cfi_endproc
                                        ## -- End function
.subsections_via_symbols
