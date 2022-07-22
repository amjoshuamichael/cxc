	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 12, 0
	.globl	_main                           ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:                               ## %entry
	movl	$0, -4(%rsp)
	movl	$0, -8(%rsp)
	movl	$128, -8(%rsp)
	movl	$128, -4(%rsp)
	movl	-8(%rsp), %eax
	movl	-4(%rsp), %edx
	retq
	.cfi_endproc
                                        ## -- End function
.subsections_via_symbols
