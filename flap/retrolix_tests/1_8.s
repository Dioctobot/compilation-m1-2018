.data
.text
	.globl main
.p2align 3, 144
main:
	/* Program entry point. */
	subq $8, %rsp
	call .I_129913994
	movq $0, %rdi
	call exit
.p2align 3, 144
.I_129913994:
	/* Initializer for . */
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	movq $5, %rdi
