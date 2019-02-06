.data
	/* z */
	.quad 0
	/* y */
	.quad 0
	/* x */
	.quad 0
	/* k */
	.quad 0
.text
	.globl main
.p2align 3, 144
main:
	/* Program entry point. */
	subq $8, %rsp
	call .I_154575915
	call .I_912297694
	call .I_601236118
	call .I_19854321
	movq $0, %rdi
	call exit
.p2align 3, 144
.I_154575915:
	/* Initializer for x. */
.p2align 3, 144
.I_912297694:
	/* Initializer for y. */
.p2align 3, 144
.I_601236118:
	/* Initializer for z. */
.p2align 3, 144
.I_19854321:
	/* Initializer for k. */
