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
	pushq %rbp
	movq %rsp, %rbp
	subq %rdi, %rbp
	movq $6, ()
	movq (), %rdi
.p2align 3, 144
.I_912297694:
	/* Initializer for y. */
	pushq %rbp
	movq %rsp, %rbp
	subq %rdi, %rbp
	movq $7, ()
	movq (), %rdi
.p2align 3, 144
.I_601236118:
	/* Initializer for z. */
	pushq %rbp
	movq %rsp, %rbp
	subq %rdi, %rbp
	movq (), %r15
	imulq (), %r15
	movq %r15, ()
	movq (), %rdi
.p2align 3, 144
.I_19854321:
	/* Initializer for k. */
	pushq %rbp
	movq %rsp, %rbp
	subq %rdi, %rbp
	movq (), %r15
	subq (), %r15
	movq %r15, ()
	movq (), %rdi
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	addq $16, %rsp
	popq %rbp
	ret
