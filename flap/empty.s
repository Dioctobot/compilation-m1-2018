.data
x:
	/* x */
	.quad 0
.text
	.globl main
.p2align 3, 144
main:
	/* Program entry point. */
	subq $8, %rsp
	call .I_154575915
	movq $0, %rdi
	call exit
.p2align 3, 144
fact:
	/* Retrolix function fact. */
	movq n(%rip), %rax
.p2align 3, 144
.I_154575915:
	/* Initializer for x. */
	movq $0, %r15
	subq $1, %r15
	movq %r15, x(%rip)
	movq x(%rip), %rdi
	call fact
	call observe_int
