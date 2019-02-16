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
.I_154575915:
	/* Initializer for x. */
	movq $0, %r15
	subq $1, %r15
	movq %r15, x(%rip)
	movq x(%rip), %rdi
	cmpq x(%rip), %rdi
	jle l2
	jmp l1
l1:
	jmp l2
l2:
	movq %rdi, %r15
	subq $1, %r15
	movq %r15, %rdi
	call observe_int
