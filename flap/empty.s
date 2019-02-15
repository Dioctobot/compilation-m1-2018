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
	/* Retrolix function fact. */
	movq $1, %rax
l0:
	cmpq %rdi, $1
	movq %rax, %r15
	imulq %rdi, %r15
	movq %r15, %rax
	movq %rdi, %r15
	subq $1, %r15
	movq %r15, %rdi
	jmp l0
	movq %rdi, %rdi
.p2align 3, 144
.I_129913994:
	/* Initializer for . */
	movq $5, %rdi
	movq %rax, %rdi
