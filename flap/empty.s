.data
y:
	/* y */
	.quad 0
x:
	/* x */
	.quad 0
.text
	.globl main
.p2align 3, 144
main:
	/* Program entry point. */
	subq $8, %rsp
	call .I_681798664
	movq $0, %rdi
	call exit
.p2align 3, 144
double_int:
	/* Retrolix function double_int. */
	pushq %rbp
	movq %rsp, %rbp
	movq -8(%rip,%rsp,1), %r15
	addq -16(%rip,%rsp,1), %r15
	movq %r15, %rax
	popq %rbp
	ret
.p2align 3, 144
.I_681798664:
	/* Initializer for x, y. */
	pushq %rbp
	movq %rsp, %rbp
	subq $24, %rsp
	movq $0, x(%rip)
	movq $1, y(%rip)
	movq $2, 8(%rip,%rsp,1)
	movq $3, 16(%rip,%rsp,1)
	movq $4, 24(%rip,%rsp,1)
	pushq 24(%rip,%rsp,1)
	pushq 16(%rip,%rsp,1)
	call double_int
	addq $24, %rsp
	movq %rax, %rdi
	call observe_int
	movq $0, %rdi
	call exit
