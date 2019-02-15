	.file	"printf.c"
	.text
	.section	.rodata
	.align 8
.LC0:
	.string	"a = %d, b = %d, c = %d, d = %d, e = %d, f = %d, g = %d, h = %d\n"
	.text
	.globl	print
	.type	print, @function
print:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	%edx, -12(%rbp)
	movl	%ecx, -16(%rbp)
	movl	%r8d, -20(%rbp)
	movl	%r9d, -24(%rbp)
	movl	-20(%rbp), %r8d
	movl	-16(%rbp), %edi
	movl	-12(%rbp), %ecx
	movl	-8(%rbp), %edx
	movl	-4(%rbp), %eax
	subq	$8, %rsp
	movl	24(%rbp), %esi
	pushq	%rsi
	movl	16(%rbp), %esi
	pushq	%rsi
	movl	-24(%rbp), %esi
	pushq	%rsi
	movl	%r8d, %r9d
	movl	%edi, %r8d
	movl	%eax, %esi
	movl	$.LC0, %edi
	movl	$0, %eax
	call	printf
	addq	$32, %rsp
	leave
	ret
	.size	print, .-print
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	$8
	pushq	$7
	movl	$6, %r9d
	movl	$5, %r8d
	movl	$4, %ecx
	movl	$3, %edx
	movl	$2, %esi
	movl	$1, %edi
	call	print
	addq	$16, %rsp
	leave
	ret
	.size	main, .-main
	.ident	"GCC: (Ubuntu 7.3.0-27ubuntu1~18.04) 7.3.0"
	.section	.note.GNU-stack,"",@progbits
