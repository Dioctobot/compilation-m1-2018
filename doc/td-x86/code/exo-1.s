	.file	"hello.c"
	.text
	.section	.rodata
.LC0:
	.string	"Hello World!"
	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	$.LC0, %edi
	call	puts
	movl	$0, %edi
	call	exit
	.size	main, .-main
	.ident	"GCC: (Debian 8.2.0-13) 8.2.0"
	.section	.note.GNU-stack,"",@progbits
