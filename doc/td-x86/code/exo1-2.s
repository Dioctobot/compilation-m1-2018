	.file	"exo1-2.c"
	.section	.text.unlikely,"ax",@progbits
.LCOLDB0:
	.text
.LHOTB0:
	.p2align 4,,15
	.globl	fact
	.type	fact, @function
fact:
	testl	%edi, %edi
	movl	$1, %eax
	je	.L4
	.p2align 4,,10
	.p2align 3
.L3:
	imull	%edi, %eax
	subl	$1, %edi
	jne	.L3
	rep ret
.L4:
	rep ret
	.size	fact, .-fact
	.section	.text.unlikely
.LCOLDE0:
	.text
.LHOTE0:
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC1:
	.string	"The factorial of %d is %d.\n"
	.section	.text.unlikely
.LCOLDB2:
	.section	.text.startup,"ax",@progbits
.LHOTB2:
	.p2align 4,,15
	.globl	main
	.type	main, @function
main:
	subq	$8, %rsp
	movq	8(%rsi), %rdi
	movl	$10, %edx
	xorl	%esi, %esi
	call	strtol
	testl	%eax, %eax
	je	.L10
	movl	%eax, %esi
	movl	$1, %ecx
	.p2align 4,,10
	.p2align 3
.L9:
	imull	%esi, %ecx
	subl	$1, %esi
	jne	.L9
.L8:
	movl	%eax, %edx
	movl	$1, %edi
	movl	$.LC1, %esi
	xorl	%eax, %eax
	call	__printf_chk
	xorl	%edi, %edi
	call	exit
.L10:
	movl	$1, %ecx
	jmp	.L8
	.size	main, .-main
	.section	.text.unlikely
.LCOLDE2:
	.section	.text.startup
.LHOTE2:
	.ident	"GCC: (Ubuntu 5.4.0-6ubuntu1~16.04.10) 5.4.0 20160609"
	.section	.note.GNU-stack,"",@progbits
