	.file	"exo1-3.c"
	.section	.text.unlikely,"ax",@progbits
.LCOLDB0:
	.text
.LHOTB0:
	.p2align 4,,15
	.globl	f0
	.type	f0, @function
f0:
	xorl	%eax, %eax
	ret
	.size	f0, .-f0
	.section	.text.unlikely
.LCOLDE0:
	.text
.LHOTE0:
	.section	.text.unlikely
.LCOLDB1:
	.text
.LHOTB1:
	.p2align 4,,15
	.globl	f1
	.type	f1, @function
f1:
	movl	%edi, %eax
	ret
	.size	f1, .-f1
	.section	.text.unlikely
.LCOLDE1:
	.text
.LHOTE1:
	.section	.text.unlikely
.LCOLDB2:
	.text
.LHOTB2:
	.p2align 4,,15
	.globl	f2
	.type	f2, @function
f2:
	leal	(%rdi,%rsi), %eax
	ret
	.size	f2, .-f2
	.section	.text.unlikely
.LCOLDE2:
	.text
.LHOTE2:
	.section	.text.unlikely
.LCOLDB3:
	.text
.LHOTB3:
	.p2align 4,,15
	.globl	f3
	.type	f3, @function
f3:
	leal	(%rdi,%rsi), %eax
	addl	%edx, %eax
	ret
	.size	f3, .-f3
	.section	.text.unlikely
.LCOLDE3:
	.text
.LHOTE3:
	.section	.text.unlikely
.LCOLDB4:
	.text
.LHOTB4:
	.p2align 4,,15
	.globl	f4
	.type	f4, @function
f4:
	leal	(%rdi,%rsi), %eax
	addl	%edx, %eax
	addl	%ecx, %eax
	ret
	.size	f4, .-f4
	.section	.text.unlikely
.LCOLDE4:
	.text
.LHOTE4:
	.section	.text.unlikely
.LCOLDB5:
	.text
.LHOTB5:
	.p2align 4,,15
	.globl	f5
	.type	f5, @function
f5:
	addl	%edi, %esi
	addl	%esi, %edx
	addl	%edx, %ecx
	leal	(%rcx,%r8), %eax
	ret
	.size	f5, .-f5
	.section	.text.unlikely
.LCOLDE5:
	.text
.LHOTE5:
	.section	.text.unlikely
.LCOLDB6:
	.text
.LHOTB6:
	.p2align 4,,15
	.globl	f6
	.type	f6, @function
f6:
	addl	%edi, %esi
	addl	%esi, %edx
	addl	%edx, %ecx
	addl	%ecx, %r8d
	leal	(%r8,%r9), %eax
	ret
	.size	f6, .-f6
	.section	.text.unlikely
.LCOLDE6:
	.text
.LHOTE6:
	.section	.text.unlikely
.LCOLDB7:
	.text
.LHOTB7:
	.p2align 4,,15
	.globl	f7
	.type	f7, @function
f7:
	addl	%edi, %esi
	movl	8(%rsp), %eax
	addl	%esi, %edx
	addl	%edx, %ecx
	addl	%ecx, %r8d
	addl	%r8d, %r9d
	addl	%r9d, %eax
	ret
	.size	f7, .-f7
	.section	.text.unlikely
.LCOLDE7:
	.text
.LHOTE7:
	.section	.text.unlikely
.LCOLDB8:
	.text
.LHOTB8:
	.p2align 4,,15
	.globl	f8
	.type	f8, @function
f8:
	addl	%edi, %esi
	movl	8(%rsp), %eax
	addl	%esi, %edx
	addl	%edx, %ecx
	addl	%ecx, %r8d
	addl	%r8d, %r9d
	addl	%r9d, %eax
	addl	16(%rsp), %eax
	ret
	.size	f8, .-f8
	.section	.text.unlikely
.LCOLDE8:
	.text
.LHOTE8:
	.section	.text.unlikely
.LCOLDB9:
	.text
.LHOTB9:
	.p2align 4,,15
	.globl	f9
	.type	f9, @function
f9:
	addl	%edi, %esi
	movl	8(%rsp), %eax
	addl	%esi, %edx
	addl	%edx, %ecx
	addl	%ecx, %r8d
	addl	%r8d, %r9d
	addl	%r9d, %eax
	addl	16(%rsp), %eax
	addl	24(%rsp), %eax
	ret
	.size	f9, .-f9
	.section	.text.unlikely
.LCOLDE9:
	.text
.LHOTE9:
	.section	.text.unlikely
.LCOLDB10:
	.text
.LHOTB10:
	.p2align 4,,15
	.globl	f10
	.type	f10, @function
f10:
	addl	%edi, %esi
	movl	8(%rsp), %eax
	addl	%esi, %edx
	addl	%edx, %ecx
	addl	%ecx, %r8d
	addl	%r8d, %r9d
	addl	%r9d, %eax
	addl	16(%rsp), %eax
	addl	24(%rsp), %eax
	addl	32(%rsp), %eax
	ret
	.size	f10, .-f10
	.section	.text.unlikely
.LCOLDE10:
	.text
.LHOTE10:
	.ident	"GCC: (Ubuntu 5.4.0-6ubuntu1~16.04.10) 5.4.0 20160609"
	.section	.note.GNU-stack,"",@progbits
