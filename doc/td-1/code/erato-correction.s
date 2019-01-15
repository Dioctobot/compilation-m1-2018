#  Notice that 2^28 is equal to $268435456
	.file	"erato.s"
	.text
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"%d\n"
	.text
	.globl	main
	.type	main, @function

main:
.LFB22:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_def_cfa_offset 16

# Set the limit in a callee-saved register
	movq	$268435456, %r12
#	movq	$10, %r12

# Allocate the sieve on the heap
	movq	%r12, %rdi
	call	malloc@PLT

# Initialise the sieve with "1" everywhere.
	leaq    (%rax, %r12), %rdx # %rdx is the end of the sieve
	movq    %rax, %rcx	   # %rcx is iterator
.L1:
	movb    $1, (%rcx)
	incq    %rcx
	cmpq    %rcx, %rdx
	jne     .L1

# For each integer %r13 from 2 to LIMIT
	movq   $2, %r13
	movb   $1, %bh
.L2:
	movb   (%rax, %r13), %bl
	cmpb   %bl, %bh
	jne .LCONT
# If it is prime, remove its factor from the sieve

	movq %rax, %rcx
	addq %r13, %rcx
.L3:
	addq %r13, %rcx
	cmpq %rdx, %rcx
	jnb .LCONT
	movb $0, (%rcx)
	jmp .L3
.LCONT:
	incq   %r13
	cmpq   %r13, %r12
	jne .L2

# Now count the number of 1 in the sieve from cell numbered 2
	movq $0, %r14
	movq $2, %r13
	movb $1, %bh
	movq %rax, %rcx
.L4:
	movb (%rcx, %r13), %bl
	cmpb %bl, %bh
	jne  .LCONT2
	incq %r14
.LCONT2:
	incq %r13
	cmpq %r13, %r12
	jne .L4

# Show this number
	movq    %r14, %rax
	movl    %eax, %esi
        leaq    .LC0(%rip), %rdi
        movl    $0, %eax
        call    printf@PLT

.LSTOP:
	movl	$0, %edi
	call	exit@PLT
	.cfi_endproc
.LFE22:
	.size	main, .-main
	.ident	"GCC: (Debian 8.2.0-13) 8.2.0"
	.section	.note.GNU-stack,"",@progbits
