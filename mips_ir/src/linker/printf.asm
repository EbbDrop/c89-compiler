printf:
	ori	$t7, $zero, '%'
	addiu	$t5, $sp, 4
	lw	$t1, 0($sp)
	or	$t0, $zero, $t1
$printf.main_loop:
	lb	$t2, 0($t1)
	beq	$t2, $zero, $printf.end
	beq	$t2, $t7, $printf.handle_code_start
	addiu 	$t1, $t1, 1
	j $printf.main_loop
$printf.handle_code_start:
	beq	$t0, $t1, $printf.handle_code
	sb	$zero, 0($t1)
	ori	$v0, $zero, 4
	or	$a0, $zero, $t0
	syscall
	sb	$t7, 0($t1)
$printf.handle_code:
	lb	$t3, 1($t1)
	ori	$t6, $zero, '%'
	bne	$t3, $t6, $printf.handle_not_percent

	ori	$v0, $zero, 11
	ori	$a0 , $zero, '%'
	syscall
	j	$printf.code_handle_end
$printf.handle_not_percent:
	ori	$t6, $zero, 'f'
	bne	$t3, $t6, $printf.handle_not_float
	addiu	$t5, $t5, 7
	srl	$t5, $t5, 3
	sll	$t5, $t5, 3
	ldc1 	$f12, 0($t5)
	ori	$v0, $zero, 3
	syscall
	addiu	$t5, $t5, 8
	j		$printf.code_handle_end
$printf.handle_not_float:
	lw	$a0, 0($t5)
	addiu	$t5, $t5, 4
	ori	$t6, $zero, 'd'
	beq	$t3, $t6, $printf.handle_int
	ori	$t6, $zero, 'i'
	bne	$t3, $t6, $printf.handle_not_int
$printf.handle_int:
	ori	$v0, $zero, 1
	syscall
	j	$printf.code_handle_end
$printf.handle_not_int:
	ori	$t6, $zero, 'c'
	bne	$t3, $t6, $printf.handle_not_char
	ori	$v0, $zero, 11
	syscall
	j	$printf.code_handle_end
$printf.handle_not_char:
	ori	$t6, $zero, 's'
	bne	$t3, $t6, $printf.handle_not_string
	ori	$v0, $zero, 4
	syscall
	j	$printf.code_handle_end
$printf.handle_not_string:
	or	$a0, $zero, $t3
	ori	$v0, $zero, 11
	syscall
$printf.code_handle_end:
	addiu 	$t1, $t1, 2
	or	$t0, $zero, $t1
	j	$printf.main_loop
$printf.end:
	beq	$t0, $t1, $printf.return
	ori	$v0, $zero, 4
	or	$a0, $zero, $t0
	syscall
$printf.return:
	ori	$v0, $zero, 1
	jr	$ra
