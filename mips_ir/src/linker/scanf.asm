scanf:
	addiu	$t5, $sp, 4
	ori 	$t4, $zero, 0
	ori	$t9, $zero, 0
	lw	$t1, 0($sp)
$scanf.main_loop_with_char_read:
	lb	$t2, 0($t1)
	beq	$t2, $zero, $scanf.end
	ori	$v0, $zero, 12
	syscall
$scanf.main_loop:
	lb	$t2, 0($t1)
	beq	$t2, $zero, $scanf.end
	ori	$t6, $zero, '%'
	beq	$t2, $t6, $scanf.handle_code
	ori	$t9, $zero, 0
	bne	$v0, $t2, $scanf.end
	addiu 	$t1, $t1, 1
	j $scanf.main_loop_with_char_read
$scanf.handle_code:
	lb	$t3, 1($t1)
	addi	$t8, $t3, -48
	sltiu	$a0, $t8, 10
	beq	$a0, $zero, $scanf.handle_not_length
	ori	$t6, $zero, 10
	multu	$t9, $t6
	mflo	$t9
	addu	$t9, $t9, $t8
	addiu 	$t1, $t1, 1
	j $scanf.handle_code
$scanf.handle_not_length:
	ori	$t6, $zero, '%'
	bne	$t3, $t6, $scanf.handle_not_percent
	bne	$v0, $t6, $scanf.end
	j	$scanf.code_handle_end
$scanf.handle_not_percent:
	lw	$t7, 0($t5)
	addiu	$t5, $t5, 4
	addiu	$t4, $t4, 1
	ori	$t6, $zero, 'f'
	bne	$t3, $t6, $scanf.handle_not_float
	ori	$v0, $zero, 6
	syscall
	swc1	$f0, 0($t7)
	j	$scanf.code_handle_end
$scanf.handle_not_float:
	ori	$t6, $zero, 'c'
	bne	$t3, $t6, $scanf.handle_not_char
	bne	$t9, $zero, $scanf.handle_char_loop
	ori	$t9, $zero, 1
$scanf.handle_char_loop:
	sb	$v0, 0($t7)
	addiu	$t7, $t7, 1
	addiu	$t9, $t9, -1
	beq	$t9, $zero, $scanf.code_handle_end
	ori	$v0, $zero, 12
	syscall
	j	$scanf.handle_char_loop
$scanf.handle_not_char:
	j	$scanf.trim_whitespace
$scanf.trim_whitespace_cont:
	ori	$v0, $zero, 12
	syscall
$scanf.trim_whitespace:
	ori	$t6, $zero, ' '
	beq	$v0, $t6, $scanf.trim_whitespace_cont
	ori	$t6, $zero, 0x0A
	beq	$v0, $t6, $scanf.trim_whitespace_cont
	ori	$t6, $zero, 0x09
	beq	$v0, $t6, $scanf.trim_whitespace_cont
	ori	$t6, $zero, 'd'
	beq	$t3, $t6, $scanf.handle_int
	ori	$t6, $zero, 'i'
	bne	$t3, $t6, $scanf.handle_not_int
$scanf.handle_int:
	ori	$t0, $zero, 0
$scanf.handle_int_loop:
	addi	$v1, $v0, -48
	sltiu	$a0, $v1, 10
	beq	$a0, $zero,  $scanf.handle_int_end_no_char
	ori	$t6, $zero, 10
	multu	$t0, $t6
	mflo	$t0
	addu	$t0, $t0, $v1
	beq	$t9, $zero, $scanf.handle_int_loop_cont
	addiu	$t9, $t9, -1
	bne	$t9, $zero, $scanf.handle_int_loop_cont
	sw	$t0, 0($t7)
	j	$scanf.code_handle_end
$scanf.handle_int_loop_cont:
	ori	$v0, $zero, 12
	syscall
	j	$scanf.handle_int_loop
$scanf.handle_int_end_no_char:
	sw	$t0, 0($t7)
	j	$scanf.code_handle_end_no_char
$scanf.handle_not_int:
	ori	$t6, $zero, 's'
	bne	$t3, $t6, $scanf.handle_not_string
$scanf.handle_string_loop:
	beq	$v0, $zero, $scanf.handle_string_end_no_char
	ori	$t6, $zero, ' '
	beq	$v0, $t6, $scanf.handle_string_end_no_char
	ori	$t6, $zero, 0x0A
	beq	$v0, $t6, $scanf.handle_string_end_no_char
	ori	$t6, $zero, 0x09
	beq	$v0, $t6, $scanf.handle_string_end_no_char
	sb	$v0, 0($t7)
	addiu	$t7, $t7, 1
	beq	$t9, $zero, $scanf.handle_string_loop_cont
	addiu	$t9, $t9, -1
	bne	$t9, $zero, $scanf.handle_string_loop_cont
	sb	$zero, 0($t7)
	j	$scanf.code_handle_end
$scanf.handle_string_loop_cont:
	ori	$v0, $zero, 12
	syscall
	j	$scanf.handle_string_loop
$scanf.handle_string_end_no_char:
	sb	$zero, 0($t7)
	j	$scanf.code_handle_end_no_char
$scanf.handle_not_string:
$scanf.code_handle_end:
	addiu 	$t1, $t1, 2
	j	$scanf.main_loop_with_char_read
$scanf.code_handle_end_no_char:
	addiu 	$t1, $t1, 2
	j	$scanf.main_loop
$scanf.end:
	or	$v0, $zero, $t4
	jr	$ra
