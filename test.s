.text
.globl main
_add:
  lw $t0, 0($sp)
  lw $t1, 4($sp)
  add $v0, $t0, $t1
  jr $ra
_mul:
  lw $t0, 0($sp)
  lw $t1, 4($sp)
  mul $v0, $t0, $t1
  jr $ra
puti:
  lw $a0, 0($sp)
  li $v0, 1
  syscall
  jr $ra
geti:
  lw $a0, 0($sp)
  li $v0, 5
  syscall
  jr $ra
puts:
  lw $a0, 0($sp)
  li $v0, 4
  syscall
  jr $ra
seconde:
  addi $sp, $sp, -16
  sw $ra, 12($sp)
  sw $fp, 8($sp)
  addi $fp, $sp, 12
  li $v0, 4
  sw $v0, 8($fp)
  li $v0, 1
  sw $v0, 12($fp)
  lw $v0, 8($fp)
  b ret0
ret0:
  addi $sp, $sp, 16
  lw $ra, 0($fp)
  lw $fp, -4($fp)
  jr $ra
main:
  addi $sp, $sp, -24
  sw $ra, 20($sp)
  sw $fp, 16($sp)
  addi $fp, $sp, 20
  li $v0, 10
  sw $v0, 8($fp)
  li $v0, 1
  sw $v0, 12($fp)
  la $v0, str1
  sw $v0, 16($fp)
  li $v0, 4
  sw $v0, 20($fp)
  lw $v0, 20($fp)
  b ret1
ret1:
  addi $sp, $sp, 24
  lw $ra, 0($fp)
  lw $fp, -4($fp)
  jr $ra

.data
str1: .asciiz "this is a string"
