.text
.globl main
main:
  addi $sp, $sp, -20
  sw $ra, 16($sp)
  sw $fp, 12($sp)
  addi $fp, $sp, 16
  li $v0, 10
  sw $v0, 8($fp)
  li $v0, 1
  sw $v0, 12($fp)
  la $v0, str1
  sw $v0, 16($fp)
  lw $v0, 8($fp)
  b ret0
ret0:
  addi $sp, $sp, 20
  lw $ra, 0($fp)
  lw $fp, -4($fp)
  jr $ra
seconde:
  addi $sp, $sp, -16
  sw $ra, 12($sp)
  sw $fp, 8($sp)
  addi $fp, $sp, 12
  li $v0, 10
  sw $v0, 8($fp)
  li $v0, 1
  sw $v0, 12($fp)
  lw $v0, 12($fp)
  b ret1
ret1:
  addi $sp, $sp, 16
  lw $ra, 0($fp)
  lw $fp, -4($fp)
  jr $ra

.data
str1: .asciiz "thisisastring"
