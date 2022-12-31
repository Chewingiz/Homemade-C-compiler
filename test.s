.text
.globl main
main:
  addi $sp, $sp, 4
  sw $ra, -8($sp)
  sw $fp, -12($sp)
  addi $fp, $sp, -8
  li $v0, 10
  sw $v0, 8($fp)
  li $v0, 1
  sw $v0, 4($fp)
  la $v0, str1
  sw $v0, 0($fp)
  lw $v0, 8($fp)
  b ret0
ret0:
  addi $sp, $sp, -4
  lw $ra, 0($fp)
  lw $fp, -4($fp)
  jr $ra
seconde:
  addi $sp, $sp, 0
  sw $ra, -4($sp)
  sw $fp, -8($sp)
  addi $fp, $sp, -4
  li $v0, 10
  sw $v0, 8($fp)
  li $v0, 1
  sw $v0, 4($fp)
  lw $v0, 4($fp)
  b ret1
ret1:
  addi $sp, $sp, 0
  lw $ra, 0($fp)
  lw $fp, -4($fp)
  jr $ra

.data
str1: .asciiz "thisisastring"
