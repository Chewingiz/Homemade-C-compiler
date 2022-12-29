.text
.globl main
main:
 move $fp, $sp
 addi $sp, $sp, -8
  li $v0, 10
  sw $v0, 0($fp)
  li $v0, 1
  sw $v0, -4($fp)
  lw $v0, 0($fp)
  jr $ra
  move $a0, $v0
  li $v0, 1
  syscall
  jr $ra

.data
