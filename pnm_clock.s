main :

  lw    $at, $zero, 28

  li    $a0, 60
  divu  $at, $a0
  mfhi  $t0
  mflo  $at
  divu  $at, $a0
  mfhi  $t1
  mflo  $at
  li    $a0, 24
  divu  $at, $a0
  mfhi  $t2
  mflo  $at

  li    $t5, 1

  li    $a0, 10
  sll   $a0, $a0, 16
  ori   $a0, $a0, 63802
  addu  $at, $at, $a0

  li    $a0, 2
  sll   $a0, $a0, 16
  ori   $a0, $a0, 15025
  divu  $at, $a0
  mfhi  $at
  mflo  $v0
  li    $v1, 400
  multu $v0, $v1
  mflo  $t6
  addu  $t5, $t5, $t6

  ori   $a0, $zero, 36524
  divu  $at, $a0
  mfhi  $at
  mflo  $v0
  li    $v1, 100
  multu $v0, $v1
  mflo  $t6
  addu  $t5, $t5, $t6

  li    $a0, 1461
  divu  $at, $a0
  mfhi  $at
  mflo  $v0
  li    $v1, 4
  multu $v0, $v1
  mflo  $t6
  addu  $t5, $t5, $t6

  li    $a0, 365
  divu  $at, $a0
  mfhi  $at
  mflo  $t6
  addu  $t5, $t5, $t6

  sltiu $a0, $t6, 1
  addiu $a0, $a0, 28
  li    $a1, 31
  li    $a2, 30
  li    $t4, 1

  sltu  $v0, $at, $a1
  bne   $v0, $zero, end
  subu  $at, $at, $a1
  addiu $t4, $t4, 1

  sltu  $v0, $at, $a0
  bne   $v0, $zero, end
  subu  $at, $at, $a0
  addiu $t4, $t4, 1

  sltu  $v0, $at, $a1
  bne   $v0, $zero, end
  subu  $at, $at, $a1
  addiu $t4, $t4, 1

  sltu  $v0, $at, $a2
  bne   $v0, $zero, end
  subu  $at, $at, $a2
  addiu $t4, $t4, 1

  sltu  $v0, $at, $a1
  bne   $v0, $zero, end
  subu  $at, $at, $a1
  addiu $t4, $t4, 1

  sltu  $v0, $at, $a2
  bne   $v0, $zero, end
  subu  $at, $at, $a2
  addiu $t4, $t4, 1

  sltu  $v0, $at, $a1
  bne   $v0, $zero, end
  subu  $at, $at, $a1
  addiu $t4, $t4, 1

  sltu  $v0, $at, $a1
  bne   $v0, $zero, end
  subu  $at, $at, $a1
  addiu $t4, $t4, 1

  sltu  $v0, $at, $a2
  bne   $v0, $zero, end
  subu  $at, $at, $a2
  addiu $t4, $t4, 1

  sltu  $v0, $at, $a1
  bne   $v0, $zero, end
  subu  $at, $at, $a1
  addiu $t4, $t4, 1

  sltu  $v0, $at, $a2
  bne   $v0, $zero, end
  subu  $at, $at, $a2
  addiu $t4, $t4, 1

end:
  li    $t3, 1
  addu  $t3, $t3, $at
  sw    $t0, $zero, 0
  sw    $t1, $zero, 4
  sw    $t2, $zero, 8
  sw    $t3, $zero, 12
  sw    $t4, $zero, 20
  sw    $t5, $zero, 24

  j     main

