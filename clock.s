main :

  lw    $at, $zero, 28

  li    $a0, 60
  divu  $at, $a0
  mfhi  $s0
  mflo  $at
  divu  $at, $a0
  mfhi  $s1
  mflo  $at
  li    $a0, 24
  divu  $at, $a0
  mfhi  $s2
  mflo  $at

  li    $s5, 1

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
  mflo  $t0
  addu  $s5, $s5, $t0
  li    $a0, 2
  sll   $a0, $a0, 16
  ori   $a0, $a0, 14659
  sltu  $t1, $a0, $at

  li    $a0, 36524
  divu  $at, $a0
  mfhi  $at
  mflo  $v0
  li    $v1, 100
  multu $v0, $v1
  mflo  $t0
  addu  $s5, $s5, $t0
  li    $a0, 36159
  sltu  $t2, $at, $a0

  li    $a0, 1461
  divu  $at, $a0
  mfhi  $at
  mflo  $v0
  li    $v1, 4
  multu $v0, $v1
  mflo  $t0
  addu  $s5, $s5, $t0
  li    $a0, 1095
  sltu  $t3, $a0, $at

  li    $a0, 365
  divu  $at, $a0
  mfhi  $at
  mflo  $t0
  addu  $s5, $s5, $t0


  or    $a0, $t2, $zero
  and   $a0, $a0, $t3
  or    $a0, $a0, $t1

  addiu $a0, $a0, 28
  li    $a1, 31
  li    $a2, 30

  li    $s4, 1

  sltu  $v0, $at, $a1
  bne   $v0, $zero, end
  subu  $at, $at, $a1
  addiu $s4, $s4, 1

  sltu  $v0, $at, $a0
  bne   $v0, $zero, end
  subu  $at, $at, $a0
  addiu $s4, $s4, 1

  sltu  $v0, $at, $a1
  bne   $v0, $zero, end
  subu  $at, $at, $a1
  addiu $s4, $s4, 1

  sltu  $v0, $at, $a2
  bne   $v0, $zero, end
  subu  $at, $at, $a2
  addiu $s4, $s4, 1

  sltu  $v0, $at, $a1
  bne   $v0, $zero, end
  subu  $at, $at, $a1
  addiu $s4, $s4, 1

  sltu  $v0, $at, $a2
  bne   $v0, $zero, end
  subu  $at, $at, $a2
  addiu $s4, $s4, 1

  sltu  $v0, $at, $a1
  bne   $v0, $zero, end
  subu  $at, $at, $a1
  addiu $s4, $s4, 1

  sltu  $v0, $at, $a1
  bne   $v0, $zero, end
  subu  $at, $at, $a1
  addiu $s4, $s4, 1

  sltu  $v0, $at, $a2
  bne   $v0, $zero, end
  subu  $at, $at, $a2
  addiu $s4, $s4, 1

  sltu  $v0, $at, $a1
  bne   $v0, $zero, end
  subu  $at, $at, $a1
  addiu $s4, $s4, 1

  sltu  $v0, $at, $a2
  bne   $v0, $zero, end
  subu  $at, $at, $a2
  addiu $s4, $s4, 1

end:
  li    $s3, 1
  addu  $s3, $s3, $at
  sw    $s0, $zero, 0
  sw    $s1, $zero, 4
  sw    $s2, $zero, 8
  sw    $s3, $zero, 12
  sw    $s4, $zero, 20
  sw    $s5, $zero, 24

  j     main
