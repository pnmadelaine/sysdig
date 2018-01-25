main:
  li     $at, 861183900
  
  li     $t0, 60
  li     $t1, 24
  li     $t2, 146097
  li     $t3, 400
  li     $t4, 36524
  li     $t5, 100
  li     $t6, 1461
  li     $t7, 4
  li     $t8, 365

  divu   $at, $t0
  mfhi   $s0
  mflo   $at
  
  divu   $at, $t0
  mfhi   $s1
  mflo   $at

  divu   $at, $t1
  mfhi   $s2
  mflo   $at

  addi   $at, $at, 719435
  
  divu   $at, $t2
  mfhi   $at
  mflo   $a0
  multu  $a0, $t3
  mflo   $s3
  sltiu  $v0, $at, 366
  bne    $v0, $zero, bisex

  divu   $at, $t4
  mfhi   $at
  mflo   $a1
  multu  $a1, $t5
  mflo   $a3
  addu   $s3, $s3, $a3
  sltiu  $v0, $at, 365
  bne    $v0, $zero, notbisex    

  divu   $at, $t6
  mfhi   $at
  mflo   $a2
  multu  $a2, $t7
  mflo   $a3
  addu   $s3, $s3, $a3
  sltiu  $v0, $at, 366
  bne    $v0, $zero, bisex

  divu   $at, $t8
  mfhi   $at
  mflo   $a3
  addu   $s3, $s3, $a3

notbisex:

  li     $s4, 1
  sltiu  $v0, $at, 31
  bne    $v0, $zero, finish
  addiu  $s4, $s4, 1
  sltiu  $v0, $at, 59
  bne    $v0, $zero, finish
  addiu  $s4, $s4, 1
  sltiu  $v0, $at, 90
  bne    $v0, $zero, finish
  addiu  $s4, $s4, 1
  sltiu  $v0, $at, 120
  bne    $v0, $zero, finish
  addiu  $s4, $s4, 1
  sltiu  $v0, $at, 151
  bne    $v0, $zero, finish
  addiu  $s4, $s4, 1
  sltiu  $v0, $at, 181
  bne    $v0, $zero, finish
  addiu  $s4, $s4, 1
  sltiu  $v0, $at, 212
  bne    $v0, $zero, finish
  addiu  $s4, $s4, 1
  sltiu  $v0, $at, 243
  bne    $v0, $zero, finish
  addiu  $s4, $s4, 1
  sltiu  $v0, $at, 273
  bne    $v0, $zero, finish
  addiu  $s4, $s4, 1
  sltiu  $v0, $at, 304
  bne    $v0, $zero, finish
  addiu  $s4, $s4, 1
  sltiu  $v0, $at, 335
  bne    $v0, $zero, finish
  addiu  $s4, $s4, 1
  j      finish

bisex:

  li     $s4, 1
  sltiu  $v0, $at, 31
  bne    $v0, $zero, finish
  addiu  $s4, $s4, 1
  sltiu  $v0, $at, 60
  bne    $v0, $zero, finish
  addiu  $s4, $s4, 1
  sltiu  $v0, $at, 91
  bne    $v0, $zero, finish
  addiu  $s4, $s4, 1
  sltiu  $v0, $at, 121
  bne    $v0, $zero, finish
  addiu  $s4, $s4, 1
  sltiu  $v0, $at, 152
  bne    $v0, $zero, finish
  addiu  $s4, $s4, 1
  sltiu  $v0, $at, 182
  bne    $v0, $zero, finish
  addiu  $s4, $s4, 1
  sltiu  $v0, $at, 213
  bne    $v0, $zero, finish
  addiu  $s4, $s4, 1
  sltiu  $v0, $at, 244
  bne    $v0, $zero, finish
  addiu  $s4, $s4, 1
  sltiu  $v0, $at, 274
  bne    $v0, $zero, finish
  addiu  $s4, $s4, 1
  sltiu  $v0, $at, 305
  bne    $v0, $zero, finish
  addiu  $s4, $s4, 1
  sltiu  $v0, $at, 336
  bne    $v0, $zero, finish
  addiu  $s4, $s4, 1
  j      finish

finish:
  addiu  $s5, $at, 1
  j main






