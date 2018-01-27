main:
  j gettime
  
gettime:
  lw $at,$zero,28
  li $t0,0
  li $t1,0
  li $t2,0
  li $t3,1
  li $t4,3
  li $t5,1
  li $t6,1970
  
  lui $ra, 1926
  addiu $ra,$ra,8064
  divu $at,$ra
  mflo $a1
  sll $ra,$a1,2
  addu $t6,$t6,$ra
  
  mfhi $s0
  lui $ra, 481
  addiu $ra,$ra,13184
  divu $s0,$ra
  mflo $a2
  addu $t6,$t6,$a2

  mfhi $ra
  subu $fp,$at,$ra

  
  li $ra,5
  multu $ra,$a1
  mflo $ra
  addu $t4,$t4,$ra  
  addu $t4,$t4,$a2
  
  li $ra,7
  divu $t4,$ra
  mfhi $t4
  
  j init
  
  
init:
  sw $t3,$zero,12
  sw $t5,$zero,20
  sw $t6,$zero,24
  li $a0,60
  li $a1,24
  li $a2, 7
  
  li $v0, 29
  li $v1, 30
  li $k0, 31
  li $k1, 32
  
  li $a3,13
  
  li $sp,100
  
  j second

second:
  lw $at,$zero,28
  beq $at,$fp,second

  addiu $fp,$fp,1

  addiu $t0,$t0,1
  beq $t0,$a0,minute
  sw $t0,$zero,0
  j second
  
  
minute:
  li $t0,0
  sw $t0,$zero,0
  addiu $t1,$t1,1
  beq $t1,$a0,hour
  sw $t1,$zero,4
  j second
  
hour:
  li $t1,0
  sw $t1,$zero,4
  addiu $t2,$t2,1
  beq $t2,$a1,day
  sw $t2,$zero,8
  j second
  
day:
  li $t2,0  
  sw $t2,$zero,8
  addiu $t4,$t4,1 

  addiu $t3,$t3,1
  sw $t3,$zero,12

  beq $t4,$a2,week  
  sw $t4,$zero,16
  j machin

week:
  li $t4,0  
  sw $t4,$zero,16
  j machin

machin:
  li $ra,1
  beq $ra,$t5,nd_31
  li $ra,2
  beq $ra,$t5,nd_fevrier
  li $ra,3
  beq $ra,$t5,nd_31
  li $ra,4
  beq $ra,$t5,nd_30
  li $ra,5
  beq $ra,$t5,nd_31
  li $ra,6
  beq $ra,$t5,nd_30
  li $ra,7
  beq $ra,$t5,nd_31
  li $ra,8
  beq $ra,$t5,nd_31
  li $ra,9
  beq $ra,$t5,nd_30
  li $ra,10
  beq $ra,$t5,nd_31
  li $ra,11
  beq $ra,$t5,nd_30
  li $ra,12
  beq $ra,$t5,nd_31
  
nd_30:
  beq $t3,$k0,month
  j second
  
nd_31:
  beq $t3,$k1,month
  j second
  
nd_fevrier:
  beq $t3,$v1,month
  beq $t3,$v0,n_28
  j second

n_28:
  andi $ra,$t6,3
  beq $ra,$zero,bissextile
  j month
  
bissextile:
  divu $t6,$sp
  mfhi $ra
  beq $ra,$zero,cent_bissextile
  j second
  
cent_bissextile:
  mflo $ra
  andi $ra,$ra,3
  beq $ra,$zero,second
  j month
  
month:
  li $t3,1
  sw $t3,$zero,12
  addiu $t5,$t5,1
  beq $t5,$a3,year
  sw $t5,$zero,20
  j second
  
year:
  li $t5,1
  sw $t5,$zero,20
  addiu $t6,$t6,1  
  sw $t6,$zero,24
  j second
  
  
