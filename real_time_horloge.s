main:
  j gettime
 
ganvier:
  j nd_31
  
fevrier:
  j nd_fevrier
  
mars:
  j nd_31
  
avril:
  j nd_30
  
mai:
  j nd_31
  
guin:
  j nd_30
  
guillet:
  j nd_31
  
aout:
  j nd_31
  
septembre:
  j nd_30
  
octobre:
  j nd_31
  
novembre:
  j nd_30
  
decembre:
  j nd_31
  
  
gettime:
  lw $at,$zero,10
  li $t0,0
  li $t1,0
  li $t2,0
  li $t3,1
  li $t4,3
  li $t5,1
  li $t6,1970
  
  lw $at,$zero,10
  
  li $ra, 126230400
  divu $at,$ra
  mflo $a1
  sll $ra,$a1,2
  addu $t6,$t6,$ra
  
  mfhi $fp
  li $ra, 31536000
  divu $fp,$ra
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
  lw $at,$zero,10
  beq $at,$fp,second

  addiu $fp,$fp,1
  addiu $t0,$t0,1
  beq $t0,$a0,minute
  sb $t0,$zero,0
  j second
  
  
minute:
  li $t0,0
  sb $t0,$zero,0
  addiu $t1,$t1,1
  beq $t1,$a0,hour
  sb $t1,$zero,1
  j second
  
hour:
  li $t1,0
  sb $t1,$zero,1
  addiu $t2,$t2,1
  beq $t2,$a1,day
  sb $t2,$zero,2
  j second
  
day:
  li $t2,0  
  sb $t2,$zero,2
  addiu $t4,$t4,1 

  addiu $t3,$t3,1
  sb $t3,$zero,3

  beq $t4,$a2,week  
  sb $t4,$zero,4
  jr $t5

week:
  li $t4,0  
  sb $t4,$zero,4
  jr $t5
  
nd_30:
  beq $t3,$k0,month
  j second
  
nd_31:
  beq $t3,$k1,month
  j second
  
nd_fevrier:

  beq $t3,$v1,month
  bne $t3,$v0,second

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
  sb $t3,$zero,3
  addiu $t5,$t5,1
  beq $t5,$a3,year
  sb $t5,$zero,5
  j second
  
year:
  li $t5,1
  sb $t5,$zero,5
  addiu $t6,$t6,1  
  sw $t6,$zero,6
  j second
  
