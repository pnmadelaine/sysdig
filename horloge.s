main:
  j init
 
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
  
  
init:
  li $a0,60
  li $a1,24
  li $a2, 7
  
  li $v0, 28
  li $v1, 29
  li $k0, 30
  li $k1, 31
  
  li $a3,12
  
  li $gp,4
  li $sp,100
  li $fp,400
  
  j second

second:
  addiu $t0,$t0,1
  divu $t0,$a0
  mfhi $t0
  beq $t0,$zero,minute
  j second
  
  
minute:
  addiu $t1,$t1,1
  divu $t1,$a0
  mfhi $t1
  beq $t1,$zero,hour
  j second
  
hour:
  addiu $t2,$t2,1
  divu $t2,$a1
  mfhi $t2
  beq $t2,$zero,day
  j second
  
day:  
  addiu $t4,$t4,1
  divu $t4,$a2
  mfhi $t4
  
  addiu $t3,$t3,1
  jr $t5
 
nd_28:
  divu $t3,$v0
  mfhi $t3
  beq $t3,$zero,month
  j second 
  
nd_29:
  divu $t3,$v1
  mfhi $t3
  beq $t3,$zero,month
  j second
  
nd_30:
  divu $t3,$k0
  mfhi $t3
  beq $t3,$zero,month
  j second
  
nd_31:
  divu $t3,$k1
  mfhi $t3
  beq $t3,$zero,month
  j second
  
nd_fevrier:
  divu $t6,$gp
  mfhi $ra
  beq $ra,$zero,bissextile
  j nd_28
  
bissextile:
  divu $t6,$sp
  mfhi $ra
  beq $ra,$zero,cent_bissextile
  j nd_29
  
cent_bissextile:
  divu $t6,$fp
  mfhi $ra
  beq $ra,$zero,nd_29
  j nd_28
  
month:
  addiu $t5,$t5,1
  divu $t5,$a3
  mfhi $t5
  beq $t5,$zero,year
  j second
  
year:
  addiu $t6,$t6,1
  j second
  
