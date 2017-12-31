module Cpu.Nalu ( nalu
                , nalu_control
                ) where

import Netlist.Jazz

import Cpu.Instr
import Cpu.Control

data Nalu_control = Nalu_control { alu_enable_carry :: Bit
                                 , alu_carry_in     :: Bit
                                 , alu_force_or     :: Bit
                                 , alu_disable_and  :: Bit
                                 , alu_invert_x     :: Bit
                                 , alu_invert_y     :: Bit
                                 }

nalu_control_from_wire :: Wr a => a -> Jazz Nalu_control
nalu_control_from_wire w =
  bits w >>= \l -> return $
  Nalu_control { alu_enable_carry = l !! 0
               , alu_carry_in     = l !! 1
               , alu_force_or     = l !! 2
               , alu_disable_and  = l !! 3
               , alu_invert_x     = l !! 4
               , alu_invert_y     = l !! 5
               }

-- enable_carry carry_in force_or invert_x invert_y

nalu_control :: Instr -> Jazz Nalu_control
nalu_control instr = do
  ctrl_add  <- wire [ True,  False, False, True,  False, False ]
  ctrl_sub  <- wire [ True,  True,  False, True,  False, True  ]
  ctrl_and  <- wire [ False, False, False, False, False, True  ]
  ctrl_or   <- wire [ False, False, True,  False, True,  False ]
  ctrl_nor  <- wire [ False, False, False, False, True,  False ]
  ctrl_xor  <- wire [ False, False, False, True,  False, False ] -- TODO: add the opcode
  ctrl_def  <- wire [ False, False, False, False, False, False ]
  let ctrl_mux = Opcode_mux { op_j       = wire ctrl_def
                            , op_jal     = wire ctrl_add
                            , op_beq     = wire ctrl_xor
                            , op_bne     = wire ctrl_xor
                            , op_addi    = wire ctrl_add
                            , op_addiu   = wire ctrl_add
                            , op_slti    = wire ctrl_sub
                            , op_sltiu   = wire ctrl_sub
                            , op_andi    = wire ctrl_and
                            , op_ori     = wire ctrl_or
                            , op_lui     = wire ctrl_def
                            , op_lw      = wire ctrl_add
                            , op_lbu     = wire ctrl_add
                            , op_lhu     = wire ctrl_add
                            , op_sb      = wire ctrl_add
                            , op_sh      = wire ctrl_add
                            , op_sw      = wire ctrl_add
                            , op_ll      = wire ctrl_add
                            , op_sc      = wire ctrl_add

                            , op_sll     = wire ctrl_def
                            , op_srl     = wire ctrl_def
                            , op_sra     = wire ctrl_def
                            , op_jr      = wire ctrl_def
                            , op_mfhi    = wire ctrl_def
                            , op_mflo    = wire ctrl_def
                            , op_mult    = wire ctrl_def
                            , op_multu   = wire ctrl_def
                            , op_div     = wire ctrl_def
                            , op_divu    = wire ctrl_def
                            , op_add     = wire ctrl_add
                            , op_addu    = wire ctrl_add
                            , op_sub     = wire ctrl_sub
                            , op_subu    = wire ctrl_sub
                            , op_and     = wire ctrl_and
                            , op_or      = wire ctrl_or
                            , op_nor     = wire ctrl_nor
                            , op_slt     = wire ctrl_sub
                            , op_sltu    = wire ctrl_sub

                            , op_nop     = wire ctrl_def
                            }
  x <- opcode_mux instr ctrl_mux
  nalu_control_from_wire x

fullalu :: (Bt a, Bt b, Bt c) => Nalu_control -> a -> b -> c -> Jazz (Bit, Bit)
fullalu ctrl a b c = do
  a1 <- a <> (alu_invert_x ctrl)
  b1 <- b <> (alu_invert_y ctrl)
  r <- ((a1 /\ b1) \/ ((a1 \/ b1) /\ c)) /\ (alu_enable_carry ctrl)
  s <- ( (a1 <> b1 <> c) /\ (a1 \/ (alu_disable_and ctrl)) ) <> (alu_force_or ctrl)
  return (r, s)

nalu :: (Bt a, Bt b) => Nalu_control -> [a] -> [b] -> Jazz (Bit, [Bit])
nalu ctrl xs ys =
  let aux :: (Bt a, Bt b, Bt c) => Nalu_control -> [a] -> [b] -> c -> Jazz (Bit, [Bit])
      aux _ [] [] c = do
        x <- bit c
        return (x, [])
      aux ctrl (x:xs) (y:ys) c = do
        (c',z) <- fullalu ctrl x y c
        (c_out, zs) <- aux ctrl xs ys c'
        return (c_out, z:zs)
  in
  do aux ctrl xs ys (alu_carry_in ctrl)

