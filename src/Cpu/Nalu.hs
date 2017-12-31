module Cpu.Nalu ( nalu
                , nalu_control
                ) where

import Netlist.Jazz

import Cpu.Instr
import Cpu.Control

data Nalu_control = Nalu_control { alu_enable_carry :: Bit
                                 , alu_carry_in     :: Bit
                                 , alu_force_or   :: Bit
                              --  alu_disable_and   :: Bit On a toujours alu_enable_carry = alu_disable_and
                                 , alu_invert_x     :: Bit
                                 , alu_invert_y     :: Bit
                                 }

nalu_control_from_wire :: Wr a => a -> Jazz Nalu_control
nalu_control_from_wire w =
  bits w >>= \l -> return $
  Nalu_control { alu_enable_carry = l !! 0
               , alu_carry_in     = l !! 1
               , alu_force_or     = l !! 2
               , alu_invert_x     = l !! 3
               , alu_invert_y     = l !! 4
               }

-- enable_carry carry_in force_or invert_x invert_y
ctrl_add  = wire_of_bool_list [ True,  False, False, False, False ]
ctrl_sub  = wire_of_bool_list [ True,  True,  False, False, True  ]
ctrl_and  = wire_of_bool_list [ False, False, False, False, True  ]
ctrl_or   = wire_of_bool_list [ False, False, True,  True,  False ]
ctrl_nor  = wire_of_bool_list [ False, False, False, True,  False ]
ctrl_def  = wire_of_bool_list [ False, False, False, False, False ]

ctrl_mux :: Jazz Opcode_mux
ctrl_mux = do x <- (opcode_def ctrl_def)
              return $ x { op_beq   = ctrl_sub
                         , op_bne   = ctrl_sub
                         , op_addi  = ctrl_add
                         , op_addiu = ctrl_add
                         , op_andi  = ctrl_and
                         , op_ori   = ctrl_or

                         , op_add   = ctrl_add
                         , op_addu  = ctrl_add
                         , op_sub   = ctrl_sub
                         , op_subu  = ctrl_sub
                         , op_and   = ctrl_and
                         , op_or    = ctrl_or
                         , op_nor   = ctrl_nor
                         }

nalu_control :: Instr -> Jazz Nalu_control
nalu_control instr =
  ctrl_mux
  >>= opcode_mux instr
  >>= nalu_control_from_wire

fullalu :: (Bt a, Bt b, Bt c) => Nalu_control -> a -> b -> c -> Jazz (Bit, Bit)
fullalu ctrl a b c = do
  a1 <- a <> (alu_invert_x ctrl)
  b1 <- b <> (alu_invert_y ctrl)
  r <- ((a1 /\ b1) \/ ((a1 \/ b1) /\ c)) /\ (alu_enable_carry ctrl)
  s <- ( (a1 <> b1 <> c) /\ (a1 \/ (alu_enable_carry ctrl)) ) <> (alu_force_or ctrl)
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

