module Cpu.Alu where

import Netlist.Jazz
import Cpu.Misc
import Cpu.Memory

import Control.Monad (mapM, mapM_)
import Data.List as List

data Alu_control = Alu_control { alu_enable_carry :: Bit
                               , alu_carry_in     :: Bit
                               , alu_force_or   :: Bit
                              -- , alu_disable_and   :: Bit On a toujours alu_enable_carry = alu_disable_and
                               , alu_invert_x     :: Bit
                               , alu_invert_y     :: Bit
                               }

data Alu_flag = Alu_flag { carry_out :: Bit
                         , is_zero   :: Bit
                         }

-- Liste des controles valant true pour chaque opération
-- Add -> alu_enable_carry, alu_disable_and
-- Sub -> alu_enable_carry, alu_carry_in, alu_invert_y, alu_disable_and
-- And -> alu_invert_y
-- Nor -> alu_invert_x
-- Or  -> alu_force_or, alu_invert_x

fullalu :: (Bt a, Bt b, Bt c) => Alu_control -> a -> b -> c -> Jazz (Bit, Bit)
fullalu ctrl a b c = do
  a1 <- a <> (alu_invert_x ctrl)
  b1 <- b <> (alu_invert_y ctrl)
  r <- ((a1 /\ b1) \/ ((a1 \/ b1) /\ c)) /\ (alu_enable_carry ctrl)
  s <- ( (a1 <> b1 <> c) /\ (a1 \/ (alu_enable_carry ctrl)) ) <> (alu_force_or ctrl)
  return (r, s)

nalu :: (Bt a, Bt b, Bt c) => Alu_control -> [a] -> [b] -> c -> Jazz (Bit, [Bit])
nalu ctrl [] [] c = do
  x <- bit c
  return (x, [])
nalu ctrl (x:xs) (y:ys) c = do
  (c',z) <- fullalu ctrl x y c
  (c_out, zs) <- nalu ctrl xs ys c'
  return (c_out, z:zs)

alu :: (Wr a, Wr b) => Alu_control -> a -> b -> Jazz (Alu_flag, Wire)
alu ctrl x y = do
  xs <- bits x
  ys <- bits y
  (c_out, zs) <- nalu ctrl xs ys (alu_carry_in ctrl)
  z <- wire zs
  d <- isZero z
  return ( Alu_flag { carry_out = c_out
                    , is_zero = d
                    }
         , z
         )

--decides how immediate is extended
extension_mode :: Instr -> Jazz Bit
extension_mode instr =
  select 5 (instr_opcode instr) \/ neg (select 2 (instr_opcode instr))

nonZero :: Wr a => a -> Jazz Bit
nonZero w =
  let aux :: Bt a => [a] -> Jazz Bit
      aux [x] = bit x
      aux (x:xs) = x \/ nonZero xs
  in
  bits w >>= aux

isZero :: Wr a => a -> Jazz Bit
isZero x = nonZero x >>= neg

--decides which is input of the ALU between rt and immediate
--imm_ctrl is true if ALU reads immediate value
imm_ctrl :: Instr -> Jazz Bit
imm_ctrl instr = do
  --every instruction that read rt have an opcode < 8
  y <- slice 3 6 (instr_opcode instr)
  nonZero y

alu_inputs :: Instr -> Jazz (Wire, Wire)
alu_inputs instr = do
  input1    <- read_reg (instr_rs instr)
  value_rt  <- read_reg (instr_rt instr)
  signed    <- extension_mode instr
  immediate <- extend signed 16 (instr_imm instr)
  input2    <- mux (imm_ctrl instr) immediate value_rt
  return (input1, input2)

alu_control_from_wire :: Wr a => a -> Jazz Alu_control
alu_control_from_wire w =
  bits w >>= \l -> return $
  Alu_control { alu_enable_carry = l !! 0
              , alu_carry_in     = l !! 1
              , alu_force_or     = l !! 2
              , alu_invert_x     = l !! 3
              , alu_invert_y     = l !! 4
              }

-- enable_carry carry_in force_or invert_x invert_y
ctrl_add  = [ True,  False, False, False, False ]
ctrl_sub  = [ True,  True,  False, False, True  ]
ctrl_and  = [ False, False, False, False, True  ]
ctrl_or   = [ False, False, True,  True,  False ]
ctrl_nor  = [ False, False, False, True,  False ]
ctrl_def  = [ False, False, False, False, False ]

alu_control :: Instr -> Jazz Alu_control
alu_control instr =
  let aux_i 8 = ctrl_add  -- addi
      aux_i 9 = ctrl_add  -- addiu
      aux_i 12 = ctrl_and -- andi
      aux_i 13 = ctrl_or  -- ori
      --aux_i 14 = ... -- xori
      aux_i _ = ctrl_def
  in
  let aux_r 32 = ctrl_add -- add
      aux_r 33 = ctrl_add -- addu
      aux_r 34 = ctrl_sub -- sub
      aux_r 35 = ctrl_sub -- subu
      aux_r 36 = ctrl_and -- and
      aux_r 37 = ctrl_or -- or
      --aux_i 38 = ... -- xor
      aux_r 39 = ctrl_nor -- nor
      aux_r _ = ctrl_def
  in
  let f i = wire $ if i >= 64 then aux_r (i-64) else aux_i i in
  let opcode = instr_opcode instr in
  let funct = instr_funct instr in
  let op = conc (mux (nonZero opcode) opcode funct) [isZero opcode] in
  multiplex f (mux (isZero opcode) funct opcode)
  >>= alu_control_from_wire


