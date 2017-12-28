module Cpu.Alu where

import Netlist.Jazz
import Cpu.Misc
import Cpu.Memory

import Control.Monad (mapM, mapM_)
import Data.List as List

data Alu_control = Alu_control { alu_enable_carry :: Bit
                               , alu_carry_in     :: Bit
                               , alu_enable_xor   :: Bit
                               , alu_enable_and   :: Bit
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
  s <- ((a1 <> b1 <> c) /\ (alu_enable_xor ctrl)) \/ (a1 /\ b1 /\ (alu_enable_and ctrl))
  return (r, s)

-- d tracks if the result is zero
nalu :: (Bt a, Bt b, Bt c) => Alu_control -> [a] -> [b] -> c -> Jazz (Bit, [Bit], Bit)
nalu ctrl [] [] c = do
  x <- bit c
  cst_true <- bit True
  return (x, [], cst_true)
nalu ctrl (x:xs) (y:ys) c = do
  (c',z) <- fullalu ctrl x y c
  (c_out, zs, d) <- nalu ctrl xs ys c'
  d' <- d /\ (neg z)
  return (c_out, z:zs, d)

alu :: (Bt a, Bt b) => Alu_control -> [a] -> [b] -> Jazz (Alu_flag, [Bit])
alu ctrl x y = do
  (c_out, z, d) <- nalu ctrl x y (alu_carry_in ctrl)
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
      aux [] = bit True
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
  y <- bits (slice 3 7 (instr_opcode instr))
  nonZero y

alu_inputs :: Instr -> Jazz (Wire, Wire)
alu_inputs instr = do
  input1 <- read_reg (instr_rs instr)
  value_rt <- read_reg (instr_rt instr)
  signed <- extension_mode instr
  immediate <- extend signed 16 (instr_imm instr)
  input2 <- mux (imm_ctrl instr) immediate value_rt
  return (input1, input2)

