module Cpu.Alu where

import Netlist.Jazz
import Cpu.Misc
import Cpu.Memory

import Control.Monad (mapM, mapM_)
import Data.List as List

data Alu_control = Alu_control { alu_enable_carry :: Bit
                               , alu_carry_in     :: Bit
                               , alu_force_or   :: Bit
                               --, alu_disable_and   :: Bit On a toujours alu_enable_carry = alu_disable_and
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

isZero :: [Bit] -> Jazz Bit
isZero [] = do
  a <- bit True
  return a
isZero (x:xs) = do
  a <- isZero xs
  b <- (neg x) /\ a
  return b

alu :: (Bt a, Bt b) => Alu_control -> [a] -> [b] -> Jazz (Alu_flag, [Bit])
alu ctrl x y = do
  (c_out, z) <- nalu ctrl x y (alu_carry_in ctrl)
  d <- isZero z
  return ( Alu_flag { carry_out = c_out
                    , is_zero = d
                    }
         , z
         )

--decides how immediate is extended
extension_mode :: Instr -> Jazz Bit
extension_mode instr = do
  x <- (select 5 (instr_opcode instr)) \/ (neg (select 2 (instr_opcode instr)))
  return x

--decides which is input of the ALU between rt and immediate
computing_mode :: Instr -> [Bit] -> [Bit] -> Jazz [Bit]
computing_mode instr value_rt immediate = do
  --every instruction that read rt have an opcode < 8
  y <- bits (slice 3 7 (instr_opcode instr))
  bit_ctrl <- isZero y
  answ <- bits (mux bit_ctrl immediate value_rt) --sous reserve que mux 0 a b = a et mux 1 a b = b
  return answ


alu_inputs :: Instr -> Jazz ([Bit],[Bit])
alu_inputs instr = do
  value_rs <- bits (read_reg (instr_rs instr))
  value_rt <- bits (read_reg (instr_rt instr))
  signed <- extension_mode instr
  immediate <- extend signed 16 (instr_imm instr)
  input2 <- computing_mode instr value_rt immediate
  return (value_rs, input2)


get_ctrl_alu :: Instr -> Jazz Alu_control
get_ctrl_alu instr = do

  let opcode = instr_opcode instr
  let funct = instr_funct instr

  disable_carry_I <- (select 2 opcode) /\ (select 3 opcode) -- c d
  disable_carry_R <- (isZero opcode) /\ (select 2 funct) -- 0/24 0/25 0/27
  enable_carry <- neg (disable_carry_I \/ disable_carry_R)

  carry_in_I <- (neg (select 5 opcode)) /\ ((select 1 opcode) \/ ((select 2 opcode) /\ (neg (select 3 opcode)))) -- a b 4 5 (2 et 3 ignorés car ne nécessitent pas de calcul arith/logique)
  -- pas de 2 devant /\ ({a,b,2,3} \/ ({4,5,c,d}\{c,d}))
  carry_in_R <- (isZero opcode) /\ (select 2 funct) /\ (neg (select 3 funct)) -- 0/2a 0/2b 0/22 0/23
  -- {_a, _b, _2, _3, _7} \ {_7} (0/02 ignoré car ne nécessite pas de calcul arith/logique)
  carry_in <- carry_in_I \/ carry_in_R

  force_or_R <- (isZero opcode) /\  (select 0 funct) /\ (select 2 funct) /\ (neg ((select 1 funct) \/ (select 3 funct))) -- 0/_5
  force_or_I <- (select 3 opcode) /\ (select 2 opcode) /\ (select 0 opcode) -- d (f neg car pas d'arith/logique)
  force_or <- force_or_R \/ force_or_I

  invert_y_I <- (select 3 opcode) /\ (select 2 opcode) /\ (neg (select 0 opcode)) --c
  invert_y_R <- (isZero opcode) /\  (select 2 funct) /\ (neg ((select 1 funct) \/ (select 3 funct) \/ (select 0 funct))) -- 0/_4
  invert_y <- invert_y_I \/ invert_y_R \/ carry_in --test d'avant que pour et

  invert_x <- neg (enable_carry \/ invert_y)

  return $ Alu_control { alu_enable_carry = enable_carry
                       , alu_carry_in = carry_in
                       , alu_force_or = force_or
                       , alu_invert_x = invert_x
                       , alu_invert_y = invert_y
                       }
  

