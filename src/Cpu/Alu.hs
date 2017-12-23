module Cpu.Alu where

import Netlist.Jazz
import Cpu.Misc

import Control.Monad (mapM, mapM_)
import Data.List as List

data Alu_control = Alu_control { alu_enable_carry :: Bit
                               , alu_carry_in     :: Bit
                               , alu_force_or   :: Bit
                               , alu_force_and   :: Bit
                               , alu_invert_x     :: Bit
                               , alu_invert_y     :: Bit
                               }

data Alu_flag = Alu_flag { carry_out :: Bit
                         , is_zero   :: Bit
                         }

fulladder :: (Bt a, Bt b, Bt c) => a -> b -> c -> Jazz (Bit, Bit)
fulladder a b c = do
  s <- a <> b <> c
  r <- (a /\ b) \/ ((a \/ b) /\ c)
  return (r, s)

nadder :: (Bt a, Bt b, Bt c) => a -> [b] -> [c] -> Jazz (Bit, [Bit])
nadder c [] [] = do
  x <- bit c
  return (x, [])
nadder c (x:xs) (y:ys) = do
  (c', z) <- fulladder c x y
  (c_out, zs) <- nadder c' xs ys
  return (c_out, z:zs)

-- Liste des controles valant true pour chaque opération
-- Add -> alu_enable_carry
-- Sub -> alu_enable_carry, alu_carry_in, alu_invert_y
-- And -> alu_force_and
-- Nor -> alu_force_and, alu_invert_x, alu_invert_y
-- Or  -> alu_force_and, alu_force_or, alu_invert_x, alu_invert_y

fullalu :: (Bt a, Bt b, Bt c) => Alu_control -> a -> b -> c -> Jazz (Bit, Bit)
fullalu ctrl a b c = do
  a1 <- a <> (alu_invert_x ctrl)
  b1 <- b <> (alu_invert_y ctrl)
  r <- ((a1 /\ b1) \/ ((a1 \/ b1) /\ c)) /\ (alu_enable_carry ctrl)
  s <- ( (a1 <> b1 <> c) /\ (a1 \/ (neg (alu_force_and ctrl))) ) <> (alu_force_or ctrl)
  return (r, s)

--d tracks if the result is zero
nalu :: (Bt a, Bt b, Bt c) => Alu_control -> [a] -> [b] -> c -> Jazz (Bit, [Bit], Bit)
nalu ctrl [] [] c= do
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
  return (Alu_flag { carry_out = c_out
                   , is_zero = d
                   }
          ,z)





  
