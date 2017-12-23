module Cpu.Alu where

import Netlist.Jazz
import Cpu.Misc

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

