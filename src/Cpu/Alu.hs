module Cpu.Alu where

import Netlist.Ast
import Netlist.Build
import Cpu.Misc

import Control.Monad (mapM, mapM_)
import Data.List as List

data Alu_control = Alu_control { alu_enable_carry :: Argument
                               , alu_carry_in     :: Argument
                               , alu_enable_xor   :: Argument
                               , alu_enable_and   :: Argument
                               , alu_invert_x     :: Argument
                               , alu_invert_y     :: Argument
                               }

data Alu_flag = Alu_flag { carry_out :: Argument
                         , is_zero   :: Argument
                         }

fulladder :: (Bit a, Bit b, Bit c) => a -> b -> c -> Jazz (Argument, Argument)
fulladder a b c = do
  s <- a <> b <> c
  r <- (a /\ b) \/ ((a \/ b) /\ c)
  return (r, s)

nadder :: (Bit a, Bit b, Bit c) => a -> [b] -> [c] -> Jazz (Argument, [Argument])
nadder c [] [] = do
  x <- bit c
  return (x, [])
nadder c (x:xs) (y:ys) = do
  (c', z) <- fulladder c x y
  (c_out, zs) <- nadder c' xs ys
  return (c_out, z:zs)

