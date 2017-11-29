module Alu where

import qualified Data.List as List
import Control.Monad (mapM)

import Netlist.Ast
import Netlist.Build

data ALU_options = ALU_options { opt_enable_carry :: Argument
                               , opt_carry_in     :: Argument
                               , opt_enable_xor   :: Argument
                               , opt_enable_and   :: Argument
                               , opt_invert_y     :: Argument
                               , opt_shift_left   :: Argument
                               , opt_shift_value  :: [Argument]
                               }

data ALU_flags = ALU_flags { flag_carry_out :: Argument
                          -- zero
                          -- overflow
                           }

fulladder :: (Bit a, Bit b, Bit c) => a -> b -> c -> Jazz (Argument, Argument)
fulladder a b c = do
  r <- (a /\ b) \/ ((a \/ b) /\ c)
  s <- a <> b <> c
  return (r,s)

nadder :: (Bit a, Bit b, Bit c) => a -> [b] -> [c] -> Jazz (Argument, [Argument])
nadder c [] [] = do
  c <- funk c
  return (c, [])
nadder c (x:xs) (y:ys) = do
  (r, z) <- fulladder c x y
  (c, zs) <- nadder r xs ys
  return (c, z:zs)

shiftl :: (Bit a, Bit b) => [a] -> [b] -> Jazz [Argument]
shiftl xs ys =
  let n = List.genericLength xs in
  let aux :: (Bit a, Bit b) => Integer -> [a] -> [b] -> Jazz [Argument]
      aux i xs [] =
        mapM funk xs
      aux i xs (y:ys) = do
        a <- conc (List.genericReplicate i False) (List.genericTake (n-i) xs)
        b <- mux y a xs
        aux (2*i) b ys
  in
  aux 1 xs ys

shiftr :: (Bit a, Bit b) => [a] -> [b] -> Jazz [Argument]
shiftr xs ys = do
  zs <- shiftl (List.reverse xs) ys
  return $ List.reverse zs

foo :: Jazz ()
foo = do x <- input "x" 8
         y <- input "y" 3
         v <- shiftl x y
         w <- shiftr x y
         output "v" v
         output "w" w

netlist = build foo

