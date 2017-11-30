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

shift :: (Bit a, Bit b, Bit c) => a -> [b] -> [c] -> Jazz [Argument]
shift side xs ys =
  let n = List.genericLength xs in
  let aux :: (Bit a, Bit b) => Integer -> [a] -> [b] -> Jazz [Argument]
      aux i xs [] =
        mapM funk xs
      aux i xs (y:ys) = do
        let l = List.genericReplicate i False
        a <- conc l (List.genericTake (n-i) xs)
        b <- conc (List.genericDrop i xs) l
        c <- mux side a b
        d <- mux y c xs
        aux (2*i) d ys
  in
  aux 1 xs ys

foo :: Jazz ()
foo = do x <- input "x" 8
         y <- input "y" 3
         [z] <- input "z" 1
         w <- shift z x y
         output "w" w

netlist = build foo

