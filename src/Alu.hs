module Alu where

import qualified Data.List as List

import Netlist.Ast
import Netlist.Build

data ALU_options = ALU_options { enable_add   :: Argument
                               , carry_in     :: Argument
                               , invert_x     :: Argument
                               , invert_y     :: Argument
                               , shift_value  :: Argument
                               , shift_l      :: Argument
                               , enable_xor   :: Argument
                               , enable_and   :: Argument
                               }

data ALU_flags = ALU_flags { carry_out :: Argument
                          -- zero
                          -- overflow
                           }

fulladder :: Argument -> Argument -> Argument -> Jazz (Argument, Argument)
fulladder x y z = do
  let a = arg x
  let b = arg y
  let c = arg z
  r <- (a /\ b) \/ ((a \/ b) /\ c)
  s <- a <> b <> c
  return (r, s)

shift_left :: [Argument] -> [Argument] -> Jazz [Argument]
shift_left zs xs =
  let aux :: [Argument] -> [Argument] -> Integer -> Jazz [Argument]
      aux [] xs _ = return xs
      aux (z:zs) xs i = do let l = List.genericReplicate i (ArgCst [False])
                           as <- aux zs xs (2*i)
                           let bs = l ++ List.genericTake (List.genericLength xs - i) as
                           w <- mux (arg z) (funnel bs) (funnel as)
                           smash w (List.genericLength as)
  in
  aux zs xs 1

shift_right zs xs = do ys <- shift_left zs (List.reverse xs)
                       return $ List.reverse ys

nadder :: Argument -> [Argument] -> [Argument] -> Jazz (Argument, [Argument])
nadder c_in [] [] = return (c_in, [])
nadder c_in (x:xs) (y:ys) = do
  (r, z) <- fulladder c_in x y
  (c_out, zs) <- nadder r xs ys
  return (c_out, z:zs)

aux :: [Jazz Argument] -> Jazz [Argument]
aux [] = return []
aux (x:xs) = do a <- x
                l <- aux xs
                return $ a:l

alu :: ALU_options -> Argument -> Argument -> Jazz (ALU_flags, Argument)
alu options x y = do
  xs <- smash x 8
  ys <- smash y 8
  let w1 = List.map ((<>) (arg $ invert_x options)) (List.map arg xs)
  let w2 = List.map ((<>) (arg $ invert_y options)) (List.map arg ys)
  let w3 = List.map (\(x,y) -> (x <> y) /\ (arg $ enable_xor options)) (zip w1 w2)
  let w4 = List.map (\(x,y) -> x /\ y /\ (arg $ enable_and options)) (zip w1 w2)
  let w5 = List.map (\(x,y) -> x \/ y) (zip w3 w4)
  l1 <- aux w5
  (c_out, l2) <- nadder (carry_in options) xs ys
  w1 <- mux (arg $ enable_add options) (funnel l2) (funnel l1)
  l1 <- smash w1 8
  shft <- smash (shift_value options) 4
  l2 <- shift_left shft l1
  l3 <- shift_right shft l2
  w1 <- mux (arg $ shift_l options) (funnel l2) (funnel l3)
  let flags = ALU_flags { carry_out = c_out
                        }
  return (flags, w1)

foo = do
  x <- input "x" 8
  y <- input "y" 8
  o1 <- input "enable_add" 1
  o2 <- input "carry_in" 1
  o3 <- input "invert_x" 1
  o4 <- input "invert_y" 1
  o5 <- input "shift_value" 4
  o6 <- input "shift_left" 1
  o7 <- input "enable_xor" 1
  o8 <- input "enable_and" 1
  let options = ALU_options { enable_add  = o1
                            , carry_in    = o2
                            , invert_x    = o3
                            , invert_y    = o4
                            , shift_value = o5
                            , shift_l     = o6
                            , enable_xor  = o7
                            , enable_and  = o8
                            }
  (r,z) <- alu options x y
  output "z" 8 (arg z)

netlist = build foo

