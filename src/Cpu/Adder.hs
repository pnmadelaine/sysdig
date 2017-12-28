module Cpu.Adder where

import Netlist.Jazz

fulladder :: (Bt a, Bt b, Bt c) => a -> b -> c -> Jazz (Bit, Bit)
fulladder a b c = do
  r <- (a /\ b) \/ ((a \/ b) /\ c)
  s <- a <> b <> c
  return (r, s)

nadder :: (Bt a, Bt b, Bt c) => [a] -> [b] -> c -> Jazz (Bit, [Bit])
nadder [] [] c_in = bit c_in >>= \c_out -> return (c_out, [])
nadder (x:xs) (y:ys) c_in = do
  (r, s) <- fulladder x y c_in
  (c_out, zs) <- nadder xs ys r
  return (c_out, s:zs)

adder :: (Wr a, Wr b, Bt c) => a -> b -> c -> Jazz (Bit, Wire)
adder a b c_in = do
  xs <- bits a
  ys <- bits b
  (c_out, zs) <- nadder xs ys c_in
  w <- wire zs
  return (c_out, w)

