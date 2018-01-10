module Cpu.Misc where

import Netlist.Jazz

import Control.Monad (mapM, mapM_)
import Data.List as List

zero32 = wire (32 :: Integer, 0 :: Integer)
zero8 = wire (8 :: Integer, 0 :: Integer)

nonZero :: Wr a => a -> Jazz Bit
nonZero w =
  let aux :: [Bit] -> Jazz Bit
      aux [x] = bit x
      aux (x:xs) = x \/ nonZero xs
  in
  bits w >>= aux

isZero :: Wr a => a -> Jazz Bit
isZero x = nonZero x >>= neg

comp_unsigned :: (Wr a, Wr b) => a -> b -> Jazz Bit
comp_unsigned x y = do
  let aux [] [] = bit False
      aux (x:xs) (y:ys) = (neg x /\ y ) \/ (x /\ y /\ aux xs ys)
  xs <- bits x
  ys <- bits y
  aux xs ys

xor_wire :: (Wr a, Wr b) => a -> b -> Jazz Wire
xor_wire a b = do
  xs <- bits a
  ys <- bits b
  let zs = List.map (\(x, y) -> x <> y) (List.zip xs ys)
  wire zs

multiplex :: Wr a => (Integer -> Jazz Wire) -> a -> Jazz Wire
multiplex f x =
  let aux :: Bt a => Integer -> Integer -> [a] -> Jazz Wire
      aux _ j [] = f j
      aux i j (x:xs) = do
        a <- aux (2*i) (j+i) xs
        b <- aux (2*i) j     xs
        mux x a b
  in
  bits x >>= \l -> aux 1 0 l

left_shift :: (Wr a, Wr b) => a -> b -> Jazz Wire
left_shift value sh = do
  u <- wire value
  n <- wire_size u
  let aux :: Integer -> [Bit] -> Jazz Wire
      aux _ [] = wire u
      aux i (x:xs) = do
        u1 <- aux (2*i) xs
        u2 <- conc (List.genericReplicate i False) (slice 0 (n-i) u1)
        mux x u2 u1
  xs <- bits sh
  aux 1 xs

right_shift :: (Wr a, Wr b) => a -> b -> Jazz Wire
right_shift value sh = do
  u <- wire value
  n <- wire_size u
  let aux :: Integer -> [Bit] -> Jazz Wire
      aux _ [] = wire u
      aux i (x:xs) = do
        u1 <- aux (2*i) xs
        u2 <- conc (slice i n u1) (List.genericReplicate i False)
        mux x u2 u1
  xs <- bits sh
  aux 1 xs

right_arith_shift :: (Wr a, Wr b) => a -> b -> Jazz Wire
right_arith_shift value sh = do
  u <- wire value
  n <- wire_size u
  ext <- select (n-1) u
  let aux :: Integer -> [Bit] -> Jazz Wire
      aux _ [] = wire u
      aux i (x:xs) = do
        u1 <- aux (2*i) xs
        u2 <- conc (slice i n u1) (List.genericReplicate i ext)
        mux x u2 u1
  xs <- bits sh
  aux 1 xs

{-
-- signed and unsigned extension
-- signed n value
extend :: (Bt a, Wr b) => a -> Integer -> b -> Jazz Wire
extend s n x = do
  xs <- bits x
  a <- s /\ (List.head $ List.reverse xs)
  conc xs $ mux a
              (List.genericReplicate n True)
              (List.genericReplicate n False)
-}

