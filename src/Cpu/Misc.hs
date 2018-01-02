module Cpu.Misc where

import Netlist.Jazz

import Control.Monad (mapM, mapM_)
import Data.List as List

nonZero :: Wr a => a -> Jazz Bit
nonZero w =
  let aux :: Bt a => [a] -> Jazz Bit
      aux [x] = bit x
      aux (x:xs) = x \/ nonZero xs
  in
  bits w >>= aux

isZero :: Wr a => a -> Jazz Bit
isZero x = nonZero x >>= neg

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

-- signed and unsigned extension
-- signed n value
extend :: (Bt a, Wr b) => a -> Integer -> b -> Jazz Wire
extend s n x = do
  xs <- bits x
  a <- s /\ (List.head $ List.reverse xs)
  conc xs $ mux a
              (List.genericReplicate n True)
              (List.genericReplicate n False)

-- get_ctrl_alu :: Instr -> Jazz (Alu_control)

-- instr ram_output alu_output alu_flags
-- write_output :: (Bt a, Bt b) => Instr -> [a] -> [b] -> Alu_flag -> Jazz ()

-- instr data addr
-- memory :: (Bt a, Bt b) => Instr -> [a] -> [b] -> Jazz [Bit]

