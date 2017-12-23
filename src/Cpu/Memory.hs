module Cpu.Memory where

import Netlist.Jazz
import Cpu.Misc

import Control.Monad (mapM, mapM_)
import Data.List as List

registers_names :: [String]
registers_names = ["zero",
                   "at",
                   "v0", "v1",
                   "a0", "a1", "a2", "a3",
                   "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7",
                   "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7",
                   "t8", "t9",
                   "k0", "k1",
                   "gp",
                   "sp",
                   "fp",
                   "ra"
                   ]

init_registers :: Jazz ()
init_registers = mapM_ (\i -> new_reg i 32) $ ["pc"]
                                           ++ List.tail registers_names

nth 0 (x:xs) = x
nth i (x:xs) = nth (i-1) xs

f :: Integer -> Jazz Wire
f 0 = wire $ List.replicate 32 False
f i = reg_out (nth i registers_names)

g :: Wr a => Integer -> a -> Integer -> Jazz Wire
g i x j = if i == j then wire x
                    else reg_out (nth i registers_names)

read_reg :: Wr a => a -> Jazz Wire
read_reg x = multiplex f x

write_reg :: (Bt a, Bt b) => [a] -> [b] -> Jazz ()
write_reg addr xs =
  mapM_ (\i -> reg_in (nth i registers_names) (multiplex (g i xs) addr)) [1..31]

-- reads pc and fetches the instruction
fetch :: Jazz [Bit]
fetch = do
  x <- reg_out "pc"
  bits $ rom x

-- updates the pc at the end of the cycle
branch :: Instr -> Jazz ()
branch instr = do
  xs <- reg_out "pc"
  ys <- bits xs -- todo: branching 
  reg_in "pc" ys

