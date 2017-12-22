module Cpu where

import Netlist.Ast
import Netlist.Build

import Control.Monad (mapM, mapM_)
import Data.List as List

data Instr = Instr { instr_opcode :: [Argument]
                   , instr_rs     :: [Argument]
                   , instr_rd     :: [Argument]
                   , instr_rt     :: [Argument]
                   , instr_shamt  :: [Argument]
                   , instr_funct  :: [Argument]
                   , instr_imm    :: [Argument]
                   , instr_addr   :: [Argument]
                   }

data Alu_control = Alu_control { alu_enable_carry :: Argument
                               , alu_carry_in     :: Argument
                               , alu_enable_xor   :: Argument
                               , alu_enable_and   :: Argument
                               , alu_invert_x     :: Argument
                               , alu_invert_y     :: Argument
                               , alu_shift_left   :: Argument
                               , alu_shift_value  :: [Argument]
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
  x <- funk c
  return (x, [])
nadder c (x:xs) (y:ys) = do
  (c', z) <- fulladder c x y
  (c_out, zs) <- nadder c' xs ys
  return (c_out, z:zs)

reg_names :: [Ident]
reg_names = ["zero",
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

multiplex :: Bit a => (Integer -> Jazz [Argument]) -> [a] -> Jazz [Argument]
multiplex f xs =
  let aux :: Bit a => Integer -> Integer -> [a] -> Jazz [Argument]
      aux _ j [] = f j
      aux i j (x:xs) =
        mux x
          (aux (2*i) (j+i) xs)
          (aux (2*i) j     xs)
  in
  aux 1 0 xs

-- direction number_of_shifts value
shift :: (Bit a, Bit b, Bit c) => a -> [b] -> [c] -> Jazz [Argument]
shift a ws xs =
  let aux :: (Bit a, Bit b) => Integer -> [a] -> [b] -> Jazz [Argument]
      aux _ [] xs = mapM funk xs
      aux i (w:ws) xs = do
        xs <- aux (2*i) ws xs
        y1 <- conc
                (List.genericReplicate i False)
                (List.genericTake (List.genericLength xs - i) xs)
        y2 <- conc
                (List.genericDrop i xs)
                (List.genericReplicate i False)
        ys <- mux a y1 y2
        mux w ys xs
  in
  aux 1 ws xs

-- program_counter :: Instr -> Alu_flag -> Jazz [Argument]

-- signed n value
-- extend :: (Bit a, Bit b) => a -> Integer -> [b] -> Jazz [Argument]

fetch :: Bit a => [a] -> Jazz [Argument]
fetch = rom

decode :: Wire a => a -> Jazz Instr
decode w = do
  instr <- prog w
  return $ Instr { instr_opcode = List.drop 6 instr
                 , instr_rs = List.take 5 (List.drop 11 instr)
                 , instr_rd = List.take 5 (List.drop 21 instr)
                 , instr_rt = List.take 5 (List.drop 16 instr)
                 , instr_shamt = List.take 5 (List.drop 26 instr)
                 , instr_funct = List.take 6 instr
                 , instr_imm = List.take 16 instr
                 , instr_addr = List.take 26 instr
                 }

-- get_ctrl_alu :: Instr -> Jazz (Alu_control)

-- alu :: (Bit a, Bit b) => Instr -> [a] -> [b] -> Jazz (Alu_flag, [Argument])

-- alu_inputs :: Instr -> Jazz ([Argument], [Argument])

-- instr ram_output alu_output alu_flags
-- write_output :: (Bit a, Bit b) => Instr -> [a] -> [b] -> Alu_flag -> Jazz ()

-- instr data addr
-- memory :: (Bit a, Bit b) => Instr -> [a] -> [b] -> Jazz [Argument]

f i = prog [mod i 2 == 1]

cpu = do xs <- input "x" 4
         ys <- multiplex f xs
         output "y" ys

netlist = build cpu

