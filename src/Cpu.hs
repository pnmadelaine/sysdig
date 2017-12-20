module Cpu where

import Netlist.Ast
import Netlist.Build

import Control.Monad (mapM)
import Data.List as List

data Instr = Instr { inst_opcode :: [Argument]
                   , inst_rs     :: [Argument]
                   , inst_rd     :: [Argument]
                   , inst_rt     :: [Argument]
                   , inst_shamt  :: [Argument]
                   , inst_funct  :: [Argument]
                   , inst_imm    :: [Argument]
                   , inst_addr   :: [Argument]
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
reg_names = ["zero", "at"]

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

-- fetch :: Bit a => [a] -> Jazz [Argument]

-- decode :: Bit a => [a] -> Jazz Instr

-- get_ctrl_alu :: Instr -> Jazz (Alu_control)

-- alu :: (Bit a, Bit b) => Instr -> [a] -> [b] -> Jazz (Alu_flag, [Argument])

-- alu_inputs :: Instr -> Jazz ([Argument], [Argument])

-- instr ram_output alu_output alu_flags
-- write_output :: (Bit a, Bit b) => Instr -> [a] -> [b] -> Alu_flag -> Jazz ()

-- instr data addr
-- memory :: (Bit a, Bit b) => Instr -> [a] -> [b] -> Jazz [Argument]

cpu = do xs <- input "x" 8
         ys <- input "y" 3
         [d] <- input "d" 1
         zs <- shift d ys xs
         output "z" zs

netlist = build cpu

