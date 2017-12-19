module Cpu where

import Netlist.Ast
import Netlist.Build

import Control.Monad (mapM)

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
  funk (x, [])
nadder c (x:xs) (y:ys) = do
  (c', z) <- fulladder c x y
  (c_out, zs) <- nadder c' xs ys
  return (c_out, z:zs)

reg_names :: [Ident]
reg_names = ["zero", "at"]

-- direction number_of_shifts value
shift :: (Bit a, Bit b, Bit c) => a -> [b] -> [c] -> Jazz [Argument]

program_counter :: Instr -> Alu_flag -> Jazz [Argument]

-- signed n value
extend :: (Bit a, Bit b) => a -> Integer -> [b] -> Jazz [Argument]

fetch :: Bit a => [a] -> Jazz [Argument]

decode :: Bit a => [a] -> Jazz Instr

get_ctrl_alu :: Instr -> Jazz (Alu_control)

alu :: (Bit a, Bit b) => Instr -> [a] -> [b] -> Jazz (Alu_flag, [Argument])

alu_inputs :: Instr -> Jazz ([Argument], [Argument])

-- instr ram_output alu_output alu_flags
write_output :: (Bit a, Bit b) => Instr -> [a] -> [b] -> Alu_flag -> Jazz ()

-- instr data addr
memory :: (Bit a, Bit b) => Instr -> [a] -> [b] -> Jazz [Argument]

cpu = do xs <- input "x" 4
         w <- read_reg "boloss" 4
         (c_out, zs) <- nadder False xs boloss
         write_reg "boloss" zs

netlist = build cpu

