module Cpu.Memory where

import Control.Monad (mapM, mapM_)
import Data.List as List

import Netlist.Jazz

import Cpu.Misc
import Cpu.Instr
import Cpu.Control

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
init_registers = do mapM_ (\i -> new_reg i 32) $ ["pc", "mult_acc", "hi", "lo"]
                                              ++ List.tail registers_names
                    new_reg "mult_state" 5

nth 0 (x:xs) = x
nth i (x:xs) = nth (i-1) xs

read_reg :: Wr a => a -> Jazz Wire
read_reg x =
  let f :: Integer -> Jazz Wire
      f 0 = wire $ List.replicate 32 False
      f i = reg_out (nth i registers_names)
  in
  multiplex f x

write_reg :: (Wr a, Wr b) => a -> b -> Jazz ()
write_reg addr xs = do
  let g :: Wr a => Integer -> a -> Integer -> Jazz Wire
      g i x j = if i == j then wire x
                          else reg_out (nth i registers_names)
  mapM_ (\i -> reg_in (nth i registers_names) (multiplex (g i xs) addr)) [1..31]

output_reg :: Instr -> Jazz Wire
output_reg instr = do
  let rt = wire $ instr_rt instr
  let rd = wire $ instr_rd instr
  let zero = wire (5 :: Integer, 0 :: Integer)
  let ctrl_mux = Opcode_mux { op_j       = zero
                            , op_jal     = wire (5 :: Integer, 31 :: Integer)
                            , op_beq     = zero
                            , op_bne     = zero
                            , op_addi    = rt
                            , op_addiu   = rt
                            , op_slti    = rt
                            , op_sltiu   = rt
                            , op_andi    = rt
                            , op_ori     = rt
                            , op_lui     = rt
                            , op_lw      = rt
                            , op_lbu     = rt
                            , op_lhu     = rt
                            , op_sb      = zero
                            , op_sh      = zero
                            , op_sw      = zero
                            , op_ll      = rt
                            , op_sc      = rt

                            , op_sll     = rd
                            , op_srl     = rd
                            , op_sra     = rd
                            , op_jr      = rt
                            , op_mfhi    = rd
                            , op_mflo    = rd
                            , op_mult    = zero
                            , op_multu   = zero
                            , op_div     = zero
                            , op_divu    = zero
                            , op_add     = rd
                            , op_addu    = rd
                            , op_sub     = rd
                            , op_subu    = rd
                            , op_and     = rd
                            , op_or      = rd
                            , op_nor     = rd
                            , op_slt     = rd
                            , op_sltu    = rd

                            , op_nop     = zero
                            }
  opcode_mux instr ctrl_mux

