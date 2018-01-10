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
init_registers = do let l = [ "pc", "hi", "lo"]
                         ++ List.tail registers_names
                    mapM_ (\i -> new_reg i 32) l
                    new_reg "state" 6

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

handle_ram :: (Wr a, Wr b) => Instr -> a -> b -> Jazz Wire
handle_ram instr addr dt = do
  let write_mux = Opcode_mux { op_j       = wire [False, False, False, False]
                             , op_jal     = wire [False, False, False, False]
                             , op_beq     = wire [False, False, False, False]
                             , op_bne     = wire [False, False, False, False]
                             , op_addi    = wire [False, False, False, False]
                             , op_addiu   = wire [False, False, False, False]
                             , op_slti    = wire [False, False, False, False]
                             , op_sltiu   = wire [False, False, False, False]
                             , op_andi    = wire [False, False, False, False]
                             , op_ori     = wire [False, False, False, False]
                             , op_lui     = wire [False, False, False, False]
                             , op_lw      = wire [False, False, False, False]
                             , op_lbu     = wire [False, False, False, False]
                             , op_lhu     = wire [False, False, False, False]
                             , op_sb      = wire [True,  False, False, False]
                             , op_sh      = wire [True,  True,  False, False]
                             , op_sw      = wire [True,  True,  True,  True ]
                             , op_ll      = wire [False, False, False, False]
                             , op_sc      = wire [False, False, False, False] -- huh?

                             , op_sll     = wire [False, False, False, False]
                             , op_srl     = wire [False, False, False, False]
                             , op_sra     = wire [False, False, False, False]
                             , op_jr      = wire [False, False, False, False]
                             , op_mfhi    = wire [False, False, False, False]
                             , op_mflo    = wire [False, False, False, False]
                             , op_mult    = wire [False, False, False, False]
                             , op_multu   = wire [False, False, False, False]
                             , op_div     = wire [False, False, False, False]
                             , op_divu    = wire [False, False, False, False]
                             , op_add     = wire [False, False, False, False]
                             , op_addu    = wire [False, False, False, False]
                             , op_sub     = wire [False, False, False, False]
                             , op_subu    = wire [False, False, False, False]
                             , op_and     = wire [False, False, False, False]
                             , op_or      = wire [False, False, False, False]
                             , op_nor     = wire [False, False, False, False]
                             , op_slt     = wire [False, False, False, False]
                             , op_sltu    = wire [False, False, False, False]

                             , op_nop     = wire [False, False, False, False]
                             }

  a0 <- conc [False, False] (slice 2 32 addr)
  a1 <- conc [True,  False] (slice 2 32 addr)
  a2 <- conc [False, True]  (slice 2 32 addr)
  a3 <- conc [True,  True]  (slice 2 32 addr)

  d0 <- slice 0  8  dt
  d1 <- slice 8  16 dt
  d2 <- slice 16 24 dt
  d3 <- slice 24 32 dt

  b  <- opcode_mux instr write_mux
  b0 <- select 0 b
  b1 <- select 1 b
  b2 <- select 2 b
  b3 <- select 3 b

  w0 <- ram a0 b0 a0 d0
  w1 <- ram a1 b1 a1 d1
  w2 <- ram a2 b2 a2 d2
  w3 <- ram a3 b3 a3 d3
  let zero32 = wire (32 :: Integer, 0 :: Integer)
  let zero8  = wire (8 :: Integer,  0 :: Integer)
  let read_mux = Opcode_mux { op_j       = zero32
                            , op_jal     = zero32
                            , op_beq     = zero32
                            , op_bne     = zero32
                            , op_addi    = zero32
                            , op_addiu   = zero32
                            , op_slti    = zero32
                            , op_sltiu   = zero32
                            , op_andi    = zero32
                            , op_ori     = zero32
                            , op_lui     = zero32
                            , op_lw      = conc w0 $ conc w1 $ conc w2 w3
                            , op_lbu     = conc w0 $ conc zero8 $ conc zero8 zero8
                            , op_lhu     = conc w0 $ conc w1 $ conc zero8 zero8
                            , op_sb      = zero32
                            , op_sh      = zero32
                            , op_sw      = zero32
                            , op_ll      = zero32 --huh?
                            , op_sc      = zero32

                            , op_sll     = zero32
                            , op_srl     = zero32
                            , op_sra     = zero32
                            , op_jr      = zero32
                            , op_mfhi    = zero32
                            , op_mflo    = zero32
                            , op_mult    = zero32
                            , op_multu   = zero32
                            , op_div     = zero32
                            , op_divu    = zero32
                            , op_add     = zero32
                            , op_addu    = zero32
                            , op_sub     = zero32
                            , op_subu    = zero32
                            , op_and     = zero32
                            , op_or      = zero32
                            , op_nor     = zero32
                            , op_slt     = zero32
                            , op_sltu    = zero32

                            , op_nop     = zero32
                            }
  opcode_mux instr read_mux

