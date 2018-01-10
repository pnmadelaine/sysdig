module Cpu.Mult where

import Netlist.Jazz

import Cpu.Misc
import Cpu.Adder
import Cpu.Instr
import Cpu.Memory
import Cpu.Control

-- IMPORTANT / TODO
-- actuellement la multiplication est toujours non signée !!!
-- il faut changer l'initialisation de hilo dans le cas de la multiplication signée

multiplying :: Instr -> Jazz Bit
multiplying instr = do
  let ctrl_mux = Opcode_mux { op_j       = wire [False]
                            , op_jal     = wire [False]
                            , op_beq     = wire [False]
                            , op_bne     = wire [False]
                            , op_addi    = wire [False]
                            , op_addiu   = wire [False]
                            , op_slti    = wire [False]
                            , op_sltiu   = wire [False]
                            , op_andi    = wire [False]
                            , op_ori     = wire [False]
                            , op_lui     = wire [False]
                            , op_lw      = wire [False]
                            , op_lbu     = wire [False]
                            , op_lhu     = wire [False]
                            , op_sb      = wire [False]
                            , op_sh      = wire [False]
                            , op_sw      = wire [False]
                            , op_ll      = wire [False]
                            , op_sc      = wire [False]

                            , op_sll     = wire [False]
                            , op_srl     = wire [False]
                            , op_sra     = wire [False]
                            , op_jr      = wire [False]
                            , op_mfhi    = wire [False]
                            , op_mflo    = wire [False]
                            , op_mult    = wire [True]
                            , op_multu   = wire [True]
                            , op_div     = wire [False]
                            , op_divu    = wire [False]
                            , op_add     = wire [False]
                            , op_addu    = wire [False]
                            , op_sub     = wire [False]
                            , op_subu    = wire [False]
                            , op_and     = wire [False]
                            , op_or      = wire [False]
                            , op_nor     = wire [False]
                            , op_slt     = wire [False]
                            , op_sltu    = wire [False]

                            , op_nop     = wire [False]
                            }
  bit (select 0 (opcode_mux instr ctrl_mux))

dividing :: Instr -> Jazz Bit
dividing instr = do
  let ctrl_mux = Opcode_mux { op_j       = wire [False]
                            , op_jal     = wire [False]
                            , op_beq     = wire [False]
                            , op_bne     = wire [False]
                            , op_addi    = wire [False]
                            , op_addiu   = wire [False]
                            , op_slti    = wire [False]
                            , op_sltiu   = wire [False]
                            , op_andi    = wire [False]
                            , op_ori     = wire [False]
                            , op_lui     = wire [False]
                            , op_lw      = wire [False]
                            , op_lbu     = wire [False]
                            , op_lhu     = wire [False]
                            , op_sb      = wire [False]
                            , op_sh      = wire [False]
                            , op_sw      = wire [False]
                            , op_ll      = wire [False]
                            , op_sc      = wire [False]

                            , op_sll     = wire [False]
                            , op_srl     = wire [False]
                            , op_sra     = wire [False]
                            , op_jr      = wire [False]
                            , op_mfhi    = wire [False]
                            , op_mflo    = wire [False]
                            , op_mult    = wire [False]
                            , op_multu   = wire [False]
                            , op_div     = wire [True]
                            , op_divu    = wire [True]
                            , op_add     = wire [False]
                            , op_addu    = wire [False]
                            , op_sub     = wire [False]
                            , op_subu    = wire [False]
                            , op_and     = wire [False]
                            , op_or      = wire [False]
                            , op_nor     = wire [False]
                            , op_slt     = wire [False]
                            , op_sltu    = wire [False]

                            , op_nop     = wire [False]
                            }
  bit (select 0 (opcode_mux instr ctrl_mux))

update_state :: Instr -> (Bit, Wire) -> Jazz ()
update_state instr (c_out, res) = do
  state <- reg_out "state"
  hi <- reg_out "hi"
  lo <- reg_out "lo"
  rs <- read_reg (instr_rs instr)
  rt <- read_reg (instr_rt instr)

  hi_mult <- mux (select 0 lo) (conc (slice 1 32 res) [c_out]) (conc (slice 1 32 hi) [False])
  lo_mult <- conc (slice 1 32 lo) (mux (select 0 lo) [select 0 res] [select 0 hi])
  hi_div <- mux c_out res (conc [select 31 lo] (slice 0 31 hi))
  lo_div <- conc [c_out] (slice 0 31 lo)

  hi' <- mux (isZero state) zero32 $ mux (multiplying instr) hi_mult hi_div
  lo' <- mux (isZero state) rs $     mux (multiplying instr) lo_mult lo_div

  reg_in "hi" $ mux (multiplying instr \/ dividing instr) hi' hi
  reg_in "lo" $ mux (multiplying instr \/ dividing instr) lo' lo

  (_, x) <- adder (6 :: Integer, 0 :: Integer) state True
  state' <- mux (dividing instr \/ multiplying instr) x state
  reg_in "state" (mux (select 5 state) (6 :: Integer, 0 :: Integer) state')

