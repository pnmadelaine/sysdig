module Cpu.Misc where

import Netlist.Build

import Control.Monad (mapM, mapM_)
import Data.List as List

data Instr = Instr { instr_opcode :: [Bit]
                   , instr_rs     :: [Bit]
                   , instr_rd     :: [Bit]
                   , instr_rt     :: [Bit]
                   , instr_shamt  :: [Bit]
                   , instr_funct  :: [Bit]
                   , instr_imm    :: [Bit]
                   , instr_addr   :: [Bit]
                   }

multiplex :: Bt a => (Integer -> Jazz [Bit]) -> [a] -> Jazz [Bit]
multiplex f xs =
  let aux :: Bt a => Integer -> Integer -> [a] -> Jazz [Bit]
      aux _ j [] = f j
      aux i j (x:xs) =
        mux x
          (aux (2*i) (j+i) xs)
          (aux (2*i) j     xs)
  in
  aux 1 0 xs

-- direction number_of_shifts value
shift :: (Bt a, Bt b, Bt c) => a -> [b] -> [c] -> Jazz [Bit]
shift a ws xs =
  let aux :: (Bt a, Bt b) => Integer -> [a] -> [b] -> Jazz [Bit]
      aux _ [] xs = mapM bit xs
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

-- signed and unsigned extension
-- signed n value
extend :: (Bt a, Bt b) => a -> Integer -> [b] -> Jazz [Bit]
extend s n xs = do
  a <- s /\ (List.head $ List.reverse xs)
  bits $ conc xs $ mux a
              (List.genericReplicate n True)
              (List.genericReplicate n False)

-- decodes the instruction
decode :: Wr a => a -> Jazz Instr
decode w = do
  instr <- bits w
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

-- alu :: (Bt a, Bt b) => Instr -> [a] -> [b] -> Jazz (Alu_flag, [Bit])

-- alu_inputs :: Instr -> Jazz ([Bit], [Bit])

-- instr ram_output alu_output alu_flags
-- write_output :: (Bt a, Bt b) => Instr -> [a] -> [b] -> Alu_flag -> Jazz ()

-- instr data addr
-- memory :: (Bt a, Bt b) => Instr -> [a] -> [b] -> Jazz [Bit]

