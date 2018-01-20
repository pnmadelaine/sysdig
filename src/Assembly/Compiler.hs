module Assembly.Compiler where

import qualified Data.List as List
import Assembly.Ast

print_tab :: [Bool] -> String
print_tab l = List.reverse . List.map aux $ l
    where aux True = '1'
          aux False = '0'


print_instr :: Instr -> String
print_instr i =
  aux2 0 . aux1 $ i
    where aux1 (Rexpr opcode rs rt rd shamt funct) =
            List.concat . List.map print_tab $ [opcode, rs, rt, rd, shamt, funct]
          aux1 (Iexpr opcode rs rt imm) =
            List.concat . List.map print_tab $ [opcode, rs, rt, imm]
          aux1 (Jump opcode address) =
            List.concat . List.map print_tab $ [opcode, address]
          aux1 _ = error "wrong instruction"

          aux2 _ [] = []
          aux2 8 l  = ' ':(aux2 0 l)
          aux2 i (x:xs) = x:(aux2 (i+1) xs)

print_prog :: Prog -> String
print_prog p = List.unlines . List.map print_instr . List.reverse $ p
