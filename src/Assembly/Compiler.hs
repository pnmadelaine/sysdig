module Assembly.Compiler where

import qualified Data.List as List
import Assembly.Ast

print_tab :: [Bool] -> String
print_tab [] = ""
print_tab l = (aux (List.head l)) ++ (print_tab (List.tail l))
    where aux True = "1"
          aux False = "0"


print_instr :: Instr -> String
print_instr (Rexpr o s t d shamt funct) = (print_tab funct) ++ (print_tab shamt) ++ (print_tab d) ++ (print_tab t) ++ (print_tab s) ++ (print_tab o)
print_instr (Iexpr o s t imm) = (print_tab imm) ++ (print_tab t) ++ (print_tab s) ++ (print_tab o)
print_instr (Jump o a) = (print_tab a) ++ (print_tab o)
print_instr _ = error "wrong instruction"


print_prog :: Prog -> String
print_prog p = List.unlines (List.map print_instr p)
