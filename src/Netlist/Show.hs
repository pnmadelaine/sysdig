module Netlist.Show where

import qualified Data.List as List
import Netlist.Ast

join s = List.foldl (++) "" . List.intersperse s

string_of_wire w = aux "" w
  where aux acc []         = acc
        aux acc (True:xs)  = aux ("1"++acc) xs
        aux acc (False:xs) = aux ("0"++acc) xs

string_of_arg (ArgCst w) = string_of_wire w
string_of_arg (ArgVar i) = i

string_of_binop Or   = "OR"
string_of_binop Xor  = "XOR"
string_of_binop And  = "AND"
string_of_binop Nand = "NAND"

string_of_expr (Earg a)           = string_of_arg a
string_of_expr (Ereg id)          = "REG " ++ id
string_of_expr (Enot a)           = "NOT " ++ string_of_arg a
string_of_expr (Ebinop op a b)    = string_of_binop op ++ " " ++ string_of_arg a
                                                       ++ " " ++ string_of_arg b
string_of_expr (Emux a b c)       = "MUX " ++ string_of_arg a ++ " " ++ string_of_arg b
string_of_expr (Erom i j a)       = "ROM " ++ show i ++ " " ++ show j ++ " " ++ string_of_arg a
string_of_expr (Eram i j a b c d) = "RAM " ++ show i ++ " " ++ show j 
                                           ++ " " ++ string_of_arg a ++ " " ++ string_of_arg b
                                           ++ " " ++ string_of_arg c ++ " " ++ string_of_arg d
string_of_expr (Econcat a b)      = "CONCAT " ++ string_of_arg a ++ " " ++ string_of_arg b
string_of_expr (Eslice i j a)     = "SLICE "  ++ show i ++ " " ++ show j
                                              ++ " " ++ string_of_arg a
string_of_expr (Eselect i a)      = "SELECT " ++ show i ++ " " ++ string_of_arg a

string_of_equation (id, expr) = id ++ " = " ++ string_of_expr expr

string_of_var_with_size (id, 1) = id
string_of_var_with_size (id, n) = id ++ ":" ++ show n

string_of_netlist netlist = 
     "INPUT "  ++ join ", " (input netlist) ++ "\n"
  ++ "OUTPUT " ++ join ", " (output netlist) ++ "\n"
  ++ "VAR "    ++ join ", " (List.map string_of_var_with_size (var netlist)) ++ "\n"
  ++ "IN\n"    ++ join "\n" (List.map string_of_equation (equations netlist)) 
  ++ "\n"

instance Show Netlist where
  show = string_of_netlist
 
