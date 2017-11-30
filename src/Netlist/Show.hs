module Netlist.Show where

import qualified Data.List as List
import Netlist.Ast

join s = List.foldl (++) "" . List.intersperse s

string_of_var (id, 1) = id
string_of_var (id, n) = id ++ ":" ++ show n

string_of_bool_list w = aux "" w
    where aux acc []         = acc
          aux acc (True:xs)  = aux ('1':acc) xs
          aux acc (False:xs) = aux ('0':acc) xs

instance Show BinOp where
  show Or   = "OR"
  show Xor  = "XOR"
  show And  = "AND"
  show Nand = "NAND"

instance Show Argument where
  show (ArgCst w) = string_of_bool_list w
  show (ArgVar i) = i

instance Show Expression where
  show (Earg a)           = show a
  show (Ereg id)          = "REG " ++ id
  show (Enot a)           = "NOT " ++ show a
  show (Ebinop op a b)    = show op ++ " " ++ show a
                                    ++ " " ++ show b
  show (Emux a b c)       = "MUX " ++ show a ++ " " ++ show b
                                             ++ " " ++ show c
  show (Erom a)           = "ROM " ++ show a
  show (Eram a b c d)     = "RAM " ++ show a ++ " " ++ show b
                            ++ " " ++ show c ++ " " ++ show d
  show (Econcat a b)      = "CONCAT " ++ show a ++ " " ++ show b
  show (Eslice i j a)     = "SLICE "  ++ show i ++ " " ++ show j
                                                ++ " " ++ show a
  show (Eselect i a)      = "SELECT " ++ show i ++ " " ++ show a

string_of_equation (id, expr) = id ++ " = " ++ show expr

instance Show Netlist where
  show netlist = "INPUT "  ++ join ", " (netlist_in netlist) ++ "\n"
              ++ "OUTPUT " ++ join ", " (netlist_out netlist) ++ "\n"
              ++ "VAR "    ++ join ", " (List.map string_of_var (netlist_var netlist)) ++ "\n"
              ++ "IN\n"    ++ join "\n" (List.map string_of_equation (netlist_eq netlist)) 
              ++ "\n"
