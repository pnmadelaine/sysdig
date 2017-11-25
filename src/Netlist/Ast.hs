module Netlist.Ast where

import qualified Data.Map.Strict as Map

type Ident = String
type Value = [Bool]

data BinOp = Or | Xor | And | Nand
             deriving (Eq, Ord)

data Argument = ArgCst Value | ArgVar Ident
                deriving (Eq, Ord)

data Expression = Earg Argument
                | Ereg Ident
                | Enot Argument
                | Ebinop BinOp Argument Argument
                | Emux Argument Argument Argument
                | Erom Integer Integer Argument
                  -- addr_size word_size read_addr
                | Eram Integer Integer Argument Argument Argument Argument
                  -- addr_size word_size read_addr write_enable write_addr data
                | Econcat Argument Argument
                | Eslice Integer Integer Argument
                | Eselect Integer Argument
                deriving (Eq, Ord)

type Equation = (Ident, Expression)

data Netlist = Netlist { netlist_eq  :: [Equation]
                       , netlist_in  :: [Ident]
                       , netlist_out :: [Ident]
                       , netlist_var :: [(Ident,Integer)]
                       }

type Ram  = Map.Map Integer Value
type Vars = Map.Map Ident Value

