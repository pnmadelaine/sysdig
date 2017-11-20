module Alu where

import qualified Data.List as List

import Netlist.Build

data ALU_options = ALU_options { enable_carry :: Formula
                               , enable_xor   :: Formula
                               , enable_and   :: Formula
                               , carry_in     :: Formula
                               , invert_x     :: Formula
                               , invert_y     :: Formula
                               }

alu_add = ALU_options { enable_carry = Fcst True
                      , enable_xor   = Fcst True
                      , enable_and   = Fcst False
                      , carry_in     = Fcst False
                      , invert_x     = Fcst False
                      , invert_y     = Fcst False
                      }
alu_sub = ALU_options { enable_carry = Fcst True
                      , enable_xor   = Fcst True
                      , enable_and   = Fcst False
                      , carry_in     = Fcst True
                      , invert_x     = Fcst False
                      , invert_y     = Fcst True
                      }
alu_or  = ALU_options { enable_carry = Fcst False
                      , enable_xor   = Fcst True
                      , enable_and   = Fcst True
                      , carry_in     = Fcst False
                      , invert_x     = Fcst False
                      , invert_y     = Fcst False
                      }
alu_xor = ALU_options { enable_carry = Fcst False
                      , enable_xor   = Fcst True
                      , enable_and   = Fcst False
                      , carry_in     = Fcst False
                      , invert_x     = Fcst False
                      , invert_y     = Fcst False
                      }
alu_and = ALU_options { enable_carry = Fcst False
                      , enable_xor   = Fcst False
                      , enable_and   = Fcst True
                      , carry_in     = Fcst False
                      , invert_x     = Fcst False
                      , invert_y     = Fcst False
                      }
alu_nand = ALU_options { enable_carry = Fcst False
                       , enable_xor   = Fcst True
                       , enable_and   = Fcst True
                       , carry_in     = Fcst False
                       , invert_x     = Fcst True
                       , invert_y     = Fcst True
                       }
                 
data ALU_flags = ALU_flags { carry_out :: Formula
                           -- zero
                           -- overflow
                           }

fulladder a b c =
  let r = (a /\ b) \/ ((a \/ b) /\ c) in
  let s = a <> b <> c in
  (r, s)

alu :: ALU_options -> [Formula] -> [Formula] -> (ALU_flags, [Formula])
alu options [] [] =
  ( ALU_flags{ carry_out = carry_in options
             }
  , []
  )
alu options (x:xs) (y:ys) =
  let (flags,zs) = alu options xs ys in
  let a = x <> invert_x options in
  let b = y <> invert_y options in
  let c = carry_out flags  in
  let (r, s) = fulladder a b c in
  let z = (s /\ enable_xor options) \/ (a /\ b /\ enable_and options) in
  ( ALU_flags { carry_out = r /\ enable_carry options
              }
  , z:zs
  )

--

x = Winput "x"
y = Winput "y"

xs = smash x 8 
ys = smash y 8

(_,zs) = alu alu_sub xs ys

z = funnel zs

--

inputs  = [ ("x", 8)
          , ("y", 8)
          ]

outputs = [ ("z", 8)
          ]

defs    = [ ("z", z)
          ]

--

netlist = build inputs outputs defs
