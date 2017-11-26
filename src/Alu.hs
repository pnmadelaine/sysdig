module Alu where

import qualified Data.List as List

import Netlist.Ast
import Netlist.Build

data ALU_options = ALU_options { enable_carry :: Jazz
                               , enable_xor   :: Jazz
                               , enable_and   :: Jazz
                               , carry_in     :: Jazz
                               , invert_x     :: Jazz
                               , invert_y     :: Jazz
                               }

alu_add = ALU_options { enable_carry = argcst [True]
                      , enable_xor   = argcst [True]
                      , enable_and   = argcst [False]
                      , carry_in     = argcst [False]
                      , invert_x     = argcst [False]
                      , invert_y     = argcst [False]
                      }
alu_sub = ALU_options { enable_carry = argcst [True]
                      , enable_xor   = argcst [True]
                      , enable_and   = argcst [False]
                      , carry_in     = argcst [True]
                      , invert_x     = argcst [False]
                      , invert_y     = argcst [True]
                      }
alu_or  = ALU_options { enable_carry = argcst [False]
                      , enable_xor   = argcst [True]
                      , enable_and   = argcst [True]
                      , carry_in     = argcst [False]
                      , invert_x     = argcst [False]
                      , invert_y     = argcst [False]
                      }
alu_xor = ALU_options { enable_carry = argcst [False]
                      , enable_xor   = argcst [True]
                      , enable_and   = argcst [False]
                      , carry_in     = argcst [False]
                      , invert_x     = argcst [False]
                      , invert_y     = argcst [False]
                      }
alu_and = ALU_options { enable_carry = argcst [False]
                      , enable_xor   = argcst [False]
                      , enable_and   = argcst [True]
                      , carry_in     = argcst [False]
                      , invert_x     = argcst [False]
                      , invert_y     = argcst [False]
                      }
alu_nand = ALU_options { enable_carry = argcst [False]
                       , enable_xor   = argcst [True]
                       , enable_and   = argcst [True]
                       , carry_in     = argcst [False]
                       , invert_x     = argcst [True]
                       , invert_y     = argcst [True]
                       }

data ALU_flags = ALU_flags { carry_out :: Jazz
                           -- zero
                           -- overflow
                           }

fulladder a b c =
  let r = (a /\ b) \/ ((a \/ b) /\ c) in
  let s = a <> b <> c in
  (r, s)

alu :: ALU_options -> [Jazz] -> [Jazz] -> (ALU_flags, [Jazz])
alu options xs ys =
  let alu_aux :: Jazz -> (Jazz, Jazz) -> (Jazz, Jazz)
      alu_aux c (a,b) =
        let (r, s) = fulladder a b c in
        let z = (s /\ enable_xor options) \/ (a /\ b /\ enable_and options) in
        ( r /\ enable_carry options, z )
  in
  let as = List.map ((<>) (invert_x options)) xs in
  let bs = List.map ((<>) (invert_y options)) ys in
  let c_in = carry_in options in
  let (c_out, zs) = List.mapAccumL alu_aux c_in (zip as bs) in
  ( ALU_flags { carry_out = c_out
              }
  , zs
  )

foo = let x = argvar "x" in
      let y = argvar "y" in
      let xs = smash x 8 in
      let ys = smash y 8 in
      let (flags, zs) = alu alu_sub xs ys in
      let z = funnel zs in
      do input "x" 8
         input "y" 8
         output "z" 8 z
         output "carry_out" 1 (carry_out flags)

netlist = build foo

