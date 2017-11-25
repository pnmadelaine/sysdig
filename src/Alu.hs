module Alu where

import qualified Data.List as List

import Netlist.Ast
import Netlist.Build

fulladder a b c = let r = (a /\ b) \/ ((a \/ b) /\ c) in
                  let s = a <> b <> c in
                  (r, s)

nadder as bs =
  List.foldl (\(c, zs) (a,b)-> let (r, z) = fulladder a b c in (r, z:zs))
             (argcst [False], [])
             (List.zip as bs)

test = let a = input "a" 8 in
       let b = input "b" 8 in
       let as = smash a 8 in
       let bs = smash b 8 in
       let (r, cs) = nadder as bs in
       let c = funnel cs in
       do output "r" 1 r
          output "c" 8 c

netlist = build test

