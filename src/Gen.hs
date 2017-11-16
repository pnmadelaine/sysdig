module Main where

import Netlist.Ast
import Netlist.Show
import Netlist.Build
import Netlist.Typer

fulladder a b c =
  let r = (a /\ b) \/ ((a \/ b) /\ c) in
  let s = a `xor` b `xor` c in
  (r, s)

nadder [x] [y] =
  let (z1, z0) = fulladder x y (Fcst [False]) in
  [z1,z0]
nadder (x:xs) (y:ys) =
  let z:zs = nadder xs ys in
  let (z1, z0) = fulladder x y z in
  z1:z0:zs

--

x = Winput "x"
y = Winput "y"

xs = smash x 8
ys = smash y 8

zs = nadder xs ys

z = funnel zs

--

inputs  = [ ("x", 8)
          , ("y", 8)
          ]

outputs = [ ("z", 9)
          ]

defs    = [ ("z", z)
          ]

--

net = build inputs outputs defs

main :: IO ()
main = do writeFile "nadder.net" (show net)
          case verify net of
            Left err -> putStrLn err
            Right _  -> return ()

