module Main where

import Netlist.Ast
import Netlist.Show
import Netlist.Parser (read_netlist)
import Netlist.Typer
import Netlist.Scheduler
import Netlist.Simulator

import System.IO
import System.Environment
import System.FilePath.Posix
import System.Console.GetOpt
import Control.Monad (guard, foldM, mapM_)

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Lazy ((!))
import Data.Maybe (fromJust)

data Flag =
    PrintOnly
  | NumberOfSteps Integer
  | LoadRom FilePath
  | LoadRam FilePath
  | Optimize
  deriving (Eq, Show)

option_descriptions :: [OptDescr Flag]
option_descriptions =
  [ Option ['n'] []
           (ReqArg (NumberOfSteps . read) "number of steps to simulate")
           "Number of steps to simulate"
  , Option ['r'] ["rom"]
           (ReqArg LoadRom "path to bin file")
           "Load a ROM file"
  , Option [] ["ram"]
           (ReqArg LoadRam "path to bin file")
           "Load RAM file"
  , Option ['o'] ["optimize"]
           (NoArg Optimize)
           "Enable optimizations"
  ]

get_options :: [String] -> IO ([Flag], [String])
get_options argv =
  case getOpt Permute option_descriptions argv of
    (o, n, [])   ->
      return (o,n)
    (_, _, errs) ->
      ioError (userError (concat errs ++ usageInfo header option_descriptions))
    where header = "Usage: simulator file [OPTION...]..."

bool_list_of_string :: String -> [Bool]
bool_list_of_string = aux []
  where aux acc []       = acc
        aux acc ('1':xs) = aux (True:acc)  xs
        aux acc ('0':xs) = aux (False:acc) xs

read_ram :: FilePath -> IO Ram
read_ram filepath = do s  <- readFile filepath
                       let values = List.map bool_list_of_string $ lines s
                       let index  = [0..(List.genericLength values - 1)]
                       let l = zip index values
                       return $ Map.fromList l

read_netlist_in :: Netlist -> IO (Map.Map Ident Value)
read_netlist_in net =
  let sizes = Map.fromList $ netlist_var net in
  let aux m i = do putStr (i++"("++show(sizes!i)++"):")
                   hFlush stdout
                   s <- getLine -- TODO verify length
                   let v = bool_list_of_string s
                   return $ Map.insert i v m
  in do
  putStrLn "---- Input ----- "
  let f m (i,n) = Map.insert i (List.replicate (fromInteger n) False) m
  let m = List.foldl f Map.empty (netlist_var net)
  foldM aux m (netlist_in net)

print_vars :: Map.Map Ident [Bool] -> [Ident] -> IO ()
print_vars vars l = do
  putStrLn $ "---- Output ----"
  mapM_ (print_var vars) l
    where print_var var i =
            putStrLn $ i ++ ":" ++ string_of_bool_list (fromJust $ Map.lookup i var)

get_n [] = 1
get_n ((NumberOfSteps n):_) = n
get_n (_:os) = get_n os

get_ram [] = return Map.empty
get_ram ((LoadRam path):_) = read_ram path
get_ram (_:os) = get_ram os

get_rom [] = return Map.empty
get_rom ((LoadRom path):_) = read_ram path
get_rom (_:os) = get_rom os

run_simulation options net_sch = do
  let n = get_n options
  rom <- get_rom options
  ram <- get_ram options
  vars <- read_netlist_in net_sch
  let (ram', vars') = simulate n rom ram vars net_sch
  print_vars vars' (netlist_out net_sch)
  -- TODO: update RAM file

handle_netlist options name = do
  code <- readFile (name ++ ".net")
  let netlist = read_netlist code
  case schedule netlist of
    Left err      -> putStrLn err
    Right net_sch -> run_simulation options net_sch

stripThisDamnedExtension :: String -> String
stripThisDamnedExtension [] = ""
stripThisDamnedExtension str = List.reverse (aux (List.reverse str))
  where aux [] = []
        aux ('.':cs) = cs
        aux (c:cs) = c:(aux cs)


main :: IO ()
main = do
  (options, files) <- getArgs >>= get_options
  if null files then
    putStrLn "Error: no netlist file specified"
  else do
    handle_netlist options (stripThisDamnedExtension (List.head files))
    -- case stripThisDamnedExtension ".net" netlist_path
    --   Nothing   ->
    --     putStrLn "Bad extension, use .net"
    --   Just name ->
    --     handle_netlist options name

