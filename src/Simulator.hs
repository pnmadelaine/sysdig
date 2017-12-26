module Main where

import Netlist.Ast
import Netlist.Show
import Netlist.Parser (read_netlist)
import Netlist.Typer
import Netlist.Scheduler
import Netlist.Simulator
import Netlist.Opt

import System.IO
import System.Environment
import System.FilePath
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
  [ Option ['p'] ["print"]
           (NoArg PrintOnly)
           "Only print the result of scheduling"
  , Option ['n'] []
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

read_file :: FilePath -> IO Ram
read_file filepath = do s  <- readFile filepath
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
get_ram ((LoadRam path):_) = read_file path
get_ram (_:os) = get_rom os

get_rom [] = return Map.empty
get_rom ((LoadRom path):_) = read_file path
get_rom (_:os) = get_rom os

run_simulation options name netlist = do
  let Right net_sch = schedule netlist
  let n = get_n options
  rom <- get_rom options
  ram <- get_ram options
  if List.elem Optimize options then do
    let opt_path = name ++ "_opt.net"
    let Right net_opt = schedule $ optimize net_sch
    writeFile opt_path (show net_opt)
    vars <- read_netlist_in net_opt
    let (ram', vars') = simulate n rom ram vars net_opt
    print_vars vars' (netlist_out net_opt)
  else do
    vars <- read_netlist_in net_sch
    let (ram', vars') = simulate n rom ram vars net_sch
    print_vars vars' (netlist_out netlist)

handle_netlist options name = do
  code <- readFile (name ++ ".net")
  let netlist = read_netlist code
  let sch_path = name ++ "_sch.net"
  case schedule netlist of
    Left err ->
      putStrLn err
    Right net_sch -> do
      writeFile sch_path (show net_sch)
      if List.elem PrintOnly options then
        return ()
      else do
        run_simulation options name netlist

main :: IO ()
main = do
  (options, files) <- getArgs >>= get_options
  if null files then
    putStrLn "Error: no netlist file specified"
  else do
    let netlist_path = List.head files
    case stripExtension ".net" netlist_path of
      Nothing   ->
        putStrLn "Bad extension, use .net"
      Just name ->
        handle_netlist options name

