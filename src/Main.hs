module Main where

import Netlist.Ast
import Netlist.Show
import Netlist.Parser (read_netlist)
import Netlist.Typer
import Netlist.Scheduler
import Netlist.Simulator 

import System.IO
import System.Environment
import System.FilePath
import System.Console.GetOpt
import Control.Monad (guard, foldM, mapM_)

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

data Flag =
    PrintOnly
  | NumberOfSteps Integer
  | LoadRom FilePath
  | LoadRam FilePath
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
  ]

get_options :: [String] -> IO ([Flag], [String])
get_options argv =
  case getOpt Permute option_descriptions argv of
    (o, n, [])   ->
      return (o,n)
    (_, _, errs) ->
      ioError (userError (concat errs ++ usageInfo header option_descriptions))
    where header = "Usage: netlist file [OPTION...]..."

bool_list_of_string :: String -> [Bool]
bool_list_of_string = aux []
  where aux acc []       = acc
        aux acc ('1':xs) = aux (True:acc)  xs
        aux acc ('0':xs) = aux (False:acc) xs

string_of_bool_list = aux []
  where aux acc []         = acc
        aux acc (True:xs)  = aux ('1':acc) xs
        aux acc (False:xs) = aux ('0':acc) xs

read_file :: FilePath -> IO Ram
read_file filepath = do s  <- readFile filepath
                        let values = List.map bool_list_of_string $ lines s
                        let index  = [0..(List.genericLength values - 1)]
                        let l = zip index values
                        return $ Map.fromList l

read_options :: (Integer,Ram,Ram) -> [Flag] -> IO (Integer,Ram,Ram)
read_options acc         []                    = return acc
read_options (_,rom,ram) (NumberOfSteps n:os)  = read_options (n,rom,ram) os
read_options (n,_,ram)   (LoadRom filepath:os) = do rom <- read_file filepath
                                                    read_options (n,rom,ram) os
read_options (n,rom,_)   (LoadRam filepath:os) = do ram <- read_file filepath
                                                    read_options (n,rom,ram) os
read_options acc         (_:os)                = read_options acc os

read_input :: Netlist -> IO (Map.Map Ident Value)
read_input net = do
  putStrLn "---- Input ----- "
  let f m (i,n) = Map.insert i (List.replicate (fromInteger n) False) m 
  let m = List.foldl f Map.empty (var net) 
  foldM aux m (input net)
    where aux m i = do putStr (i++":")
                       hFlush stdout
                       s <- getLine -- TODO verify length
                       let v = bool_list_of_string s  
                       return $ Map.insert i v m

print_vars :: Map.Map Ident [Bool] -> [Ident] -> IO ()
print_vars vars l = do
  putStrLn $ "---- Output ----"
  mapM_ (print_var vars) l
    where print_var var i =
            putStrLn $ i ++ ":" ++ string_of_bool_list (fromJust $ Map.lookup i var)

main :: IO ()
main = do (options, files) <- getArgs >>= get_options
          let file_path = List.head files
          -- TODO: check file has .net extension
          let output_path = dropExtension file_path ++ "_sch" ++ ".net" 
          code <- readFile file_path
          let net = read_netlist code
          case verify net of
            Left err -> putStrLn err
            Right _  -> 
              case schedule net of
                Left err      -> putStrLn err
                Right net_sch -> do
                  writeFile output_path $ show net_sch
                  if List.elem PrintOnly options then
                    return ()
                  else do
                    (n,rom,ram) <- read_options (1, Map.empty, Map.empty) options 
                    vars <- read_input net
                    (ram', vars') <- simulate n rom ram vars net_sch 
                    print_vars vars' (output net)

