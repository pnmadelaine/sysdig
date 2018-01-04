module Netlist.Compiler (compile) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Netlist.Ast

import System.IO
import Data.Char
import Control.Monad (when)

string_of_bool_list :: [Bool] -> String
string_of_bool_list = aux []
  where aux acc []         = acc
        aux acc (True:bs)  = aux (acc++"1") bs
        aux acc (False:bs) = aux (acc++"0") bs

handle_var :: [(Ident, Integer)] -> String
handle_var l = aux "" l
  where aux acc []              = acc
        aux acc ((id, _):types) = aux (acc++"\nint "++id++" = 0;") types

handle_init :: Map.Map Ident Integer -> [Ident] -> String
handle_init _ _ = ""

reg_id :: String -> String
reg_id s = "_reg"++s++"_"

reg_init :: [(Ident, Integer)] -> String
reg_init l = aux "" l
  where aux acc []              = acc
        aux acc ((id, _):types) = aux (acc++"\nint "++(reg_id id)++" = 0;") types

reg_save :: [(Ident, Integer)] -> String
reg_save l = aux "" l
  where aux acc []              = acc
        aux acc ((id, _):types) = aux (acc++"\n"++(reg_id id)++" = "++id++";") types

getvalue :: Argument -> String
getvalue arg = case arg of
                 ArgVar i -> i
                 ArgCst v -> "0b"++(string_of_bool_list v)

getsize :: Map.Map Ident Integer -> Argument -> Integer
getsize szs arg = case arg of
                    ArgVar i -> szs Map.! i
                    ArgCst v -> toInteger $ List.length v

handle_eq :: Map.Map Ident Integer -> (Ident, Expression) -> String
handle_eq szs (id, exp) = case exp of
                            Earg a            -> "\n"++id++" = "++(getvalue a)++";"
                            Ereg id2          -> "\n"++id++" = "++(getvalue (ArgVar (reg_id id2)))++";"
                            Enot a            -> "\n"++id++" = ("++(getvalue a)++" == 0) ? 1 : 0;"
                            Ebinop op a b     -> case op of
                                                   Or   -> "\n"++id++" = "++(getvalue a)++" | "++(getvalue b)++";"
                                                   Xor  -> "\n"++id++" = "++(getvalue a)++" ^ "++(getvalue b)++";"
                                                   And  -> "\n"++id++" = "++(getvalue a)++" & "++(getvalue b)++";"
                                                   Nand -> "\n"++id++" = ~ ("++(getvalue a)++" & "++(getvalue b)++");"
                            Emux a b c        -> "\n"++id++" = ("++(getvalue a)++" & 1 == 1) ? "
                                               ++(getvalue b)++" : "++(getvalue c)++";"
                            Econcat a b       -> "\n"++id++" = ("++(getvalue b)++" << "
                                               ++(show $ getsize szs a)++") + "++(getvalue a)++";"
                            Eslice i j a      -> "\n"++id++" = "++(getvalue a)++" >> "++(show i)++";"
                            Eselect i a       -> "\n"++id++" = "++(getvalue a)++" >> "++(show i)++";"
                            Eram ra we wa d   -> "\n"++id++" = _ram["++(getvalue ra)++"];"
                                               ++" if ("++(getvalue we)++" & 1){"
                                               ++" _ram["++(getvalue wa)++"] = "++(getvalue d)++";"
                                               ++" }"
                            Erom ra           -> "\n"++id++" = _rom["++(getvalue ra)++"];"

handle_out :: Ident -> String
handle_out id = "\nprint("++id++");"

kompilator :: Netlist -> String
kompilator netl = let sizes = Map.fromList (netlist_var netl)
    in "\n"
    ++ "\nint main(){"
    ++ "\nint* _ram = malloc(sizeof(int) * "++(show $ 2^24)++");"
    ++ "\nint* _rom = malloc(sizeof(int) * "++(show $ 2^24)++");"
    ++ (handle_var (netlist_var netl))
    ++ (handle_init sizes (netlist_in netl))
    ++ (reg_init (netlist_var netl))
    ++ "\nwhile (1) {"
    ++ (concat (List.map (\x -> handle_eq sizes x) (netlist_eq netl)))
    ++ (concat (List.map handle_out (netlist_out netl)))
    ++ (reg_save (netlist_var netl))
    ++ "\n}\n}\n"

compile :: Netlist -> IO ()
compile ntlst = do
    content <- readFile "../../../src/Netlist/template.c"
    let newContent = content++(kompilator ntlst)
    when (length newContent > 0) $
        writeFile "test.c" newContent

-- [TODO]
-- saisie des variables d'entrée par fenêtre de prompt (handle_init)
-- {????} vérifications à faire à la compilation
-- {osef} Integer / Int en Haskell

-- [OK]
-- rendre le compilateur utilisable (écriture à la suite d'un fichier C faisant office de template)
-- getvalue : idée à implémenter, préchargement des constantes présentes dans les équations de la netlist
-- REG
-- passer de bool list à int pour tracer sa mère
-- RAM/ROM
