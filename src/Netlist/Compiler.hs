module Netlist.Compiler (kompilator, compile) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Netlist.Ast

import System.IO
import Data.Char
import Control.Monad (when)


int_of_bool_list :: [Bool] -> Integer
int_of_bool_list l = aux 0 (List.reverse l)
  where aux acc []         = acc
        aux acc (True:xs)  = aux (1+2*acc) xs
        aux acc (False:xs) = aux (2*acc)   xs

bool_list_of_string :: String -> [Bool]
bool_list_of_string = aux []
  where aux acc []       = acc
        aux acc ('1':xs) = aux (True:acc)  xs
        aux acc ('0':xs) = aux (False:acc) xs

handle_var :: [(Ident, Integer)] -> String
handle_var l = aux "" l
  where aux acc [] = acc
        aux acc ((id, size):types) = aux (acc++"\nint "++id++"["++(show size)++"] = {0};") types

handle_init :: Map.Map Ident Integer -> [Ident] -> String
handle_init _ _ = ""

reg_temp_id :: String -> String
reg_temp_id s = "reg"++s++"_"

reg_init :: [(Ident, Integer)] -> String
reg_init l = aux "" l
  where aux acc [] = acc
        aux acc ((id, size):types) = aux (acc++"\nint "++(reg_temp_id id)++"["++(show size)++"] = {0};") types

reg_save :: [(Ident, Integer)] -> String
reg_save l = aux "" l
  where aux acc [] = acc
        aux acc ((id, size):types) =
          let endofacc = concat $ List.map (\n -> "\n"++(reg_temp_id id)++"["++(show n)++"] = "++id++"["++(show n)++"];") [0..(size-1)]
          in aux (acc++endofacc) types

preload_filter :: [Argument] -> [Value]
preload_filter lst = aux [] lst
  where aux acc [] = acc
        aux acc ((ArgVar i):as) = aux acc as
        aux acc ((ArgCst c):as) = aux (c:acc) as

-- generate the temp variable id that will be used to store preloaded constants
preload_temp_id :: String -> Integer -> String
preload_temp_id s i = "temp"++s++"_"++(show i)++"_"

-- preload auxiliar function
pa :: [Argument] -> Ident -> [(String, Value)]
pa lst id = List.zip
              (List.map (\(arg, n) -> preload_temp_id id n) (List.zip lst [1..]))
              (preload_filter lst)

preload :: [(Ident, Expression)] -> [(String, Value)]
preload l = aux [] l
  where aux acc [] = acc
        aux acc ((id, exp):eqs) = case exp of
            Earg a1        -> aux ((pa [a1] id)++acc) eqs
            Enot a1        -> aux ((pa [a1] id)++acc) eqs
            Eslice _ _ a1    -> aux ((pa [a1] id)++acc) eqs
            Eselect _ a1   -> aux ((pa [a1] id)++acc) eqs
            Ebinop _ a1 a2 -> aux ((pa [a1, a2] id)++acc) eqs
            Econcat a1 a2  -> aux ((pa [a1, a2] id)++acc) eqs
            Emux a1 a2 a3  -> aux ((pa [a1, a2, a3] id)++acc) eqs

preload_cv :: (String, Value) -> String
preload_cv (str, val) = "\n int "++str++"["++(show $ List.length val)++"];"
                  ++(aux [] $ List.zip val [0..])
  where aux acc [] = acc
        aux acc ((v, i):vs) = aux (acc++"\n"++str++"["++(show i)++"] = "++(show v)++";") vs

-- oklm
getvalue :: Ident -> Integer -> Argument -> String
getvalue id k arg = case arg of
                       ArgVar i -> i
                       ArgCst v -> preload_temp_id id k

getvalue2 :: Ident -> Value -> String
getvalue2 id v = aux [] $ List.zip v [0..]
  where aux acc [] = acc
        aux acc ((v, i):vs) = aux (acc++"\n"++id++"["++(show i)++"] = "++(preload_temp_id id 1)++"["++(show i)++"];") vs

getvalue3 :: Map.Map Ident Integer -> Ident -> Ident -> String
getvalue3 szs id1 id2 =
    let sz = szs Map.! id1
    in aux [] id1 id2 [0..(sz-1)]
  where aux acc _ _ [] = acc
        aux acc id1 id2 (n:ns) = aux (acc++"\n"++id1++"["++(show n)++"] = "++id2++"["++(show n)++"];") id1 id2 ns

handle_eq :: Map.Map Ident Integer -> (Ident, Expression) -> String
handle_eq szs (id, exp) = case exp of
                            Earg (ArgCst v)      -> getvalue2 id v
                            Earg (ArgVar id2)    -> getvalue3 szs id id2
                            Enot a           -> "\n"++id++" = enot("++(getvalue id 1 a)++");"
                            Ebinop op a b    -> case op of
                                                  Or   -> "\n"++id++" = eor("++(getvalue id 1 a)++", "++(getvalue id 2 b)++");"
                                                  Xor  -> "\n"++id++" = exor("++(getvalue id 1 a)++", "++(getvalue id 2 b)++");"
                                                  And  -> "\n"++id++" = eand("++(getvalue id 1 a)++", "++(getvalue id 2 b)++");"
                                                  Nand -> "\n"++id++" = enand("++(getvalue id 1 a)++", "++(getvalue id 2 b)++");"
                            Emux a b c          -> "\n"++id++" = emux("++(getvalue id 1 a)++", "++(getvalue id 2 b)++", "++(getvalue id 3 c)++");"
                            Econcat a b         -> "\n"++id++" = econcat("++(getvalue id 1 a)++", "++(getvalue id 2 b)++");"
                            Eslice i j a        -> "\n"++id++" = eslice("++(getvalue id 1 a)++", "++(show i)++", "++(show j)++");"
                            Eselect i a         -> "\n"++id++" = eselect("++(getvalue id 1 a)++", "++(show i)++");"

handle_out :: Ident -> String
handle_out id = "\nprintbl("++id++");"

kompilator :: Netlist -> String
kompilator netl = let sizes = Map.fromList (netlist_var netl)
    in
       (handle_var (netlist_var netl))
    ++ (reg_init (netlist_var netl))
    ++ "\nint main { while 1 {"
    ++ (concat (List.map (\x -> handle_eq sizes x) (netlist_eq netl)))
    ++ (concat (List.map handle_out (netlist_out netl)))
    ++ (reg_save (netlist_var netl))
    ++ "}}"

compile :: Netlist -> IO ()
compile ntlst = do
    content <- readFile "src/Netlist/template.c"
    let newContent = content++(kompilator ntlst)
    when (length newContent > 0) $
        writeFile "test.c" newContent

-- [TODO]
-- RAM/ROM
-- saisie des variables d'entrée par fenêtre de prompt
-- passer de bool list à int pour tracer sa mère
-- vérifications à faire à la compilation

-- [OK]
-- rendre le compilateur utilisable (écriture à la suite d'un fichier C faisant office de template avec toutes les fonctions préchargées)
-- getvalue : idée à implémenter, préchargement des constantes présentes dans les équations de la netlist
-- REG
