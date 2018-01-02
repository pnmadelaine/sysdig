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

-- déclaration des variables en jeu #1 [OK]
handle_var :: [(Ident, Integer)] -> String
handle_var l = aux "" l
  where aux acc [] = acc
        aux acc ((id, size):types) = aux (acc++"\nint "++id++"["++(show size)++"] = {0};") types

-- saisie des variables d'entrée #2
-- handle_in_prompt :: [Ident] -> Map Ident Integer -> String
-- initialisation des variables en fonction des variables saisies en tant que variables d'entrée #3
-- handle_in_init :: [Ident] -> Map.Map Ident Integer -> String
-- handle_in_init idents sizes = aux "" idents sizes
--   where aux acc [] _ = acc
--         aux acc (i:is) szs = aux (acc++(concat ["\n"++i++"["++(show k)++"] = 0;" | k <- [0..(Map.lookup i szs)]])) is szs

getvalue :: Argument -> String
getvalue arg = case arg of
                 ArgCst v -> "haskell4ever"
                 ArgVar i -> i

handle_eq :: (Ident, Expression) -> String
handle_eq (id, exp) = case exp of
            Earg a           -> "\n"++id++" = getvalue a;"
            Enot a           -> "\n"++id++" = enot("++(getvalue a)++");"
            Ebinop op a b    -> case op of
                                  Or   -> "\n"++id++" = eor("++(getvalue a)++", "++(getvalue b)++");"
                                  Xor  -> "\n"++id++" = exor("++(getvalue a)++", "++(getvalue b)++");"
                                  And  -> "\n"++id++" = eand("++(getvalue a)++", "++(getvalue b)++");"
                                  Nand -> "\n"++id++" = enand("++(getvalue a)++", "++(getvalue b)++");"
            Econcat a b      -> "\n"++id++" = econcat("++(getvalue a)++", "++(getvalue b)++");"
            Eslice i j a     -> "\n"++id++" = eslice("++(getvalue a)++", "++(show i)++", "++(show j)++");"
            Eselect i a      -> "\n"++id++" = eselect("++(getvalue a)++", "++(show i)++");"

handle_out :: Ident -> String
handle_out id = "\nprintbl("++id++");"

kompilator :: Netlist -> String
kompilator netl =
       (handle_var (netlist_var netl))
    ++ "\nwhile 1 {"
    ++ (concat (List.map handle_eq (netlist_eq netl)))
    ++ (concat (List.map handle_out (netlist_out netl)))
    ++ "}"

compile :: Netlist -> IO ()
compile ntlst = do
    content <- readFile "src/Netlist/template.c"
    let newContents = content++(kompilator ntlst)
    when (length newContents > 0) $
        writeFile "test.c" newContents

-- TODO :
-- REG/RAM/ROM
-- saisie des variables d'entrée par fenêtre de prompt
-- getvalue : idée à implémenter, préchargement des constantes présentes dans les équations de la netlist
-- rendre le compilateur utilisable (écriture à la suite d'un fichier C faisant office de template avec toutes les fonctions préchargées)

-- passer de bool list à int pour tracer sa mère

























-- vérification à faire à la compilation -> impossible
-- saisie des variables d'entrée TODO
-- tout remplacer par des int pour tracer sa mère (autre version, en dernier) TODO
-- SLICE, CONCAT, SELECT, MUX, NOT, BINOP (OR AND XOR NAND)

-- SELECT, SLICE, CONCAT difficiles à gérer si la représentation utilisée est un int
-- getvalue !!!
-- idée : en cas de saisie d'une valeur constante à chaque étape, on la précharge -> A FAIRE

-- écrire à la suite d'un fichier .c faisant office de template avec toutes les fonctions préchargées
