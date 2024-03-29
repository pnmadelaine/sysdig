module Netlist.Compiler (compile) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Netlist.Ast

import System.FilePath
import System.IO
import Data.Char
import Control.Monad (when)

ram_size = 24
rom_size = 24
ram_limit = 23
rom_limit = 23
debug_ram = False

string_of_bool_list :: [Bool] -> String
string_of_bool_list = aux []
  where aux acc []         = acc
        aux acc (True:bs)  = aux ('1':acc) bs
        aux acc (False:bs) = aux ('0':acc) bs

bool_list_of_string :: String -> [Bool]
bool_list_of_string = aux []
  where aux acc [] = acc
        aux acc ('1':cs) = aux (True:acc) cs
        aux acc ('0':cs) = aux (False:acc) cs


handle_var :: [(Ident, Integer)] -> String
handle_var l = aux "" l
  where aux acc []              = acc
        aux acc ((id, _):types) = aux (acc++"\nunsigned long int "++id++" = 0;") types

handle_init :: Map.Map Ident Integer -> [(Ident, Value)] -> String
handle_init szs ins = aux [] ins
  where aux acc [] = acc
        aux acc ((id, v):idvs) = (if ((toInteger $ List.length v) == (szs Map.! id))
                                 then aux (acc++"\n"++id++" = 0b"++(string_of_bool_list v)++";") idvs
                                 else aux (acc++"\n/* error : bad size for "++id++" : 0b"++(string_of_bool_list v)) [])

reg_id :: String -> String
reg_id s = "_reg"++s++"_"

reg_selection :: [(Ident, Expression)] -> [Ident]
reg_selection lst = aux [] lst
  where aux acc [] = acc
        aux acc ((id, exp):eqs) = case exp of
                                    Ereg id2 -> aux (id2:acc) eqs
                                    _        -> aux acc eqs 

reg_init :: [Ident] -> String
reg_init l = aux "" l
  where aux acc []       = acc
        aux acc (id:ids) = aux (acc++"\nunsigned long int "++(reg_id id)++" = 0;") ids

reg_save :: [Ident] -> String
reg_save l = aux "" l
  where aux acc []       = acc
        aux acc (id:ids) = aux (acc++"\n"++(reg_id id)++" = "++id++";") ids

getvalue :: Argument -> String
getvalue arg = case arg of
                 ArgVar i -> i
                 ArgCst v -> "0b"++(string_of_bool_list v)

getsize :: Map.Map Ident Integer -> Argument -> Integer
getsize szs arg = case arg of
                    ArgVar i -> szs Map.! i
                    ArgCst v -> toInteger $ List.length v

-- assuming sizeof(long int) = 8 bytes
-- ..001(<- pos j)1..1(<-pos i)00..
mask :: Integer -> Integer -> String
mask i j = aux [] 64
  where aux acc 0 = acc
        aux acc k = if i > j || k > j || k < i
                    then aux (acc++"0") (k-1)
                    else aux (acc++"1") (k-1)

masko :: Argument -> Integer -> Integer -> String
masko arg i j = case arg of
                  ArgVar id -> "("++id++" & 0b"++(mask i j)++")"
                  ArgCst v ->
                              let result = List.map (\(b1, b2) -> b1 && b2) $ List.zip (bool_list_of_string (mask i j)) v
                              in "0b"++(string_of_bool_list result)

handle_eq :: Map.Map Ident Integer -> (Ident, Expression) -> String
handle_eq szs (id, exp) =
  case exp of
    Earg a            -> "\n"++id++" = "++(getvalue a)++";"
    Ereg id2          -> "\n"++id++" = "++(getvalue (ArgVar (reg_id id2)))++";"
    Enot a            -> "\n"++id++" = "++(getvalue a)++" ^ (-1);"
    Ebinop op a b     -> case op of
                           Or   -> "\n"++id++" = "++(getvalue a)++" | "++(getvalue b)++";"
                           Xor  -> "\n"++id++" = "++(getvalue a)++" ^ "++(getvalue b)++";"
                           And  -> "\n"++id++" = "++(getvalue a)++" & "++(getvalue b)++";"
                           Nand -> "\n"++id++" = ("++(getvalue a)++" & "++(getvalue b)++") ^ (-1);"
    Emux a b c        -> "\n"++id++" = ("++(getvalue a)++" & 1 == 1) ? "
                       ++(getvalue b)++" : "++(getvalue c)++";"
    Econcat a b       -> "\n"++id++" = ("++(getvalue a)++" << "++(show $ getsize szs b)
                      ++") | "++(masko b 0 (getsize szs b))++";"
                      -- ++") | (0b"++(mask 0 (pred $ getsize szs b))++" & "++(getvalue b)++");"
    Eslice i j a      -> "\n"++id++" = "++(getvalue a)++" >> "++(show i)++";"
    Eselect i a       -> "\n"++id++" = "++(getvalue a)++" >> "++(show i)++";"
    Eram ra we wa d   -> (if debug_ram then "\nprint("++(getvalue ra)++");" else "")
                       ++"\n"++id++" = _ram[("++(getvalue ra)++" & 0b"++(mask 0 ram_limit)++")];"
                       ++(if debug_ram then "\nprint("++(getvalue wa)++");" else "")
                       ++" if ("++(getvalue we)++" & 1 == 1){"
                       ++" _ram[("++(getvalue wa)++" & 0b"++(mask 0 ram_limit)++")] = "++(getvalue d)++";"
                       ++" }"
    Erom ra           -> "\n"++id++" = _rom[("++(getvalue ra)++" & 0b"++(mask 0 ram_limit)++")];"

handle_out :: Map.Map Ident Integer -> Ident -> String
handle_out szs id =
  let sz = szs Map.! id
  in "\nprintf(\"%s\", \""++id++": \"); print("++(masko (ArgVar id) 0 sz)++");"

handle_rom_split :: String -> [String]
handle_rom_split s = aux [] [] 0 s
  where aux acc _    _  []        = List.reverse acc
        aux acc acc2 8  str       = aux ((List.reverse acc2):acc) []       0     str
        aux acc acc2 k  (' ':cs)  = aux acc                       acc2     k     cs
        aux acc acc2 k  ('\n':cs) = aux acc                       acc2     k     cs
        aux acc acc2 k  (c:cs)    = aux acc                       (c:acc2) (k+1) cs

handle_rom_cell_init :: String -> Int -> String
handle_rom_cell_init content addr = "\n_rom["++(show addr)++"] = 0b"++content++";"

handle_rom_init :: [String] -> String
handle_rom_init instr_lst =
  let last_addr = List.length instr_lst in
  let instr_id_lst = List.zip instr_lst (List.reverse [0..(pred last_addr)]) in
  concat (List.map (\(c, a) -> handle_rom_cell_init c a) instr_id_lst)

rom_init :: String -> String
rom_init str = handle_rom_init (handle_rom_split str)

kompilator :: Netlist -> [(Ident, Value)] -> String -> (String, String)
kompilator netl ins rom =
    let sizes = Map.fromList (netlist_var netl) in
    let regs  = reg_selection (netlist_eq netl) in
       (
           "\nunsigned int _ram_size = "++(show $ 2^(ram_size))++";"
        ++ "\nunsigned int _ram_limit = "++(show $ 2^(ram_limit))++";",

           "\nint* _rom = malloc(sizeof(int) * "++(show $ 2^(rom_size))++");"
        ++ (handle_var (netlist_var netl))
        ++ (handle_init sizes ins)
        ++ (reg_init regs)
        ++ (rom_init rom)
        ++ "\nwhile (_n == -1 || _n > 0) {"
        ++ "\nif (_n > 0) { _n = _n - 1; }"
        ++ (concat (List.map (\x -> handle_eq sizes x) (netlist_eq netl)))
        -- ++ (concat (List.map (\(x,y) -> handle_out sizes x) (netlist_var netl)))
        ++ (reg_save regs)
        ++ "\n}"
        -- ++ (concat (List.map (\x -> handle_out sizes x) (netlist_out netl)))
        ++ "\n\n"
        )

compile :: String -> Netlist -> [(Ident, Value)] -> String -> IO ()
compile filename ntlst in_values rom  =
  let kcontent = kompilator ntlst in_values rom in
  let kcontent1 = fst kcontent in
  let kcontent2 = snd kcontent in
  do
    content <- readFile "template.c"
    content2 <- readFile "template2.c"
    content3 <- readFile "template3.c"
    let newContent = content++kcontent1++content2++kcontent2++content3
    when (length newContent > 0) $
        writeFile (dropExtension filename++".c") newContent

-- [TODO cambouis]
-- multithread
-- ajout d'options à la compilation (nombre de cycles)
-- modification du parsing des arguments (nombre de cycles + fichier rom)
-- saisie des variables d'entrée par fenêtre de prompt (handle_init) (2/2)

-- [OK]
-- rendre le compilateur utilisable (écriture à la suite d'un fichier C faisant office de template)
-- getvalue : idée à implémenter, préchargement des constantes présentes dans les équations de la netlist
-- REG
-- passer de bool list à int pour tracer sa mère
-- RAM/ROM
-- optimisation sur les registres
-- saisie des variables d'entrée par fenêtre de prompt (handle_init) (1/2)
-- ROM par chargement de fichier opérationnelle

-- {useless} vérifications à faire à la compilation
-- {osef} Integer / Int en Haskell
-- {tequila, heineken, pas le temps de niaiser} optimisation sur les types
