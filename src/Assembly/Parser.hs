module Assembly.Parser where

import Assembly.Ast

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map ((!))

import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language
---import qualified Text.Parsec.Number as N

def = Tok.LanguageDef { Tok.commentStart    = ""
                      , Tok.commentEnd      = ""
                      , Tok.commentLine     = ";"
                      , Tok.nestedComments  = False
                      , Tok.identStart      = letter
                      , Tok.identLetter     = letter <|> digit <|> char '_'
                      , Tok.opStart         = anyToken
                      , Tok.opLetter        = anyToken
                      , Tok.reservedNames   = r_instr ++ i_instr ++ j_instr ++ ["move"]
                      , Tok.reservedOpNames = []
                      , Tok.caseSensitive   = True
                      }

lexer = Tok.makeTokenParser def

symbol  = Tok.symbol lexer
ident   = Tok.identifier lexer
natural = Tok.natural lexer

---                  ---
--- CALCUL DES JUMPS ---
---                  ---

get_labels :: Int -> Prog -> Map.Map String Int
get_labels n [] = Map.empty
get_labels n l = aux (List.head l) (List.tail l)
     where aux (Lexpr l) p =  Map.insert l n (get_labels n p)
           aux _ p = get_labels (n+1) p

update_jumps :: Map.Map String Int -> Prog -> Prog
update_jumps m [] = []
update_jumps m l = do let aux (Jexpr opc l) p = do let j = Jump opc $ extend_list 26 $ convert_imm (m ! l)
                                                   [j] ++ (update_jumps m p)
                          aux (Lexpr l) p = update_jumps m p --on a plus besoin des labels
                          aux (Bexpr opc s t l) p = do let b = Iexpr opc s t $ extend_list 16 $ convert_imm (m ! l)
                                                       [b] ++ (update_jumps m p)
                          aux i p = [i] ++ (update_jumps m p)
                      aux (List.head l) (List.tail l)

understand_assembly :: Prog -> Prog
understand_assembly p = do let m = get_labels 0 p
                           update_jumps m p

---         ---
--- PARSING ---
---         ---

---fonction appelée pour avoir un programme---
read_assembly :: String -> Either ParseError Prog
read_assembly code = parse parse_prog "assembly" code

---gestion d'un programme---
parse_prog :: Parser Prog
parse_prog = do spaces
                p <- many (parse_instr)
                eof
                return p


---gestion d'une instruction arbitraire---
parse_instr :: Parser Instr
parse_instr = parse_i_instr
          <|> parse_r_instr --ordre important sinon add a l'avantage sur addi
          <|> parse_j_instr
          <|> do try (symbol "move")
                 d <- register
                 symbol ","
                 s <- register
                 return $ Rexpr (chb 0 0) s reg_zero d reg_zero (chb 2 0)
          <|> do try (symbol "li")
                 t <- register
                 symbol ","
                 i <- immediate 16
                 return $ Iexpr (chb 0 8) reg_zero t i
          <|> lab

---gestion des labels---
lab :: Parser Instr
lab = do l <- ident
         symbol ":"
         return $ Lexpr l

---gestion des instructions de type R---

reg_zero :: Reg
reg_zero = [False, False, False, False, False]


parse_r_instr :: Parser Instr
parse_r_instr = do op <- choice (List.map (try . symbol) r_instr)
                   let opcode = chb 0 0
                   let funct = r_corres ! op
                   let aux "jr" = do s <- register
                                     return $ Rexpr opcode s reg_zero reg_zero reg_zero funct --troisième reg_zero = shamt
                       aux op | op == "mfhi" || op == "mflo" = do d <- register
                                                                  return $ Rexpr opcode reg_zero reg_zero d reg_zero funct
                       aux op | op == "sll" || op == "srl" = do d <- register
                                                                symbol ","
                                                                s <- register
                                                                symbol ","
                                                                shamt <- immediate 5
                                                                return $ Rexpr opcode s reg_zero d shamt funct
                       aux "sra" = do d <- register
                                      symbol ","
                                      t <- register
                                      symbol ","
                                      shamt <- immediate 5
                                      return $ Rexpr opcode reg_zero t d shamt funct
                       aux op | op == "div" || op == "divu" || op == "mult" || op == "multu" = 
                           do s <- register
                              symbol ","
                              t <- register
                              return $ Rexpr opcode s t reg_zero reg_zero funct
                       aux _ = do d <- register
                                  symbol ","
                                  s <- register
                                  symbol ","
                                  t <- register
                                  return $ Rexpr opcode s t d reg_zero funct --troisième reg_zero = shamt

                   aux op

---gestion des instructions de type I---
parse_i_instr :: Parser Instr
parse_i_instr = do op <- choice (List.map (try . symbol) i_instr)
                   let opcode = i_corres ! op
                   t <- register
                   symbol ","
                   let aux op | op == "beq" || op == "bne" = do s <- register
                                                                symbol ","
                                                                l <- ident
                                                                return $ Bexpr opcode s t l
                       aux "lui" = do i <- immediate 16
                                      return $ Iexpr opcode reg_zero t i
                       aux _ = do s <- register
                                  symbol ","
                                  i <- immediate 16
                                  return $ Iexpr opcode s t i
                   aux op

---gestion des instructions de type J---
parse_j_instr :: Parser Instr
parse_j_instr = do op <- try (symbol "jal") <|> try (symbol "j")
                   let opcode = j_corres ! op
                   l <- ident
                   return $ Jexpr opcode l

---gestion des immediates---

int :: Parser Int --TODO gérer les négatifs--
int = do s <- many1 digit
         spaces
         return (read s)

convert_imm :: Int -> Imm -- poids faible à gauche convertit la valeur absolue d'un nombre en binaire
convert_imm 0 = [False]
convert_imm 1 = [True]
convert_imm n =
  [aux (mod n 2)] ++ (convert_imm (div n 2))
  where aux 0 = False
        aux _ = True

extend_list :: Int -> [Bool] -> [Bool] -- extends list to length desired by adding b at the end
extend_list n l = if List.length l < n then
                      extend_list n (l ++ [False])
                    else
                      l

add_1 :: Imm -> Imm
add_1 [] = error "Immediate too big"
add_1 n = let h = List.head n in
          let t = List.tail n in
          if h then
            [False] ++ (add_1 t)
          else
            [True] ++ t

not_imm :: Imm -> Imm
not_imm [] = []
not_imm n = [not (List.head n)] ++ (not_imm $ List.tail n)

neg_imm :: Imm -> Imm
neg_imm n = add_1 $ not_imm n

immediate :: Int -> Parser Imm
immediate n = do v <- int
                 if v >= 0 then
                   return $ extend_list n $ convert_imm v
                 else
                   return $ neg_imm $ extend_list n $ convert_imm v


---gestion des registres---

registers_names :: [String]
registers_names = ["zero",
                   "at",
                   "v0", "v1",
                   "a0", "a1", "a2", "a3",
                   "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7",
                   "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7",
                   "t8", "t9",
                   "k0", "k1",
                   "gp",
                   "sp",
                   "fp",
                   "ra"
                   ]


register :: Parser Reg --petit-endian
register = do try (symbol "$")
              r <- ident
              return $ aux $ List.elemIndex r registers_names
   where aux (Just n) = extend_list 5 $ convert_imm n
         aux Nothing  = error "Not a register"


---calcul des opcodes et funct---

chb :: Int -> Int -> Assembly.Ast.Opcode --convert_hex_binary creates an opcode of 6 bits from hexadecimal representation
chb x1 x2 = let l1 = extend_list 2 $ convert_imm x1 in
            (extend_list 4 $ convert_imm x2) ++ l1

r_instr :: [String]
r_instr = ["addu",
           "add",
           "and",
           "jr",
           "nor",
           "or",
           "sltu",
           "slt",
           "sll",
           "srl",
           "subu",
           "sub",
           "divu",
           "div",
           "multu",
           "mult",
           "sra",
           "mfhi",
           "mflo"
          ]

r_funct :: [Funct]
r_funct = [chb 2 1,
           chb 2 0,
           chb 2 4,
           chb 0 8,
           chb 2 7,
           chb 2 5,
           chb 2 11,
           chb 2 10,
           chb 0 0,
           chb 0 2,
           chb 2 3,
           chb 2 2,
           chb 1 11,
           chb 1 10,
           chb 1 9,
           chb 1 8,
           chb 0 3,
           chb 1 0,
           chb 1 2
          ]

r_corres :: Map.Map String Funct
r_corres = Map.fromList (List.zip r_instr r_funct)

i_instr :: [String]
i_instr = ["addiu",
           "addi",
           "andi",
           "beq",
           "bne",
           "lbu",
           "lha",
           "ll",
           "lui",
           "lw",
           "ori",
           "sltiu",
           "slti",
           "sb",
           "sc",
           "sh",
           "sw"
          ]

i_opcode :: [Opcode]
i_opcode = [chb 0 9,
            chb 0 8,
            chb 0 12,
            chb 0 4,
            chb 0 5,
            chb 2 4,
            chb 2 5,
            chb 3 0,
            chb 0 15,
            chb 2 3,
            chb 0 13,
            chb 0 11,
            chb 0 10,
            chb 2 8,
            chb 3 8,
            chb 2 9,
            chb 2 11
           ]

i_corres :: Map.Map String Funct
i_corres = Map.fromList (List.zip i_instr i_opcode)

j_instr :: [String]
j_instr = ["jal",
           "j"
          ]

j_opcode :: [Opcode]
j_opcode = [ chb 0 3
           , chb 0 2
           ]

j_corres :: Map.Map String Funct
j_corres = Map.fromList (List.zip j_instr j_opcode)
