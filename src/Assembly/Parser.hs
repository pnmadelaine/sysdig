module Assembly.Parser where

import Assembly.Ast

import qualified Data.List as List

import Text.Parsec.String
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language

def = Tok.LanguageDef { Tok.commentStart    = ""
                      , Tok.commentEnd      = ""
                      , Tok.commentLine     = ";"
                      , Tok.nestedComments  = False
                      , Tok.identStart      = letter
                      , Tok.identLetter     = letter <|> digit
                      , Tok.opStart         = anyToken
                      , Tok.opLetter        = anyToken
                      , Tok.reservedNames   = [] -- à compléter
                      , Tok.reservedOpNames = []
                      , Tok.caseSensitive   = True
                      }

lexer = Tok.makeTokenParser def

symbol  = Tok.symbol lexer
ident   = Tok.identifier lexer
natural = Tok.natural lexer

---gestion des immediates---

convert_imm :: Int -> Imm -- poids faible à gauche
convert_imm 0 = [False]
convert_imm 1 = [True]
convert_imm n =
  [n mod 2] ++ convert_imm (n/2) 
  
extend_list :: Int -> Bool -> [Bool] -> [Bool] -- extends list to length desired by adding b at the end
extend_list n b l = if List.length l < n then
										extend_list n b (l ++ [b])
									else
									  l
									  
add_1 :: Imm -> Imm
add_1 [] = [] --erreur imm trop gros
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
neg_im n = add_1 $ not_imm n


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


register :: Parser Reg
register = do try (symbol "$")
              r <- ident
              aux $ List.elemIndex r regnames
   where aux (Just n) = convert_imm n
         aux Nothing  = error "tocard !" --error
         

---calcul des opcodes et funct---

chb :: Int -> Int -> Assembly.Ast.Opcode --convert_hex_binary creates an opcode of 6 bits from hexadecimal representation
chb x1 x2 = let l1 = List.reverse $ extend_list 2 False $ convert_imm x1 in
						l1 ++ $  List.reverse $ extend_list 4 False $ convert_imm x2

r_instr :: [String]
r_instr = ["add",
					 "addu",
					 "and",
					 "jr",
					 "nor",
					 "or",
					 "slt",
					 "sltu",
					 "sll",
					 "srl",
					 "sub",
					 "subu"
					]
					
r_funct :: [Assembly.Ast.Opcode]
r_funct = [chb 2 0,
           chb 2 1,
           chb 2 4,
           chb 0 8,
           chb 2 7,
           chb 2 5,
           chb 2 10,
           chb 2 11,
           chb 0 0,
           chb 0 2,
           chb 2 2,
           chb 2 3
          ]
					
i_instr :: [String]					
i_instr = ["addi",
					 "addiu",
					 "andi",
					 "beq",
					 "bne",
					 "lbu",
					 "lha",
					 "ll",
					 "lui",
					 "lw",
					 "ori",
					 "slti",
					 "sltiu",
					 "sb",
					 "sc",
					 "sh",
					 "sw"
          ]
          
i_opcode :: [Assembly.Ast.Opcode]
i_opcode = [chb 0 8,
            chb 0 9,
            chb 0 12,
            chb 0 4,
            chb 0 5,
            chb 2 4,
            chb 2 5,
            chb 3 0,
            chb 0 15,
            chb 2 3,
            chb 0 13,
            chb 0 10,
            chb 0 11,
            chb 2 8,
            chb 3 8,
            chb 2 9,
            chb 2 11
           ]
 
j_instr :: [String]	         
j_instr = ["j",
					 "jal"
          ]
          
j_opcode :: [Assembly.Ast.Opcode]
j_opcode = [chb 0 2,
            chb 0 3
           ]
          
          
                
                
              
 
