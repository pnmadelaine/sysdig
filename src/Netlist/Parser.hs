module Netlist.Parser where

import Netlist.Ast
import qualified Data.List as List

import Text.Parsec hiding (spaces)
import Text.Parsec.String
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language

def = Tok.LanguageDef
        { Tok.commentStart    = ""
        , Tok.commentEnd      = ""
        , Tok.commentLine     = "#"
        , Tok.nestedComments  = False
        , Tok.identStart      = char '_' <|> letter
        , Tok.identLetter     = letter <|> digit <|> char '\'' <|> char '_'
        , Tok.opStart         = anyToken
        , Tok.opLetter        = anyToken
        , Tok.reservedNames   = [ "INPUT", "OUTPUT", "VAR", "IN"
                                , "OR", "XOR", "AND", "NAND"
                                , "REG", "NOT", "MUX", "ROM", "RAM"
                                , "CONCAT", "SLICE", "SELECT"
                                ]
        , Tok.reservedOpNames = []
        , Tok.caseSensitive   = True
        }

lexer = Tok.makeTokenParser def

symbol  = Tok.symbol lexer
ident   = Tok.identifier lexer
natural = Tok.natural lexer

binop :: Parser BinOp
binop = try (symbol "OR"   >> return Or)
    <|> try (symbol "XOR"  >> return Xor)
    <|> try (symbol "AND"  >> return And)
    <|> try (symbol "NAND" >> return Nand)

spaces :: Parser ()
spaces = skipMany1 space

ident_with_size :: Parser (Ident, Integer)
ident_with_size = do id <- ident
                     n <- try ((skipMany space) >> (symbol ":") >> natural)
                      <|> return 1
                     symbol ""
                     return (id,n)

constant :: Parser [Bool]
constant = do x <- many1 (oneOf "01")
              symbol ""
              return $ List.reverse (List.map aux x)
  where aux '0' = False
        aux '1' = True

argument :: Parser Argument
argument = do x <- constant
              return $ ArgCst x
       <|> do x <- ident
              return $ ArgVar x

expression :: Parser Expression
expression = do op <- try binop
                a <- argument
                b <- argument
                return $ Ebinop op a b

         <|> do try (symbol "REG")
                i <- ident
                return $ Ereg i

         <|> do try (symbol "NOT")
                a <- argument
                return $ Enot a

         <|> do try (symbol "MUX")
                a <- argument
                b <- argument
                c <- argument
                return $ Emux a b c

         <|> do try (symbol "ROM")
                a <- argument
                return $ Erom a

         <|> do try (symbol "RAM")
                a <- argument
                b <- argument
                c <- argument
                d <- argument
                return $ Eram a b c d

         <|> do try (symbol "CONCAT")
                a <- argument
                b <- argument
                return $ Econcat a b

         <|> do try (symbol "SLICE")
                i <- natural
                j <- natural
                a <- argument
                return $ Eslice i j a

         <|> do try (symbol "SELECT")
                i <- natural
                a <- argument
                return $ Eselect i a

         <|> do a <- argument
                return $ Earg a

equation :: Parser Equation
equation = do id <- ident
              symbol "="
              expr <- expression
              return (id, expr)

netlist :: Parser Netlist
netlist = do skipMany space
             symbol "INPUT"
             l1 <- sepBy ident (symbol ",")
             symbol "OUTPUT"
             l2 <- sepBy ident (symbol ",")
             symbol "VAR"
             l3 <- sepBy ident_with_size (symbol ",")
             symbol "IN"
             l4 <- many equation
             return $ Netlist { netlist_eq  = l4
                              , netlist_in  = l1
                              , netlist_out = l2
                              , netlist_var = l3
                              }

read_netlist :: String -> Netlist
read_netlist code = case parse netlist "netlist" code of
                      Left err -> error ("Parsing error: " ++ show err)
                      Right val -> val

