module Lexer where
import Data.Bifunctor (first)
import Data.Char (isDigit, isSpace)
import Data.Semigroup (Min(Min))

data Token = Ident String
           | Assign
           | Plus
           | Minus
           | Declare
           | Int Int
           | EndStatement
           | Function
           | OpenParen
           | CloseParen
           | OpenCurly
           | CloseCurly
           | Comma
           | EOF
       deriving (Show, Eq)

isIdentChar :: Char -> Bool
isIdentChar x = x `elem` ['a'..'z'] ++ ['A'..'Z']

readIdent :: String -> (Token, String)
readIdent = first Ident . span isIdentChar


readInt :: String -> (Token, String)
readInt = first (Int . read) . span isDigit

lexerNext :: String -> (Token, String)
lexerNext ('=':xs) = (Assign, xs)
lexerNext ('+':xs) = (Plus, xs)
lexerNext ('-':xs) = (Minus, xs)
lexerNext (';':xs) = (EndStatement, xs)
lexerNext (':':'=':xs) = (Declare, xs)
lexerNext ('f':'n':xs) = (Function, xs)
lexerNext ('(':xs) = (OpenParen, xs)
lexerNext (')':xs) = (CloseParen, xs)
lexerNext ('{':xs) = (OpenCurly, xs)
lexerNext ('}':xs) = (CloseCurly, xs)
lexerNext (',':xs) = (Comma, xs)
lexerNext input@(x:xs) 
  | isIdentChar x = readIdent input
  | isDigit x = readInt input
  | isSpace x = lexerNext xs
lexerNext [] = (EOF, [])
lexerNext xs = error $ "unexpected input: " ++ xs

tokenize :: String -> [Token]
tokenize xs = token : if token /= EOF then tokenize rest else []
  where
  (token, rest) = lexerNext xs
