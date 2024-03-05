module Parser where

import Lexer
import Data.Bifunctor (second)
import Data.Maybe (fromMaybe, isJust, fromJust)
import Debug.Trace

data Statement = LetExpr String Expression
               | AssignExpr String Expression
               | FunctionExpr String [String] [Statement]
    deriving (Show, Eq)

data Expression = IdentExpr String
                | IntLiteral Int
                | AddExpr Expression Expression
                | MinusExpr Expression Expression
    deriving (Show, Eq)

findOperator :: Token -> [Token] -> Maybe ([Token], [Token], Token)
findOperator toFind xs 
  | null rest = Nothing
  | length rest == 1 = error "nothing on left of operand"
  | null before = error "nothing on right of operand"
  | otherwise = Just (before, tail rest, toFind)
  where
    (before, rest) = span (/= toFind)  xs


findFirst :: a -> [a -> Maybe b]  -> b
findFirst _ [] = error "no operator found"
findFirst x (f:fs) = fromMaybe (findFirst x fs) result 
  where
    result =  f x

myTail :: [a] -> [a]
myTail [] = []
myTail xs = tail xs

parseExpr :: [Token] -> (Expression, [Token])
parseExpr ((Int x):EndStatement:xs) = (IntLiteral x, xs)
parseExpr ((Ident x):EndStatement:xs) = (IdentExpr x, xs)
parseExpr [Int x] = (IntLiteral x, [])
parseExpr [Ident x] = (IdentExpr x, [])
parseExpr input = (expression,rest)
  where
    (before, rest) = second myTail $ span (/= EndStatement) input
    (before_operator, after_operator, operator) = findFirst before $  map findOperator [Plus, Minus]
    expression = case operator of
      Plus -> AddExpr (fst $ parseExpr before_operator) (fst $ parseExpr after_operator)
      Minus -> MinusExpr (fst $ parseExpr before_operator) (fst $ parseExpr after_operator)

parse :: [Token] -> [Statement]
parse ((Ident ident):Declare:xs) = let (expression, rest) = parseExpr xs in 
                                  LetExpr ident expression : parse rest
parse ((Ident ident):Assign:xs) = let (expression, rest) = parseExpr xs in 
                                  AssignExpr ident expression : parse rest
--parse (Function:(Ident name):OpenParen:CloseParen:OpenCurly:xs) = let (block, rest) = span (/= CloseCurly) xs in 
--                                  FunctionExpr name [] (parse block) : parse (myTail rest)
parse (Function:(Ident name):OpenParen:xs) = let (args, rest) = span (/= CloseParen) xs in 
                                             let (block, rest2) = span (/= CloseCurly) $ myTail rest in
                                  FunctionExpr name (parseArgs args) (parse (myTail block)) : parse (myTail rest2)
parse (EOF:_) = []
parse [] = []
parse idk = error $ "parse error: " ++ show idk

parseArgs :: [Token] -> [String]
parseArgs (CloseParen:xs) = []
parseArgs (Comma:xs) = parseArgs xs
parseArgs ((Ident x):xs) = x : parseArgs xs
parseArgs [] = []
parseArgs idk = error $ "parseArgs error: " ++ show idk

                                    
