module NewParser where
{- HLINT ignore "Use <$>" -}

import Control.Applicative
import Data.Char

data Statement = LetExpr String Expression
               | AssignExpr String Expression
               | Return Expression
               | Scope [Statement]
               | FunctionExpr String [String] Statement -- has to be scope
    deriving (Show, Eq)

data Expression = IdentExpr String
                | IntLiteral Int
                | AddExpr Expression Expression
                | MinusExpr Expression Expression
                | MulExpr Expression Expression
                | DivExpr Expression Expression
                | FuncCall String [Expression]
    deriving (Show, Eq)

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) =
      Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
  return = pure
  (Parser p) >>= f =
    Parser $ \input -> do
      (input', x) <- p input
      runParser (f x) input'


charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y:ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

isIdentChar :: Char -> Bool
isIdentChar x = x `elem` ['a'..'z'] ++ ['A'..'Z']

spanP :: (Char -> Bool) -> Parser String
spanP f =
  Parser $ \input ->
    let (token, rest) = span f input
     in Just (rest, token)

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = ((:) <$> element <*> many (sep *> element)) <|> pure []

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if null xs
      then Nothing
      else Just (input', xs)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
    where 
        rest a = (do
            f <- op
            b <- p
            rest (f a b)) <|> pure a

ws :: Parser String
ws = spanP isSpace

ident :: Parser String
ident = notNull $ spanP isIdentChar

identExpr :: Parser Expression
identExpr = IdentExpr <$> ident

intLiteral  :: Parser Expression
intLiteral = f <$> notNull (spanP isDigit)
    where f ds = IntLiteral $ read ds


expression :: Parser Expression
expression = term `chainl1` addOp 

term :: Parser Expression
term = factor `chainl1` mulOp

factor :: Parser Expression
factor = functionCall <|> identExpr <|> intLiteral <|> (charP '(' *> expression <* charP ')') 

addOp :: Parser (Expression -> Expression -> Expression)
addOp = (ws *> (charP '+' *> pure AddExpr) <* ws) <|> (ws *> (charP '-' *> pure MinusExpr) <* ws)

mulOp :: Parser (Expression -> Expression -> Expression)
mulOp = (ws *> (charP '*' *> pure MulExpr) <* ws) <|> (ws *> (charP '/' *> pure DivExpr) <* ws)

functionCall :: Parser Expression
functionCall = do
  name <- ident
  _ <- charP '('
  arguments <- sepBy (ws *> charP ',' <* ws) expression
  _ <- charP ')'
  return $ FuncCall name arguments

letStatement :: Parser Statement
letStatement = do
  name <- ident
  _ <- ws
  _ <- stringP ":="
  _ <- ws
  expression <- expression
  _ <- ws
  _ <- charP ';'
  return $ LetExpr name expression

assignStatement :: Parser Statement
assignStatement = do
  name <- ident
  _ <- ws
  _ <- stringP "="
  _ <- ws
  expression <- expression
  _ <- ws
  _ <- charP ';'
  return $ AssignExpr name expression

scopeExpr :: Parser Statement
scopeExpr = do
  _ <- charP '{'
  _ <- ws
  code <- parseStatements
  _ <- ws
  _ <- charP '}'
  return $ Scope code

functionExpr :: Parser Statement
functionExpr = do
  _ <- ws
  _ <- stringP "fn"
  _ <- ws
  name <- ident
  _ <- charP '('
  arguments <- sepBy (ws *> charP ',' <* ws) ident
  _ <- charP ')'
  _ <- ws
  code <- scopeExpr
  _ <- ws
  return $ FunctionExpr name arguments  code

returnExpr :: Parser Statement
returnExpr = do
  _ <- stringP "return"
  _ <- ws
  expression <- expression
  _ <- ws
  _ <- charP ';'
  return $ Return expression

statement :: Parser Statement
statement = letStatement <|> assignStatement <|> functionExpr <|> returnExpr

parseStatements :: Parser [Statement]
parseStatements =  notNull ( sepBy ws statement)
