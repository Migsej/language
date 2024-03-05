module QBE where

import NewParser
import Text.Printf
import Data.List (intercalate)

function :: String -> [String] -> [Statement] -> String
function name args code = printf "export function l $%s(%s) {\n@start\n%s%s}\n" name argscompiled allocatedargs (compile code)
  where
    argscompiled = intercalate ", " $ map (printf "l %%%sn") args
    allocatedargs :: String
    allocatedargs = concatMap (\x -> printf "%%%s =l alloc8 8\nstorel %%%sn, %%%s\n" x x x) args

declare :: String -> Expression -> String
declare name expr = printf "%s%%%s =l alloc8 8\nstorel %%%s, %%%s\n" (compileexpression ename expr) name ename name
  where 
   ename = name ++ "e"
assign :: String -> Expression -> String
assign name expr = printf "%sstorel %%%s, %%%s\n" (compileexpression ename expr) ename name
  where 
   ename = name ++ "e"

compreturn :: Expression -> String
compreturn expr = printf "%sret %%%s\n" (compileexpression "returnsymbol" expr) "returnsymbol"

binary :: String -> Expression -> Expression -> String -> String
binary resultname a b operation = compileda ++ compiledb ++ printf "%%%s =l %s %%%s, %%%s\n" resultname operation aname bname
  where
    aname = resultname ++ "a"
    bname = resultname ++ "b"
    compileda = compileexpression aname a
    compiledb = compileexpression bname b


argumentnames :: String -> [String]
argumentnames name = map (\x -> name ++ "arg" ++ show x) [1..]


compileexpression :: String -> Expression -> String
compileexpression resultname (IntLiteral x)  = printf "%%%s =l alloc8 8\nstorel %d, %%%s\n" resultname x resultname  
compileexpression resultname (IdentExpr x)   = printf "%%%s =l loadl %%%s\n" resultname x 
compileexpression resultname (AddExpr a b)   = binary resultname a b "add"
compileexpression resultname (MinusExpr a b) = binary resultname a b "sub"
compileexpression resultname (FuncCall name args) = printf "%s%%%s =l call $%s(%s)\n" (concat compiledexpressions) resultname name argscompiled
  where
    compiledexpressions = zipWith compileexpression (argumentnames resultname) args 
    argscompiled = intercalate ", " $ map (printf "l %%%s") $ take (length compiledexpressions) $ argumentnames resultname 

compileexpression _ other = error ("what is " ++ show other)

compile :: [Statement] -> String
compile ((FunctionExpr name args code):xs) = function name args code ++ compile xs
compile ((LetExpr name expr):xs) = declare name expr ++ compile xs
compile ((AssignExpr name expr):xs) = assign name expr ++ compile xs
compile ((Return expr):xs) = compreturn expr ++ compile xs
compile [] = []
--compile other = error ("what is " ++ show other)
