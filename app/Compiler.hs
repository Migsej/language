module Compiler where

import NewParser

import Data.List (sort, group)
import qualified Data.Map as M
import Text.Printf
import Debug.Trace

data Location = Name String
              | Location String
              | Intlit Int
              | RbpOff Int
              | Rbp
              | Rsp
              | Rax
              | Rdi
instance Show Location where
  show Rax = "rax"
  show Rbp = "rbp"
  show Rsp = "rsp"
  show Rdi = "rdi"
  show (Intlit num) = show num
  show (Name name) = name
  show (Location name) = name
  show (RbpOff num) = printf (if num < 0 then "[%s %d]" else "[%s +%d]") (show Rbp) num
  


data Instruction = Push Location
                 | Pop Location
                 | Mov Location Location
                 | Sub Location Location
                 | Add Location Location
                 | Label String
                 | Call String
                 | Ret

instance Show Instruction where
  show (Push loc) = printf "\tpush %s" (show loc)
  show (Pop loc) = printf "\tpop %s" (show loc)
  show (Mov loc1 loc) = printf "\tmov %s, %s" (show loc1) (show loc)
  show (Sub loc1 loc) = printf "\tsub %s, %s" (show loc1) (show loc)
  show (Add loc1 loc) = printf "\tadd %s, %s" (show loc1) (show loc)
  show (Label name) = printf "%s:" name
  show Ret = printf "\tret" 
  show (Call name) = printf "\tcall %s" name

scope :: [(String, Int)] -> Statement -> [Instruction]
scope arguments (Scope code) = [Push Rbp, Mov Rbp Rsp, Sub Rsp (Intlit allocatedspace)] ++ backtracked 
  where
    compiled = compile code

    getLetExprs ((LetExpr name expr):rest) = name : getLetExprs rest
    getLetExprs (_:rest) = getLetExprs rest
    getLetExprs [] = []
    namesfromscope = zip (getLetExprs code) [-8,-16..]
    names = M.fromList arguments `M.union` M.fromList namesfromscope

    setname (Push (Name name)) = Push (RbpOff (names M.! name))
    setname (Pop (Name name)) = Pop (RbpOff (names M.! name))

    setname (Mov (Name name1) (Name name)) = Mov (RbpOff (names M.! name1)) (RbpOff (names M.! name))
    setname (Mov idkman (Name name)) = Mov idkman (RbpOff (names M.! name))
    setname (Mov (Name name) idkman ) = Mov (RbpOff (names M.! name)) idkman 

    setname (Sub (Name name1) (Name name)) = Sub (RbpOff (names M.! name1)) (RbpOff (names M.! name))
    setname (Sub idkman (Name name)) = Sub idkman (RbpOff (names M.! name))
    setname (Sub (Name name) idkman ) = Sub (RbpOff (names M.! name)) idkman 
    setname other = other 

    backtracked = map setname compiled
    allocatedspace = length namesfromscope * 8


compileExpr :: Expression -> [Instruction]
compileExpr (IntLiteral num) = [Push (Intlit num)]
compileExpr (AddExpr a b) = compileExpr a ++ compileExpr b ++ [Pop Rax, Pop Rdi, Add Rax Rdi, Push Rax]
compileExpr (MinusExpr a b) = compileExpr a ++ compileExpr b ++ [Pop Rax, Pop Rdi, Sub Rax Rdi, Push Rax]
compileExpr (IdentExpr name) = [Mov Rax (Name name), Push Rax]
compileExpr (FuncCall name arguments) = concatMap compileExpr arguments ++ [Call name, Push Rax]
compileExpr other = error $ "what is expression " ++ show other

compile :: [Statement] -> [Instruction]
compile (FunctionExpr name args code :rest) = Label name : 
                                                scope 
                                                (zip args [16, 24 .. ]) code ++ 
                                                compile rest
compile (LetExpr name expr: rest) = compileExpr expr ++ [Pop Rax, Mov (Name name) Rax] ++ compile rest
compile (Return expr:rest) = compileExpr expr ++ [Pop Rax, Mov Rsp Rbp, Pop Rbp, Ret] ++ compile rest
compile (bla:_) = error $ "what is statement" ++ show bla
compile [] = []
