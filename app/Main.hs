module Main where

import NewParser
import Compiler

import Debug.Trace
import Data.Maybe
import System.Process

assemble :: [Instruction] -> String
assemble instructions = unlines ["format ELF64", 
                                "section '.text' executable", 
                                "public _start"] 
                                ++ unlines ( map show instructions) 
                                ++ unlines 
                                ["_start:", 
                                "\tcall main", 
                                "\tmov rdi, rax",
                                "\tmov rax, 60",
                                "\tsyscall"]

main :: IO ()
main = do
    input <- readFile "input.lang"
    let parsed = snd $ fromJust $ runParser parseStatements input
    print parsed

    let asm = assemble $ compile parsed
    putStrLn asm
    writeFile "output.asm" asm
    _ <- system "fasm output.asm"
    _ <- system "ld -o output output.o"
    return ()

