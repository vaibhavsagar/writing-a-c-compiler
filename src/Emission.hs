module Emission where

import qualified CodeGen
import qualified Parser

import Control.Monad.Trans.State.Strict
import Data.Foldable (traverse_)
import Data.List
import System.FilePath

emit :: CodeGen.Program -> String
emit program = intercalate "\n" $ execState (emitProgram program) []

writeAssembly :: CodeGen.Program -> FilePath -> IO FilePath
writeAssembly program inputFilePath = do
    let outputFilePath = replaceFileName inputFilePath (takeBaseName inputFilePath ++ ".s")
    let assembly = emit program
    writeFile outputFilePath assembly
    pure outputFilePath

emitString :: String -> State [String] ()
emitString str = modify' (\s -> s <> [str])

emitProgram :: CodeGen.Program -> State [String] ()
emitProgram (CodeGen.Program function) = do
    emitFunction function
    emitString "\t.section .note.GNU-stack,\"\",@progbits"

emitFunction :: CodeGen.Function -> State [String] ()
emitFunction (CodeGen.Function (Parser.Identifier name) instrs) = do
    emitString $ "\t.globl " <> name
    emitString $ name <> ":"
    traverse_ emitInstruction instrs

emitInstruction :: CodeGen.Instruction -> State [String] ()
emitInstruction instr = case instr of
    CodeGen.Mov src dst -> emitString $ "\tmovl " <> (getOperand src) <> ", " <> (getOperand dst)
    CodeGen.Ret -> emitString "\tret"

getOperand :: CodeGen.Operand -> String
getOperand op = case op of
    CodeGen.Imm int -> "$" <> show int
    CodeGen.Register -> "%eax"
