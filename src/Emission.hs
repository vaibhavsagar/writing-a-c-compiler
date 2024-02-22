module Emission where

import qualified CodeGen

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
    emitString ""

emitFunction :: CodeGen.Function -> State [String] ()
emitFunction (CodeGen.Function (CodeGen.Identifier name) instrs) = do
    emitString $ "\t.globl " <> name
    emitString $ name <> ":"
    emitString $ "\tpushq %rbp"
    emitString $ "\tmovq %rsp, %rbp"
    traverse_ emitInstruction instrs

emitInstruction :: CodeGen.Instruction -> State [String] ()
emitInstruction instr = case instr of
    CodeGen.Mov src dst -> emitString $ "\tmovl " <> (getOperand src) <> ", " <> (getOperand dst)
    CodeGen.Unary op operand -> emitUnary op operand
    CodeGen.AllocateStack int -> emitString $ "subq $" <> show int <> ", %rsp"
    CodeGen.Ret -> do
        emitString "\tmovq %rbp, %rsp"
        emitString "\tpopq %rbp"
        emitString "\tret"

emitUnary :: CodeGen.UnaryOperator -> CodeGen.Operand -> State [String] ()
emitUnary op operand = case op of
    CodeGen.Neg -> emitString $ "\tnegl " <> getOperand operand
    CodeGen.Not -> emitString $ "\tnotl " <> getOperand operand

getOperand :: CodeGen.Operand -> String
getOperand op = case op of
    CodeGen.Reg CodeGen.AX -> "%eax"
    CodeGen.Reg CodeGen.R10 -> "%r10d"
    CodeGen.Stack int -> show int <> "(%rbp)"
    CodeGen.Imm int -> "$" <> show int
    CodeGen.Pseudo identifier -> error $ "pseudoregister found: " <> show identifier
