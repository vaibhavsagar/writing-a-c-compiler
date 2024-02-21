module CodeGen where

-- import Control.Monad.Trans.State.Strict

import qualified Parser

data Program = Program Function deriving (Eq, Show)
data Function = Function { functionName :: Parser.Identifier, instructions :: [Instruction] } deriving (Eq, Show)
data Instruction = Mov { movSrc :: Operand, movDst :: Operand } | Ret deriving (Eq, Show)
data Operand = Imm Int | Register deriving (Eq, Show)

codeGenProgram :: Parser.Program -> Program
codeGenProgram (Parser.Program function) = Program (codeGenFunction function)

codeGenFunction :: Parser.Function -> Function
codeGenFunction (Parser.Function name body) = Function name (codeGenStatement body)

codeGenStatement :: Parser.Statement -> [Instruction]
codeGenStatement (Parser.Return expression) =
    [ Mov (codeGenExpression expression) Register
    , Ret
    ]

codeGenExpression :: Parser.Expression -> Operand
codeGenExpression (Parser.Expression (Parser.Constant int)) = Imm int
