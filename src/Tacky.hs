module Tacky where

import Control.Monad.Trans.State.Strict

import qualified Parser

newtype Identifier = Identifier String deriving (Eq, Show)
data Program = Program Function deriving (Eq, Show)
data Function = Function
    { functionName :: Identifier
    , functionBody :: [Instruction]
    }
    deriving (Eq, Show)
data Instruction
    = ReturnInstruction Value
    | UnaryInstruction
        { unaryInstructionOperator :: UnaryOperator
        , unaryInstructionSrc :: Value
        , unaryInstructionDst :: Value
        }
    deriving (Eq, Show)
data Value
    = ConstantValue Int
    | VarValue Identifier
    deriving (Eq, Show)
data UnaryOperator
    = Complement
    | Negate
    deriving (Eq, Show)

tacky :: Parser.Program -> Program
tacky = flip evalState 0 . emitProgram

emitProgram :: Parser.Program -> State Int Program
emitProgram (Parser.Program function) = Program <$> emitFunction function

emitFunction :: Parser.Function -> State Int Function
emitFunction (Parser.Function identifier body) = do
    let (Parser.Identifier name) = identifier
    instructions <- emitStatement body name
    pure $ Function (Identifier name) instructions

emitStatement :: Parser.Statement -> String -> State Int [Instruction]
emitStatement (Parser.Return expr) name = do
    (dst, instructions) <- emitExpr expr name
    pure $ instructions <> [ReturnInstruction dst]

emitExpr :: Parser.Expression -> String -> State Int (Value, [Instruction])
emitExpr expr prefix = case expr of
    Parser.ConstantExpression (Parser.Constant i) -> pure (ConstantValue i, [])
    Parser.UnaryExpression op inner -> do
        (src, instructions) <- emitExpr inner prefix
        var <- VarValue . mkVar prefix <$> freshVar
        pure (var, instructions <> [emitUnary op src var])

emitUnary :: Parser.UnaryOperator -> Value -> Value -> Instruction
emitUnary op src dst = case op of
    Parser.Complement -> UnaryInstruction Complement src dst
    Parser.Negate -> UnaryInstruction Negate src dst

mkVar :: String -> Int -> Identifier
mkVar prefix i = Identifier $ prefix <> "." <> show i

freshVar :: State Int Int
freshVar = do
    varId <- get
    modify' (+1)
    pure varId
