module Parser where

import Control.Monad.Trans.State.Strict
import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Lexer

data Program = Program Function deriving (Eq, Show)

data Function = Function { functionName :: Identifier, functionBody :: Statement } deriving (Eq, Show)

data Statement = Return Expression deriving (Eq, Show)

data Expression = Expression Constant deriving (Eq, Show)

data Identifier = Identifier String deriving (Eq, Show)

data Constant = Constant Int deriving (Eq, Show)

expect :: Lexer.Token -> State [Lexer.RangedToken] ()
expect expectedToken = do
    actualToken <- gets head
    if expectedToken == (Lexer.rtToken actualToken)
        then do
            modify' tail
            pure ()
        else do
            error $ concat
                [ "Expected: ", show expectedToken, "\n"
                , "Got: ", show actualToken, "\n"
                ]

confirmEOF :: State [Lexer.RangedToken] ()
confirmEOF = do
    actualToken <- gets head
    remaining <- gets tail
    if (Lexer.EOF == Lexer.rtToken actualToken && null remaining)
        then pure ()
        else error $ concat
            [ "Expected EOF\n"
            , "Got: ", show actualToken, "\n"
            ]

parseIdentifier :: State [Lexer.RangedToken] Identifier
parseIdentifier = do
    actualToken <- gets head
    case Lexer.rtToken actualToken of
        Lexer.Identifier nameBS -> do
            modify' tail
            pure $ Identifier $ BS.unpack nameBS
        _ -> error $ concat
                [ "Expected identifier\n"
                , "Got: ", show actualToken, "\n"
                ]

parseConstant :: State [Lexer.RangedToken] Constant
parseConstant = do
    actualToken <- gets head
    case Lexer.rtToken actualToken of
        Lexer.Constant intValue -> do
            modify' tail
            pure $ Constant intValue
        _ -> error $ concat
                [ "Expected identifier\n"
                , "Got: ", show actualToken, "\n"
                ]

parseProgram :: State [Lexer.RangedToken] Program
parseProgram = do
    fn <- parseFunction
    confirmEOF
    pure $ Program fn

parseFunction :: State [Lexer.RangedToken] Function
parseFunction = do
    expect Lexer.KwInt
    name <- parseIdentifier
    expect Lexer.LPar
    expect Lexer.KwVoid
    expect Lexer.RPar
    expect Lexer.LBracket
    body <- parseStatement
    expect Lexer.RBracket
    pure $ Function name body

parseStatement :: State [Lexer.RangedToken] Statement
parseStatement = do
    expect Lexer.KwReturn
    value <- parseExpression
    expect Lexer.Semicolon
    pure $ Return value

parseExpression :: State [Lexer.RangedToken] Expression
parseExpression = Expression <$> parseConstant

parse :: [Lexer.RangedToken] -> Program
parse = evalState parseProgram
