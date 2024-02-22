{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser where

import Control.Applicative (Alternative(..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Lexer

newtype Parser s a = Parser { runParser :: ExceptT [String] (State s) a } deriving (Functor, Applicative, Alternative, Monad)

data Program = Program Function deriving (Eq, Show)

data Function = Function { functionName :: Identifier, functionBody :: Statement } deriving (Eq, Show)

data Statement = Return Expression deriving (Eq, Show)

data Expression
    = ConstantExpression Constant
    | UnaryExpression UnaryOperator Expression
    deriving (Eq, Show)

data UnaryOperator
    = Complement
    | Negate
    deriving (Eq, Show)

newtype Identifier = Identifier String deriving (Eq, Show)

data Constant = Constant Int deriving (Eq, Show)

throwError :: String -> Parser s a
throwError = Parser . throwE . (:[])

expect :: Lexer.Token -> Parser [Lexer.RangedToken] ()
expect expectedToken = do
    actualToken <- Parser $ lift $ gets head
    if expectedToken == (Lexer.rtToken actualToken)
        then do
            Parser $ lift $ modify' tail
            pure ()
        else throwError $ concat
            [ "Expected: ", show expectedToken, "\n"
            , "Got: ", show actualToken, "\n"
            ]

confirmEOF :: Parser [Lexer.RangedToken] ()
confirmEOF = do
    actualToken <- Parser $ lift $ gets head
    remaining <- Parser $ lift $ gets tail
    if (Lexer.EOF == Lexer.rtToken actualToken && null remaining)
        then pure ()
        else throwError $ concat
            [ "Expected EOF\n"
            , "Got: ", show actualToken, "\n"
            ]

parseIdentifier :: Parser [Lexer.RangedToken] Identifier
parseIdentifier = do
    actualToken <- Parser $ lift $ gets head
    case Lexer.rtToken actualToken of
        Lexer.Identifier nameBS -> do
            Parser $ lift $ modify' tail
            pure $ Identifier $ BS.unpack nameBS
        _ -> throwError $ concat
                [ "Expected identifier\n"
                , "Got: ", show actualToken, "\n"
                ]

parseConstant :: Parser [Lexer.RangedToken] Constant
parseConstant = do
    actualToken <- Parser $ lift $ gets head
    case Lexer.rtToken actualToken of
        Lexer.Constant intValue -> do
            Parser $ lift $ modify' tail
            pure $ Constant intValue
        _ -> throwError $ concat
                [ "Expected identifier\n"
                , "Got: ", show actualToken, "\n"
                ]

parseProgram :: Parser [Lexer.RangedToken] Program
parseProgram = do
    fn <- parseFunction
    confirmEOF
    pure $ Program fn

parseFunction :: Parser [Lexer.RangedToken] Function
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

parseStatement :: Parser [Lexer.RangedToken] Statement
parseStatement = do
    expect Lexer.KwReturn
    value <- parseExpression
    expect Lexer.Semicolon
    pure $ Return value

parseExpression :: Parser [Lexer.RangedToken] Expression
parseExpression
    =   (expect Lexer.Complement *> (UnaryExpression Complement <$> parseExpression))
    <|> (expect Lexer.Negate *> (UnaryExpression Negate <$> parseExpression))
    <|> (expect Lexer.LPar *> parseExpression <* expect Lexer.RPar)
    <|> (ConstantExpression <$> parseConstant)

parse :: [Lexer.RangedToken] -> Either [String] Program
parse tokens = evalState (runExceptT $ runParser parseProgram) tokens
