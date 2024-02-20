{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.Trans.State.Strict
import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Lexer

newtype ParseResult a = ParseResult { getParseResult :: Either String a } deriving (Functor, Applicative, Monad)

instance Alternative ParseResult where
    empty = ParseResult $ Left ""
    (ParseResult r1) <|> (ParseResult r2) = ParseResult $ either (const r2) Right r1

instance MonadPlus ParseResult where
    mzero = empty
    mplus = (<|>)

newtype Parser s a = Parser { runParser :: StateT s ParseResult a } deriving (Functor, Applicative, Alternative, Monad)

data Program = Program Function deriving (Eq, Show)

data Function = Function { functionName :: Identifier, functionBody :: Statement } deriving (Eq, Show)

data Statement = Return Expression deriving (Eq, Show)

data Expression = Expression Constant deriving (Eq, Show)

data Identifier = Identifier String deriving (Eq, Show)

data Constant = Constant Int deriving (Eq, Show)

throwError :: String -> Parser s a
throwError = Parser . StateT . const . ParseResult . Left

expect :: Lexer.Token -> Parser [Lexer.RangedToken] ()
expect expectedToken = do
    actualToken <- Parser $ gets head
    if expectedToken == (Lexer.rtToken actualToken)
        then do
            Parser $ modify' tail
            pure ()
        else throwError $ concat
            [ "Expected: ", show expectedToken, "\n"
            , "Got: ", show actualToken, "\n"
            ]

confirmEOF :: Parser [Lexer.RangedToken] ()
confirmEOF = do
    actualToken <- Parser $ gets head
    remaining <- Parser $ gets tail
    if (Lexer.EOF == Lexer.rtToken actualToken && null remaining)
        then pure ()
        else throwError $ concat
            [ "Expected EOF\n"
            , "Got: ", show actualToken, "\n"
            ]

parseIdentifier :: Parser [Lexer.RangedToken] Identifier
parseIdentifier = do
    actualToken <- Parser $ gets head
    case Lexer.rtToken actualToken of
        Lexer.Identifier nameBS -> do
            Parser $ modify' tail
            pure $ Identifier $ BS.unpack nameBS
        _ -> throwError $ concat
                [ "Expected identifier\n"
                , "Got: ", show actualToken, "\n"
                ]

parseConstant :: Parser [Lexer.RangedToken] Constant
parseConstant = do
    actualToken <- Parser $ gets head
    case Lexer.rtToken actualToken of
        Lexer.Constant intValue -> do
            Parser $ modify' tail
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
parseExpression = Expression <$> parseConstant

parse :: [Lexer.RangedToken] -> Either String Program
parse = getParseResult . evalStateT (runParser parseProgram)
