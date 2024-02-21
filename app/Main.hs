{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.List
import Options.Applicative
import System.Directory (removeFile)
import System.Exit

import qualified CodeGen
import Preprocess (preprocess)
import Lexer (lexer)
import Parser (parse)
import Emission (writeAssembly)
import AssembleAndLink (assembleAndLink)

data Args = Args
    { argsInputFile :: String
    , argsLex :: Bool
    , argsParse :: Bool
    , argsCodeGen :: Bool
    , argsS :: Bool
    } deriving (Eq, Show)

parserArgs :: Parser Args
parserArgs = Args
    <$> argument str (metavar "FILE")
    <*> switch
        ( long "lex"
        <> help "stop before parsing"
        )
    <*> switch
        ( long "parse"
        <> help "stop before assembly generation"
        )
    <*> switch
        ( long "codegen"
        <> help "stop before code emission"
        )
    <*> switch
        ( short 'S'
        <> help "emit an assembly file"
        )

main :: IO ()
main = driver =<< execParser (info parserArgs fullDesc)

driver :: Args -> IO ()
driver args = do
    let inputFilePath = argsInputFile args
    if | argsLex args -> do
            preprocessOutputFilePath <- preprocess inputFilePath
            lexerOutput <- lexer preprocessOutputFilePath
            removeFile preprocessOutputFilePath
            either die (const exitSuccess) lexerOutput
       | argsParse args -> do
            preprocessOutputFilePath <- preprocess inputFilePath
            lexerOutput <- lexer preprocessOutputFilePath
            removeFile preprocessOutputFilePath
            case lexerOutput of
                Left errMsg -> die errMsg
                Right rangedTokens -> do
                    let parserOutput = parse rangedTokens
                    either (die . intercalate "\n") (const exitSuccess) parserOutput
       | argsCodeGen args -> do
            preprocessOutputFilePath <- preprocess inputFilePath
            lexerOutput <- lexer preprocessOutputFilePath
            removeFile preprocessOutputFilePath
            case lexerOutput of
                Left errMsg -> die errMsg
                Right rangedTokens -> do
                    let parserOutput = parse rangedTokens
                    let codeGenOutput = either Left (Right . CodeGen.codeGenProgram) parserOutput
                    either (die . intercalate "\n") (const exitSuccess) codeGenOutput
       | argsS args -> do
            preprocessOutputFilePath <- preprocess inputFilePath
            lexerOutput <- lexer preprocessOutputFilePath
            removeFile preprocessOutputFilePath
            case lexerOutput of
                Left errMsg -> die errMsg
                Right rangedTokens -> do
                    let parserOutput = parse rangedTokens
                    let codeGenOutput = either Left (Right . CodeGen.codeGenProgram) parserOutput
                    case codeGenOutput of
                        Left errMsgs -> die $ intercalate "\n" errMsgs
                        Right program -> do
                            _ <- writeAssembly program inputFilePath
                            exitSuccess
       | otherwise -> do
            preprocessOutputFilePath <- preprocess inputFilePath
            lexerOutput <- lexer preprocessOutputFilePath
            removeFile preprocessOutputFilePath
            case lexerOutput of
                Left errMsg -> die errMsg
                Right rangedTokens -> do
                    let parserOutput = parse rangedTokens
                    let codeGenOutput = either Left (Right . CodeGen.codeGenProgram) parserOutput
                    case codeGenOutput of
                        Left errMsgs -> die $ intercalate "\n" errMsgs
                        Right program -> do
                            assemblyOutputFilePath <- writeAssembly program inputFilePath
                            _ <- assembleAndLink assemblyOutputFilePath
                            removeFile assemblyOutputFilePath
                            exitSuccess
