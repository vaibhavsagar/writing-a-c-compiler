{
-- At the top of the file, we define the module and its imports, similarly to Haskell.
module Lexer
    ( -- * Invoking Alex
      Alex
    , AlexPosn (..)
    , alexGetInput
    , alexError
    , runAlex
    , alexMonadScan
    , scanMany
    , lexer

    , Range (..)
    , RangedToken (..)
    , Token (..)
    ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
}
-- In the middle, we insert our definitions for the lexer, which will generate the lexemes for our grammar.
%wrapper "monadUserState-bytestring"

$alpha = [a-zA-Z]
$digit = [0-9]

@id = ($alpha | \_) ($alpha | $digit | \_)*

tokens :-

<0> $white+ ;

-- Keywords

<0> "int" {tok KwInt}
<0> "void" {tok KwVoid}
<0> "return" {tok KwReturn}

-- Parentheses

<0> "(" {tok LPar}
<0> ")" {tok RPar}

-- Brackets

<0> "{" {tok LBracket}
<0> "}" {tok RBracket}

-- Semicolon

<0> ";" {tok Semicolon}

-- Identifiers

<0> @id {tokId}

-- Constants

<0> $digit+ {tokConstant}

{
-- At the bottom, we may insert more Haskell definitions, such as data structures, auxiliary functions, etc.
data AlexUserState = AlexUserState
    {
    }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState

alexEOF :: Alex RangedToken
alexEOF = do
    (pos, _, _, _) <- alexGetInput
    pure $ RangedToken EOF (Range pos pos)

data Range = Range
    { start :: AlexPosn
    , stop :: AlexPosn
    } deriving (Eq, Show)

data RangedToken = RangedToken
    { rtToken :: Token
    , rtRange :: Range
    } deriving (Eq, Show)

data Token
    -- Identifiers
    = Identifier ByteString
    -- Constants
    | Constant Int
    -- Keywords
    | KwInt
    | KwVoid
    | KwReturn
    -- Parentheses
    | LPar
    | RPar
    -- Brackets
    | LBracket
    | RBracket
    -- Semicolon
    | Semicolon
    | EOF
    deriving (Eq, Show)

mkRange :: AlexInput -> Int64 -> Range
mkRange (start, _, str, _) len = Range{start = start, stop = stop}
    where
        stop = BS.foldl' alexMove start $ BS.take len str

tokId :: AlexAction RangedToken
tokId inp@(_, _, str, _) len =
    pure RangedToken
        { rtToken = Identifier $ BS.take len str
        , rtRange = mkRange inp len
        }

tok :: Token -> AlexAction RangedToken
tok ctor inp len =
    pure RangedToken
        { rtToken = ctor
        , rtRange = mkRange inp len
        }

tokConstant :: AlexAction RangedToken
tokConstant inp@(_, _, str, _) len =
    pure RangedToken
        { rtToken = Constant $ read $ BS.unpack $ BS.take len str
        , rtRange = mkRange inp len
        }

scanMany :: ByteString -> Either String [RangedToken]
scanMany input = runAlex input go
    where
        go = do
            output <- alexMonadScan
            if rtToken output == EOF
                then pure [output]
                else (output :) <$> go

lexer :: FilePath -> IO (Either String [RangedToken])
lexer inputFilePath = do
    input <- BS.readFile inputFilePath
    pure $ scanMany input
}
