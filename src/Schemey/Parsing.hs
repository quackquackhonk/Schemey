-- | Module containing functions to parse Schemey input

module Schemey.Parsing
    ( readExpression
    ) where

import System.Environment ()
import Text.ParserCombinators.Parsec ( oneOf, many, parse, Parser, skipMany1, (<|>), char, noneOf, many1 )
import Text.Read (reset)
import Data.Bool (bool)
import Control.Monad (liftM)
import Text.Parsec.Char (digit, space, letter)
import GHC.Show (Show)


{-|
Schemey Language Grammar

value: <number> | <character>*
expression: value | ( <operator> <expression> )
operator: + | - | * | / | % 
program: <start> <operator> <expression> <eof>
|-}

-- | Data Definition for a Scheme Value
data SValue = SAtom String
            | SNumber Integer
            | SString String
            | SBool Bool
            | SList [SValue]
            | SDottedList [SValue] SValue
            deriving (Show, Eq)

-- | Reads a String of Schemey input and returns an output
readExpression :: String -> String
readExpression input = case parse parseSValue "lisp" input of
    Left  err -> "No match: " ++ show err
    Right val   -> show val

parseSValue :: Parser SValue
parseSValue = parseAtom
            <|> parseString
            <|> parseNumber

-- | Parser to ignore whitespace
spaces :: Parser ()
spaces = skipMany1 space

-- | Parses to recognize identifiers
symbol :: Parser Char
symbol = oneOf "~#$%&|*+_/:<=>?@_~"

-- | Parses an SBool or an SAtom
-- Will parse 'true' and 'false' as boolean literals
-- all other symbols will be parsed as atoms
parseAtom :: Parser SValue
parseAtom = do
    first <- letter <|> symbol
    rest <- many $ letter <|> digit <|> symbol
    let atom = first : rest
    return $ case atom of
        "true" -> SBool True
        "false" -> SBool False
        _       -> SAtom atom

-- | Parses an SNumber
-- So far only parses whole numbers
parseNumber :: Parser SValue
parseNumber = SNumber . read <$> many1 digit

-- | Parses an SString
parseString :: Parser SValue
parseString = do
    char '"'
    s <- many $ noneOf "\""
    char '"'
    return $ SString s

-- | Parses an SList
-- | Parses an SDottedList