{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- | Module containing functions to parse Schemey input

module Schemey.Parsing
    ( readExpression
    ) where

import System.Environment ()
import Text.ParserCombinators.Parsec ( oneOf, many, parse, Parser, skipMany1, (<|>), char, noneOf, many1, newline, tab, alphaNum, octDigit, try )
import Text.Read (reset)
import Data.Bool (bool)
import Control.Monad (liftM)
import Text.Parsec.Char (digit, space, letter, endOfLine, hexDigit)
import GHC.Show (Show)
import Text.Parsec.Char (string)
import Numeric (readHex, readOct)
import Data.Char (toTitle, digitToInt)
import GHC.Real (Real)
import GHC.Unicode (toTitle)
import Data.Bits (toIntegralSized)


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
            <|> parseBool

-- | Parser to ignore whitespace
spaces :: Parser ()
spaces = skipMany1 space

-- | Parses to recognize identifiers
symbol :: Parser Char
symbol = oneOf "~$%&|*+_/:<=>?@_~"

-- | Parser to recognize escape characters
escapeChar :: Parser Char
escapeChar = do
        char '\\'
        c <- oneOf "\\\"nrt"
        return $ case c of
                    '\\' -> c
                    '\"' -> c
                    'n' -> '\n'
                    'r' -> '\r'
                    't' -> '\t'
                    _ -> c

-- | Parses an SBool or an SAtom
-- Will parse 'true' and 'false' as boolean literals
-- all other symbols will be parsed as atoms
parseAtom :: Parser SValue
parseAtom = do
    first <- letter <|> symbol
    rest <- many $ letter <|> digit <|> symbol
    return $ SAtom (first : rest)

-- | Parses an SBool
parseBool :: Parser SValue
parseBool = do
    char '#'
    b <- oneOf "tf"
    return $ case b of
                't' -> SBool True
                'f' -> SBool False
                _   -> SBool False

-- | Parses an SNumber
-- So far only parses whole numbers
parseNumber :: Parser SValue
-- parseNumber = many1 digit >>= return . SNumber . read
parseNumber = parseDecimalNumber
            <|> parseDecimalNumberStrict
            <|> parseHexadecimalNumber
            <|> parseOctalNumber
            <|> parseBinaryNumber

-- | Parses decimal numbers into SNumbers
parseDecimalNumber :: Parser SValue
parseDecimalNumber = many1 digit >>= return . SNumber . read

-- | Parses decimal numbers into SNumbers
-- requires the #d prefix
parseDecimalNumberStrict :: Parser SValue
parseDecimalNumberStrict = do
    try $ string "#d"
    x <- many1 $ digit 
    return $ SNumber . read $ x

-- | Parses hexadecimal numbers into SNumbers
parseHexadecimalNumber :: Parser SValue
parseHexadecimalNumber = do
    try $ string "#x"
    x <- many1 $ hexDigit
    return $ SNumber (extract x)
  where
    extract x = fst $ head $ readHex x

-- | Parses octal numbers into SNumber
parseOctalNumber :: Parser SValue
parseOctalNumber = do
    try $ string "#o"
    x <- many1 $ octDigit
    return $ SNumber (extract x)
  where
    extract x = fst $ head $ readOct x

-- | Helper function for converting binary nums to decimal
bin2num :: [Char] -> Integer -> Integer
bin2num ds total = foldl doubleAddDigit total ds
  where doubleAddDigit x y = x * 2 + binDig y
        binDig c = if c == '0' then 0 else 1

-- | Parses binary numbers into an SNumber
parseBinaryNumber :: Parser SValue
parseBinaryNumber = do
    try $ string "#b"
    x <- many1 $ oneOf "10"
    return $ SNumber (bin2num x 0)

-- | Parses an SString
parseString :: Parser SValue
parseString = do
    char '"'
    -- non quote character <|> \" <|> \'
    s <- many $ escapeChar <|> noneOf "\"\\"
    char '"'
    return $ SString s


-- | Parses an SList
-- | Parses an SDottedList