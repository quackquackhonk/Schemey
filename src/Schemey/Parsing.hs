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
import Text.Parsec.Char
    ( digit, space, letter, endOfLine, hexDigit, anyChar, string )
import GHC.Show (Show)
import Numeric (readHex, readOct, readFloat)
import Data.Char (toTitle, digitToInt)
import GHC.Real (Real, (%))
import GHC.Unicode (toTitle)
import Data.Bits (toIntegralSized)

-- | Data Definition for a Scheme Value
data SValue = SAtom String
            | SString String
            | SBool Bool
            | SCharacter Char
            | SComplex Float Float
            | SRational Rational
            | SInteger Integer
            | SFloat Float
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
            <|> try parseCharacter
            <|> try parseBool
            <|> try parseComplex
            <|> try parseFloat
            <|> try parseRational
            <|> try parseInteger

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

-- | Parses an SCharacter
parseCharacter :: Parser SValue
parseCharacter = do
    try $ string "#\\"
    c <- many anyChar
    return $ SCharacter $ case c of
                    "" -> ' '
                    "newline" -> '\n'
                    [a] -> a
                    _ -> '\0'

-- | Parses an SNumber
-- So far only parses whole numbers
parseInteger :: Parser SValue
parseInteger = parseDecimalInteger 
            <|> parseDecimalIntegerStrict 
            <|> parseHexadecimalInteger 
            <|> parseOctalInteger 
            <|> parseBinaryInteger 

-- | Parses decimal numbers into SNumbers
parseDecimalInteger :: Parser SValue
parseDecimalInteger = many1  digit >>= return . SInteger .  read

-- | Parses decimal numbers into SNumbers
-- requires the #d prefix
parseDecimalIntegerStrict :: Parser SValue
parseDecimalIntegerStrict = do
    try $ string "#d" <|> string "#D"
    x <- many1 digit
    return $ SInteger . read $ x

-- | Parses hexadecimal numbers into SNumbers
parseHexadecimalInteger :: Parser SValue
parseHexadecimalInteger = do
    try $ string "#x" <|> string "#X"
    x <- many1 hexDigit
    return $ SInteger (extract x)
  where
    extract x = fst $ head $ readHex x

-- | Parses octal numbers into SNumber
parseOctalInteger :: Parser SValue
parseOctalInteger = do
    try $ string "#o" <|> string "#O"
    x <- many1 $ octDigit
    return $ SInteger (extract x)
  where
    extract x = fst $ head $ readOct x

-- | Helper function for converting binary nums to decimal
bin2num :: [Char] -> Integer -> Integer
bin2num ds total = foldl doubleAddDigit total ds
  where doubleAddDigit x y = x * 2 + binDig y
        binDig c = if c == '0' then 0 else 1

-- | Parses binary numbers into an SNumber
parseBinaryInteger :: Parser SValue
parseBinaryInteger = do
    try $ string "#b"
    x <- many1 $ oneOf "10"
    return $ SInteger (bin2num x 0)

-- | Parses an SString
parseString :: Parser SValue
parseString = do
    char '"'
    -- non quote character <|> \" <|> \'
    s <- many $ escapeChar <|> noneOf "\"\\"
    char '"'
    return $ SString s

-- | Parses an SComplex number
parseComplex :: Parser SValue
parseComplex = do
    real <- try $ parseDecimalInteger <|> parseFloat
    char '+'
    complex <- try $ parseDecimalInteger <|> parseFloat
    char 'i'
    return $ SComplex (toDouble real) (toDouble complex)
  where
      toDouble (SInteger x) = fromIntegral x
      toDouble (SFloat x) = realToFrac x

-- | Parses an SReal (Floating point)
parseFloat :: Parser SValue
parseFloat = do
    a <- many1 digit
    char '.'
    b <- many1 digit
    return $ SFloat $ fst . head $ readFloat (a++"."++b)

-- | Parses an SRational number
parseRational :: Parser SValue
parseRational = do
    numerator <- many1 digit 
    char '/'
    denominator <- many1 digit 
    return $ SRational $ read numerator % read denominator

-- | Parses an SList
-- | Parses an SDottedList