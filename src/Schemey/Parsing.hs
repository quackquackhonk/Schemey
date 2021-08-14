-- | Module containing functions to parse Schemey input

module Schemey.Parsing
    ( SValue (..)
    , SNumberVal (..)
    , parseSValue
    ) where

import Text.ParserCombinators.Parsec ( oneOf, many, parse, Parser, skipMany1, (<|>), char, noneOf, many1, newline, tab, alphaNum, octDigit, try, sepBy, endBy, sepEndBy )
import Data.Bool (bool)
import Control.Monad (liftM)
import Text.Parsec.Char
    ( digit, space, letter, endOfLine, hexDigit, anyChar, string, spaces )
import GHC.Show (Show)
import Numeric (readHex, readOct, readFloat)
import GHC.Real ((%))
import Data.Bits (toIntegralSized)
import GHC.Arr (Array, listArray)
import Data.Functor ( (<&>) )

-- | Data Definition for a Scheme Value
data SValue = SNil ()
            | SAtom String
            | SString String
            | SBool Bool
            | SNumber SNumberVal
            | SCharacter Char
            | SList [SValue]
            | SDottedList [SValue] SValue
            | SVector (Array Int SValue)
            deriving (Eq)

data SNumberVal = SNComplex Float Float
                | SNRational Rational
                | SNFloat Float
                | SNInteger Integer
                deriving (Eq)

instance Show SValue where show = showSValue
instance Show SNumberVal where show = showSNumberVal

-- | Helper function for showing Lists
unwordsList :: [SValue] -> String
unwordsList = unwords . map showSValue

-- | Shows the given SValue
showSValue :: SValue -> String
showSValue (SAtom aa) = aa
showSValue (SString ss) = "\"" ++ ss ++ "\""
showSValue (SBool True) = "#t"
showSValue (SBool False) = "#f"
showSValue (SNumber vv) = show vv
showSValue (SCharacter cc) = show cc
showSValue (SList xs) = "(" ++ unwordsList xs ++ ")"
showSValue (SDottedList xs xx) =
    "(" ++ unwordsList xs ++ "." ++ showSValue xx ++ ")"
showSValue (SVector arr) = show arr
showSValue (SNil _) = "()"

-- | Shows the given SNumberVal
showSNumberVal :: SNumberVal -> String
showSNumberVal (SNComplex rr cc) = show rr ++ "+" ++ show cc ++ "i"
showSNumberVal (SNRational xx) = show xx
showSNumberVal (SNInteger xx) = show xx
showSNumberVal (SNFloat xx) = show xx

-- | Parser to ignore whitespace
spaces' :: Parser ()
spaces' = skipMany1 space

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

-- | Main Parser for SValues
-- delegates to other parsers when they are complex i.e. the number parser
parseSValue :: Parser SValue
parseSValue = parseAtom
            <|> parseString
            <|> try parseBool
            <|> try parseCharacter
            <|> try parseNumber
            <|> parseList

-- | Parses an SAtom 
-- Will parse 'true' and 'false' as boolean literals
-- all other symbols will be parsed as atoms
parseAtom :: Parser SValue
parseAtom = do
    first <- letter <|> symbol
    rest <- many $ letter <|> digit <|> symbol
    return $ case first : rest of
       "nil" -> SNil ()
       _ -> SAtom (first : rest)

-- | Parses an SString
parseString :: Parser SValue
parseString = do
    char '"'
    -- non quote character <|> \" <|> \'
    s <- many $ escapeChar <|> noneOf "\"\\"
    char '"'
    return $ SString s

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

-- | Parser to handle all types of number formats
parseNumber :: Parser SValue
parseNumber = try parseComplex
            <|> try parseFloat
            <|> try parseRational
            <|> try parseInteger

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
-- parseDecimalInteger = many1  digit >>= return . SInteger .  read
parseDecimalInteger = many1 digit <&> SNumber . SNInteger .  read

-- | Parses decimal numbers into SNumbers
-- requires the #d prefix
parseDecimalIntegerStrict :: Parser SValue
parseDecimalIntegerStrict = do
    try $ string "#d" <|> string "#D"
    x <- many1 digit
    return $ SNumber . SNInteger . read $ x

-- | Parses hexadecimal numbers into SNumbers
parseHexadecimalInteger :: Parser SValue
parseHexadecimalInteger = do
    try $ string "#x" <|> string "#X"
    x <- many1 hexDigit
    return $ SNumber . SNInteger $ extract x
  where
    extract x = fst $ head $ readHex x

-- | Parses octal numbers into SNumber
parseOctalInteger :: Parser SValue
parseOctalInteger = do
    try $ string "#o" <|> string "#O"
    x <- many1 octDigit
    return $ SNumber . SNInteger $ (extract x)
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
    return $ SNumber . SNInteger $ (bin2num x 0)

-- | Parses an SComplex number
parseComplex :: Parser SValue
parseComplex = do
    real <- try $ parseDecimalInteger <|> parseFloat
    char '+'
    complex <- try $ parseDecimalInteger <|> parseFloat
    char 'i'
    return $ SNumber $ SNComplex (toDouble real) (toDouble complex)
  where
      toDouble (SNumber (SNInteger x)) = fromIntegral x
      toDouble (SNumber (SNFloat x)) = realToFrac x
      toDouble _ = -1

-- | Parses an SReal (Floating point)
parseFloat :: Parser SValue
parseFloat = do
    a <- many1 digit
    char '.'
    b <- many1 digit
    return $ SNumber . SNFloat$ fst . head $ readFloat (a++"."++b)

-- | Parses an SRational number
parseRational :: Parser SValue
parseRational = do
    numerator <- many1 digit
    char '/'
    denominator <- many1 digit
    return $ SNumber . SNRational $ read numerator % read denominator


-- | Parses either an SList, or an SDottedList
parseList :: Parser SValue
parseList = parseVector
            <|> parseQuotedList
            <|> parseQuasiquoteList
            <|> try parseUnquoteSplicingList
            <|> parseBackquoteList
            <|> parseAnySList

-- Parses either an SList or an SDottedList
parseAnySList :: Parser SValue
parseAnySList = do
    char '(' >> spaces
    head <- sepEndBy parseSValue spaces'
    tail <- (char '.' >> spaces >> parseSValue) <|> return (SNil ())
    spaces >> char ')'
    return $ case tail of
        SNil () -> SList head
        _ -> SDottedList head tail


-- | Parses an SList
-- TODO: needs to be updated for all expressions, not just values
parseStandardList :: Parser SValue
parseStandardList = SList <$> sepBy parseSValue spaces'

-- | Parses an SDottedList
-- TODO: needs to be updated for all expressions, not just values
parseDottedList :: Parser SValue
parseDottedList = do
    head <- endBy parseSValue spaces'
    tail <- char '.' >> spaces' >> parseSValue
    return $ SDottedList head tail

-- | Parses a Quoted list into an SList
parseQuotedList :: Parser SValue
parseQuotedList = do
    char '\''
    x <- parseSValue
    return $ SList [SAtom "quote", x]


-- | Parses an SList constructed with a quasiquote
parseQuasiquoteList :: Parser SValue
parseQuasiquoteList = do
    char '`'
    x <- parseSValue
    return $ SList [SAtom "quasiquote", x]

-- | Parses an SList constructed with a backquote
parseBackquoteList :: Parser SValue
parseBackquoteList = do
    char ','
    x <- parseSValue
    return $ SList [SAtom "backquote", x]

-- | Parses an SList constructed using unquote splicing
parseUnquoteSplicingList :: Parser SValue
parseUnquoteSplicingList = do
    string ",@"
    x <- parseSValue
    return $ SList [SAtom "unquote-splicing", x]

-- | Parses an SVector
-- lists of the form #(<contents>) are parsed as vectors
parseVector :: Parser SValue
parseVector = do
    string "#("
    xs <- sepBy parseSValue spaces'
    char ')'
    return $ SVector $ listArray (0, length xs - 1) xs