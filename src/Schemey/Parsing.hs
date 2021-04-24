-- | Module containing functions to parse Schemey input

module Schemey.Parsing
    ( readExpression
    ) where

import           System.Environment
import           Text.ParserCombinators.Parsec

-- | Reads a String of Schemey input and returns an output
readExpression :: String -> String
readExpression input = case parse parseSymbol "lisp" input of
    Left  err -> "No match: " ++ show err
    Right _   -> "Found Value"

-- | Parses symbols
parseSymbol :: Parser Char
parseSymbol = oneOf "~#$%&|*+_/:<=>?@_~"
