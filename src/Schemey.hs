-- | Main Module for interating with the Schemey interpreter

module Schemey
    ( schemeyPrompt
    ) where

import Schemey.Parsing ( parseSValue )
import Schemey.Evaluation ( evalSValue )
import System.Console.Haskeline
    ( defaultSettings, getInputLine, outputStrLn, runInputT, InputT )
import Text.ParserCombinators.Parsec (parse)

-- | IO Loop for the Schemey REPL
schemeyPrompt :: IO ()
schemeyPrompt = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
        minput <- getInputLine "Schemey> "
        case minput of
            Nothing     -> return ()
            Just "quit" -> return ()
            Just input  -> do
                outputStrLn $ processInput input
                loop

-- | Parses user input into an SValue, evaluates the input, and
-- prints the evaluation
processInput :: String -> String
processInput input = case parse parseSValue "scheme-y" input of
    Left  err -> "No match: " ++ show err
    Right val   -> show . evalSValue $ val