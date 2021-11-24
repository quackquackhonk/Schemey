-- | Main Module for interating with the Schemey interpreter

module Schemey
    ( schemeyPrompt
    ) where

import Control.Monad.Except (catchError, throwError)
import Schemey.Parsing ( parseSValue )
import Schemey.Grammar (SValue)
import Schemey.Evaluation ( evalSValue )
import Schemey.Error (SError(..), ThrowsSError, extractValue, trapError)
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
                let evaledInput = fmap show $ readExpression input >>= evalSValue
                outputStrLn $ extractValue $ trapError evaledInput
                loop

-- | Parses user input into an SValue, evaluates the input, and
-- prints the evaluation
readExpression :: String -> ThrowsSError SValue
readExpression input = case parse parseSValue "scheme-y" input of
    Left  err -> throwError $ ParserError err
    Right val -> return val