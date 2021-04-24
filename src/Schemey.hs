-- | Main Module for interating with the Schemey interpreter

module Schemey
    ( schemeyPrompt
    ) where

import Schemey.Parsing
import System.Console.Haskeline

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
                outputStrLn $ readExpression input
                loop
