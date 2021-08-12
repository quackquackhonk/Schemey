-- | Main Module for interating with the Schemey interpreter

module Schemey
    ( schemeyPrompt
    ) where

import Schemey.Parsing ( readExpression )
import System.Console.Haskeline
    ( defaultSettings, getInputLine, outputStrLn, runInputT, InputT )

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
