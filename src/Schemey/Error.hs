module Schemey.Error
    ( SError(..)
    ,ThrowsSError 
    , extractValue
    , trapError
    ) where

import Control.Monad.Except (catchError, throwError)
import Schemey.Grammar (SValue(..), unwordsList)
import Text.ParserCombinators.Parsec (ParseError)

-- | Data Definition for a Scheme Error
data SError = NumArgsError Integer [SValue]
            | TypeMismatchError String SValue
            | ParserError ParseError
            | BadSpecialFormError String SValue
            | NotFunctionError String String 
            | UnboundVarError String String 
            | MiscellaneousError String

-- alias for functions that have the potential
-- to throw errors
type ThrowsSError = Either SError

-- | Function for showing SErrors
showSError :: SError -> String
showSError (UnboundVarError msg var) = msg ++ ": " ++ var
showSError (BadSpecialFormError msg form) = msg ++ ": " ++ show form
showSError (NotFunctionError msg func) = msg ++ ": " ++ func
showSError (NumArgsError expect actual) =
    "Invalid number of arguments: Expected " ++ show expect
    ++ "arguments; Found " ++ show (length actual)
    ++ " arguments in " ++ unwordsList actual
showSError (TypeMismatchError expect actual) = 
    "Invalid Type: Expected " ++  expect ++ ", found " ++ show actual
showSError (ParserError pe) = "Parse error at " ++ show pe
showSError (MiscellaneousError msg) = msg

instance Show SError where show = showSError

-- | Function to quickly convert SErrors to Strings
trapError action = catchError action $ return . show

-- | Function to grab the value of a successful computation
-- No need to define the (Left _) case for this, (Left _)
-- is only going to happen via user error
-- (will only be used after catchError)
extractValue :: ThrowsSError a -> a
extractValue (Right vv) = vv