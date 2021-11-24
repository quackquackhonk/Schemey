-- | Module to handle the evaluation of Schemey expressions
module Schemey.Evaluation
    ( evalSValue
    ) where

import Schemey.Grammar (SValue (..), SNumberVal (..))
import Schemey.Error (ThrowsSError, SError(..))
import Control.Monad.Except (throwError)
import Data.Complex (Complex(..))

-- | Evaluates the given Schemey Expression
evalSValue :: SValue -> ThrowsSError SValue
evalSValue xx@(SNil _) = return xx
evalSValue xx@(SAtom _) = return xx
evalSValue xx@(SString _) = return  xx
evalSValue xx@(SBool _) = return xx
evalSValue xx@(SNumber _) = return xx
evalSValue xx@(SCharacter _) = return xx
evalSValue (SList [SAtom "quote", xs]) = return xs
evalSValue (SList (SAtom func : args)) = mapM evalSValue args >>= applySFunc func
evalSValue (SList xs) = throwError $ MiscellaneousError "List Evaluation isn't implemented yet"
evalSValue (SDottedList xs xx) = throwError $ MiscellaneousError "D. List Evaluation isn't implemented yet"
evalSValue (SVector arr) = throwError $ MiscellaneousError "Vector Evaluation isn't implemented yet"

-- | Applies the given Schemey function with the given list of arguments
applySFunc :: String -> [SValue] -> ThrowsSError SValue
applySFunc ff args = maybe (throwError $ NotFunctionError "Primitive Function Args Not Recognized" ff)
                           ($ args)
                           (lookup ff schemeyPrimitives)

-- | Lookup list for primative schemey operations
schemeyPrimitives :: [(String, [SValue] -> ThrowsSError SValue)]
schemeyPrimitives = [ ("+", binaryNumberFunc (+))
                    , ("-", binaryNumberFunc (-))
                    , ("*", binaryNumberFunc (*))
                    , ("/", binaryNumberFunc (/))
                    , ("mod", binaryNumberFunc mod)
                    , ("quotient", binaryNumberFunc quot)
                    , ("rem", binaryNumberFunc rem)
                    -- Boolean functions
                    , ("not", unaryFunc sbNot)
                    -- Type-checking functions
                    , ("nil?", unaryFunc svNilP)
                    , ("atom?", unaryFunc svAtomP)
                    , ("boolean?", unaryFunc svBooleanP)
                    , ("string?", unaryFunc svStringP)
                    , ("number?", unaryFunc svNumberP)
                    , ("character?", unaryFunc svCharacterP)
                    , ("list?", unaryFunc svListP)
                    , ("vector?", unaryFunc svVectorP)
                    -- Symbol processing functinos
                    , ("symbol->string", unaryFunc sAtomToString)
                    , ("string->symbol", unaryFunc sStringToAtom)
                    -- , ("", )
                    ]

-- | Abstraction over the behavior of binary number functions
-- binaryNumberFunc :: Num a => (a -> a -> a) -> [SValue] -> ThrowsSError SValue
binaryNumberFunc :: (SNumberVal -> SNumberVal -> SNumberVal) -> [SValue] -> ThrowsSError SValue
binaryNumberFunc op [] = throwError $ NumArgsError 2 []
binaryNumberFunc op xx@[_] = throwError $ NumArgsError 2 xx
binaryNumberFunc op args@(xx:_) = mapM unpackNum args >>= return . SNumber . foldl1 op

-- snAdd :: SNumberVal -> SNumberVal -> SNumberVal 
-- snAdd xx yy = extract xx + extract yy
--   where
--     extract (SNFloat ff) = ff


-- unpackNum :: Num a => SValue -> ThrowsSError a
-- unpackNum (SNumber snv) = return $ extractSNV snv
--   where
--     extractSNV :: Num a => SNumberVal -> a
--     extractSNV (SNFloat ff) = ff
--     extractSNV (SNRational rr) = rr
--     extractSNV (SNInteger ii) = ii
--     extractSNV (SNComplex com@(rr :+ ii)) = rr :+ ii
unpackNum :: SValue -> ThrowsSError SNumberVal
unpackNum (SNumber snv) = return snv
unpackNum notNum = throwError $ TypeMismatchError "SNumber" notNum


-- | Abstraction over the behavior of unary boolean functions
unaryFunc :: (SValue -> SValue) -> [SValue] -> ThrowsSError SValue
unaryFunc func [] = throwError $ NumArgsError 1 []
unaryFunc func [arg] = return $ func arg
unaryFunc func twoPlusArgs@(x1:x2:_) = throwError $ NumArgsError 1 twoPlusArgs

-- | Boolean negation for SValues
-- all SValues are treated as equivalent to True, except for False
sbNot :: SValue -> SValue 
sbNot (SBool False) = SBool True 
sbNot _ = SBool False

-- | Unary type-checking functions
svNilP, svAtomP, svBooleanP, svStringP, svNumberP, svCharacterP, svListP, svVectorP :: SValue -> SValue
svNilP (SNil _) = SBool True
svNilP _ = SBool False
svAtomP (SAtom _) = SBool True
svAtomP _ = SBool False
svBooleanP (SBool _) = SBool True
svBooleanP _ = SBool False
svStringP (SString _) = SBool True
svStringP _ = SBool False
svNumberP (SNumber _) = SBool True
svNumberP _ = SBool False
svCharacterP (SCharacter _) = SBool True
svCharacterP _ = SBool False
svListP (SList _) = SBool True
svListP (SDottedList _ _) = SBool True
svListP _ = SBool False
svVectorP (SVector _) = SBool True
svVectorP _ = SBool False

-- | Converts an SAtom to an SString
sAtomToString :: SValue -> SValue 
sAtomToString (SAtom name) = SString name
sAtomToString _ = error "Expected an Atom"

-- | Converts an SString to an SAtom
sStringToAtom :: SValue -> SValue 
sStringToAtom (SString name) = SAtom name
sStringToAtom _ = error "Expected a String"