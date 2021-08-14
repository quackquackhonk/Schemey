-- | Module to handle the evaluation of Schemey expressions
module Schemey.Evaluation
    ( evalSValue
    ) where

import Schemey.Parsing (SValue (..), SNumberVal (SNComplex, SNFloat, SNRational, SNInteger))

-- | Evaluates the given Schemey Expression
evalSValue :: SValue -> SValue
evalSValue xx@(SNil _) = xx
evalSValue xx@(SAtom _) = xx
evalSValue xx@(SString _) = xx
evalSValue xx@(SBool _) = xx
evalSValue xx@(SNumber _) = xx
evalSValue xx@(SCharacter _) = xx
evalSValue (SList [SAtom "quote", xs]) = xs
evalSValue (SList (SAtom func : args)) = applySFunc func $ map evalSValue args
evalSValue (SList xs) = SNil ()
evalSValue (SDottedList xs xx) = SNil ()
evalSValue (SVector arr) = SNil ()

-- | Applies the given Schemey function with the given list of arguments
applySFunc :: String -> [SValue] -> SValue
applySFunc ff args = maybe (SNil ()) ($ args) $ lookup ff schemeyPrimitives

-- | Lookup list for primative schemey operations
schemeyPrimitives :: [(String, [SValue] -> SValue)]
schemeyPrimitives = [ ("+", binaryNumberFunc snAdd)
                    , ("-", binaryNumberFunc snSub)
                    , ("*", binaryNumberFunc snMult)
                    , ("/", binaryNumberFunc snDiv)
                    , ("mod", binaryNumberFunc snMod)
                    , ("quotient", binaryNumberFunc snQuot)
                    , ("rem", binaryNumberFunc snRem)
                    , ("not", unaryFunc sbNot)
                    , ("nil?", unaryFunc svNilP)
                    , ("atom?", unaryFunc svAtomP)
                    , ("boolean?", unaryFunc svBooleanP)
                    , ("string?", unaryFunc svStringP)
                    , ("number?", unaryFunc svNumberP)
                    , ("character?", unaryFunc svCharacterP)
                    , ("list?", unaryFunc svListP)
                    , ("dottedlist?", unaryFunc svDotListP)
                    , ("vector?", unaryFunc svVectorP)
                    -- , ("", )
                    ]

-- | Abstraction over the behavior of binary number functions
binaryNumberFunc :: (SNumberVal -> SNumberVal -> SNumberVal) -> [SValue] -> SValue
binaryNumberFunc op args = SNumber $ foldl1 op $ map unpackNum args

unpackNum :: SValue -> SNumberVal 
unpackNum (SNumber snv) = snv
unpackNum _ = SNInteger 0

-- | Binary add operation for SNumberVals
snAdd :: SNumberVal -> SNumberVal -> SNumberVal
snAdd (SNComplex xr xc) (SNComplex yr yc) = SNComplex (xr + yr) (xc + yc)
snAdd (SNFloat xx) (SNFloat yy) = SNFloat $ xx + yy
snAdd (SNRational xx) (SNRational yy) = SNRational $ xx + yy
snAdd (SNInteger xx) (SNInteger yy) = SNInteger $ xx + yy
snAdd _ _ = error "type mismatch when adding"

-- | Binary subtraction operation for SNumberVals
snSub :: SNumberVal -> SNumberVal -> SNumberVal
snSub (SNComplex xr xc) (SNComplex yr yc) = SNComplex (xr - yr) (xc - yc)
snSub (SNFloat xx) (SNFloat yy) = SNFloat $ xx - yy
snSub (SNRational xx) (SNRational yy) = SNRational $ xx - yy
snSub (SNInteger xx) (SNInteger yy) = SNInteger $ xx - yy
snSub _ _ = error "type mismatch when subtracting"

-- | Binary multiplication for SNumberVals
snMult :: SNumberVal -> SNumberVal -> SNumberVal
snMult (SNComplex xr xc) (SNComplex yr yc) =
    SNComplex (xr * yr - xc * yc) (xr * yc + xc * yr)
snMult (SNFloat xx) (SNFloat yy) = SNFloat $ xx * yy
snMult (SNRational xx) (SNRational yy) = SNRational $ xx * yy
snMult (SNInteger xx) (SNInteger yy) = SNInteger $ xx * yy
snMult _ _ = error "type mismatch when multiplying"

-- | Binary Division for SNumberVals
snDiv :: SNumberVal -> SNumberVal -> SNumberVal
snDiv xx yy@(SNComplex yr yc)
    | (yr == 0) && (yc == 0) = error "division by 0"
    | otherwise = SNComplex (numR / denom) (numC / denom)
  where
      yrecip@(SNComplex yr' yc') = SNComplex yr (negate yc)
      SNComplex numR numC = snMult xx yrecip
      -- we can ignore the complex part of the denom since
      -- yy * yrecip is a difference of squares, and will
      -- never have a complex part
      SNComplex denom _ = snMult yy yrecip
snDiv (SNFloat xx) (SNFloat yy)
    | yy == 0.0 = error "division by 0"
    | otherwise = SNFloat $ xx  / yy
snDiv (SNRational xx) (SNRational yy)
    | yy == 0 = error "division by 0"
    | otherwise = SNRational $ xx  / yy
snDiv (SNInteger xx) (SNInteger yy)
    | yy == 0 = error "division by 0"
    | otherwise = SNInteger $ div xx yy
snDiv _ _ = error "type mismatch when multiplyting"

-- | Modulo operator for SNumberVals
snMod :: SNumberVal -> SNumberVal -> SNumberVal 
snMod _ cc@(SNComplex _ _) = error "modulo is not implemented for complex numbers"
snMod cc@(SNComplex _ _) _ = error "modulo is not implemented for complex numbers"
snMod _ (SNFloat yy) = error "modulo is not implemented for floats"
snMod (SNFloat xx) _ = error "modulo is not implemented for floats"
snMod _ (SNRational yy) = error "modulo is not implemented for rationals"
snMod (SNRational xx) _ = error "modulo is not implemented for rationals"
snMod (SNInteger xx) (SNInteger yy) = SNInteger $ mod xx yy

-- | Quotient operator for SNumberVals
snQuot :: SNumberVal -> SNumberVal -> SNumberVal 
snQuot _ cc@(SNComplex _ _) = error "quotient is not implemented for complex numbers"
snQuot cc@(SNComplex _ _) _ = error "quotient is not implemented for complex numbers"
snQuot (SNFloat xx) _ = error "quotient is not implemented for floats"
snQuot _ (SNFloat yy) = error "quotient is not implemented for floats"
snQuot (SNRational xx) _ = error "quotient is not implemented for rationals"
snQuot _ (SNRational yy) = error "quotient is not implemented for rationals"
snQuot (SNInteger xx) (SNInteger yy) = SNInteger $ quot xx yy

-- | Remainder operator for SNumberVals
snRem :: SNumberVal -> SNumberVal -> SNumberVal 
snRem _ cc@(SNComplex _ _) = error "remainder is not implemented for complex numbers"
snRem cc@(SNComplex _ _) _ = error "remainder is not implemented for complex numbers"
snRem (SNFloat xx) _ = error "rem is not implementec for floats"
snRem _ (SNFloat yy) = error "rem is not implementec for floats"
snRem (SNRational xx) _ = error "rem is not implemented for rationals"
snRem _ (SNRational yy) = error "rem is not implemented for rationals"
snRem (SNInteger xx) (SNInteger yy) = SNInteger $ rem xx yy

-- | Abstraction over the behavior of unary boolean functions
unaryFunc :: (SValue -> SValue) -> [SValue] -> SValue 
unaryFunc func [arg] = func arg
unaryFunc _ _ = SNil ()

-- | Boolean negation for SValues
-- all SValues are treated as equivalent to True, except for False
sbNot :: SValue -> SValue 
sbNot (SBool False) = SBool True 
sbNot _ = SBool False

-- | Unary type-checking functions
svNilP, svAtomP, svBooleanP, svStringP, svNumberP, svCharacterP, svListP, svDotListP, svVectorP :: SValue -> SValue
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
svListP _ = SBool False
svDotListP (SDottedList _ _) = SBool True
svDotListP _ = SBool False
svVectorP (SVector _) = SBool True
svVectorP _ = SBool False