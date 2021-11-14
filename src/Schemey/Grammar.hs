module Schemey.Grammar
    ( SValue(..)
    , SNumberVal(..)
    , unwordsList
    ) where

import GHC.Arr (Array, listArray)
import Data.Complex (Complex(..), realPart, imagPart)
import Numeric (showIntAtBase)

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

instance Show SValue where show = showSValue

data SNumberVal = SNComplex (Complex Float)
                | SNRational Rational
                | SNFloat Float
                | SNInteger Integer
                deriving (Eq)

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
showSNumberVal (SNComplex xx) = show xx
showSNumberVal (SNRational xx) = show xx
showSNumberVal (SNInteger xx) = show xx
showSNumberVal (SNFloat xx) = show xx

instance Ord SNumberVal where
    -- compare for SNumberVals
    -- same rules for different types
    -- complex numbers only compare the real part
    compare (SNComplex xx) (SNComplex yy) = compare (realPart xx) (realPart yy)
    compare (SNComplex xx) (SNFloat yy) = compare (realPart xx) $ realPart (yy :+ 0)
    compare (SNComplex xx) (SNRational yy) = compare (realPart xx) $ realPart (realToFrac yy :+ 0)
    compare (SNComplex xx) (SNInteger yy) = compare (realPart xx) $ realPart (realToFrac yy :+ 0)
    compare (SNFloat xx) (SNComplex yy) = compare (realPart (realToFrac xx :+ 0)) $ realPart yy
    compare (SNFloat xx) (SNFloat yy) = compare xx yy
    compare (SNFloat xx) (SNRational yy) = compare xx $ realToFrac yy
    compare (SNFloat xx) (SNInteger yy) = compare xx $ realToFrac yy
    compare (SNRational xx) (SNComplex yy) = compare (realPart (realToFrac xx :+ 0)) $ realPart yy
    compare (SNRational xx) (SNFloat yy) = compare (realToFrac xx) yy
    compare (SNRational xx) (SNRational yy) = compare xx yy
    compare (SNRational xx) (SNInteger yy) = compare xx $ toRational yy
    compare (SNInteger xx) (SNComplex yy) = compare (realPart (realToFrac xx :+ 0)) $ realPart yy
    compare (SNInteger xx) (SNFloat yy) = compare (realToFrac xx) yy
    compare (SNInteger xx) (SNRational yy) = compare (toRational xx) yy
    compare (SNInteger xx) (SNInteger yy) = compare xx yy

instance Num SNumberVal where
    -- Binary addition
    -- If the given numbers are the same type, preserves type
    -- If the types are different, uses the number with the type
    -- higher on the number tower:
    -- Number
    --    Complex
    --    Real (Float)
    --    Rational
    --    Integer
    -- So, adding an Integer to a Float will result in a Float
    SNComplex xx + SNComplex yy = SNComplex $ xx + yy
    SNComplex xx + SNFloat yy = SNComplex $ (realPart xx + yy) :+ imagPart xx
    SNComplex xx + SNRational yy = SNComplex $ (realPart xx + realToFrac yy) :+ imagPart xx
    SNComplex xx + SNInteger yy = SNComplex $ (realPart xx + realToFrac yy) :+ imagPart xx
    SNFloat xx + SNComplex yy = SNComplex $ (realPart yy + realToFrac xx) :+ imagPart yy
    SNFloat xx + SNFloat yy = SNFloat $ xx + yy
    SNFloat xx + SNRational yy = SNFloat $ xx + realToFrac yy
    SNFloat xx + SNInteger yy = SNFloat $ xx + realToFrac yy
    SNRational xx + SNComplex yy = SNComplex $ (realPart yy + realToFrac xx) :+ imagPart yy
    SNRational xx + SNFloat yy = SNFloat $ realToFrac xx + yy
    SNRational xx + SNRational yy = SNRational $ xx + yy
    SNRational xx + SNInteger yy = SNRational $ xx + fromInteger yy
    SNInteger xx + SNComplex yy = SNComplex $ (realToFrac xx + realPart yy) :+ imagPart yy
    SNInteger xx + SNFloat yy = SNFloat $ fromInteger xx + yy
    SNInteger xx + SNRational yy = SNRational $ fromInteger xx + yy
    SNInteger xx + SNInteger yy = SNInteger $ xx + yy
    -- Unary negation
    negate (SNComplex xx) = SNComplex . negate $ xx
    negate (SNFloat xx) = SNFloat . negate $ xx
    negate (SNRational xx) = SNRational . negate $ xx
    negate (SNInteger xx) = SNInteger . negate $ xx
    -- Binary subtraction
    xx - yy = xx + negate yy
    -- Binary Multiplication
    -- works the same with mismatching types as binary addition
    SNComplex xx * SNComplex yy = SNComplex $ xx * yy
    SNComplex xx * SNFloat yy =
        SNComplex $ realPart xx * yy :+ imagPart xx * yy
    SNComplex xx * SNRational yy =
        SNComplex $ realPart xx * realToFrac yy :+ imagPart xx * realToFrac yy
    SNComplex xx * SNInteger yy =
        SNComplex $ realPart xx * realToFrac yy :+ imagPart xx * realToFrac yy
    SNFloat xx * SNComplex yy =
        SNComplex $ realPart yy * realToFrac xx :+ imagPart yy * xx
    SNFloat xx * SNFloat yy = SNFloat $ xx * yy
    SNFloat xx * SNRational yy = SNFloat $ xx * realToFrac yy
    SNFloat xx * SNInteger yy = SNFloat $ xx * realToFrac yy
    SNRational xx * SNComplex yy =
        SNComplex $ realPart yy * realToFrac xx :+ imagPart yy * realToFrac xx
    SNRational xx * SNFloat yy = SNFloat $ realToFrac xx * yy
    SNRational xx * SNRational yy = SNRational $ xx * yy
    SNRational xx * SNInteger yy = SNRational $ xx * fromInteger yy
    SNInteger xx * SNComplex yy =
        SNComplex $ realToFrac xx * realPart yy :+ imagPart yy * realToFrac xx
    SNInteger xx * SNFloat yy = SNFloat $ fromInteger xx * yy
    SNInteger xx * SNRational yy = SNRational $ fromInteger xx * yy
    SNInteger xx * SNInteger yy = SNInteger $ xx * yy
    -- Absolute values
    abs (SNComplex xx) = SNComplex . abs $ xx
    abs (SNFloat xx) = SNFloat . abs $ xx
    abs (SNRational xx) = SNRational . abs $  xx
    abs (SNInteger xx) = SNInteger . abs $ xx
    -- Signum
    signum (SNComplex xx) = SNComplex . signum $ xx
    signum (SNFloat xx) = SNFloat . signum $ xx
    signum (SNRational xx) = SNRational . signum $ xx
    signum (SNInteger xx) = SNInteger . signum $ xx
    -- fromInteger
    fromInteger = SNInteger

instance Fractional SNumberVal where
    -- fromRational
    fromRational xx = SNRational xx
    -- reciprocal
    recip (SNComplex xx) = SNComplex . recip $ xx
    recip (SNFloat xx) = SNFloat . recip $ xx
    recip (SNRational xx) = SNRational . recip $ xx
    recip (SNInteger xx) = SNRational . recip . fromInteger $ xx

