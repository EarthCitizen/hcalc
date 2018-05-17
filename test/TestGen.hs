{-# LANGUAGE BangPatterns #-}

module TestGen (genAddInteger, genAddFloat, genSubInteger, genSubFloat, genMulInteger, genDivIntegerEvenly)  where

import Alias
import AST
import Control.Monad (forM)
import Data.List (nub, sort)
import qualified Data.Map.Strict as M
import qualified Eval as E
import FlexNum
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import TestData
-- import Data.Number.BigFloat (BigFloat)
-- import Data.Number.Fixed (Prec50, Prec500)
import Data.Scientific (fromFloatDigits, toRealFloat)

-- type Float500 = BigFloat Prec500

emptyL = ("", 0, 0)

isRemZero z x = 0 == rem z x

factorsOf z = let p = filter (isRemZero z) likelyFactors
                  r = nub . sort $ (*) <$> p <*> p
                 in filter (isRemZero z) r

genInteger :: Gen Integer
genInteger = Gen.integral $ Range.constant (-50000000) 50000000

genFloat :: Gen Double
genFloat = Gen.realFloat $ Range.constant (-50000000) 50000000

genAddInteger :: Gen (FlexNum, Expr)
genAddInteger = do
    res <- genInteger
    lo  <- genInteger
    let ro = res - lo
    return (FlexInt res, OperAdd emptyL (LitInt emptyL lo) (LitInt emptyL ro))

genAddFloat :: Gen (FlexNum, Expr)
genAddFloat = do
    resf <- genFloat
    lof  <- genFloat
    let los  = fromFloatDigits lof
        ress = fromFloatDigits resf
        ros  = ress - los
        rof  = toRealFloat ros
    return (FlexFloat resf, OperAdd emptyL (LitFloat emptyL lof) (LitFloat emptyL rof))

genSubInteger :: Gen (FlexNum, Expr)
genSubInteger = do
    res <- genInteger
    ro  <- genInteger
    let lo = res + ro
    return (FlexInt res, OperSub emptyL (LitInt emptyL lo) (LitInt emptyL ro))

genSubFloat :: Gen (FlexNum, Expr)
genSubFloat = do
    resf <- genFloat
    rof  <- genFloat
    let ros  = fromFloatDigits rof
        ress = fromFloatDigits resf
        los  = ress + ros
        lof  = toRealFloat los
    return (FlexFloat resf, OperSub emptyL (LitFloat emptyL lof) (LitFloat emptyL rof))

genMulInteger :: Gen (FlexNum, Expr)
genMulInteger = do
    res <- genInteger
    s   <- Gen.element [(-1), 1]
    rf  <- Gen.element $ factorsOf res
    let lo = s * rf
        ro = div res lo
    return (FlexInt res, OperMul emptyL (LitInt emptyL lo) (LitInt emptyL ro))

genDivIntegerEvenly :: Gen (FlexNum, Expr)
genDivIntegerEvenly = do
    res <- genInteger
    ro  <- genInteger
    let lo = res * ro
    return (FlexInt res, OperDiv emptyL (LitInt emptyL lo) (LitInt emptyL ro))

genX :: Int -> Gen a -> Gen [a]
genX s g = forM [1..s] (\_ -> g)
