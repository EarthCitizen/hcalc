{-# LANGUAGE BangPatterns #-}

module TestGen (genAddInteger, genSubInteger, genMulInteger, genDivIntegerEvenly)  where

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

emptyL = ("", 0, 0)

primesPlusOne :: [Integer]
!primesPlusOne = initial ++ [ x | x <- [14..40000], isPrime x]
    where initial = [1, 2, 3, 5, 7, 9, 11, 13]
          isPrime x = (not . any ((0 ==) . (rem x))) [2..(x-1)]

isRemZero z x = 0 == rem z x

factorsOf z = let p = filter (isRemZero z) primesPlusOne
                  r = nub . sort $ (*) <$> p <*> p
                 in filter (isRemZero z) r

genInteger :: Gen Integer
genInteger = Gen.integral $ Range.constant (-50000000) 50000000

genAddInteger :: Gen (FlexNum, Expr)
genAddInteger = do
    res <- genInteger
    lo  <- genInteger
    let ro = res - lo
    return (FlexInt res, OperAdd emptyL (LitInt emptyL lo) (LitInt emptyL ro))

genSubInteger :: Gen (FlexNum, Expr)
genSubInteger = do
    res <- genInteger
    lo  <- genInteger
    let ro = res + lo
    return (FlexInt res, OperSub emptyL (LitInt emptyL lo) (LitInt emptyL ro))

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
