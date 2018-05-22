{-# LANGUAGE BangPatterns #-}

module TestGen ( genAddInteger
               , genAddFloat
               -- , genExpr
               , genFlexNum
               , genFloat
               , genFnName
               , genInteger
               , genIntegerBetween
               , genIntegerWhere
               , genSubInteger
               , genSubFloat
               , genMulInteger
               , genDivIntegerEvenly
               , genN
               ) where

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
genInteger = genIntegerBetween (-50000000) 50000000

genIntegerBetween :: Integer -> Integer -> Gen Integer
genIntegerBetween x y = Gen.integral $ Range.constant x y

genIntegerWhere :: (Integer -> Bool) -> Gen Integer
genIntegerWhere f = Gen.filter f genInteger

genFloat :: Gen Double
genFloat = genFloatBetween (-50000000) 50000000

genFloatBetween :: Double -> Double -> Gen Double
genFloatBetween x y = Gen.realFloat $ Range.constant x y

genFloatWhere :: (Double -> Bool) -> Gen Double
genFloatWhere f = Gen.filter f genFloat

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

genN :: Int -> Gen a -> Gen [a]
genN s g = forM [1..s] (\_ -> g)

genFnName :: Gen String
genFnName = do
    c1 <- Gen.alpha
    cx <- Gen.list (Range.constant 0 50) Gen.alphaNum
    return (c1:cx)

genFlexNum :: Gen FlexNum
genFlexNum = Gen.choice [genFlexInt, genFlexInt]

genFlexFloat :: Gen FlexNum
genFlexFloat = FlexFloat <$> genFloat

genFlexInt :: Gen FlexNum
genFlexInt = FlexInt <$> genInteger

type Depth = Size

genExprFor :: FlexNum -> Depth -> Gen Expr
genExprFor fn depth | depth < 0 = genExprLitFor fn
                    | otherwise = Gen.sized go
    where nextDepth = depth + 1
          go :: Size -> Gen Expr
          go sz = if depth >= sz
                  then genExprLitFor fn
                  else Gen.choice [ genExprLitFor fn
                                  , genExprAddFor fn nextDepth
                                  ]

genExprLitFor :: FlexNum -> Gen Expr
genExprLitFor (FlexFloat f) = return (LitFloat emptyL f)
genExprLitFor (FlexInt i)   = return (LitInt emptyL i)

genExprAddFor :: FlexNum -> Depth -> Gen Expr
genExprAddFor (FlexFloat f) depth = do
    lof  <- genFloat
    let los  = fromFloatDigits lof
        ress = fromFloatDigits f
        ros  = ress - los
        rof  = toRealFloat ros
    le <- Gen.choice [ return (LitFloat emptyL lof)
                     , genExprFor (FlexFloat lof) depth
                     ]
    re <- Gen.choice [ return (LitFloat emptyL rof)
                     , genExprFor (FlexFloat rof) depth
                     ]
    return (OperAdd emptyL le re)
genExprAddFor (FlexInt i) depth = do
    lo  <- genInteger
    let ro = i - lo
    le <- Gen.choice [ return (LitInt emptyL lo)
                     , genExprFor (FlexInt lo) depth
                     ]
    re <- Gen.choice [ return (LitInt emptyL ro)
                     , genExprFor (FlexInt ro) depth
                     ]
    return (OperAdd emptyL le re)
