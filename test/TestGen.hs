{-# LANGUAGE BangPatterns #-}

module TestGen ( genAddInteger
               , genAddFloat
               , genFlexNum
               , genFlexNumExpr
               , genFloat
               , genFloatBetween
               , genFloatWhere
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

import Debug.Trace (trace, traceM)

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
import Data.Scientific (fromFloatDigits, toRealFloat)

emptyL = ("", 0, 0)

isRemZero z x = 0 == rem z x

factorsOf z = let p = filter (isRemZero z) likelyFactors
                  r = nub . sort $ (*) <$> p <*> p
                 in filter (isRemZero z) r

gtltZero = (\x -> x > 0 || x < 0)

genInteger :: Gen Integer
genInteger = genIntegerBetween (-50000000) 50000000

genIntegerBetween :: Integer -> Integer -> Gen Integer
genIntegerBetween x y = Gen.integral $ Range.constant x y

genIntegerWhere :: (Integer -> Bool) -> Gen Integer
genIntegerWhere f = Gen.filter f genInteger

genFloat :: Gen Float50
genFloat = Gen.choice [ genFloatBetween (-5) 5
                      , genFloatBetween (-3000000) (-100)
                      , genFloatBetween 3000000 100
                      , genFloatBetween (-300) (-900)
                      , genFloatBetween 300 900
                      , genFloatBetween (-0.005) 0.005
                      , genFloatBetween (-0.000005) 0.000005
                      ]

genFloatBetween :: Float50 -> Float50 -> Gen Float50
genFloatBetween x y = Gen.realFrac_ $ Range.constant x y

genFloatWhere :: (Float50 -> Bool) -> Gen Float50
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

genFlexNumExpr :: Gen (FlexNum, Expr)
genFlexNumExpr = Gen.choice [ genFlexFloatExpr, genFlexIntExpr ]

genFlexFloatExpr :: Gen (FlexNum, Expr)
genFlexFloatExpr = do
    f <- genFloatWhere (/=0)
    e <- genExprFor (FlexFloat f) 0
    -- traceM ("genFlexFloatExpr: " ++ show f ++ "\n")
    return (FlexFloat f, e)

genFlexIntExpr :: Gen (FlexNum, Expr)
genFlexIntExpr = do
    i <- genIntegerWhere (/=0)
    e <- genExprFor (FlexInt i) 0
    -- traceM ("genFlexIntExpr: " ++ show i ++ "\n")
    return (FlexInt i, e)

genExprFor :: FlexNum -> Depth -> Gen Expr
genExprFor fn depth | depth < 0 = genExprLitFor fn
                    | otherwise = Gen.sized go
    where nextDepth = depth + 1
          go :: Size -> Gen Expr
          go sz = if depth >= sz
                  then genExprLitFor fn
                  else Gen.frequency [ (1, genExprLitFor fn)
                                     , (9, genExprAddFor fn nextDepth)
                                     , (9, genExprSubFor fn nextDepth)
                                     , (9, genExprMulFor fn nextDepth)
                                     , (9, genExprDivFor fn nextDepth)
                                     ]

genExprLitFor :: FlexNum -> Gen Expr
genExprLitFor (FlexFloat f) = return (LitFloat emptyL f)
genExprLitFor (FlexInt i)   = return (LitInt emptyL i)

genExprAddFor :: FlexNum -> Depth -> Gen Expr
genExprAddFor (FlexFloat f) depth = do
    lof  <- genFloat
    let los  = realToFrac lof :: Float500
        ress = realToFrac f   :: Float500
        ros  = ress - los
        rof  = realToFrac ros :: Float50
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

genExprSubFor :: FlexNum -> Depth -> Gen Expr
genExprSubFor (FlexFloat f) depth = do
    rof <- genFloat
    let ros = realToFrac rof :: Float500
        fs  = realToFrac f   :: Float500
        los = fs + ros
        lof = realToFrac los :: Float50
    le <- Gen.choice [ return (LitFloat emptyL lof)
                     , genExprFor (FlexFloat lof) depth
                     ]
    re <- Gen.choice [ return (LitFloat emptyL rof)
                     , genExprFor (FlexFloat rof) depth
                     ]
    return (OperSub emptyL le re)
genExprSubFor (FlexInt i) depth = do
    ro <- genInteger
    let lo = i + ro
    le <- Gen.choice [ return (LitInt emptyL lo)
                     , genExprFor (FlexInt lo) depth
                     ]
    re <- Gen.choice [ return (LitInt emptyL ro)
                     , genExprFor (FlexInt ro) depth
                     ]
    return (OperSub emptyL le re)

genExprMulFor :: FlexNum -> Depth -> Gen Expr
genExprMulFor (FlexFloat f) depth = do
    ro  <- genFloatWhere gtltZero
    let lo = f / ro
    le <- Gen.choice [ return (LitFloat emptyL lo)
                     , genExprFor (FlexFloat lo) depth
                     ]
    re <- Gen.choice [ return (LitFloat emptyL ro)
                     , genExprFor (FlexFloat ro) depth
                     ]
    return (OperMul emptyL le re)
genExprMulFor (FlexInt i) depth = do
    s   <- Gen.element [(-1), 1]
    rf  <- Gen.element $ factorsOf i
    let lo = s * rf
        ro = div i lo
    le <- Gen.choice [ return (LitInt emptyL lo)
                     , genExprFor (FlexInt lo) depth
                     ]
    re <- Gen.choice [ return (LitInt emptyL ro)
                     , genExprFor (FlexInt ro) depth
                     ]
    return (OperMul emptyL le re)

genExprDivFor :: FlexNum -> Depth -> Gen Expr
genExprDivFor (FlexFloat f) depth = do
    ro  <- genFloatWhere gtltZero
    let lo = f * ro
    le <- Gen.choice [ return (LitFloat emptyL lo)
                     , genExprFor (FlexFloat lo) depth
                     ]
    re <- Gen.choice [ return (LitFloat emptyL ro)
                     , genExprFor (FlexFloat ro) depth
                     ]
    return (OperDiv emptyL le re)
genExprDivFor (FlexInt i) depth = do
    ro  <- genIntegerWhere (/=0)
    let lo = i * ro
    le <- Gen.choice [ return (LitInt emptyL lo)
                     , genExprFor (FlexInt lo) depth
                     ]
    re <- Gen.choice [ return (LitInt emptyL ro)
                     , genExprFor (FlexInt ro) depth
                     ]
    return (OperDiv emptyL le re)

exprToString :: Expr -> String
exprToString (LitFloat _ d) = "(" ++ show d ++ ")"
exprToString (LitInt   _ i) = "(" ++ show i ++ ")"
exprToString (Negate   _ e) = "-" ++ exprToString e
exprToString (OperExp  _ el er) = "(" ++ exprToString el ++ " ^ " ++ exprToString er ++ ")"
exprToString (OperMul  _ el er) = "(" ++ exprToString el ++ " * " ++ exprToString er ++ ")"
exprToString (OperDiv  _ el er) = "(" ++ exprToString el ++ " / " ++ exprToString er ++ ")"
exprToString (OperAdd  _ el er) = "(" ++ exprToString el ++ " + " ++ exprToString er ++ ")"
exprToString (OperSub  _ el er) = "(" ++ exprToString el ++ " - " ++ exprToString er ++ ")"
exprToString _ = "(a function call)"
