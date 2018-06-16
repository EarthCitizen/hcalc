{-# LANGUAGE BangPatterns #-}

module TestGen ( genExpr
               , genFloat
               , genFloatBetween
               , genFloatWhere
               , genFnName
               , genN
               ) where

import Debug.Trace (trace, traceM)

import Alias
import AST
import Control.Monad (forM)
import Data.List (nub, sort)
import Data.Scientific (fromFloatDigits, toRealFloat)
import qualified Data.Map.Strict as M
import qualified Eval as E
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import TestData

emptyL = ("", 0, 0)

isRemZero z x = 0 == rem z x

factorsOf z = let p = filter (isRemZero z) likelyFactors
                  r = nub . sort $ (*) <$> p <*> p
                 in filter (isRemZero z) r

gtltZero = (\x -> x > 0 || x < 0)

-- genInteger :: Gen Integer
-- genInteger = genIntegerBetween (-50000000) 50000000
--
-- genIntegerBetween :: Integer -> Integer -> Gen Integer
-- genIntegerBetween x y = Gen.integral $ Range.constant x y

-- genIntegerWhere :: (Integer -> Bool) -> Gen Integer
-- genIntegerWhere f = Gen.filter f genInteger

genWhole :: Gen (Float50 -> Float50)
genWhole = Gen.choice [ return $ \x -> fromIntegral $ truncate x
                       , return id
                       ]

genFloat :: Gen Float50
genFloat = Gen.choice [ genWhole <*> genFloatBetween (-5) 5
                      , genWhole <*> genFloatBetween (-3000000) (-100)
                      , genWhole <*> genFloatBetween 3000000 100
                      , genWhole <*> genFloatBetween (-300) (-900)
                      , genWhole <*> genFloatBetween 300 900
                      , genFloatBetween (-0.005) 0.005
                      , genFloatBetween (-0.000005) 0.000005
                      ]

genFloatBetween :: Float50 -> Float50 -> Gen Float50
genFloatBetween x y = genWhole <*> (Gen.realFrac_ $ Range.constant x y)

genFloatWhere :: (Float50 -> Bool) -> Gen Float50
genFloatWhere f = Gen.filter f genFloat

genN :: Int -> Gen a -> Gen [a]
genN s g = forM [1..s] (\_ -> g)

genFnName :: Gen String
genFnName = do
    c1 <- Gen.alpha
    cx <- Gen.list (Range.constant 0 50) Gen.alphaNum
    return (c1:cx)

type Depth = Size

genExpr :: Gen (Float50, Expr)
genExpr = do
    f <- genFloatWhere (/=0)
    e <- genExprFor f 0
    return (f, e)

genExprFor :: Float50 -> Depth -> Gen Expr
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

genExprLitFor :: Float50 -> Gen Expr
genExprLitFor f = return (LitNum emptyL f)

genExprAddFor :: Float50 -> Depth -> Gen Expr
genExprAddFor f depth = do
    lof  <- genFloat
    let los  = realToFrac lof :: Float500
        ress = realToFrac f   :: Float500
        ros  = ress - los
        rof  = realToFrac ros :: Float50
    le <- Gen.choice [ return (LitNum emptyL lof)
                     , genExprFor lof depth
                     ]
    re <- Gen.choice [ return (LitNum emptyL rof)
                     , genExprFor rof depth
                     ]
    return (OperAdd emptyL le re)

genExprSubFor :: Float50 -> Depth -> Gen Expr
genExprSubFor f depth = do
    rof <- genFloat
    let ros = realToFrac rof :: Float500
        fs  = realToFrac f   :: Float500
        los = fs + ros
        lof = realToFrac los :: Float50
    le <- Gen.choice [ return (LitNum emptyL lof)
                     , genExprFor lof depth
                     ]
    re <- Gen.choice [ return (LitNum emptyL rof)
                     , genExprFor rof depth
                     ]
    return (OperSub emptyL le re)

genExprMulFor :: Float50 -> Depth -> Gen Expr
genExprMulFor f depth = do
    ro  <- genFloatWhere gtltZero
    let lo = f / ro
    le <- Gen.choice [ return (LitNum emptyL lo)
                     , genExprFor lo depth
                     ]
    re <- Gen.choice [ return (LitNum emptyL ro)
                     , genExprFor ro depth
                     ]
    return (OperMul emptyL le re)

genExprDivFor :: Float50 -> Depth -> Gen Expr
genExprDivFor f depth = do
    ro  <- genFloatWhere gtltZero
    let lo = f * ro
    le <- Gen.choice [ return (LitNum emptyL lo)
                     , genExprFor lo depth
                     ]
    re <- Gen.choice [ return (LitNum emptyL ro)
                     , genExprFor ro depth
                     ]
    return (OperDiv emptyL le re)

exprToString :: Expr -> String
exprToString (LitNum  _ d) = "(" ++ show d ++ ")"
exprToString (Negate  _ e) = "-" ++ exprToString e
exprToString (OperExp _ el er) = "(" ++ exprToString el ++ " ^ " ++ exprToString er ++ ")"
exprToString (OperMul _ el er) = "(" ++ exprToString el ++ " * " ++ exprToString er ++ ")"
exprToString (OperDiv _ el er) = "(" ++ exprToString el ++ " / " ++ exprToString er ++ ")"
exprToString (OperAdd _ el er) = "(" ++ exprToString el ++ " + " ++ exprToString er ++ ")"
exprToString (OperSub _ el er) = "(" ++ exprToString el ++ " - " ++ exprToString er ++ ")"
exprToString _ = "(a function call)"
