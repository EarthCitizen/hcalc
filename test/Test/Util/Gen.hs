{-# LANGUAGE BangPatterns #-}

module Test.Util.Gen ( StateGen(..)
                     , runStateGen
                     , runStateGenWith
                     , forAllStateGen
                     , genExpr
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
import Control.Monad.State.Strict
import Control.Monad.Trans.Class (lift)
import Data.List (nub, sort)
import Data.Scientific (fromFloatDigits, toRealFloat)
import qualified Data.Map.Strict as M
import qualified Eval as E
import FnStore
import GHC.Stack (HasCallStack)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Util.Data

isRemZero z x = 0 == rem z x

factorsOf z = let p = filter (isRemZero z) likelyFactors
                  r = nub . sort $ (*) <$> p <*> p
                 in filter (isRemZero z) r

gtltZero = (\x -> x > 0 || x < 0)

newtype StateGen a = StateGen { unStateGen :: StateT FnStore Gen a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState FnStore
             )

instance GetFnStore StateGen where
    getFnStore = get

instance PutFnStore StateGen where
    putFnStore = put

instance MonadGen StateGen where
    liftGen = StateGen . liftGen
    shrinkGen as ma = StateGen $ shrinkGen as $ unStateGen ma
    pruneGen = StateGen . pruneGen . unStateGen
    scaleGen sf ma = StateGen $ scaleGen sf $ unStateGen ma
    freezeGen ma = do
        (a, mb) <- StateGen $ freezeGen $ unStateGen ma
        return (a, StateGen mb)

runStateGen :: StateGen a -> Gen (a, FnStore)
runStateGen sg = runStateT (unStateGen sg) emptyFnStore

runStateGenWith :: StateGen a -> FnStore -> Gen (a, FnStore)
runStateGenWith sg fs = runStateT (unStateGen sg) fs

forAllStateGen :: (Monad m, Show a, HasCallStack) => StateGen a -> PropertyT m (a, FnStore)
forAllStateGen = forAll . runStateGen

sampleStateGen :: (MonadIO m) => StateGen a -> m (a, FnStore)
sampleStateGen = Gen.sample . runStateGen

-- genInteger :: StateGen Integer
-- genInteger = genIntegerBetween (-50000000) 50000000
--
-- genIntegerBetween :: Integer -> Integer -> StateGen Integer
-- genIntegerBetween x y = Gen.integral $ Range.constant x y

-- genIntegerWhere :: (Integer -> Bool) -> StateGen Integer
-- genIntegerWhere f = Gen.filter f genInteger

genWhole :: (MonadGen m) => m (Float50 -> Float50)
genWhole = Gen.choice [ return $ \x -> fromIntegral $ truncate x
                      , return id
                      ]

genFloat :: (MonadGen m) => m Float50
genFloat = Gen.choice [ genWhole <*> genFloatBetween (-5) 5
                      , genWhole <*> genFloatBetween (-3000000) (-100)
                      , genWhole <*> genFloatBetween 3000000 100
                      , genWhole <*> genFloatBetween (-300) (-900)
                      , genWhole <*> genFloatBetween 300 900
                      , genFloatBetween (-0.005) 0.005
                      , genFloatBetween (-0.000005) 0.000005
                      ]

genFloatBetween :: (MonadGen m) => Float50 -> Float50 -> m Float50
genFloatBetween x y = Gen.filter (filter x y) $ genWhole <*> (Gen.realFrac_ $ Range.constant x y)
    -- This fixes a subtle bug in which 1.1 was
    -- passed to x, but 1 was returned due to genWhole
    where filter a b | a < b = \x -> x >= a && x <= b
          filter a b = \x -> x >= b && x <= a

genFloatWhere :: (MonadGen m) => (Float50 -> Bool) -> m Float50
genFloatWhere f = Gen.filter f genFloat

genN :: (MonadGen m) => Int -> m a -> m [a]
genN s g = forM [1..s] (\_ -> g)

genFnName :: (MonadGen m) => m String
genFnName = do
    c1 <- Gen.alpha
    cx <- Gen.list (Range.constant 0 50) Gen.alphaNum
    return (c1:cx)

type Depth = Size

genExpr :: StateGen (Float50, Expr)
genExpr = do
    f <- genFloatWhere (/=0)
    e <- genExprFor f 0
    return (f, e)

genExprFor :: Float50 -> Depth -> StateGen Expr
genExprFor fn depth | depth < 0 = genExprLitFor fn
                    | otherwise = Gen.sized go
    where nextDepth = depth + 1
          go :: Size -> StateGen Expr
          go sz = if depth >= sz
                  then genExprLitFor fn
                  else Gen.frequency [ (1, genExprLitFor fn)
                                     , (2, genExprExpFor fn nextDepth)
                                     , (5, genExprFnFor  fn nextDepth)
                                     , (9, genExprAddFor fn nextDepth)
                                     , (9, genExprSubFor fn nextDepth)
                                     , (9, genExprMulFor fn nextDepth)
                                     , (9, genExprDivFor fn nextDepth)
                                     ]

genExprLitFor :: Float50 -> StateGen Expr
genExprLitFor f = do
    -- The parser uses Negate for negative
    -- values, so that same behavior needs
    -- to be duplicated here
    return $ if signum(f) < 0
             then Negate emptyL $ LitNum emptyL $ abs f
             -- abs turns negative zero into zero
             -- signum gives -0 for -0 but -0 < 0 == False
             -- so that is fixed by abs f
             else LitNum emptyL $ abs f

genExprAddFor :: Float50 -> Depth -> StateGen Expr
genExprAddFor f depth = do
    lof  <- genFloat
    let los  = realToFrac lof :: Float500
        ress = realToFrac f   :: Float500
        ros  = ress - los
        rof  = realToFrac ros :: Float50
    le <- Gen.choice [ genExprLitFor lof
                     , genExprFor lof depth
                     ]
    re <- Gen.choice [ genExprLitFor rof
                     , genExprFor rof depth
                     ]
    return (OperAdd emptyL le re)

genExprSubFor :: Float50 -> Depth -> StateGen Expr
genExprSubFor f depth = do
    rof <- genFloat
    let ros = realToFrac rof :: Float500
        fs  = realToFrac f   :: Float500
        los = fs + ros
        lof = realToFrac los :: Float50
    le <- Gen.choice [ genExprLitFor lof
                     , genExprFor lof depth
                     ]
    re <- Gen.choice [ genExprLitFor rof
                     , genExprFor rof depth
                     ]
    return (OperSub emptyL le re)

genExprMulFor :: Float50 -> Depth -> StateGen Expr
genExprMulFor f depth = do
    ro  <- genFloatWhere gtltZero
    let lo = f / ro
    le <- Gen.choice [ genExprLitFor lo
                     , genExprFor lo depth
                     ]
    re <- Gen.choice [ genExprLitFor ro
                     , genExprFor ro depth
                     ]
    return (OperMul emptyL le re)

genExprDivFor :: Float50 -> Depth -> StateGen Expr
genExprDivFor f depth = do
    ro  <- genFloatWhere gtltZero
    let lo = f * ro
    le <- Gen.choice [ genExprLitFor lo
                     , genExprFor lo depth
                     ]
    re <- Gen.choice [ genExprLitFor ro
                     , genExprFor ro depth
                     ]
    return (OperDiv emptyL le re)

genExprExpFor :: Float50 -> Depth -> StateGen Expr
genExprExpFor f depth
    | f <= 0 = genExprLitFor f
    | otherwise = do
        -- 1.1 prevents calucation of Infinity for
        -- e, which happens if b = 1
        b <- genFloatBetween 1.1 10000
        let e = log f / log b
        bo <- Gen.choice [ genExprLitFor b
                         , genExprFor b depth
                         ]
        eo <- Gen.choice [ genExprLitFor e
                         , genExprFor e depth
                         ]
        return (OperExp emptyL bo eo)

genExprFnFor :: Float50 -> Depth -> StateGen Expr
genExprFnFor f depth = do
    fs <- getFnStore
    let p = "a"
    let notUsedNotParam = \k -> let a = not $ hasFn k fs
                                    b = k /= p
                                 in a && b
    n  <- Gen.filter notUsedNotParam genFnName
    ro <- genFloatWhere gtltZero
    re <- genExprExpFor ro depth
    let lo = f / ro
        le = FnCall emptyL p []
        me = OperMul emptyL le re
        fd = FnExpr n [p] me
    putFnM fd
    el <- genExprLitFor lo
    return $ FnCall emptyL n [el]

exprToString :: Expr -> String
exprToString (LitNum  _ d) = "(" ++ show d ++ ")"
exprToString (Negate  _ e) = "-" ++ exprToString e
exprToString (OperExp _ el er) = "(" ++ exprToString el ++ " ^ " ++ exprToString er ++ ")"
exprToString (OperMul _ el er) = "(" ++ exprToString el ++ " * " ++ exprToString er ++ ")"
exprToString (OperDiv _ el er) = "(" ++ exprToString el ++ " / " ++ exprToString er ++ ")"
exprToString (OperAdd _ el er) = "(" ++ exprToString el ++ " + " ++ exprToString er ++ ")"
exprToString (OperSub _ el er) = "(" ++ exprToString el ++ " - " ++ exprToString er ++ ")"
exprToString _ = "(a function call)"

