module TestGen (genAddInteger, genSubInteger)  where

import Alias
import AST
import Control.Monad (forM)
import qualified Data.Map.Strict as M
import qualified Eval as E
import FlexNum
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

emptyL = ("", 0, 0)

genInteger :: Gen Integer
genInteger = Gen.integral $ Range.constantFrom 0 (-50000000) 50000000

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

genX :: Int -> Gen a -> Gen [a]
genX s g = forM [1..s] (\_ -> g)
