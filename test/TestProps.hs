module TestProps where

import Alias
import AST
import FlexNum
import qualified Data.Map.Strict as M
import qualified Eval as E
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import TestGen
import Data.AEq ((~==))

emptyL = ("", 0, 0)
emptyS = M.empty :: M.Map Name FnDef
testCount = 2000000 :: TestLimit

newtype TestFlexNum = TestFlexNum { unTestFlexNum :: FlexNum }

instance Eq TestFlexNum where
    (TestFlexNum (FlexFloat x)) == (TestFlexNum (FlexFloat y)) = x ~== y
    (TestFlexNum (FlexInt x))   == (TestFlexNum (FlexInt y))   = x == y
    _ == _ = False

instance Show TestFlexNum where
    show = show . unTestFlexNum

hprop_evalAddsFloatAndInteger :: Property
hprop_evalAddsFloatAndInteger =
    withTests testCount $ property $ do
        (f, i, (fnl:fnr:[])) <- forAll $ do
            f'  <- genFloat
            i'  <- genInteger
            fns <- Gen.shuffle [LitFloat emptyL f', LitInt emptyL i']
            return (f', i', fns)
        let expected = Right $ TestFlexNum $ FlexFloat (f + fromIntegral i)
            expr     = OperAdd emptyL fnl fnr
            actual   = TestFlexNum <$> E.eval expr emptyS
        actual === expected

hprop_evalSubtractsFloatFromInteger :: Property
hprop_evalSubtractsFloatFromInteger =
    withTests testCount $ property $ do
        (f, i) <- forAll $ do
            f'  <- genFloat
            i'  <- genInteger
            return (f', i')
        let expected = Right $ TestFlexNum $ FlexFloat (fromIntegral i - f)
            expr     = OperSub emptyL (LitInt emptyL i) (LitFloat emptyL f)
            actual   = TestFlexNum <$> E.eval expr emptyS
        actual === expected

hprop_evalSubtractsIntegerFromFloat :: Property
hprop_evalSubtractsIntegerFromFloat =
    withTests testCount $ property $ do
        (f, i) <- forAll $ do
            f'  <- genFloat
            i'  <- genInteger
            return (f', i')
        let expected = Right $ TestFlexNum $ FlexFloat (f - fromIntegral i)
            expr     = OperSub emptyL (LitFloat emptyL f) (LitInt emptyL i)
            actual   = TestFlexNum <$> E.eval expr emptyS
        actual === expected

hprop_evalMultipliesFloatAndInteger :: Property
hprop_evalMultipliesFloatAndInteger =
    withTests testCount $ property $ do
        (f, i, (fnl:fnr:[])) <- forAll $ do
            f'  <- genFloat
            i'  <- genInteger
            fns <- Gen.shuffle [LitFloat emptyL f', LitInt emptyL i']
            return (f', i', fns)
        let expected = Right $ TestFlexNum $ FlexFloat (f * fromIntegral i)
            expr     = OperMul emptyL fnl fnr
            actual   = TestFlexNum <$> E.eval expr emptyS
        actual === expected

hprop_evalAddsFloats :: Property
hprop_evalAddsFloats =
    withTests testCount $ property $ do
        (lo, ro) <- forAll $ do
            lo' <- genFloat
            ro' <- genFloat
            return (lo', ro')
        let expected = Right $ TestFlexNum $ FlexFloat (lo + ro)
            expr     = OperAdd emptyL (LitFloat emptyL lo) (LitFloat emptyL ro)
            actual   = TestFlexNum <$> E.eval expr emptyS
        actual === expected

hprop_evalSubtractsFloats :: Property
hprop_evalSubtractsFloats =
    withTests testCount $ property $ do
        (lo, ro) <- forAll $ do
            lo' <- genFloat
            ro' <- genFloat
            return (lo', ro')
        let expected = Right $ TestFlexNum $ FlexFloat (lo - ro)
            expr     = OperSub emptyL (LitFloat emptyL lo) (LitFloat emptyL ro)
            actual   = TestFlexNum <$> E.eval expr emptyS
        actual === expected

hprop_evalMultipliesFloats :: Property
hprop_evalMultipliesFloats =
    withTests testCount $ property $ do
        (lo, ro) <- forAll $ do
            lo' <- genFloat
            ro' <- genFloat
            return (lo', ro')
        let expected = Right $ FlexFloat (lo * ro)
            expr     = OperMul emptyL (LitFloat emptyL lo) (LitFloat emptyL ro)
        E.eval expr emptyS === expected

hprop_evalDividesFloats :: Property
hprop_evalDividesFloats =
    withTests testCount $ property $ do
        (lo, ro) <- forAll $ do
            lo' <- genFloat
            ro' <- genFloat
            return (lo', ro')
        let expected = Right $ FlexFloat (lo / ro)
            expr     = OperDiv emptyL (LitFloat emptyL lo) (LitFloat emptyL ro)
        E.eval expr emptyS === expected

hprop_evalRaisesFloats :: Property
hprop_evalRaisesFloats =
    withTests testCount $ property $ do
        (b, n) <- forAll $ do
            b' <- genFloat
            n' <- genFloat
            return (b', n')
        let expected = Right $ TestFlexNum $ FlexFloat (b ** n)
            expr     = OperExp emptyL (LitFloat emptyL b) (LitFloat emptyL n)
            actual   = TestFlexNum <$> E.eval expr emptyS
        actual === expected

hprop_evalAddsIntegers :: Property
hprop_evalAddsIntegers =
    withTests testCount $ property $ do
        (lo, ro) <- forAll $ do
            lo' <- genInteger
            ro' <- genInteger
            return (lo', ro')
        let expected = Right $ FlexInt (lo + ro)
            expr     = OperAdd emptyL (LitInt emptyL lo) (LitInt emptyL ro)
        E.eval expr emptyS === expected

hprop_evalSubtractsIntegers :: Property
hprop_evalSubtractsIntegers =
    withTests testCount $ property $ do
        (lo, ro) <- forAll $ do
            lo' <- genInteger
            ro' <- genInteger
            return (lo', ro')
        let expected = Right $ FlexInt (lo - ro)
            expr     = OperSub emptyL (LitInt emptyL lo) (LitInt emptyL ro)
        E.eval expr emptyS === expected

hprop_evalMultipliesIntegers :: Property
hprop_evalMultipliesIntegers =
    withTests testCount $ property $ do
        (lo, ro) <- forAll $ do
            lo' <- genInteger
            ro' <- genInteger
            return (lo', ro')
        let expected = Right $ FlexInt (lo * ro)
            expr     = OperMul emptyL (LitInt emptyL lo) (LitInt emptyL ro)
        E.eval expr emptyS === expected


hprop_evalDividesIntegersByNonZeroEvenly :: Property
hprop_evalDividesIntegersByNonZeroEvenly =
    withTests testCount $ property $ do
        (q, d) <- forAll $ do
            q' <- genInteger
            d' <- Gen.filter (/=0) genInteger
            return (q', d')
        let expected = Right $ FlexInt q
            dividend = q * d
            expr     = OperDiv emptyL (LitInt emptyL dividend) (LitInt emptyL d)
        E.eval expr emptyS === expected

hprop_evalRaisesIntegersByNonNegativeBase :: Property
hprop_evalRaisesIntegersByNonNegativeBase =
    withTests testCount $ property $ do
        (b, n) <- forAll $ do
            b' <- genInteger
            n' <- Gen.integral $ Range.constant 1 50
            return (b', n')
        let expected = Right $ FlexInt (b ^ n)
            expr     = OperExp emptyL (LitInt emptyL b) (LitInt emptyL n)
        E.eval expr emptyS === expected

hprop_evalRaisesIntegersByNegativeBase :: Property
hprop_evalRaisesIntegersByNegativeBase =
    withTests testCount $ property $ do
        (b, n) <- forAll $ do
            b' <- genInteger
            n' <- Gen.integral $ Range.constant (-1) (-50)
            return (b', n')
        let expected = Right $ FlexFloat (fromIntegral b **  fromIntegral n)
            expr     = OperExp emptyL (LitInt emptyL b) (LitInt emptyL n)
        E.eval expr emptyS === expected
