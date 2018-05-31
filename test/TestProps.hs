module TestProps where

import Debug.Trace (trace, traceM)

import Alias
import AST
import FlexNum
import qualified Data.Map.Strict as M
import qualified Eval as E
import Hedgehog
import Hedgehog.Internal.Source (HasCallStack)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import TestGen
import Data.AEq ((~==))
import TypeLevel.NaturalNumber (Ten, Fifteen)
import Data.Eq.Approximate (AbsolutelyApproximateValue(..), Digits(..))

emptyL = ("", 0, 0)
emptyS = M.empty :: M.Map Name FnDef
-- testCount = 30000 :: TestLimit
testCount = 3000 :: TestLimit
exprSplitSize = 7 :: Size

(~===) :: (MonadTest m, Eq a, Show a, HasCallStack) => a -> a -> m ()
(~===) a b = do
    footnote $ "actual:   " ++ show a
    footnote $ "expected: " ++ show b
    a === b

hprop_evalAddsFloatAndInteger :: Property
hprop_evalAddsFloatAndInteger =
    withTests testCount $ property $ do
        (f, i, (fnl:fnr:[])) <- forAll $ do
            f'  <- genFloat
            i'  <- genInteger
            fns <- Gen.shuffle [LitFloat emptyL f', LitInt emptyL i']
            return (f', i', fns)
        let expected = Right $ FlexFloat (f + fromIntegral i)
            expr     = OperAdd emptyL fnl fnr
            actual   = E.eval expr emptyS
        actual === expected

hprop_evalAddsFloats :: Property
hprop_evalAddsFloats =
    withTests testCount $ property $ do
        (lo, ro) <- forAll $ do
            lo' <- genFloat
            ro' <- genFloat
            return (lo', ro')
        let expected = Right $ FlexFloat (lo + ro)
            expr     = OperAdd emptyL (LitFloat emptyL lo) (LitFloat emptyL ro)
            actual   = E.eval expr emptyS
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

hprop_evalSubtractsFloatFromInteger :: Property
hprop_evalSubtractsFloatFromInteger =
    withTests testCount $ property $ do
        (f, i) <- forAll $ do
            f'  <- genFloat
            i'  <- genInteger
            return (f', i')
        let expected = Right $ FlexFloat (fromIntegral i - f)
            expr     = OperSub emptyL (LitInt emptyL i) (LitFloat emptyL f)
            actual   = E.eval expr emptyS
        actual === expected

hprop_evalSubtractsIntegerFromFloat :: Property
hprop_evalSubtractsIntegerFromFloat =
    withTests testCount $ property $ do
        (f, i) <- forAll $ do
            f'  <- genFloat
            i'  <- genInteger
            return (f', i')
        let expected = Right $ FlexFloat (f - fromIntegral i)
            expr     = OperSub emptyL (LitFloat emptyL f) (LitInt emptyL i)
            actual   = E.eval expr emptyS
        actual === expected

hprop_evalSubtractsFloats :: Property
hprop_evalSubtractsFloats =
    withTests testCount $ property $ do
        (lo, ro) <- forAll $ do
            lo' <- genFloat
            ro' <- genFloat
            return (lo', ro')
        let expected = Right $ FlexFloat (lo - ro)
            expr     = OperSub emptyL (LitFloat emptyL lo) (LitFloat emptyL ro)
            actual   = E.eval expr emptyS
        actual === expected

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

hprop_evalMultipliesFloatAndInteger :: Property
hprop_evalMultipliesFloatAndInteger =
    withTests testCount $ property $ do
        (f, i, (fnl:fnr:[])) <- forAll $ do
            f'  <- genFloat
            i'  <- genInteger
            fns <- Gen.shuffle [LitFloat emptyL f', LitInt emptyL i']
            return (f', i', fns)
        let expected = Right $ FlexFloat (f * fromIntegral i)
            expr     = OperMul emptyL fnl fnr
            actual   = E.eval expr emptyS
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

hprop_evalDividesFloatByInteger :: Property
hprop_evalDividesFloatByInteger =
    withTests testCount $ property $ do
        (f, i) <- forAll $ do
            f'  <- genFloat
            i'  <- genInteger
            return (f', i')
        let expected = Right $ FlexFloat (f / fromIntegral i)
            expr     = OperDiv emptyL (LitFloat emptyL f) (LitInt emptyL i)
            actual   = E.eval expr emptyS
        actual === expected

hprop_evalDividesIntegerByFloat :: Property
hprop_evalDividesIntegerByFloat =
    withTests testCount $ property $ do
        (f, i) <- forAll $ do
            f'  <- genFloat
            i'  <- genInteger
            return (f', i')
        let expected = Right $ FlexFloat (fromIntegral i / f)
            expr     = OperDiv emptyL (LitInt emptyL i) (LitFloat emptyL f)
            actual   = E.eval expr emptyS
        actual === expected

hprop_evalDividesFloats :: Property
hprop_evalDividesFloats =
    withTests testCount $ property $ do
        (lo, ro) <- forAll $ do
            lo' <- genFloat
            ro' <- genFloat
            return (lo', ro')
        let expected = Right $ FlexFloat (lo / ro)
            expr     = OperDiv emptyL (LitFloat emptyL lo) (LitFloat emptyL ro)
            actual   = E.eval expr emptyS
        actual === expected

hprop_evalDividesIntegersByNonZeroCreatingInteger :: Property
hprop_evalDividesIntegersByNonZeroCreatingInteger =
    withTests testCount $ property $ do
        (q, d) <- forAll $ do
            q' <- genInteger
            d' <- genIntegerWhere (/=0)
            return (q', d')
        let expected = Right $ FlexInt q
            dividend = q * d
            expr     = OperDiv emptyL (LitInt emptyL dividend) (LitInt emptyL d)
        E.eval expr emptyS === expected

hprop_evalDividesIntegersByNonZeroCreatingFloat :: Property
hprop_evalDividesIntegersByNonZeroCreatingFloat =
    withTests testCount $ property $ do
        let cond = (\(dividend, d) -> rem dividend d /= 0)
        (dividend, d) <- forAll $ Gen.filter cond getTwoIntegers
        let expected = Right $ FlexFloat (fromIntegral dividend / fromIntegral d)
            expr     = OperDiv emptyL (LitInt emptyL dividend) (LitInt emptyL d)
        E.eval expr emptyS === expected
    where getTwoIntegers :: Gen (Integer, Integer)
          getTwoIntegers = do
              i1 <- genInteger
              i2 <- genIntegerWhere (/=0)
              return (i1, i2)

hprop_evalDividesIntegersZeroByZeroCreatingNaN :: Property
hprop_evalDividesIntegersZeroByZeroCreatingNaN =
    withTests 1 $ property $ do
        let expr = OperDiv emptyL (LitInt emptyL 0) (LitInt emptyL 0)
        actual <- evalEither $ E.eval expr emptyS
        actual === FlexNaN


-- NEED SOME TEST CASES FOR BASES AND EXPONENTS OF 0 AND 1

-- case 1, case 5 - with sanitize
hprop_evalRaisesWholeFloatsByPositiveInteger :: Property
hprop_evalRaisesWholeFloatsByPositiveInteger =
    withTests testCount $ property $ do
        (b, n) <- forAll $ do
            b' <- genInteger
            n' <- genIntegerBetween (0) 9
            return (fromIntegral b', n')
        let expected = Right $ FlexInt $ ((truncate b) ^ n)
            expr     = OperExp emptyL (LitFloat emptyL b) (LitInt emptyL n)
            actual   = E.eval expr emptyS
        actual ~=== expected

-- case 1
hprop_evalRaisesFractionalFloatsByInteger :: Property
hprop_evalRaisesFractionalFloatsByInteger =
    withTests testCount $ property $ do
        (b, n) <- forAll $ do
            w <- genIntegerBetween (-20000) 20000
            f <- genFloatBetween 0.1 0.0009
            let b' = (fromIntegral w) + f
            -- keep exponent low to avoid long test times
            -- 0 exponent results in return of FlexInt 1
            -- avoiding exponent of 0, edge case not a big deal
            n' <- Gen.filter (/=0) $ genIntegerBetween (-9) 9
            return (b', n')
        let expected = Right $ FlexFloat $ (b ^^ n)
            expr     = OperExp emptyL (LitFloat emptyL b) (LitInt emptyL n)
            actual   = E.eval expr emptyS
        actual ~=== expected

-- case 2, case 4
hprop_evalRaisesIntegersByFractionalFloat :: Property
hprop_evalRaisesIntegersByFractionalFloat =
    withTests testCount $ property $ do
        (b, n) <- forAll $ do
            -- avoiding 0 base, 0 causes exception from
            -- numbers package when calculating expected
            -- value, should be ok, covered by other cases
            -- avoiding 1 because results in FlexInt 1,
            -- to cover in other cases
            b' <- Gen.filter (\x -> x /= 0 && x /= 1) $ genIntegerBetween (0) 2000
            -- keep exponent low to avoid long test times
            nw <- Gen.filter (/=1) $ genIntegerBetween (-9) 9
            nf <- genFloatBetween 0.1 0.0009
            let n' = (fromIntegral nw) + nf
            return (b', n')
        let expected = Right $ FlexFloat $ ((fromIntegral b) ** n)
            expr     = OperExp emptyL (LitInt emptyL b) (LitFloat emptyL n)
            actual   = E.eval expr emptyS
        actual ~=== expected

-- case 3
hprop_evalRaisesNegativeFractionalFloatsByFloat :: Property
hprop_evalRaisesNegativeFractionalFloatsByFloat =
    withTests testCount $ property $ do
        (b, n) <- forAll $ do
            bw <- genIntegerBetween (-2000) (-1)
            bf <- genFloatBetween 0.1 0.0009
            let b' = (fromIntegral bw) + bf
            -- keep exponent low to avoid long test times
            nw <- genIntegerBetween (-9) 9
            nf <- genFloatBetween 0.1 0.0009
            let n' = (fromIntegral nw) + nf
            return (b', n')
        let expected = Right $ FlexNaN
            expr     = OperExp emptyL (LitFloat emptyL b) (LitFloat emptyL n)
            actual   = E.eval expr emptyS
        actual ~=== expected

-- case 4
hprop_evalRaisesPositiveFractionalFloatsByFloat :: Property
hprop_evalRaisesPositiveFractionalFloatsByFloat =
    withTests testCount $ property $ do
        (b, n) <- forAll $ do
            bw <- genIntegerBetween 0 2000
            bf <- genFloatBetween 0.1 0.0009
            let b' = (fromIntegral bw) + bf
            -- keep exponent low to avoid long test times
            nw <- genIntegerBetween (-9) 9
            nf <- genFloatBetween 0.1 0.0009
            let n' = (fromIntegral nw) + nf
            return (b', n')
        let expected = Right $ FlexFloat $ (b ** n)
            expr     = OperExp emptyL (LitFloat emptyL b) (LitFloat emptyL n)
            actual   = E.eval expr emptyS
        actual ~=== expected

-- case 5
hprop_evalRaisesIntegersByNonNegativeInteger :: Property
hprop_evalRaisesIntegersByNonNegativeInteger =
    withTests testCount $ property $ do
        (b, n) <- forAll $ do
            b' <- genInteger
            n' <- genIntegerBetween 1 50
            return (b', n')
        let expected = Right $ FlexInt (b ^ n)
            expr     = OperExp emptyL (LitInt emptyL b) (LitInt emptyL n)
        E.eval expr emptyS ~=== expected

-- case 6
hprop_evalRaisesIntegersByNegativeInteger :: Property
hprop_evalRaisesIntegersByNegativeInteger =
    withTests testCount $ property $ do
        (b, n) <- forAll $ do
            b' <- genInteger
            n' <- genIntegerBetween (-1) (-50)
            return (b', n')
        let expected = Right $ FlexFloat (fromIntegral b ^^ n)
            expr     = OperExp emptyL (LitInt emptyL b) (LitInt emptyL n)
            actual   = E.eval expr emptyS
        actual ~=== expected

hprop_evalExpressions :: Property
hprop_evalExpressions =
    -- Shrinks seem to go into an infinite loop sometimes
    -- with BigFloat unless the shrinks are set. The shrinks
    -- above 0 don't seem to have any benefit as the same
    -- values resurface often for the repeats.
    withShrinks 0 $ withTests testCount $ property $ do
        (fn, expr) <- forAll $ Gen.resize exprSplitSize $ genFlexNumExpr
        let expected = Right $ fn
            actual = E.eval expr emptyS
        actual === expected

-- hprop_evalExecutesNullaryFunctions :: Property
-- hprop_evalExecutesNullaryFunctions =
--     withTests testCount $ property $ do
--         (fn, fd, v) <- forAll $ do
--             let mkExpr
--             fnName <- genFnName
--             expr   <- Gen.choice [ LitFloat <$> genFloat
--                                  , LitInt   <$> genInteger
--                                  ]
--
--             return (fnName, FnExpr fnName [] expr
