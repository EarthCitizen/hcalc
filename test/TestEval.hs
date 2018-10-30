module TestEval where

import Debug.Trace (trace, traceM)

import Alias
import AST
import Error (Error)
import qualified Data.Map.Strict as M
import qualified Eval as E
import Hedgehog
import Hedgehog.Internal.Source (HasCallStack)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import TestGen

emptyL = ("", 0, 0)
emptyS = M.empty :: M.Map Name FnDef
testCount = 30000 :: TestLimit
exprSplitSize = 7 :: Size

newtype ApproxFloat = ApproxFloat { unApprox :: Float50 }

instance Eq ApproxFloat where
    x == y = checkApproxEqual (1e-10) (unApprox x) (unApprox y)

instance Show ApproxFloat where
    show x = show $ unApprox x

checkApproxEqual :: Float50 -> Float50 -> Float50 -> Bool
checkApproxEqual t x y =
    case (isNaN x, isNaN y) of
        (True, True) -> True
        (True, _   ) -> False
        (_,    True) -> False
        _ -> case signum x == signum y of
                 False -> False
                 _     -> case (isInfinite x, isInfinite y) of
                              (True, True) -> True
                              (True, _   ) -> False
                              (_,    True) -> False
                              _    -> abs (x - y) <= t

(~===) :: (MonadTest m, HasCallStack) => Either Error Float50 -> Either Error Float50 -> m ()
(~===) (Right a) (Right b) = do
    footnote $ "actual:   " ++ show a
    footnote $ "expected: " ++ show b
    (ApproxFloat a) === (ApproxFloat b)
(~===) a b = a === b

hprop_evalAdds :: Property
hprop_evalAdds =
    withTests testCount $ property $ do
        (lo, ro) <- forAll $ do
            lo' <- genFloat
            ro' <- genFloat
            return (lo', ro')
        let expected = Right (lo + ro)
            expr     = OperAdd emptyL (LitNum emptyL lo) (LitNum emptyL ro)
            actual   = E.eval expr emptyS
        actual ~=== expected

hprop_evalSubtracts :: Property
hprop_evalSubtracts =
    withTests testCount $ property $ do
        (lo, ro) <- forAll $ do
            lo' <- genFloat
            ro' <- genFloat
            return (lo', ro')
        let expected = Right (lo - ro)
            expr     = OperSub emptyL (LitNum emptyL lo) (LitNum emptyL ro)
            actual   = E.eval expr emptyS
        actual ~=== expected

hprop_evalMultiplies :: Property
hprop_evalMultiplies =
    withTests testCount $ property $ do
        (lo, ro) <- forAll $ do
            lo' <- genFloat
            ro' <- genFloat
            return (lo', ro')
        let expected = Right (lo * ro)
            expr     = OperMul emptyL (LitNum emptyL lo) (LitNum emptyL ro)
        E.eval expr emptyS ~=== expected

hprop_evalDivides :: Property
hprop_evalDivides =
    withTests testCount $ property $ do
        (lo, ro) <- forAll $ do
            lo' <- genFloat
            ro' <- genFloat
            return (lo', ro')
        let expected = Right (lo / ro)
            expr     = OperDiv emptyL (LitNum emptyL lo) (LitNum emptyL ro)
            actual   = E.eval expr emptyS
        actual ~=== expected

hprop_evalRaises :: Property
hprop_evalRaises =
    withTests 3000 $ property $ do
        (lo, ro) <- forAll $ do
            lo' <- genFloat
            ro' <- genFloatBetween (-10) 10
            return (lo', ro')
        let expected = Right (lo ** ro)
            expr     = OperExp emptyL (LitNum emptyL lo) (LitNum emptyL ro)
            actual   = E.eval expr emptyS
        actual ~=== expected

hprop_evalExecutesNullaryFunctions :: Property
hprop_evalExecutesNullaryFunctions =
    withTests testCount $ property $ do
        (fn, fd, v) <- forAll $ do
            fnName <- genFnName
            num    <- genFloat
            let expr = LitNum emptyL num
            return (fnName, FnExpr fnName [] expr, num)
        let expected = Right (v)
            store    = M.fromList [(fn,fd)]
            expr     = FnCall emptyL fn []
            actual   = E.eval expr store
        actual ~=== expected

hprop_evalExecutesBinaryFunctions :: Property
hprop_evalExecutesBinaryFunctions =
    withTests testCount $ property $ do
        (fn, fd, plist, ve) <- forAll $ do
            fnName <- genFnName
            numl   <- genFloat
            numr   <- genFloat
            let fl   = FnCall emptyL "a" []
                fr   = FnCall emptyL "b" []
                expr = OperSub emptyL fl fr
                pl   = LitNum emptyL numl
                pr   = LitNum emptyL numr
            return (fnName, FnExpr fnName ["a", "b"] expr, [pl, pr], numl - numr)
        let expected = Right (ve)
            store    = M.fromList [(fn,fd)]
            expr     = FnCall emptyL fn plist
            actual   = E.eval expr store
        actual ~=== expected

hprop_evalExecutesTernaryFunctions :: Property
hprop_evalExecutesTernaryFunctions =
    withTests testCount $ property $ do
        (fn, fd, plist, ve) <- forAll $ do
            fnName <- genFnName
            numl   <- genFloat
            numm   <- genFloat
            numr   <- genFloat
            let fl   = FnCall emptyL "a" []
                fm   = FnCall emptyL "b" []
                fr   = FnCall emptyL "c" []
                expr = OperSub emptyL (OperSub emptyL fl fm) fr
                pl   = LitNum emptyL numl
                pm   = LitNum emptyL numm
                pr   = LitNum emptyL numr
            return (fnName, FnExpr fnName ["a", "b", "c"] expr, [pl, pm, pr], (numl - numm) - numr)
        let expected = Right (ve)
            store    = M.fromList [(fn,fd)]
            expr     = FnCall emptyL fn plist
            actual   = E.eval expr store
        actual ~=== expected

hprop_evalExpressions :: Property
hprop_evalExpressions =
    -- Lower count because calculating exp is expensive
    withTests 3000 $ property $ do
        (fn, expr) <- forAll  $ Gen.resize exprSplitSize $ genExpr
        let expected = Right  $ fn
            actual = E.eval expr emptyS
        actual ~=== expected

