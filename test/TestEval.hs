module TestEval where

import Debug.Trace (trace, traceM)

import AST
import Alias
import Error (Error)
import FnStore (emptyFnStore)
import GHC.Stack (HasCallStack)
import Hedgehog
import Predef
import Test.Util.Data
import Test.Util.GenEval

import qualified Data.Map.Strict as M
import qualified Eval as E
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

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

hprop_eval_Adds :: Property
hprop_eval_Adds =
    withTests testCount $ property $ do
        (lo, ro) <- forAll $ do
            lo' <- genFloat
            ro' <- genFloat
            return (lo', ro')
        let expected = Right (lo + ro)
            expr     = operAdd_ (LitNum emptyL lo) (LitNum emptyL ro)
            actual   = E.eval expr emptyFnStore
        actual ~=== expected

hprop_eval_Subtracts :: Property
hprop_eval_Subtracts =
    withTests testCount $ property $ do
        (lo, ro) <- forAll $ do
            lo' <- genFloat
            ro' <- genFloat
            return (lo', ro')
        let expected = Right (lo - ro)
            expr     = operSub_ (LitNum emptyL lo) (LitNum emptyL ro)
            actual   = E.eval expr emptyFnStore
        actual ~=== expected

hprop_eval_Multiplies :: Property
hprop_eval_Multiplies =
    withTests testCount $ property $ do
        (lo, ro) <- forAll $ do
            lo' <- genFloat
            ro' <- genFloat
            return (lo', ro')
        let expected = Right (lo * ro)
            expr     = operMul_ (LitNum emptyL lo) (LitNum emptyL ro)
        E.eval expr emptyFnStore ~=== expected

hprop_eval_Divides :: Property
hprop_eval_Divides =
    withTests testCount $ property $ do
        (lo, ro) <- forAll $ do
            lo' <- genFloat
            ro' <- genFloat
            return (lo', ro')
        let expected = Right (lo / ro)
            expr     = operDiv_ (LitNum emptyL lo) (LitNum emptyL ro)
            actual   = E.eval expr emptyFnStore
        actual ~=== expected

hprop_eval_Raises :: Property
hprop_eval_Raises =
    withTests 3000 $ property $ do
        (lo, ro) <- forAll $ do
            lo' <- genFloat
            ro' <- genFloatBetween (-10) 10
            return (lo', ro')
        let expected = Right (lo ** ro)
            expr     = operExp_ (LitNum emptyL lo) (LitNum emptyL ro)
            actual   = E.eval expr emptyFnStore
        actual ~=== expected

hprop_eval_ExecutesNullaryFunctions :: Property
hprop_eval_ExecutesNullaryFunctions =
    withTests testCount $ property $ do
        (fn, fd, v) <- forAll $ do
            fnName <- genFnName
            num    <- genFloat
            let expr = litNum_ num
            return (fnName, FnExpr fnName [] expr, num)
        let expected = Right (v)
            store    = M.fromList [(fn,fd)]
            expr     = FnCall emptyL fn []
            actual   = E.eval expr store
        actual ~=== expected

hprop_eval_ExecutesBinaryFunctions :: Property
hprop_eval_ExecutesBinaryFunctions =
    withTests testCount $ property $ do
        (fn, fd, plist, ve) <- forAll $ do
            fnName <- genFnName
            numl   <- genFloat
            numr   <- genFloat
            let fl   = fnCall_ "a" []
                fr   = fnCall_ "b" []
                expr = operSub_ fl fr
                pl   = litNum_ numl
                pr   = litNum_ numr
            return (fnName, FnExpr fnName ["a", "b"] expr, [pl, pr], numl - numr)
        let expected = Right (ve)
            store    = M.fromList [(fn,fd)]
            expr     = fnCall_ fn plist
            actual   = E.eval expr store
        actual ~=== expected

hprop_eval_ExecutesTernaryFunctions :: Property
hprop_eval_ExecutesTernaryFunctions =
    withTests testCount $ property $ do
        (fn, fd, plist, ve) <- forAll $ do
            fnName <- genFnName
            numl   <- genFloat
            numm   <- genFloat
            numr   <- genFloat
            let fl   = fnCall_ "a" []
                fm   = fnCall_ "b" []
                fr   = fnCall_ "c" []
                expr = operSub_ (operSub_ fl fm) fr
                pl   = litNum_ numl
                pm   = litNum_ numm
                pr   = litNum_ numr
            return (fnName, FnExpr fnName ["a", "b", "c"] expr, [pl, pm, pr], (numl - numm) - numr)
        let expected = Right (ve)
            store    = M.fromList [(fn,fd)]
            expr     = fnCall_ fn plist
            actual   = E.eval expr store
        actual ~=== expected

hprop_eval_Expressions :: Property
hprop_eval_Expressions =
    -- Lower count because calculating exp is expensive
    withTests 3000 $ property $ do
        ((fn, expr), fs) <- forAllStateGen $ Gen.resize exprSplitSize $ genExpr
        let expected = Right  $ fn
            actual = E.eval expr fs
        actual ~=== expected

