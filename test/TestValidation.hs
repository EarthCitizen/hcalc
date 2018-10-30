module TestValidation where

import Alias
import AST
import Error
import Validation
import TestGen (genFnName)
import Hedgehog
import Hedgehog.Internal.Source (HasCallStack)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

emptyL = ("", 0, 0)

isError :: Either Error FnDef -> Bool
isError (Left (Error _)) = True
isError _ = False

hprop_validateFnDef_paramNameCannotMatchFunName :: Property
hprop_validateFnDef_paramNameCannotMatchFunName =
    property $ do
        (fn, ps) <- forAll $ do
            fn <- genFnName
            let p1 = fn
                p2 = fn ++ "x"
            return (fn, [p1, p2])
        let fnDef = FnExpr fn ps $ LitNum emptyL 5
        assert $ isError $ validateFnDef fnDef

hprop_validateFnDef_paramNameCannotBeDup :: Property
hprop_validateFnDef_paramNameCannotBeDup =
    property $ do
        (fn, ps) <- forAll $ do
            fn <- genFnName
            let p1 = fn ++ "y"
                p2 = fn ++ "x"
            return (fn, [p1, p2, p1])
        let fnDef = FnExpr fn ps $ LitNum emptyL 5
        assert $ isError $ validateFnDef fnDef

hprop_validateFnDef_passesValidFunction :: Property
hprop_validateFnDef_passesValidFunction =
    property $ do
        (fn, ps) <- forAll $ do
            fn <- genFnName
            let p1 = fn ++ "y"
                p2 = fn ++ "x"
            return (fn, [p1, p2])
        let ln    = LitNum emptyL 5
            fnDef = FnExpr fn ps ln
            isSameFnDef (FnExpr f1 p1 l1) =
                assert $ f1 == fn && p1 == ps && l1 == ln
            isSameFnDef _ = failure
        evalEither (validateFnDef fnDef) >>= isSameFnDef
