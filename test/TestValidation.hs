module TestValidation where

import Alias
import AST
import Error
import Runtime
import Validation
import TestGen (genFnName)
import Control.Monad.Except (Except, MonadError (catchError), runExcept)
import Control.Monad.Reader (runReader)
import Hedgehog
import Hedgehog.Internal.Source (HasCallStack)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

emptyL = ("", 0, 0)

isError :: Either Error a -> Bool
isError (Left (Error _)) = True
isError _ = False

-- instance MonadRuntime State where
--     getFnStore = 

assertThrowsErrror :: (MonadError Error m, MonadTest m, HasCallStack) => m a -> m ()
assertThrowsErrror f = let go = f >> failure
                        in catchError go (\e -> success)

hprop_validate_paramNameCannotMatchFunName :: Property
hprop_validate_paramNameCannotMatchFunName =
    property $ do
        (fn, ps) <- forAll $ do
            fn <- genFnName
            let p1 = fn
                p2 = fn ++ "x"
            return (fn, [p1, p2])
        let fnDef = FnExpr fn ps $ LitNum emptyL 5
            stmtFnDef = StmtFnDef emptyL fnDef
        assert $ isError $ runExcept (validate stmtFnDef :: Except Error Stmt)

hprop_validate_paramNameCannotBeDup :: Property
hprop_validate_paramNameCannotBeDup =
    property $ do
        (fn, ps) <- forAll $ do
            fn <- genFnName
            let p1 = fn ++ "y"
                p2 = fn ++ "x"
            return (fn, [p1, p2, p1])
        let fnDef = FnExpr fn ps $ LitNum emptyL 5
            stmtFnDef = StmtFnDef emptyL fnDef
        assert $ isError $ runExcept (validate stmtFnDef :: Except Error Stmt)

hprop_validate_passesValidFunction :: Property
hprop_validate_passesValidFunction =
    property $ do
        (fn, ps) <- forAll $ do
            fn <- genFnName
            let p1 = fn ++ "y"
                p2 = fn ++ "x"
            return (fn, [p1, p2])
        let ln    = LitNum emptyL 5
            fnDef = FnExpr fn ps ln
            stmtFnDef = StmtFnDef emptyL fnDef
            isSameFnDef (StmtFnDef _ (FnExpr f1 p1 l1)) =
                assert $ f1 == fn && p1 == ps && l1 == ln
            isSameFnDef _ = failure
        (evalEither $ runExcept $ (validate stmtFnDef :: Except Error Stmt)) >>= isSameFnDef

