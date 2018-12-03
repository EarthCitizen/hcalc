{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TestValidate where

import Alias
import AST
import Error
import FnStore (FnStore, GetFnStore(..), emptyFnStore, putFn)
import Runtime
import Validate
import Test.Util.Data (emptyL)
import Test.Util.GenEval (genFnName)
import Control.Monad (forM_)
import Control.Monad.Except (Except, MonadError (catchError), runExcept, runExceptT)
import Control.Monad.Reader (MonadReader (ask), ReaderT, runReaderT)
import Hedgehog
import GHC.Stack (HasCallStack)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty (TestTree)
import Test.HUnit ((@?=))
import qualified Test.HUnit as HU (Test(..))
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Text.Printf (printf)

instance (Monad m) => GetFnStore (ReaderT FnStore m) where
    getFnStore = ask

newtype TestRuntime a = TestRuntime { unTestRuntime :: ReaderT FnStore (Except Error) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader FnStore
             , MonadError Error
             )

instance GetFnStore TestRuntime where
    getFnStore = TestRuntime $ getFnStore

runTestRuntime :: TestRuntime a -> FnStore -> Either Error a
runTestRuntime t  fs = let rd = runReaderT (unTestRuntime t) fs
                        in runExcept rd

-- test_something :: TestTree
-- test_something = testCase "First" $ assertEqual "This" 4 5

-- test_one :: TestTree
-- test_one = testCase "Value" $ do
--     let vs = [1,2,3,4,5] :: [Int]
--     forM_ vs $ \x ->
--         let msg = printf "%i should be less than 4" x
--          in assertBool msg (x < 4)

isError :: Either Error a -> Bool
isError (Left (Error _)) = True
isError _ = False

assertThrowsErrror :: (MonadError Error m, MonadTest m, HasCallStack) => m a -> m ()
assertThrowsErrror f = let go = f >> failure
                        in catchError go (\e -> success)

hprop_validate_cannotOverwriteROFunction :: Property
hprop_validate_cannotOverwriteROFunction =
    property $ do
        fn <- forAll genFnName
        let f  = FnReal fn [] $ FnNullary 5
            fs = putFn f emptyFnStore
            fd = FnExpr fn [] $ LitNum emptyL 5
            sd = StmtFnDef emptyL fd
        assert $ isError $ runTestRuntime (validate sd) fs

hprop_validate_paramNameCannotMatchFunName :: Property
hprop_validate_paramNameCannotMatchFunName =
    property $ do
        (fn, ps) <- forAll $ do
            fn <- genFnName
            let p1 = fn
                p2 = fn ++ "x"
            return (fn, [p1, p2])
        let fd = FnExpr fn ps $ LitNum emptyL 5
            sd = StmtFnDef emptyL fd
        assert $ isError $ runTestRuntime (validate sd) emptyFnStore

hprop_validate_paramNameCannotBeDup :: Property
hprop_validate_paramNameCannotBeDup =
    property $ do
        (fn, ps) <- forAll $ do
            fn <- genFnName
            let p1 = fn ++ "y"
                p2 = fn ++ "x"
            return (fn, [p1, p2, p1])
        let fd = FnExpr fn ps $ LitNum emptyL 5
            sd = StmtFnDef emptyL fd
        assert $ isError $ runTestRuntime (validate sd) emptyFnStore

hprop_validate_passesValidFunction :: Property
hprop_validate_passesValidFunction =
    property $ do
        (fn, ps) <- forAll $ do
            fn <- genFnName
            let p1 = fn ++ "y"
                p2 = fn ++ "x"
            return (fn, [p1, p2])
        let ln    = LitNum emptyL 5
            fd = FnExpr fn ps ln
            sd = StmtFnDef emptyL fd
            isSameFnDef (StmtFnDef _ (FnExpr f1 p1 l1)) =
                assert $ f1 == fn && p1 == ps && l1 == ln
            isSameFnDef _ = failure
        (evalEither $ runTestRuntime (validate sd) emptyFnStore) >>= isSameFnDef

