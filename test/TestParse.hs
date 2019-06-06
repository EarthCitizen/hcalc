module TestParse where

import AST
import Error (Error)
import Parse (parseLine, parseStmt)
import Test.Util.Data
import Test.Util.GenParse
import Test.Util.GenUnparse

import Control.Monad (forM_)
import Control.Monad.Extra (concatMapM)
import Control.Monad.ListM (intercalateM)
import Hedgehog
import Test.Hspec
import Test.Tasty (TestTree)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- import Hedgehog.Internal.Property (Property(..), PropertyConfig(..))

genStringStmts :: (MonadGen m) => m (String, [Stmt])
genStringStmts = do
    xs <- genStmts
    s  <- genUnparseStmts xs
    return (s, xs)

operPrec = [ ("1*2+3", operAdd_ (operMul_ (litNum_ 1) (litNum_ 2)) (litNum_ 3))
           , ("1/2-3", operSub_ (operDiv_ (litNum_ 1) (litNum_ 2)) (litNum_ 3))
           , ("1^2*3", operMul_ (operExp_ (litNum_ 1) (litNum_ 2)) (litNum_ 3))
           ]

-- ^ is right associative
operAsoc = [ ("1^2^3", operExp_ (litNum_ 1) (operExp_ (litNum_ 2) (litNum_ 3)))
           , ("1*2*3", operMul_ (operMul_ (litNum_ 1) (litNum_ 2)) (litNum_ 3))
           , ("1/2/3", operDiv_ (operDiv_ (litNum_ 1) (litNum_ 2)) (litNum_ 3))
           , ("1+2+3", operAdd_ (operAdd_ (litNum_ 1) (litNum_ 2)) (litNum_ 3))
           , ("1-2-3", operSub_ (operSub_ (litNum_ 1) (litNum_ 2)) (litNum_ 3))
           ]

parenths = [ ("(1^2)^3", operExp_ (operExp_ (litNum_ 1) (litNum_ 2)) (litNum_ 3))
           , ("(1*2)*3", operMul_ (operMul_ (litNum_ 1) (litNum_ 2)) (litNum_ 3))
           , ("(1/2)/3", operDiv_ (operDiv_ (litNum_ 1) (litNum_ 2)) (litNum_ 3))
           , ("(1+2)+3", operAdd_ (operAdd_ (litNum_ 1) (litNum_ 2)) (litNum_ 3))
           , ("(1-2)-3", operSub_ (operSub_ (litNum_ 1) (litNum_ 2)) (litNum_ 3))
           , ("1*(2+3)", operMul_ (litNum_ 1) (operAdd_ (litNum_ 2) (litNum_ 3)))
           , ("1/(2-3)", operDiv_ (litNum_ 1) (operSub_ (litNum_ 2) (litNum_ 3)))
           , ("1^(2*3)", operExp_ (litNum_ 1) (operMul_ (litNum_ 2) (litNum_ 3)))
           ]

spec_parseLine :: Spec
spec_parseLine = do
    describe "order of operations" $ do
        it "respects operator precedence" $ forM_ operPrec $
            \(input, expectation) -> do
                parseLine input `shouldBe` Right [stmtExpr_ expectation]
        it "respects operator associativity" $ forM_ operAsoc $
            \(input, expectation) -> do
                parseLine input `shouldBe` Right [stmtExpr_ expectation]
        it "respects parentheses" $ forM_ parenths $
            \(input, expectation) -> do
                parseLine input `shouldBe` Right [stmtExpr_ expectation]

hprop_parseLine_parses_expressions :: Property
hprop_parseLine_parses_expressions =
    withTests 500 $ property $ do
        (s, ss) <- forAll $ Gen.scale (\s -> min s 15) $ genStringStmts
        let expected = Right ss :: Either Error [Stmt]
            actual   = parseLine s
        annotate "Expected:"
        annotate $ show expected
        annotate "Generated String:"
        annotate s
        annotate "Parsed Statement:"
        annotate $ show $ actual
        actual === expected

