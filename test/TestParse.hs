module TestParse where

import AST
import Error (Error)
import Parse (parseLine, parseStmt)
import Test.Util.Data
import Test.Util.GenParse
import Test.Util.GenUnparse

import Control.Monad.Extra (concatMapM)
import Control.Monad.ListM (intercalateM)
import Hedgehog
import Test.Tasty (TestTree)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Hedgehog.Internal.Property (Property(..), PropertyConfig(..))

genStringStmts :: (MonadGen m) => m (String, [Stmt])
genStringStmts = do
    xs <- genStmts
    s  <- genUnparseStmts xs
    return (s, xs)

-- hprop_parseStmt_handlesWhitespace :: Property
-- hprop_parseStmt_handlesWhitespace =
--     withTests 500 $ property $ do
--         (s, e) <- forAll genStringExpr
--         let expected = Right $ stmtExpr_ e
--             actual   = parseStmt s
--         annotate "Expected:"
--         annotate $ show e
--         annotate "Generated String:"
--         annotate s
--         annotate "Parsed Statement:"
--         annotate $ show $ actual
--         actual === expected

hprop_parseLine :: Property
hprop_parseLine =
    withTests 500 $ property $ do
        (s, ss) <- forAll genStringStmts
        let expected = Right ss :: Either Error [Stmt]
            actual   = parseLine s
        annotate "Expected:"
        annotate $ show expected
        annotate "Generated String:"
        annotate s
        annotate "Parsed Statement:"
        annotate $ show $ actual
        actual === expected

