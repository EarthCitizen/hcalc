module TestParse where

import AST
import Parse (parseLine, parseStmt)
import Test.Util.Data
import Test.Util.GenParse

import Control.Monad.Extra (concatMapM)
import Control.Monad.ListM (intercalateM)
import Hedgehog
import Test.Tasty (TestTree)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Hedgehog.Internal.Property (Property(..), PropertyConfig(..))

-- The current weakness is these tests is
-- that tokens without parentheses

wp :: [String] -> [String]
wp xs = ["("] ++ xs ++ [")"]

-- Subtraction after function call must be rendered
-- with space after or it will get interpreted as
-- a function parameter
ets :: Expr -> [String]
ets (LitNum  _ d) = wp [show d]
ets (Negate  _ e) = let (x:xs) = ets e in ('-':x):xs
ets (OperExp _ el er) = wp $ ets el ++ ["^"] ++ ets er
ets (OperMul _ el er) = wp $ ets el ++ ["*"] ++ ets er
ets (OperDiv _ el er) = wp $ ets el ++ ["/"] ++ ets er
ets (OperAdd _ el er) = wp $ ets el ++ ["+"] ++ ets er
-- ets (OperSub _ el@(FnCall _ _ _) er) = wp $ ets el ++ ["- "] ++ ets er
ets (OperSub _ el er) = wp $ ets el ++ ["-"] ++ ets er
-- ets (FnCall  _ n  es@(FnCall _ _ _:_)) = [n ++ " "] ++ concatMap ets es
ets (FnCall  _ n  es) = wp $ [n] ++ concatMap ets es

fdts :: FnDef -> [String]
fdts (FnExpr n ps e) = [n] ++ ((' ':) <$> ps) ++ ["="] ++ ets e

sts :: Stmt -> [String]
sts (StmtExpr _ e)   = ets e
sts (StmtFnDef _ fd) = fdts fd

genStringExpr :: (MonadGen m) => m (String, Expr)
genStringExpr = genExpr >>= \e -> do
    let es = ets e
    bs <- genSpacing
    is <- intercalateM genSpacing es
    as <- genSpacing
    return (bs ++ is ++ as, e)

genStringStmt :: (MonadGen m) => m (String, Stmt)
genStringStmt = genStmt >>= \s -> do
    let ss = sts s
    bs <- genSpacing
    is <- intercalateM genSpacing ss
    as <- genSpacing
    return (bs ++ is ++ as, s)

-- Generate a list of expressions and turn
-- them into one String in which each
-- expression is separated by semicolons
genStringExprList :: (MonadGen m) => m (String, [Expr])
genStringExprList = do
    ses <- genList genStringExpr
    let es = map snd ses
        ss = map fst ses
    bs <- genSemicolonList
    is <- intercalateM genSemicolonList ss
    as <- genSemicolonList
    return (bs ++ is ++ as, es)

-- Generate a list of expressions and turn
-- them into one String in which each
-- expression is separated by semicolons
genStringStmtList :: (MonadGen m) => m (String, [Stmt])
genStringStmtList = do
    sss <- genList genStringStmt
    let es = map snd sss
        ss = map fst sss
    bs <- genSemicolonList
    is <- intercalateM genSemicolonList ss
    as <- genSemicolonList
    return (bs ++ is ++ as, es)

hprop_parseStmt_handlesWhitespace :: Property
hprop_parseStmt_handlesWhitespace =
    withTests 500 $ property $ do
        (s, e) <- forAll genStringExpr
        let expected = Right $ stmtExpr_ e
            actual   = parseStmt s
        annotate "Expected:"
        annotate $ show e
        annotate "Generated String:"
        annotate s
        annotate "Parsed Statement:"
        annotate $ show $ actual
        actual === expected

hprop_parseLine_handlesWhitespaceAndLineSpliting :: Property
hprop_parseLine_handlesWhitespaceAndLineSpliting =
    withTests 500 $ property $ do
        (s, ss) <- forAll genStringStmtList
        let expected = Right ss
            actual   = parseLine s
        actual === expected

