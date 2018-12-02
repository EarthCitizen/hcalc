module TestParse where

import Test.Tasty (TestTree)
import AST
import Parse (parseLine, parseStmt)
import Test.Util.Data (emptyL, testCount)
import Test.Util.Gen (forAllStateGen, genExpr, runStateGen)
import Control.Monad.Extra (concatMapM)
import Control.Monad.ListM (intercalateM)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

ge :: Gen Expr
ge = do ((_, e), _) <- runStateGen genExpr
        return e

genStringExpr :: Gen (String, Expr)
genStringExpr = ge >>= \e -> do
    let es = ets e
    bs <- genSpacing
    is <- intercalateM genSpacing es
    as <- genSpacing
    return (bs ++ is ++ as, e)

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
ets (OperSub _ el@(FnCall _ _ _) er) = wp $ ets el ++ ["- "] ++ ets er
ets (OperSub _ el er) = wp $ ets el ++ ["-"] ++ ets er
ets (FnCall  _ n  es) = [n] ++ concatMapM ets es


-- fdts :: FnDef -> Gen String
-- fdts (FnExpr n ps e) = do
--     bs <- genSpacing
--     ns <- (' ':) <$> genSpacing
--     as <- genSpacing


genExprList :: Gen [Expr]
genExprList = let r = Range.constant 1 100
               in Gen.list r ge

genSemicolonList :: (MonadGen m) => m String
genSemicolonList = do
    let r = Range.constant 1 50
    scs <- Gen.frequency [ (1, Gen.constant [";"])
                         , (2, Gen.list r $ Gen.constant ";")
                         ]
    bs  <- genSpacing
    is  <- intercalateM genSpacing scs
    as  <- genSpacing
    return $ bs ++ is ++ as

-- Generate a list of expressions and turn
-- them into one String in which each
-- expression is separated by semicolons
genStringExprList :: Gen (String, [Expr])
genStringExprList = do
    let r = Range.constant 1 100
    ses <- Gen.list r genStringExpr
    let es = map snd ses
        ss = map fst ses
    bs <- genSemicolonList
    is <- intercalateM genSemicolonList ss
    as <- genSemicolonList
    return (bs ++ is ++ as, es)

genSpacing :: (MonadGen m) => m String
genSpacing = let r = Range.constant 1 3
                 a = Gen.list r $ Gen.element [' ', '\t']
                 b = Gen.constant []
              in Gen.frequency [ (1, a)
                               , (2, b)
                               ]

hprop_parseStmt_handlesWhitespace :: Property
hprop_parseStmt_handlesWhitespace =
    withTests testCount $ property $ do
        (s, e) <- forAll genStringExpr
        let expected = Right $ StmtExpr emptyL e
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
    withTests testCount $ property $ do
        (s, es) <- forAll genStringExprList
        let expected = Right $ map (StmtExpr emptyL) es
            actual   = parseLine s
        actual === expected

