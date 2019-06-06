{-# LANGUAGE OverloadedLists #-}

module Test.Util.GenUnparse (genUnparseStmts) where

import Alias
import AST
import Parse (parseLine)

import Control.Monad.ListM (intercalateM)
import Data.Foldable (foldlM)
import Data.List (tails)
import Data.List.NonEmpty ((<|), NonEmpty(..))
import Hedgehog

import qualified Data.List.NonEmpty as N
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

litn :: Expr -> Maybe Float50
litn (LitNum _ f) = Just f
litn _ = Nothing

pattern LN f <- (litn -> Just f)
pattern LN_ <- (LN _)

fncl :: Expr -> Bool
fncl (FnCall _ _ _) = True
fncl _ = False

pattern FN_ <- (fncl -> True)

pattern NG e <- (Negate _ e)
pattern NG_  <- (Negate _ _)

term :: Expr -> Bool
term (LN_) = True
term (FN_) = True
term (NG_) = True
term _ = False

pattern TM_ <- (term -> True)

data Unparse  a = ZeroPlus (NonEmpty (Unparse a))
                | None     (NonEmpty (Unparse a))
                | OneOf    (NonEmpty (Unparse a))
                | Token a
                deriving Show

op = \e -> None [Token "(", unparseExpr e, Token ")"]
np = \e -> None [unparseExpr e]

pattern OPFP f p <- (OpInfix _ f p _)

parenl :: Operator -> Operator -> Expr -> Unparse String
parenl (OPFP RFix pp) (OPFP RFix pc) e | pp >= pc = op e
parenl (OPFP _ pp)    (OPFP _ pc)    e | pp >  pc = op e 
parenl _ _ e = np e


parenr :: Operator -> Operator -> Expr -> Unparse String
parenr (OPFP LFix pp) (OPFP LFix pc) e | pp >= pc = op e
parenr (OPFP _ pp)    (OPFP _ pc)    e | pp >  pc = op e
parenr _ _ e = np e

inFix :: Expr -> Maybe (Operator, Expr, Expr)
inFix (OperInf _ o el er) = Just (o, el, er)
inFix _ = Nothing

pattern IF o el er <- (OperInf _ o el er)

mkOpToken :: Operator -> Unparse String
mkOpToken o = Token $ operString $ o

prefixNeg (LitNum l f)    = unparseExpr (LitNum l (negate f))
prefixNeg (FnCall l n es) = unparseExpr (FnCall l ('-':n) es)

parenNeg e = ZeroPlus [Token "-(", unparseExpr e, Token ")"]

unparseExpr :: Expr -> Unparse String
unparseExpr (LN f)                                  = ZeroPlus [Token $ show f]
unparseExpr (IF op el@TM_          er@(IF oc _ _))  = ZeroPlus [unparseExpr el,   mkOpToken op, parenr op oc er ]
unparseExpr (IF op el@(IF oc  _ _) er@TM_)          = ZeroPlus [parenl op oc el,  mkOpToken op, unparseExpr er  ]
unparseExpr (IF op el@(IF ocl _ _) er@(IF ocr _ _)) = ZeroPlus [parenl op ocl el, mkOpToken op, parenr op ocr er]
unparseExpr (IF op el er)   = ZeroPlus [unparseExpr el, mkOpToken op, unparseExpr er]
unparseExpr (NG e@TM_) = OneOf    [prefixNeg e, parenNeg e]
unparseExpr (NG e)     = parenNeg e
unparseExpr (FnCall _ n [])  = ZeroPlus [Token n]
unparseExpr (FnCall _ n es)  =
    let tks   = repeat $ Token " "
        upfas = merge tks $ unparseFnArgs es
     in ZeroPlus $ Token n <| N.fromList upfas

merge :: [a] -> [a] -> [a]
merge [] [] = []
merge [] _  = []
merge _  [] = []
merge (a:as) (b:bs) = a : b : merge as bs

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace a b (x:xs) = (if x == a then b else x) : replace a b xs

split :: [a] -> [[a]]
split = map $ \a -> [a]

rangeMax = 12 :: Int

unparseFnArgs :: [Expr] -> [Unparse String]
unparseFnArgs []         = []
unparseFnArgs (e@LN_ :es)           = unparseExpr e : unparseFnArgs es
-- Handles negation, function calls, subexpressions
unparseFnArgs (e     :es)           = None [Token "(", unparseExpr e, Token ")"] : unparseFnArgs es

unparseFnDef :: FnDef -> Unparse String
unparseFnDef (FnExpr n [] e) = None [Token n, Token "=", unparseExpr e]
unparseFnDef (FnExpr n ps e) =
    let tks  = repeat $ Token " "
        upps = merge tks $ (Token <$> ps)
     in None $ (Token n <| N.fromList upps) <> [Token "=", unparseExpr e]

unparseStmt :: Stmt -> Unparse String
unparseStmt (StmtFnDef _ fd) = unparseFnDef fd
unparseStmt (StmtExpr  _ ex) = unparseExpr  ex

unparseStmts :: [Stmt] -> [Unparse String]
unparseStmts [] = []
unparseStmts ss = unparseStmt <$> ss

unparseToString :: Unparse String -> String
unparseToString (ZeroPlus (x :| xs)) = "_ " ++ unparseToString x ++ (foldMap unparseToString xs) ++ " _"
unparseToString (None     (x :| xs)) = unparseToString x ++ (foldMap unparseToString xs)
unparseToString (OneOf    (x :| xs)) = " | " ++ unparseToString x ++ (foldMap (\u -> " OR " ++ unparseToString u) xs) ++ " | "
unparseToString (Token s) = s

genParens :: (MonadGen m) => Int -> m (String, String)
genParens min = do
    ocs <- genChars min rangeMax ['(']
    let ccs = replicate (length ocs) ')'
    oss <- intercalateM genSpacing $ split ocs
    css <- intercalateM genSpacing $ split ccs
    return (oss, css)

genSemicolons :: (MonadGen m) => m String
genSemicolons = do
    bs <- genSpacing
    ss <- (split <$> genChars 1 rangeMax [';']) >>= (intercalateM genSpacing)
    as <- genSpacing
    return (bs ++ ss ++ as)

genChars :: (MonadGen m) => Int -> Int -> [Char] -> m String
genChars min max cs = let r = Range.linear min max
                          a = Gen.element cs
                          f = \xs -> length xs >= min
                          g = \xs -> filter f $ tails $ drop 1 xs
                       in Gen.shrink g $ Gen.prune $ Gen.list r a

genSpacing :: (MonadGen m) => m String
genSpacing = genChars 0 rangeMax [' ', '\t']

genUnparse :: (MonadGen m) => Unparse String -> m String
genUnparse (Token s) = do
    bs <- genSpacing
    as <- genSpacing
    return (bs ++ s ++ as)
genUnparse (ZeroPlus us) = do
    bs         <- genSpacing
    as         <- genSpacing
    (ops, cps) <- genParens 0
    cs         <- foldlM unparseFoldMap "" us
    return (bs ++ ops ++ cs ++ cps ++ as)
genUnparse (None us) = do
    bs <- genSpacing
    as <- genSpacing
    cs <- foldlM unparseFoldMap "" us
    return (bs ++ cs ++ as)
genUnparse (OneOf us) = do
    Gen.element (N.toList us) >>= genUnparse

unparseFoldMap :: (MonadGen m) => String -> Unparse String -> m String
unparseFoldMap s u = do
    ups <- genUnparse u
    return (s ++ ups)

fromRight (Right ss) = ss

genUnparseStmt :: (MonadGen m) => Stmt -> m String
genUnparseStmt (StmtFnDef _ fd) = genUnparse $ unparseFnDef fd
genUnparseStmt (StmtExpr  _ ex) = genUnparse $ unparseExpr  ex

genUnparseStmts :: (MonadGen m) => [Stmt] -> m String
genUnparseStmts [] = return ""
genUnparseStmts ss = (mapM genUnparseStmt ss) >>= (intercalateM genSemicolons)

