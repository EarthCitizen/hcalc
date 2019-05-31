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

litf :: Expr -> Maybe Float50
litf (Negate _ (LitNum _ f)) = Just (negate f)
litf (LitNum _ f) = Just f
litf _ = Nothing

pattern FL f <- (litf -> Just f)
pattern FL_ <- (FL _)

nlitf :: Expr -> Maybe Float50
nlitf (Negate _ (LitNum _ f)) = Just (negate f)
nlitf _ = Nothing

pattern NFL f <- (nlitf -> Just f)
pattern NFL_ <- (NFL _)

expp :: Expr -> Maybe (String, Expr, Expr)
expp (OperExp _ el er) = Just ("^", el, er)
expp _ = Nothing

pattern EX o el er <- (expp -> Just (o, el, er))
pattern EX_ <- (EX _ _ _)

gteqExp :: Expr -> Bool
gteqExp (FnCall _ _ _) = True
gteqExp (OperExp _ _ _) = True
gteqExp _ = False

pattern GEX <- (gteqExp -> True)

addSub :: Expr -> Maybe (String, Expr, Expr)
addSub (OperAdd _ el er) = Just ("+", el, er)
addSub (OperSub _ el er) = Just ("-", el, er)
addSub _ = Nothing

pattern AS o el er <- (addSub -> Just (o, el, er))
pattern AS_ <- (addSub -> Just (_, _, _))

gteqAddSub :: Expr -> Bool
gteqAddSub (FnCall _ _ _) = True
gteqAddSub (OperExp _ _ _) = True
gteqAddSub (OperDiv _ _ _) = True
gteqAddSub (OperMul _ _ _) = True
gteqAddSub (OperAdd _ _ _) = True
gteqAddSub (OperSub _ _ _) = True
gteqAddSub _ = False

pattern GAS <- (gteqAddSub -> True)

mulDiv :: Expr -> Maybe (String, Expr, Expr)
mulDiv (OperDiv _ el er) = Just ("/", el, er)
mulDiv (OperMul _ el er) = Just ("*", el, er)
mulDiv _ = Nothing

pattern MD o el er <- (mulDiv -> Just (o, el, er))
pattern MD_ <- (MD _ _ _)

gteqMulDiv :: Expr -> Bool
gteqMulDiv (FnCall _ _ _) = True
gteqMulDiv (OperExp _ _ _) = True
gteqMulDiv (OperDiv _ _ _) = True
gteqMulDiv (OperMul _ _ _) = True
gteqMulDiv _ = False

pattern GMD <- (gteqMulDiv -> True)

lit :: Expr -> Maybe Expr
lit e@(Negate _ (LitNum _ _)) = Just e
lit e@(LitNum _ _) = Just e
lit _ = Nothing

pattern L e <- (lit -> Just e)

binOp :: Expr -> Maybe (String, Expr, Expr)
binOp (EX o el er) = Just (o, el, er)
binOp (MD o el er) = Just (o, el, er)
binOp (AS o el er) = Just (o, el, er)
binOp _ = Nothing

pattern BO o el er <- (binOp -> Just (o, el, er))
pattern BL o el er <- (BO o (L el) (L er))

data Unparse  a = ZeroPlus (NonEmpty (Unparse a))
                | OnePlus  (NonEmpty (Unparse a))
                | None     (NonEmpty (Unparse a))
                | Token a
                deriving Show

opNone :: String -> Expr -> Expr -> Unparse String
opNone o el er = ZeroPlus [unparseExpr el, Token o, unparseExpr er]

opLeft :: String -> Expr -> Expr -> Unparse String
opLeft o el er = ZeroPlus [OnePlus [unparseExpr el], Token o, unparseExpr er]


opRght :: String -> Expr -> Expr -> Unparse String
opRght o el er = ZeroPlus [unparseExpr el, Token o, OnePlus [unparseExpr er]]

opBoth :: String -> Expr -> Expr -> Unparse String
opBoth o el er = ZeroPlus [OnePlus [unparseExpr el], Token o, OnePlus [unparseExpr er]]

unparseExpr :: Expr -> Unparse String
unparseExpr (FL f)               = ZeroPlus [Token $ show f]
unparseExpr (BL o el er)         = opNone o el er
unparseExpr (EX o (L el) er@GEX) = opNone o el er
unparseExpr (EX o el@GEX er@GEX) = opNone o el er
unparseExpr (EX o el@GEX (L er)) = opNone o el er
unparseExpr (EX o (L el) er)     = opRght o el er
unparseExpr (EX o el (L er))     = opLeft o el er
unparseExpr (EX o el er)         = opBoth o el er
unparseExpr (MD o el@GMD (L er)) = opNone o el er
unparseExpr (MD o el@GMD er@GMD) = opNone o el er
unparseExpr (MD o (L el) er@GMD) = opNone o el er
unparseExpr (MD o el (L er))     = opLeft o el er
unparseExpr (MD o (L el) er)     = opRght o el er
unparseExpr (MD o el er)         = opBoth o el er
unparseExpr (AS o el@GAS (L er)) = opNone o el er
unparseExpr (AS o el@GAS er@GAS) = opNone o el er
unparseExpr (AS o (L el) er@GAS) = opNone o el er
unparseExpr (AS o el er)         = opNone o el er
unparseExpr (Negate _ e)    = ZeroPlus [Token "-(", unparseExpr e, Token ")"]
unparseExpr (FnCall _ n []) = ZeroPlus [Token n]
unparseExpr (FnCall _ n es) =
    let tks   = repeat $ Token " "
        upfas = merge tks $ unparseFnArgs es
     in ZeroPlus $ Token n <| N.fromList upfas

merge :: [a] -> [a] -> [a]
merge [] [] = []
merge [] _  = []
merge _  [] = []
merge (a:as) (b:bs) = a : b : merge as bs

split :: [a] -> [[a]]
split = map $ \a -> [a]

rangeMax = 12 :: Int

unparseFnArgs :: [Expr] -> [Unparse String]
unparseFnArgs []         = []
unparseFnArgs (e@NFL_:es) = OnePlus [unparseExpr e] : unparseFnArgs es
unparseFnArgs (e@FL_ :es) = unparseExpr e : unparseFnArgs es
unparseFnArgs (e     :es) = OnePlus [unparseExpr e] : unparseFnArgs es

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
unparseToString (OnePlus  (x :| xs)) = "(" ++ unparseToString x ++ (foldMap unparseToString xs) ++ ")"
unparseToString (None     (x :| xs)) = unparseToString x ++ (foldMap unparseToString xs)
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
genUnparse (OnePlus us) = do
    bs         <- genSpacing
    as         <- genSpacing
    (ops, cps) <- genParens 1
    cs         <- foldlM unparseFoldMap "" us
    return (bs ++ ops ++ cs ++ cps ++ as)
genUnparse (None us) = do
    bs <- genSpacing
    as <- genSpacing
    cs <- foldlM unparseFoldMap "" us
    return (bs ++ cs ++ as)

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

