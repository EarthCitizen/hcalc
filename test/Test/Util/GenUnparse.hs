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

ff :: Expr -> Maybe Expr
ff f@FL_ = Just f
ff f@(FnCall _ _ _) = Just f
ff _ = Nothing

pattern FF e <- (ff -> Just e)
pattern FF_ <- (FF _)

lit :: Expr -> Maybe Expr
lit e@(Negate _ (LitNum _ _)) = Just e
lit e@(LitNum _ _) = Just e
lit _ = Nothing

pattern L e <- (lit -> Just e)

data Unparse  a = ZeroPlus (NonEmpty (Unparse a))
                | None     (NonEmpty (Unparse a))
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

pattern IFX o el er <- (OperInf _ o el er)

mkOpToken :: Operator -> Unparse String
mkOpToken o = Token $ operString $ o

unparseExpr :: Expr -> Unparse String
unparseExpr (FL f)                                     = ZeroPlus [Token $ show f]
unparseExpr (IFX op (FF el)          er@(IFX oc _ _))  = ZeroPlus [unparseExpr el,   mkOpToken op, parenr op oc er ]
unparseExpr (IFX op el@(IFX oc  _ _) (FF er))          = ZeroPlus [parenl op oc el,  mkOpToken op, unparseExpr er  ]
unparseExpr (IFX op el@(IFX ocl _ _) er@(IFX ocr _ _)) = ZeroPlus [parenl op ocl el, mkOpToken op, parenr op ocr er]
unparseExpr (IFX op el er)  = ZeroPlus [unparseExpr el, mkOpToken op, unparseExpr er]
-- We are missing a case for negated function calls here
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
unparseFnArgs (e@NFL_:es) = None [Token "(", unparseExpr e, Token ")"] : unparseFnArgs es
unparseFnArgs (e@FL_ :es) = unparseExpr e : unparseFnArgs es
unparseFnArgs (e     :es) = None [Token "(", unparseExpr e, Token ")"] : unparseFnArgs es

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

