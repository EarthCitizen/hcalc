module Test.Util.GenUnparse where

import Alias
import AST
import Parse (parseLine)

import Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

litf :: Expr -> Maybe Float50
litf (Negate _ (LitNum _ f)) = Just (negate f)
litf (LitNum _ f) = Just f
litf _ = Nothing

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

data Unparse  a = AnyParenSpace a | AnySpace a | None a deriving Show

testEts = \s -> (head <$> parseLine s) >>= \(StmtExpr _ expr) -> return $ ets2 expr

ets2 :: Expr -> [String]
ets2 (litf -> Just f)     = [show f]
ets2 (BL o el er)         = ets2 el ++ [o] ++ ets2 er
ets2 (EX o (L el) er@GEX) = ets2 el ++ [o] ++ ets2 er
ets2 (EX o (L el) er)     = ets2 el ++ [o] ++ wpn er
ets2 (EX o el (L er))     = wpn el  ++ [o] ++ ets2 er
ets2 (EX o el er)         = wpn el  ++ [o] ++ wpn er
ets2 (MD o el@GMD (L er)) = ets2 el ++ [o] ++ ets2 er
ets2 (MD o el@GMD er@GMD) = ets2 el ++ [o] ++ ets2 er
ets2 (MD o (L el) er@GMD) = ets2 el ++ [o] ++ ets2 er
ets2 (MD o el@AS_ (L er)) = wpn el  ++ [o] ++ ets2 er
ets2 (MD o (L el) er)     = ets2 el ++ [o] ++ wpn er
ets2 (MD o el er)         = wpn el  ++ [o] ++ wpn er
ets2 (AS o el@GAS (L er)) = ets2 el ++ [o] ++ ets2 er
ets2 (AS o el@GAS er@GAS) = ets2 el ++ [o] ++ ets2 er
ets2 (AS o (L el) er@GAS) = ets2 el ++ [o] ++ ets2 er
ets2 (AS o (L el) er)     = ets2 el ++ [o] ++ wpn er
ets2 (AS o el er)         = wpn el  ++ [o] ++ wpn er
ets2 (Negate _ e) = ["-("] ++ ets2 e ++ [")"]
ets2 (FnCall _ n es) = [n]

wpn :: Expr -> [String]
wpn e@(Negate _ _) = ets2 e
wpn e = wp $ ets2 e

wp :: [String] -> [String]
wp ss = ["("] ++ ss ++ [")"]

aps = AnyParenSpace
as = AnySpace

unparse :: Expr -> [Unparse String]
unparse (litf -> Just f)     = [aps $ show f]
unparse (BL o el er)         = unparse el ++ [o] ++ unparse er
unparse (EX o (L el) er@GEX) = unparse el ++ [o] ++ unparse er
unparse (EX o (L el) er)     = unparse el ++ [o] ++ wpn er
unparse (EX o el (L er))     = wpn el  ++ [o] ++ unparse er
unparse (EX o el er)         = wpn el  ++ [o] ++ wpn er
unparse (MD o el@GMD (L er)) = unparse el ++ [o] ++ unparse er
unparse (MD o el@GMD er@GMD) = unparse el ++ [o] ++ unparse er
unparse (MD o (L el) er@GMD) = unparse el ++ [o] ++ unparse er
unparse (MD o el@AS_ (L er)) = wpn el  ++ [o] ++ unparse er
unparse (MD o (L el) er)     = unparse el ++ [o] ++ wpn er
unparse (MD o el er)         = wpn el  ++ [o] ++ wpn er
unparse (AS o el@GAS (L er)) = unparse el ++ [o] ++ unparse er
unparse (AS o el@GAS er@GAS) = unparse el ++ [o] ++ unparse er
unparse (AS o (L el) er@GAS) = unparse el ++ [o] ++ unparse er
unparse (AS o (L el) er)     = unparse el ++ [o] ++ wpn er
unparse (AS o el er)         = wpn el  ++ [o] ++ wpn er
unparse (Negate _ e) = ["-("] ++ unparse e ++ [")"]
unparse (FnCall _ n es) = [n]
