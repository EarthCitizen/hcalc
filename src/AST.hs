{-# LANGUAGE FlexibleInstances #-}

module AST ( FnRef(..)
           , FnDef(..)
           , Expr(..)
           , Stmt(..)
           , getExprLocation
           , getFnName
           , isFnReadOnly
           ) where

import Alias
import Text.Printf (printf)

instance Show (Float50 -> Float50) where
    show _ = "(E -> E)"

instance Show (Float50 -> Float50 -> Float50) where
    show _ = "(E -> E -> E)"

instance Show (Float50 -> Float50 -> Float50 -> Float50) where
    show _ = "(E -> E -> E -> E)"

data FnRef = FnNullary Float50
           | FnUnary   (Float50 -> Float50)
           | FnBinary  (Float50 -> Float50 -> Float50)
           | FnTernary (Float50 -> Float50 -> Float50 -> Float50)
           deriving (Show)

data FnDef = FnReal Name Params FnRef
           | FnExpr Name Params Expr
           deriving (Show)

instance Eq FnDef where
    (FnReal na pa _) == (FnReal nb pb _) =
        na == nb && pa == pb
    (FnExpr na pa ea) == (FnExpr nb pb eb) =
        na == nb && pa == pb && ea == eb
    _ == _ = False

data Expr = LitNum  Location Float50
          | Negate  Location Expr
          | OperExp Location Expr Expr
          | OperMul Location Expr Expr
          | OperDiv Location Expr Expr
          | OperAdd Location Expr Expr
          | OperSub Location Expr Expr
          | FnCall  Location Name [Expr]

instance Show Expr where
    show (LitNum  _ f) = printf "LitNum %s"   (show f)
    show (Negate  _ e) = printf "Negate (%s)" (show e)
    show (OperExp _ el er) = printf "OperExp (%s) (%s)" (show el) (show er)
    show (OperMul _ el er) = printf "OperMul (%s) (%s)" (show el) (show er)
    show (OperDiv _ el er) = printf "OperDiv (%s) (%s)" (show el) (show er)
    show (OperAdd _ el er) = printf "OperAdd (%s) (%s)" (show el) (show er)
    show (OperSub _ el er) = printf "OperSub (%s) (%s)" (show el) (show er)
    show (FnCall  _ n es)  = printf "FnCall %s %s" n (show es)

instance Eq Expr where
    (LitNum  _ fl) == (LitNum  _ fr) =
        fl == fr
    (Negate _ el) ==  (Negate _ er) =
        el == er
    (OperExp _ eal ear) == (OperExp _ ebl ebr) =
        eal == ebl && ear == ebr
    (OperMul _ eal ear) == (OperMul _ ebl ebr) =
        eal == ebl && ear == ebr
    (OperDiv _ eal ear) == (OperDiv _ ebl ebr) =
        eal == ebl && ear == ebr
    (OperAdd _ eal ear) == (OperAdd _ ebl ebr) =
        eal == ebl && ear == ebr
    (OperSub _ eal ear) == (OperSub _ ebl ebr) =
        eal == ebl && ear == ebr
    (FnCall  _ nl esl) == (FnCall  _ nr esr) =
        nl == nr && esl == esr
    _ == _ = False

getExprLocation :: Expr -> Location
getExprLocation (LitNum  l _)   = l
getExprLocation (Negate  l _)   = l
getExprLocation (OperExp l _ _) = l
getExprLocation (OperMul l _ _) = l
getExprLocation (OperDiv l _ _) = l
getExprLocation (OperAdd l _ _) = l
getExprLocation (OperSub l _ _) = l
getExprLocation (FnCall  l _ _) = l

data Stmt = StmtFnDef Location FnDef
          | StmtExpr  Location Expr

instance Show Stmt where
    show (StmtFnDef _ fd) = printf "StmtFnDef (%s)" (show fd)
    show (StmtExpr  _ e)  = printf "StmtExpr (%s)" (show e)

instance Eq Stmt where
    (StmtFnDef _ fda) == (StmtFnDef _ fdb) =
        fda == fdb
    (StmtExpr  _ ea)  == (StmtExpr  _ eb)  =
        ea == eb
    _ == _ = False

getFnName :: FnDef -> String
getFnName (FnReal n _ _) = n
getFnName (FnExpr n _ _) = n

isFnReadOnly :: FnDef -> Bool
isFnReadOnly (FnReal _ _ _) = True
isFnReadOnly _ = False

