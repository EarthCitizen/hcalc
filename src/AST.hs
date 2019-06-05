{-# LANGUAGE FlexibleInstances #-}

module AST ( FnRef(..)
           , FnDef(..)
           , Expr(..)
           , Fixity(..)
           , Operator(..)
           , Stmt(..)
           , getExprLocation
           , getFnName
           , operString
           , isFnReadOnly
           ) where

import Alias
import Data.List.NonEmpty (NonEmpty)
import Text.Printf (printf)

import qualified Data.List.NonEmpty as N

type FN0 = Float50
type FN1 = (Float50 -> Float50)
type FN2 = (Float50 -> Float50 -> Float50)
type FN3 = (Float50 -> Float50 -> Float50 -> Float50)

instance Show FN1 where
    show _ = "(E -> E)"

instance Show FN2 where
    show _ = "(E -> E -> E)"

instance Show FN3 where
    show _ = "(E -> E -> E -> E)"

data FnRef = FnNullary FN0
           | FnUnary   FN1
           | FnBinary  FN2
           | FnTernary FN3
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

data Fixity = LFix | RFix deriving (Eq, Show)

data Operator = OpInfix { chars  :: NonEmpty Char
                        , fixity :: Fixity
                        , prec   :: Int
                        , fn     :: FN2
                        }
                        deriving (Show)

instance Eq Operator where
    (OpInfix csl fxl prl _) == (OpInfix csr fxr prr _) =
        csl == csr && fxl == fxr && prl == prr

operString :: Operator -> String
operString o = N.toList $ chars o

data Expr = LitNum  Location Float50
          | Negate  Location Expr
          | OperInf Location Operator Expr Expr
          | FnCall  Location Name [Expr]

instance Show Expr where
    show (LitNum  _ f) = printf "LitNum %s"   (show f)
    show (Negate  _ e) = printf "Negate (%s)" (show e)
    show (OperInf _ op el er) = printf "OperInf %s (%s) (%s)" (show $ operString op) (show el) (show er)
    show (FnCall  _ n es)  = printf "FnCall %s %s" n (show es)

instance Eq Expr where
    (LitNum  _ fl) == (LitNum  _ fr) =
        fl == fr
    (Negate _ el) ==  (Negate _ er) =
        el == er
    (OperInf _ opl eal ear) == (OperInf _ opr ebl ebr) =
        let osl = operString opl
            osr = operString opr
         in osl == osr && eal == ebl && ear == ebr
    (FnCall  _ nl esl) == (FnCall  _ nr esr) =
        nl == nr && esl == esr
    _ == _ = False

getExprLocation :: Expr -> Location
getExprLocation (LitNum  l _)   = l
getExprLocation (Negate  l _)   = l
getExprLocation (OperInf l _ _ _) = l
getExprLocation (FnCall  l _ _)   = l

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

