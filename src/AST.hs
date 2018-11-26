{-# LANGUAGE FlexibleInstances #-}

module AST ( FnRef(..)
           , FnDef(..)
           , Expr(..)
           , Stmt(..)
           , getExprLocation
           , getFnName
           ) where

import Alias

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

data Expr = LitNum  Location Float50
          | Negate  Location Expr
          | OperExp Location Expr Expr
          | OperMul Location Expr Expr
          | OperDiv Location Expr Expr
          | OperAdd Location Expr Expr
          | OperSub Location Expr Expr
          | FnCall  Location Name [Expr]
          deriving (Eq, Show)

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
          deriving (Show)

getFnName :: FnDef -> String
getFnName (FnReal n _ _) = n
getFnName (FnExpr n _ _) = n

