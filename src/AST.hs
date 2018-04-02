{-# LANGUAGE FlexibleInstances #-}

module AST where

import Alias
import FlexNum

instance Show (FlexNum -> FlexNum) where
    show _ = "(E -> E)"

instance Show (FlexNum -> FlexNum -> FlexNum) where
    show _ = "(E -> E -> E)"

instance Show (FlexNum -> FlexNum -> FlexNum -> FlexNum) where
    show _ = "(E -> E -> E -> E)"

data FnRef = FnNullary (FlexNum)
           | FnUnary   (FlexNum -> FlexNum)
           | FnBinary  (FlexNum -> FlexNum -> FlexNum)
           | FnTernary (FlexNum -> FlexNum -> FlexNum -> FlexNum)
           deriving (Show)

data FnDef = FnReal Name Params FnRef
           | FnExpr Name Params Expr
           deriving (Show)

data Expr = LitFloat Double
          | LitInt   Integer
          | Negate   Expr
          | OperExp  Expr Expr
          | OperMul  Expr Expr
          | OperDiv  Expr Expr
          | OperAdd  Expr Expr
          | OperSub  Expr Expr
          | FnCall   Name [Expr]
          deriving (Eq, Show)
