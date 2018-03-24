module AST where

data Expression = LitFloat Double
                | LitInt   Integer
                | OperExp Expression Expression
                | OperMul Expression Expression
                | OperDiv Expression Expression
                | OperAdd Expression Expression
                | OperSub Expression Expression
                deriving (Eq, Show, Ord)
