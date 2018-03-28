module Eval (eval) where

import AST
import FlexNum

eval :: Expression -> FlexNum
eval (LitFloat f) = FlexFloat f
eval (LitInt i)   = FlexInt i
eval (OperExp el er) = eval el ^: eval er
eval (OperMul el er) = eval el *  eval er
eval (OperDiv el er) = eval el /  eval er
eval (OperAdd el er) = eval el +  eval er
eval (OperSub el er) = eval el -  eval er
