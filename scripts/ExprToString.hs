import AST

exprToString :: Expr -> String
exprToString (LitFloat _ d) = "(" ++ show d ++ ")"
exprToString (LitInt   _ i) = "(" ++ show i ++ ")"
exprToString (Negate   _ e) = "-" ++ exprToString e
exprToString (OperExp  _ el er) = exprToString el ++ " ^ " ++ exprToString er
exprToString (OperMul  _ el er) = exprToString el ++ " * " ++ exprToString er
exprToString (OperDiv  _ el er) = exprToString el ++ " / " ++ exprToString er
exprToString (OperAdd  _ el er) = exprToString el ++ " + " ++ exprToString er
exprToString (OperSub  _ el er) = exprToString el ++ " - " ++ exprToString er
exprToString _ = "(a function call)"
-- exprToString (FnCall   _ Name [Expr])
