module FnPredef where

import AST

preDefFns :: [FnDef]
preDefFns = [ FnReal "pi" []     (FnNullary pi)
            , FnReal "sin" ["x"] (FnUnary sin)
            ]
