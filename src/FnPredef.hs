module FnPredef where

import Alias
import AST
import FlexNum

preDefFns :: [FnDef]
preDefFns = [ FnReal "pi" []     (FnNullary pi)
            , FnReal "sin" ["x"] (FnUnary sin)
            ]
