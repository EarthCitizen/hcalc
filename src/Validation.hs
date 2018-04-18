module Validation (validateFnDef) where

import Alias
import AST
import Control.Monad (forM_)
import Error

validateFnDef :: FnDef -> Either Error FnDef
validateFnDef fd@(FnExpr fn ps ex) = forM_ ps (fNameNotPName fn) >> Right fd

fNameNotPName :: Name -> Name -> Either Error Name
fNameNotPName fn pn = if pn == fn
                           then Left $ Error "name reuse"
                           else Right pn
