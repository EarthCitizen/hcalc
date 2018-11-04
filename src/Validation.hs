module Validation (validateFnDef) where

import Alias
import AST
import Control.Monad (forM_)
import Data.List (elem, intercalate, nub)
import Error

validateFnDef :: FnDef -> Either Error FnDef
validateFnDef fd@(FnExpr fn ps ex) = do
    forM_ ps (fNameNotPName fn)
    pNamesNotDup ps
    return fd

fNameNotPName :: Name -> Name -> Either Error Name
fNameNotPName fn pn = if pn == fn
                      then Left $ Error "parameter name matches function name"
                      else Right pn

pNamesNotDup :: [Name] -> Either Error Name
pNamesNotDup [] = Right []
pNamesNotDup ps = case go [] ps of
                      [] -> Right []
                      xs -> let pf = "duplicate parameters: "
                                vs = intercalate ", " xs
                             in Left $ Error $ pf ++ vs
    where go :: (Eq a) => [a] -> [a] -> [a]
          go [] [] = []
          go cm [] = cm
          go cm (x:xs) = if elem x xs
                         then go (nub $ cm ++ [x]) xs
                         else go cm xs

