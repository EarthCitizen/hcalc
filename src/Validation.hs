module Validation (validate) where

import Alias
import AST
import Control.Monad (forM_)
import Control.Monad.Except (MonadError(..))
import Data.List ((\\), intercalate, nub)
import Error

validate :: (MonadError Error m) => Stmt -> m Stmt
validate s@(StmtFnDef _ fnDef) = do
    validateFnDef fnDef
    return s
validate s = return s

validateFnDef :: (MonadError Error m) => FnDef -> m ()
validateFnDef fd@(FnExpr fn ps ex) = do
    forM_ ps (fNameNotPName fn)
    pNamesNotDup ps

fNameNotPName :: (MonadError Error m) => Name -> Name -> m ()
fNameNotPName fn pn = if pn == fn
                      then throwError $ Error "parameter name matches function name"
                      else return ()

pNamesNotDup :: (MonadError Error m) => [Name] -> m ()
pNamesNotDup [] = return ()
pNamesNotDup ps = case findDups ps of
                      [] -> return ()
                      xs -> let pf = "duplicate parameters: "
                                vs = intercalate ", " xs
                             in throwError $ Error $ pf ++ vs

findDups :: Eq a => [a] -> [a]
findDups xs = xs \\ (nub xs)

