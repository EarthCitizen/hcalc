module FnStore where

import Alias
import AST

-- import Data.Maybe (isJust)

import qualified Data.Map.Strict as M

type FnStore = M.Map Name FnDef

emptyFnStore :: FnStore
emptyFnStore = M.empty

class (Monad m) => GetFnStore m where
    getFnStore :: m FnStore

class (GetFnStore m) => PutFnStore m where
    putFnStore :: FnStore -> m ()

getFn :: (GetFnStore m) => String -> m (Maybe FnDef)
getFn n = do
    fs <- getFnStore
    return $ M.lookup n fs

-- hasFn :: (GetFnStore m) => String -> m Bool
-- hasFn s = do
--     x <- getFn s
--     return $ isJust x

hasFn :: String -> FnStore -> Bool
hasFn n fs = M.member n fs

putFn :: (PutFnStore m) => FnDef -> m ()
putFn fd = do
    fs <- getFnStore
    let n = getFnName fd
    putFnStore $ M.insert n fd fs

