module FnStore where

import Alias
import AST

import Data.Maybe (isJust, maybe)

import qualified Data.Map.Strict as M

type FnStore = M.Map Name FnDef

emptyFnStore :: FnStore
emptyFnStore = M.empty

class (Monad m) => GetFnStore m where
    getFnStore :: m FnStore

class (GetFnStore m) => PutFnStore m where
    putFnStore :: FnStore -> m ()

getFnM :: (GetFnStore m) => Name -> m (Maybe FnDef)
getFnM n = do
    fs <- getFnStore
    return $ M.lookup n fs

hasFnM :: (GetFnStore m) => Name -> m Bool
hasFnM n = do
    x <- getFnM n
    return $ isJust x

hasFn :: Name -> FnStore -> Bool
hasFn n fs = M.member n fs

maybeFnM :: (GetFnStore m) => Name -> a -> (FnDef -> a) -> m a
maybeFnM n d f = do
    x <- getFnM n
    return $ maybe d f x

putFnM :: (PutFnStore m) => FnDef -> m ()
putFnM fd = do
    fs <- getFnStore
    putFnStore $ putFn fd fs

putFn :: FnDef -> FnStore -> FnStore
putFn fd fs =
    let n = getFnName fd
     in M.insert n fd fs

