{-# LANGUAGE FlexibleContexts #-}

module Runtime ( Runtime(..)
               , mkDefaultRuntime
               , mkRuntime
               , addHistory
               ) where

import Alias
import AST
import FnStore
import Control.Monad.State.Strict
import Predef (predefFns)

import qualified Data.Map.Strict as M

data Runtime = Runtime { getHistory :: [String]
                       , getStore   :: FnStore
                       } deriving (Show)

mkRuntime :: Runtime
mkRuntime = Runtime [] M.empty

fnDefToTuple :: FnDef -> (Name, FnDef)
fnDefToTuple fd@(FnReal n _ _) = (n, fd)

mkDefaultRuntime :: Runtime
mkDefaultRuntime = let s = M.fromList $ fnDefToTuple <$> predefFns
                    in Runtime [] s

addHistory :: (MonadState Runtime m) => String -> m ()
addHistory l = modify (\(Runtime h s) -> Runtime (l:h) s)

