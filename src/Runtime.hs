{-# LANGUAGE FlexibleContexts #-}

module Runtime (Runtime(getStore), mkDefaultRuntime, mkRuntime, addHistory, getFunction) where

import Alias
import AST
import Error
import FlexNum
import qualified Data.Map.Lazy as M
import Control.Monad.State.Strict

data Runtime = Runtime { getHistory :: [String]
                       , getStore   :: M.Map Name FnDef
                       } deriving (Show)

mkRuntime :: Runtime
mkRuntime = Runtime [] M.empty

preDefFns :: [FnDef]
preDefFns = [ FnReal "pi" []     (FnNullary pi)
            , FnReal "sin" ["x"] (FnUnary sin)
            ]

fnDefToTuple :: FnDef -> (Name, FnDef)
fnDefToTuple fd@(FnReal n _ _) = (n, fd)

mkDefaultRuntime :: Runtime
mkDefaultRuntime = let s = M.fromList $ fnDefToTuple <$> preDefFns
                    in Runtime [] s

addHistory :: (MonadState Runtime m) => String -> m ()
addHistory l = modify (\(Runtime h s) -> Runtime (l:h) s)

getFnName :: FnDef -> String
getFnName (FnReal n _ _) = n
getFnName (FnExpr n _ _) = n

addFunction :: (MonadState Runtime m) => FnDef -> m ()
addFunction fd = let name = getFnName fd
                     ins  = M.insert name fd
                  in modify $ \(Runtime h s) -> Runtime h $ ins s

getFunction :: (MonadState Runtime m) => String -> m (Maybe FnDef)
getFunction name = do
    r <- get
    let s = getStore r
    return $ M.lookup name s
