{-# LANGUAGE FlexibleContexts #-}

module Runtime (FnStore, emptyFnStore, Runtime(getStore), mkDefaultRuntime, mkRuntime, addFunction, addHistory, getFunction) where

import Alias
import AST
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict

data Runtime = Runtime { getHistory :: [String]
                       , getStore   :: M.Map Name FnDef
                       } deriving (Show)

type FnStore = M.Map Name FnDef

emptyFnStore :: FnStore
emptyFnStore = M.empty

mkRuntime :: Runtime
mkRuntime = Runtime [] M.empty

preDefFns :: [FnDef]
preDefFns = [ FnReal "pi"    []    (FnNullary pi)
            , FnReal "sqrt"  ["x"] (FnUnary sqrt)
            , FnReal "acos"  ["x"] (FnUnary acos)
            , FnReal "asin"  ["x"] (FnUnary asin)
            , FnReal "atan"  ["x"] (FnUnary atan)
            , FnReal "cos"   ["x"] (FnUnary cos)
            , FnReal "sin"   ["x"] (FnUnary sin)
            , FnReal "tan"   ["x"] (FnUnary tan)
            , FnReal "acosh" ["x"] (FnUnary acosh)
            , FnReal "asinh" ["x"] (FnUnary asinh)
            , FnReal "atanh" ["x"] (FnUnary atanh)
            , FnReal "cosh"  ["x"] (FnUnary cosh)
            , FnReal "sinh"  ["x"] (FnUnary sinh)
            , FnReal "tanh"  ["x"] (FnUnary tanh)
            , FnReal "exp"   ["x"] (FnUnary exp)
            , FnReal "log"   ["x"] (FnUnary log)
            , FnReal "tst"   ["a", "b"] (FnBinary (+))
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
