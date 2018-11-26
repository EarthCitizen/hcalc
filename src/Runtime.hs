{-# LANGUAGE FlexibleContexts #-}

module Runtime ( FnStore
               , emptyFnStore
               , Runtime(..)
               , mkDefaultRuntime
               , mkRuntime
               , addHistory
               -- , isExtFnRO
               , GetFnStore (..)
               , getFn
               , hasFn
               , PutFnStore (..)
               , putFn
               ) where

import Alias
import AST
import FnStore
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M

data Runtime = Runtime { getHistory :: [String]
                       , getStore   :: FnStore
                       } deriving (Show)

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

-- isExtFnRO :: (GetFnStore m) => String -> m Bool
-- isExtFnRO n = do
--     fn <- getFn n
--     return $ maybe False isReadOnlyFn fn

isReadOnlyFn :: FnDef -> Bool
isReadOnlyFn (FnReal _ _ _) = True
isReadOnlyFn _ = False

