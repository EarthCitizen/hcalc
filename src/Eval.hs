{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval (eval) where

import Alias
import AST
import Error
import FlexNum
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad (liftM2, mapM)
import Data.Either (rights)
import qualified Data.Map.Lazy as M
import Runtime

type Store = M.Map Name FnDef

getFn :: Name -> Store -> Either Error FnDef
getFn n s = case M.lookup n s of Nothing -> Left $ FunctionNotFoundError n
                                 Just fr -> Right fr

eval :: Expr -> Store -> (Either Error FlexNum)
eval e s = go e
    where go (LitFloat f) = Right $ FlexFloat f
          go (LitInt i)   = Right $ FlexInt i
          go (Negate en)  = negate <$> go en
          go (OperExp el er) = (^:) <$> (go el) <*> (go er)
          go (OperMul el er) = (*)  <$> (go el) <*> (go er)
          go (OperDiv el er) = (/)  <$> (go el) <*> (go er)
          go (OperAdd el er) = (+)  <$> (go el) <*> (go er)
          go (OperSub el er) = (-)  <$> (go el) <*> (go er)
          go (FnCall name exps) = do
              (FnReal _ _ fr) <- getFn name s
              fnums <- mapM go exps
              case callFnRef fr fnums of
                  Left (ex, ac) -> Left $ ArityMismatchError name ex ac
                  Right flexnum -> Right flexnum

callFnRef :: FnRef -> [FlexNum] -> Either (Integer, Integer) FlexNum
callFnRef (FnNullary fn) [] = Right fn
callFnRef (FnUnary   fn) (p1:[]) = Right $ fn p1
callFnRef (FnBinary  fn) (p1:p2:[]) = Right $ fn p1 p2
callFnRef (FnTernary fn) (p1:p2:p3:[]) = Right $ fn p1 p2 p3
callFnRef fr ps = Left $ (getParamCount fr, fromIntegral $ length ps)

getParamCount :: FnRef -> Integer
getParamCount (FnNullary _) = 0
getParamCount (FnUnary   _) = 1
getParamCount (FnBinary  _) = 2
getParamCount (FnTernary _) = 3
