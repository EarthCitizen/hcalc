module Eval (eval) where

import Alias
import AST
import Error
import FlexNum
import qualified Data.Map.Lazy as M
import Runtime

type Store = M.Map Name FnDef

eval :: Expr -> Store -> (Either Error FlexNum)
eval e s = go e
    where go (LitFloat _ f) = Right $ FlexFloat f
          go (LitInt   _ i) = Right $ FlexInt i
          go (Negate _ en)  = negate <$> go en
          go (OperExp _ el er) = (^:) <$> (go el) <*> (go er)
          go (OperMul _ el er) = (*)  <$> (go el) <*> (go er)
          go (OperDiv _ el er) = (/)  <$> (go el) <*> (go er)
          go (OperAdd _ el er) = (+)  <$> (go el) <*> (go er)
          go (OperSub _ el er) = (-)  <$> (go el) <*> (go er)
          go (FnCall l name exps) = do
              (FnReal _ _ fr) <- case M.lookup name s of
                                    Nothing -> Left $ FunctionNotFoundError l name
                                    Just fr -> Right fr
              fnums <- mapM go exps
              case callFnRef fr fnums of
                  Left (ex, ac) -> Left $ ArityMismatchError l name ex ac
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
