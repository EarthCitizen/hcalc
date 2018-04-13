module Eval (evalExpr) where

import Alias
import AST
import Error
import FlexNum
import qualified Data.Map.Lazy as M
import Runtime

type Store = M.Map Name FnDef

evalExpr :: Expr -> Store -> (Either Error FlexNum)
evalExpr e s = go e
    where go (LitFloat _ f) = Right $ FlexFloat f
          go (LitInt   _ i) = Right $ FlexInt i
          go (Negate _ en)  = negate <$> go en
          go (OperExp _ el er) = (^:) <$> (go el) <*> (go er)
          go (OperMul _ el er) = (*)  <$> (go el) <*> (go er)
          go (OperDiv _ el er) = (/)  <$> (go el) <*> (go er)
          go (OperAdd _ el er) = (+)  <$> (go el) <*> (go er)
          go (OperSub _ el er) = (-)  <$> (go el) <*> (go er)
          go (FnCall l name exps) = do
              fnDef <- lookupFunction name s l
              callFnDef fnDef exps s l

callFnRef :: FnRef -> [FlexNum] -> Either (Integer, Integer) FlexNum
callFnRef (FnNullary fn) [] = Right fn
callFnRef (FnUnary   fn) (p1:[]) = Right $ fn p1
callFnRef (FnBinary  fn) (p1:p2:[]) = Right $ fn p1 p2
callFnRef (FnTernary fn) (p1:p2:p3:[]) = Right $ fn p1 p2 p3
callFnRef fr ps = Left $ (getParamCount fr, fromIntegral $ length ps)

callFnDef :: FnDef -> [Expr] -> Store -> Location -> Either Error FlexNum
callFnDef (FnReal name ps fr) exps s l = do
    fnums <- mapM (\x -> evalExpr x s) exps
    case callFnRef fr fnums of
        Left (ex, ac) -> Left $ ArityMismatchError l name ex ac
        Right flexnum -> Right flexnum
callFnDef (FnExpr name ps expr) exps s l = do
    let lenPs   = fromIntegral $ length ps
        lenExps = fromIntegral $ length exps
    case lenPs == lenExps of
        True  -> Right ()
        False -> Left $ ArityMismatchError l name lenPs lenExps
    let nfn = \n e -> (n, FnExpr n [] e)
        zps = zipWith nfn ps exps
        ns  = M.union (M.fromList zps) s
    evalExpr expr ns


lookupFunction :: Name -> Store -> Location -> Either Error FnDef
lookupFunction n s l =
    case M.lookup n s of
        Nothing -> Left $ FunctionNotFoundError l n
        Just fr -> Right fr

getParamCount :: FnRef -> Integer
getParamCount (FnNullary _) = 0
getParamCount (FnUnary   _) = 1
getParamCount (FnBinary  _) = 2
getParamCount (FnTernary _) = 3
