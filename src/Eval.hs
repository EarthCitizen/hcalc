module Eval (runEval) where

import Alias
import AST
import Control.Monad.State.Strict
import Control.Monad.Except
import Error
import FlexNum
import qualified Data.Map.Strict as M

type Store = M.Map Name FnDef

data EvalState = EvalState Store Location deriving (Show)

newtype EvalContext a = EvalContext { unEvalContext :: StateT EvalState (Except Error) a }
                    deriving ( Functor
                             , Applicative
                             , Monad
                             , MonadState EvalState
                             , MonadError Error
                             )

runEval :: Expr -> Store -> Either Error FlexNum
runEval e s = do
    let initState = EvalState s $ getExprLocation e
    runExcept $ evalStateT (unEvalContext $ evalExpr e) initState

ln :: [a] -> Integer
ln xs = fromIntegral $ length xs

putStore :: Store -> EvalContext ()
putStore s = do
    (EvalState _ l) <- get
    put $ EvalState s l

getLocation :: EvalContext Location
getLocation = do
    (EvalState _ l) <- get
    return l

getStore :: EvalContext Store
getStore = do
    (EvalState s _) <- get
    return s

evalExpr :: Expr -> EvalContext FlexNum
evalExpr (LitFloat _ f) = return $ FlexFloat f
evalExpr (LitInt   _ i) = return $ FlexInt i
evalExpr (Negate _ en)  = negate <$> evalExpr en
evalExpr (OperExp _ el er) = (^:) <$> evalExpr el <*> evalExpr er
evalExpr (OperMul _ el er) = (*)  <$> evalExpr el <*> evalExpr er
evalExpr (OperDiv _ el er) = (/)  <$> evalExpr el <*> evalExpr er
evalExpr (OperAdd _ el er) = (+)  <$> evalExpr el <*> evalExpr er
evalExpr (OperSub _ el er) = (-)  <$> evalExpr el <*> evalExpr er
evalExpr (FnCall l name exps) = do
    fnDef <- lookupFn name
    (EvalState os ol) <- get
    callFnDef fnDef exps <* putStore os

lookupFn :: Name -> EvalContext FnDef
lookupFn n = do
    (EvalState s l) <- get
    case M.lookup n s of
        Nothing -> throwError $ FunctionNotFoundError l n
        Just fr -> return fr

callFnDef :: FnDef -> [Expr] -> EvalContext FlexNum
callFnDef (FnReal name ps fr) exps = do
    (EvalState s l) <- get
    fnums <- mapM evalExpr exps
    case callFnRef fr fnums of
        Left (ex, ac) -> throwError $ ArityMismatchError l name ex ac
        Right flexnum -> return flexnum
callFnDef (FnExpr name ps expr) exps = do
    (EvalState s l) <- get
    let lenPs   = ln ps
        lenExps = ln exps
    case lenPs == lenExps of
        False -> throwError $ ArityMismatchError l name lenPs lenExps
        True  -> let nfn n e = (n, FnExpr n [] e)
                     zps = zipWith nfn ps exps
                     ns  = M.union (M.fromList zps) s
                  in putStore ns >> evalExpr expr

callFnRef :: FnRef -> [FlexNum] -> Either (Integer, Integer) FlexNum
callFnRef (FnNullary fn) [] = Right fn
callFnRef (FnUnary   fn) [p1] = Right $ fn p1
callFnRef (FnBinary  fn) [p1, p2] = Right $ fn p1 p2
callFnRef (FnTernary fn) [p1, p2, p3] = Right $ fn p1 p2 p3
callFnRef fr ps = Left (getParamCount fr, ln ps)

getParamCount :: FnRef -> Integer
getParamCount (FnNullary _) = 0
getParamCount (FnUnary   _) = 1
getParamCount (FnBinary  _) = 2
getParamCount (FnTernary _) = 3
