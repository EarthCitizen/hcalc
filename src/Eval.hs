module Eval (eval) where

import Alias
import AST
import Control.Monad.Except
import Control.Monad.Reader
import Error
import qualified Data.Map.Strict as M
import FnStore (FnStore)

data EvalState = EvalState FnStore Location deriving (Show)

newtype EvalContext a = EvalContext { unEvalContext :: ReaderT EvalState (Except Error) a }
                    deriving ( Functor
                             , Applicative
                             , Monad
                             , MonadReader EvalState
                             , MonadError Error
                             )

eval :: Expr -> FnStore -> Either Error Float50
eval e s = let initState = EvalState s $ getExprLocation e
            in runEvalContext (evalExpr e) initState

runEvalContext :: EvalContext a -> EvalState -> Either Error a
runEvalContext ec es = runExcept $ runReaderT (unEvalContext ec) es

ln :: [a] -> Integer
ln xs = fromIntegral $ length xs

getLocation :: EvalContext Location
getLocation = do
    (EvalState _ l) <- ask
    return l

withLocation :: Location -> EvalContext a -> EvalContext a
withLocation l = local (\(EvalState s ol) -> EvalState s l)

withStore :: FnStore -> EvalContext a -> EvalContext a
withStore s = local (\(EvalState os l) -> EvalState s l)

evalExpr :: Expr -> EvalContext Float50
evalExpr (LitNum _ f) = return f
evalExpr (Negate _ en)  = negate <$> evalExpr en
evalExpr (OperExp _ el er) = (**) <$> evalExpr el <*> evalExpr er
evalExpr (OperMul _ el er) = (*)  <$> evalExpr el <*> evalExpr er
evalExpr (OperDiv _ el er) = (/)  <$> evalExpr el <*> evalExpr er
evalExpr (OperAdd _ el er) = (+)  <$> evalExpr el <*> evalExpr er
evalExpr (OperSub _ el er) = (-)  <$> evalExpr el <*> evalExpr er
evalExpr (FnCall l name exps) =
    let action = lookupFn name >>= \fd -> callFnDef fd exps
     in withLocation l action

lookupFn :: Name -> EvalContext FnDef
lookupFn n = do
    (EvalState s l) <- ask
    case M.lookup n s of
        Nothing -> throwError $ FunctionNotFoundError l n
        Just fr -> return fr

callFnDef :: FnDef -> [Expr] -> EvalContext Float50
callFnDef (FnReal name ps fr) exps = do
    (EvalState s l) <- ask
    fnums <- mapM evalExpr exps
    case callFnRef fr fnums of
        Left (ex, ac) -> throwError $ ArityMismatchError l name ex ac
        Right flexnum -> return flexnum
callFnDef (FnExpr name ps expr) exps = do
    (EvalState s l) <- ask
    let lenPs   = ln ps
        lenExps = ln exps
    case lenPs == lenExps of
        False -> throwError $ ArityMismatchError l name lenPs lenExps
        True  -> let nfn n e = (n, FnExpr n [] e)
                     zps = zipWith nfn ps exps
                     ns  = M.union (M.fromList zps) s
                  in withStore ns $ evalExpr expr

callFnRef :: FnRef -> [Float50] -> Either (Integer, Integer) Float50
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
