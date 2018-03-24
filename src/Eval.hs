module Eval (EvalResult(..), eval) where

import AST

(//) = quot

data EvalResult = ResultFloat Double | ResultInt Integer deriving (Show)

applyConversionRule :: EvalResult -> EvalResult -> (EvalResult, EvalResult)
applyConversionRule l@(ResultFloat _) r@(ResultFloat _) = (l, r)
applyConversionRule l@(ResultInt   _) r@(ResultInt   _) = (l, r)
applyConversionRule f@(ResultFloat _)   (ResultInt   i) = (f, ResultFloat . fromIntegral $ i)
applyConversionRule   (ResultInt   i) f@(ResultFloat _) = (ResultFloat . fromIntegral $ i, f)

doAdd :: (EvalResult, EvalResult) -> EvalResult
doAdd ((ResultFloat fl), (ResultFloat fr)) = ResultFloat $ fl + fr
doAdd ((ResultInt il),   (ResultInt ir))   = ResultInt   $ il + ir

doDiv :: (EvalResult, EvalResult) -> EvalResult
doDiv ((ResultFloat fl), (ResultFloat fr)) = ResultFloat $ fl / fr
doDiv ((ResultInt il),   (ResultInt ir))   = ResultInt   $ il // ir

doExp :: (EvalResult, EvalResult) -> EvalResult
doExp ((ResultFloat fl), (ResultFloat fr)) = ResultFloat $ fl ** fr
doExp ((ResultInt il),   (ResultInt ir))
    | ir >= 0   = ResultInt $ il ^ ir
    | otherwise = ResultFloat $ (fromIntegral il) ** (fromIntegral ir)

doMul :: (EvalResult, EvalResult) -> EvalResult
doMul ((ResultFloat fl), (ResultFloat fr)) = ResultFloat $ fl * fr
doMul ((ResultInt il),   (ResultInt ir))   = ResultInt   $ il * ir

doSub :: (EvalResult, EvalResult) -> EvalResult
doSub ((ResultFloat fl), (ResultFloat fr)) = ResultFloat $ fl - fr
doSub ((ResultInt il),   (ResultInt ir))   = ResultInt   $ il - ir

negateEvalResult :: EvalResult -> EvalResult
negateEvalResult (ResultFloat f) = ResultFloat $ negate f
negateEvalResult (ResultInt i)   = ResultInt   $ negate i

signumEvalResult :: EvalResult -> EvalResult
signumEvalResult (ResultFloat f) = ResultFloat $ signum f
signumEvalResult (ResultInt i)   = ResultInt   $ signum i

absEvalResult :: EvalResult -> EvalResult
absEvalResult (ResultFloat f) = ResultFloat $ abs f
absEvalResult (ResultInt i)   = ResultInt   $ abs i

class Primitive a where
    (^:) :: a -> a -> a
    (*:) :: a -> a -> a
    (/:) :: a -> a -> a
    (+:) :: a -> a -> a
    (-:) :: a -> a -> a

instance Primitive EvalResult where
    (^:) el er = doExp $ applyConversionRule el er
    (*:) el er = doMul $ applyConversionRule el er
    (/:) el er = doDiv $ applyConversionRule el er
    (+:) el er = doAdd $ applyConversionRule el er
    (-:) el er = doSub $ applyConversionRule el er

eval :: Expression -> EvalResult
eval (LitFloat f) = ResultFloat f
eval (LitInt i)   = ResultInt i
eval (OperExp el er) = eval el ^:  eval er
eval (OperMul el er) = eval el *:  eval er
eval (OperDiv el er) = eval el /: eval er
eval (OperAdd el er) = eval el +:  eval er
eval (OperSub el er) = eval el -:  eval er
