module FlexNum (FlexNum(..), (^:)) where

import Data.Ratio (denominator, numerator)

data FlexNum = FlexFloat Double | FlexInt Integer deriving (Eq, Show)

(^:) el er = doExp (el, er)

applyConversionRule :: FlexNum -> FlexNum -> (FlexNum, FlexNum)
applyConversionRule l@(FlexFloat _) r@(FlexFloat _) = (l, r)
applyConversionRule l@(FlexInt   _) r@(FlexInt   _) = (l, r)
applyConversionRule f@(FlexFloat _)   (FlexInt   i) = (f, FlexFloat . fromIntegral $ i)
applyConversionRule   (FlexInt   i) f@(FlexFloat _) = (FlexFloat . fromIntegral $ i, f)

doAdd :: (FlexNum, FlexNum) -> FlexNum
doAdd ((FlexFloat fl), (FlexFloat fr)) = FlexFloat $ fl + fr
doAdd ((FlexInt il),   (FlexInt ir))   = FlexInt   $ il + ir

doDiv :: (FlexNum, FlexNum) -> FlexNum
doDiv ((FlexFloat fl), (FlexFloat fr)) = FlexFloat $ fl / fr
doDiv ((FlexInt il),   (FlexInt ir))   =
    let (q, r) = il `quotRem` ir
     in case r of 0 -> FlexInt q
                  _ -> FlexFloat $ (fromIntegral il) / (fromIntegral ir)

doExp :: (FlexNum, FlexNum) -> FlexNum
doExp ((FlexFloat fl), (FlexFloat fr)) = FlexFloat $ fl ** fr
doExp ((FlexInt il),   (FlexInt ir))
    | ir >= 0   = FlexInt $ il ^ ir
    | otherwise = FlexFloat $ (fromIntegral il) ** (fromIntegral ir)

doMul :: (FlexNum, FlexNum) -> FlexNum
doMul ((FlexFloat fl), (FlexFloat fr)) = FlexFloat $ fl * fr
doMul ((FlexInt il),   (FlexInt ir))   = FlexInt   $ il * ir

doSub :: (FlexNum, FlexNum) -> FlexNum
doSub ((FlexFloat fl), (FlexFloat fr)) = FlexFloat $ fl - fr
doSub ((FlexInt il),   (FlexInt ir))   = FlexInt   $ il - ir

doCompare :: (FlexNum, FlexNum) -> Ordering
doCompare ((FlexFloat fl), (FlexFloat fr)) = fl `compare` fr
doCompare ((FlexInt il),   (FlexInt ir))   = il `compare` ir

doAbs :: FlexNum -> FlexNum
doAbs (FlexFloat f) = FlexFloat $ abs f
doAbs (FlexInt i)   = FlexInt   $ abs i

doSignum :: FlexNum -> FlexNum
doSignum (FlexFloat f) = FlexFloat $ signum f
doSignum (FlexInt i)   = FlexInt   $ signum i

doNegate :: FlexNum -> FlexNum
doNegate (FlexFloat f) = FlexFloat $ negate f
doNegate (FlexInt i)   = FlexInt   $ negate i

instance Ord FlexNum where
    compare el er = doCompare $ applyConversionRule el er

instance Num FlexNum where
    (+) el er = doAdd $ applyConversionRule el er
    (*) el er = doMul $ applyConversionRule el er
    abs         = doAbs
    signum      = doSignum
    fromInteger = FlexInt
    negate      = doNegate

instance Fractional FlexNum where
    fromRational r = case denominator r of 1 -> FlexInt   $ numerator r
                                           _ -> FlexFloat $ fromRational r
    (/) el er = doDiv $ applyConversionRule el er

mapAsFloatUnary :: (Double -> Double) -> FlexNum -> FlexNum
mapAsFloatUnary f (FlexFloat n) = FlexFloat $ f n
mapAsFloatUnary f (FlexInt   n) = FlexFloat $ f $ fromInteger n

-- instance Floating FlexNum where
--     pi    = FlexFloat pi
--     exp   = mapAsFloatUnary exp
--     log   = mapAsFloatUnary log
--     sin   = mapAsFloatUnary sin
--     cos   = mapAsFloatUnary cos
--     asin  = mapAsFloatUnary asin
--     acos  = mapAsFloatUnary acos
--     atan  = mapAsFloatUnary atan
--     sinh  = mapAsFloatUnary sinh
--     cosh  = mapAsFloatUnary cosh
--     asinh = mapAsFloatUnary asinh
--     acosh = mapAsFloatUnary acosh
--     atanh = mapAsFloatUnary atanh
