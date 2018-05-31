{-# LANGUAGE ScopedTypeVariables #-}

module FlexNum (FlexNum(..)) where

import Alias
import Control.Exception (ErrorCall, catch, evaluate)
import Data.Ratio (denominator, numerator)
import System.IO.Unsafe (unsafePerformIO)
import TypeLevel.NaturalNumber (Ten, Fifteen)
import Data.Eq.Approximate (AbsolutelyApproximateValue(..), Digits(..))

type ApproxFloat = AbsolutelyApproximateValue (Digits Ten) Float50

toApprox :: Float50 -> ApproxFloat
toApprox = AbsolutelyApproximateValue

data FlexNum = FlexFloat Float50 | FlexInt Integer | FlexNaN deriving (Show)

instance Eq FlexNum where
    (FlexNaN)     == (FlexNaN)     = True
    (FlexFloat x) == (FlexFloat y) = (toApprox x) == (toApprox y)
    (FlexInt x)   == (FlexInt y)   = x == y
    _ == _ = False

{-# NOINLINE safeOp #-}
safeOp :: forall a b. (a -> a -> a) -> a -> a -> (a -> b) -> b -> b
safeOp faa xa ya fab d = unsafePerformIO $ catch (fab <$> evaluate (faa xa ya)) dflt
    where dflt :: ErrorCall -> IO b
          dflt _ = return d

safeFloatOp :: (Float50 -> Float50 -> Float50) -> Float50 -> Float50 -> FlexNum
safeFloatOp f a b = safeOp f a b FlexFloat FlexNaN

applyConversionRule :: FlexNum -> FlexNum -> (FlexNum, FlexNum)
applyConversionRule l@(FlexFloat _) r@(FlexFloat _) = (l, r)
applyConversionRule l@(FlexInt   _) r@(FlexInt   _) = (l, r)
applyConversionRule f@(FlexFloat _)   (FlexInt   i) = (f, FlexFloat . fromIntegral $ i)
applyConversionRule   (FlexInt   i) f@(FlexFloat _) = (FlexFloat . fromIntegral $ i, f)

doAdd :: (FlexNum, FlexNum) -> FlexNum
doAdd (FlexFloat fl, FlexFloat fr) =
    let fl500 = realToFrac fl :: Float500
        fr500 = realToFrac fr :: Float500
        f50   = realToFrac (fl500 + fr500)
     in FlexFloat f50
doAdd (FlexInt il,   FlexInt ir)   = FlexInt $ il + ir

doDiv :: (FlexNum, FlexNum) -> FlexNum
doDiv (FlexFloat fl, FlexFloat 0)  = FlexNaN
doDiv (FlexFloat fl, FlexFloat fr) = safeFloatOp (/) fl fr
doDiv (FlexInt il,   FlexInt 0)    = FlexNaN
doDiv (FlexInt il,   FlexInt ir)   =
    let (q, r) = il `quotRem` ir
     in case r of 0 -> FlexInt q
                  _ -> let fnl = FlexFloat $ fromIntegral il
                           fnr = FlexFloat $ fromIntegral ir
                        in doDiv (fnl, fnr)

sanitize :: FlexNum -> FlexNum
sanitize FlexNaN = FlexNaN
sanitize fi@(FlexInt _)   = fi
sanitize ff@(FlexFloat f) =
    let (n, frac) = properFraction f
     in if frac == 0
        then FlexInt n
        else ff

doExp :: (FlexNum, FlexNum) -> FlexNum
doExp (fnl, fnr) = sanitize $ go (sanitize fnl) (sanitize fnr)
    where go (FlexFloat fl) (FlexInt ir) = FlexFloat (fl ^^ ir) -- case 1
          go (FlexInt il)   (ffr@(FlexFloat _)) = go (FlexFloat $ fromIntegral il) ffr -- case 2
          go (FlexFloat fl) (FlexFloat fr) =
              -- fr guarunteed fractional here due to sanitize
              if fl < 0
              then FlexNaN -- case 3
              else FlexFloat (fl ** fr) -- case 4
          go (FlexInt il)   (fir@(FlexInt ir))
              | ir >= 0   = FlexInt $ il ^ ir -- case 5
              | otherwise = go (FlexFloat $ fromIntegral il) fir -- case 6

doMul :: (FlexNum, FlexNum) -> FlexNum
doMul (FlexFloat fl, FlexFloat fr) = FlexFloat $ fl * fr
doMul (FlexInt il,   FlexInt ir)   = FlexInt   $ il * ir

doSub :: (FlexNum, FlexNum) -> FlexNum
doSub (FlexFloat fl, FlexFloat fr) =
    let fl500 = realToFrac fl :: Float500
        fr500 = realToFrac fr :: Float500
        f50   = realToFrac (fl500 - fr500)
     in FlexFloat f50
doSub (FlexInt il,   FlexInt ir)   = FlexInt   $ il - ir

doCompare :: (FlexNum, FlexNum) -> Ordering
doCompare (FlexFloat fl, FlexFloat fr) = fl `compare` fr
doCompare (FlexInt il,   FlexInt ir)   = il `compare` ir

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

mapAsFloatUnary :: (Float50 -> Float50) -> FlexNum -> FlexNum
mapAsFloatUnary f (FlexFloat n) = FlexFloat $ f n
mapAsFloatUnary f (FlexInt   n) = FlexFloat $ f $ fromInteger n

instance Floating FlexNum where
    (**) el er = doExp (el, er)
    pi    = FlexFloat pi
    exp   = mapAsFloatUnary exp
    log   = mapAsFloatUnary log
    sin   = mapAsFloatUnary sin
    cos   = mapAsFloatUnary cos
    asin  = mapAsFloatUnary asin
    acos  = mapAsFloatUnary acos
    atan  = mapAsFloatUnary atan
    sinh  = mapAsFloatUnary sinh
    cosh  = mapAsFloatUnary cosh
    asinh = mapAsFloatUnary asinh
    acosh = mapAsFloatUnary acosh
    atanh = mapAsFloatUnary atanh

instance Real FlexNum where
    toRational (FlexFloat f) = toRational f
    toRational (FlexInt i)   = toRational i

instance RealFrac FlexNum where
    properFraction (FlexFloat f) =
        let (i, f') = properFraction f
         in (i, FlexFloat f')
    properFraction (FlexInt i) =
        (fromInteger i, FlexInt 0)
