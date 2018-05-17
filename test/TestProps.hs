module TestProps where

import Alias
import AST
import FlexNum
import qualified Data.Map.Strict as M
import qualified Eval as E
import Hedgehog
import TestGen
import Data.AEq ((~==))

emptyS = M.empty :: M.Map Name FnDef
testCount = 1000000 :: TestLimit

newtype TestFlexNum = TestFlexNum { unTestFlexNum :: FlexNum }

instance Eq TestFlexNum where
    (TestFlexNum (FlexFloat x)) == (TestFlexNum (FlexFloat y)) = x ~== y
    (TestFlexNum (FlexInt x))   == (TestFlexNum (FlexInt y))   = x == y
    _ == _ = False

instance Show TestFlexNum where
    show = show . unTestFlexNum

hprop_evalAddsIntegers :: Property
hprop_evalAddsIntegers =
    withTests testCount $ property $ do
        (expected, expr) <- forAll genAddInteger
        E.eval expr emptyS === Right expected

hprop_evalAddsFloats :: Property
hprop_evalAddsFloats =
    withTests testCount $ property $ do
        (expected, expr) <- forAll genAddFloat
        let av = TestFlexNum <$> E.eval expr emptyS
            ev = Right $ TestFlexNum expected
        av === ev

hprop_evalSubtractsIntegers :: Property
hprop_evalSubtractsIntegers =
    withTests testCount $ property $ do
        (expected, expr) <- forAll genSubInteger
        E.eval expr emptyS === Right expected

hprop_evalSubtractsFloats :: Property
hprop_evalSubtractsFloats =
    withTests testCount $ property $ do
        (expected, expr) <- forAll genSubFloat
        let av = TestFlexNum <$> E.eval expr emptyS
            ev = Right $ TestFlexNum expected
        av === ev

hprop_evalMultipliesIntegers :: Property
hprop_evalMultipliesIntegers =
    withTests testCount $ property $ do
        (expected, expr) <- forAll genMulInteger
        E.eval expr emptyS === Right expected

hprop_evalDividesIntegers :: Property
hprop_evalDividesIntegers =
    withTests testCount $ property $ do
        (expected, expr) <- forAll genDivIntegerEvenly
        E.eval expr emptyS === Right expected
