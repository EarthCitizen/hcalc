module TestProps where

import Alias
import AST
import qualified Data.Map.Strict as M
import qualified Eval as E
import Hedgehog
import TestGen

emptyS = M.empty :: M.Map Name FnDef
testCount = 1000000 :: TestLimit

hprop_evalAddsIntegers :: Property
hprop_evalAddsIntegers =
    withTests testCount $ property $ do
        (expected, expr) <- forAll genAddInteger
        E.eval expr emptyS === Right expected

hprop_evalSubtractsIntegers :: Property
hprop_evalSubtractsIntegers =
    withTests testCount $ property $ do
        (expected, expr) <- forAll genAddInteger
        E.eval expr emptyS === Right expected

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
