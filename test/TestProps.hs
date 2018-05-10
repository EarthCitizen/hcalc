module TestProps where

import AST
import Hedgehog

hprop_equals1 :: Property
hprop_equals1 = property $ do
    1 === 1
