module Alias where

-- import Data.Number.BigFloat (BigFloat)
-- import Data.Number.Fixed (Fixed, Prec50, Prec500)

import Numeric.Decimal (ExtendedDecimal, P50, P500)

type Float50  = ExtendedDecimal P50
type Float500 = ExtendedDecimal P500

type Arity    = Integer
type Column   = Integer
type Line     = Integer
type Source   = String
type Location = (Source, Line, Column)
type Message  = String
type Name     = String
type Params   = [Name]

