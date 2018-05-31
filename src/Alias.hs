module Alias where

import Data.Number.BigFloat (BigFloat)
import Data.Number.Fixed (Prec50, Prec500)

type Float50  = BigFloat Prec50
type Float500 = BigFloat Prec500

type Arity    = Integer
type Column   = Integer
type Line     = Integer
type Source   = String
type Location = (Source, Line, Column)
type Message  = String
type Name     = String
type Params   = [Name]
