module Alias where

type Arity    = Integer
type Column   = Integer
type Line     = Integer
type Source   = String
type Location = (Source, Line, Column)
type Message  = String
type Name     = String
type Params   = [Name]
