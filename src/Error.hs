module Error (Error(..), mkDetailedError) where

import Alias
import Data.Semigroup ((<>))
import Text.Printf (printf)

type ExpParamCount = Integer
type ActParamCount   = Integer

data Error = ArityMismatchError    Location Name ExpParamCount ActParamCount
           | FunctionNotFoundError Location Name
           | Error Message
           | ParseError Location Message
           deriving (Show)

mkErrorMsg :: Error -> String
mkErrorMsg (ArityMismatchError _ n ex ac) =
    printf "Arity mismatch on function: %s: expected: %i argument(s), but received: %i" n ex ac
mkErrorMsg (FunctionNotFoundError _ n) = printf "Function not found: %s" n
mkErrorMsg (ParseError _ msg) = printf "Parse error: %s" msg
mkErrorMsg (Error msg) = printf "Error: %s" msg

getColumn :: Error -> Maybe Integer
getColumn (ArityMismatchError    (_, c) _ _ _) = Just c
getColumn (FunctionNotFoundError (_, c) _)     = Just c
getColumn (Error _)                            = Nothing
getColumn (ParseError (_, c) _)                = Just c

mkDetailedError :: String -> Error -> [String]
mkDetailedError l e =
    case getColumn e of Nothing -> [l, mkErrorMsg e]
                        Just c  -> [l, mkPointTo c, mkErrorMsg e]


mkPointTo :: Integer -> String
mkPointTo  c = replicate (fromIntegral c - 1) ' ' <> "^"
