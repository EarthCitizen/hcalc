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
           deriving (Eq, Show)

mkErrorMsg :: Error -> String
mkErrorMsg (ArityMismatchError _ n ex ac) =
    printf "Arity mismatch on function: %s: expected: %i argument(s), but received: %i" n ex ac
mkErrorMsg (FunctionNotFoundError _ n) = printf "Function not found: %s" n
mkErrorMsg (ParseError _ msg) = printf "Parse error: %s" msg
mkErrorMsg (Error msg) = printf "Error: %s" msg

getLocation :: Error -> Maybe Location
getLocation (ArityMismatchError    l _ _ _) = Just l
getLocation (FunctionNotFoundError l _)     = Just l
getLocation (Error _)                       = Nothing
getLocation (ParseError l _)                = Just l

mkDetailedError :: Error -> [String]
mkDetailedError e =
    case getLocation e of
        Nothing        -> [mkErrorMsg e]
        Just (s, _, c) -> [s, mkPointTo c, mkErrorMsg e]


mkPointTo :: Integer -> String
mkPointTo  c = replicate (fromIntegral c - 1) ' ' <> "^"
