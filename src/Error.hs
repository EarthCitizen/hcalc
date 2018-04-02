module Error (Error(..), errorMsg) where

import Alias
import Text.Printf (printf)

data Error = ArityMismatchError Name Integer Integer
           | FunctionNotFoundError Name
           | Error String
           deriving (Show)

errorMsg :: Error -> String
errorMsg (ArityMismatchError n ex ac) =
    printf "Arity mismatch on function: %s: expected: %i argument(s), but received: %i\n" n ex ac
errorMsg (FunctionNotFoundError n) = printf "Function not found: %s\n" n
errorMsg (Error msg) = printf "Error: %s\n" msg
