module Parsing.NumericStdFunctions where
import Data.HashSet


numericStdFunctions :: HashSet String
numericStdFunctions = fromList [
        "abs"
    ,   "rem"
    ,   "mod"
    ,   "<="
    ,   ">="
    ,   "shift_left"
    ,   "shift_right"
    ,   "rotate_left"
    ,   "rotate_right"
    ,   "sll"
    ,   "srl"
    ,   "resize"
    ,   "to_integer"
    ,   "to_signed"
    ,   "to_unsigned"
    ,   "std_match"
    ,   "unsigned"
    ,   "signed"
    ]


isNumericStd :: String -> Bool
isNumericStd s = elem s numericStdFunctions


