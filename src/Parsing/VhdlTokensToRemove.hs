------------------------------------------------------------------------------------------------------------------------
--                                   Represent Words That Should Not Be Info Names 
--
-- This module contains a list of words that should never make it into results for signals, ports, variables, etc. 
--
------------------------------------------------------------------------------------------------------------------------
module Parsing.VhdlTokensToRemove where 
import qualified Data.HashSet as HashSet


tokensToRemove :: HashSet.HashSet String
tokensToRemove = HashSet.fromList [
         "abs"
    ,    "access"
    ,    "after"
    ,    "alias"
    ,    "all"
    ,    "and"
    ,    "architecture"
    ,    "array"
    ,    "assert"
    ,    "attribute"
    ,    "begin"
    ,    "block"
    ,    "body"
    ,    "buffer"
    ,    "bus"
    ,    "case"
    ,    "component"
    ,    "configuration"
    ,    "constant"
    ,    "disconnect"
    ,    "downto"
    ,    "else"
    ,    "elsif"
    ,    "end"
    ,    "entity"
    ,    "exit"
    ,    "file"
    ,    "for"
    ,    "function"
    ,    "generate"
    ,    "generic"
    ,    "group"
    ,    "guarded"
    ,    "if"
    ,    "impure"
    ,    "in"
    ,    "inertial"
    ,    "inout"
    ,    "is"
    ,    "label"
    ,    "library"
    ,    "linkage"
    ,    "literal"
    ,    "loop"
    ,    "map"
    ,    "mod"
    ,    "nand"
    ,    "new"
    ,    "next"
    ,    "nor"
    ,    "not"
    ,    "null"
    ,    "of"
    ,    "on"
    ,    "open"
    ,    "or"
    ,    "others"
    ,    "out"
    ,    "package"
    ,    "port"
    ,    "postponed"
    ,    "procedure"
    ,    "process"
    ,    "pure"
    ,    "range"
    ,    "record"
    ,    "register"
    ,    "reject"
    ,    "return"
    ,    "rol"
    ,    "ror"
    ,    "select"
    ,    "severity"
    ,    "signal"
    ,    "shared"
    ,    "sla"
    ,    "sli"
    ,    "sra"
    ,    "srl"
    ,    "subtype"
    ,    "then"
    ,    "to"
    ,    "transport"
    ,    "type"
    ,    "unaffected"
    ,    "units"
    ,    "until"
    ,    "use"
    ,    "variable"
    ,    "wait"
    ,    "when"
    ,    "while"
    ,    "with"
    ,    "xnor"
    ,    "xor"
    ,    "<="
    ,    "=>"
    ,    ":="
    ,    "/="
    ,    "--"
    ,    "**"
    ,    "="
    ,    "<"
    ,    ">"
    ,    "("
    ,    ")"
    ,    "+"
    ,    "-"
    ,    "*"
    ,    "/"
    ,    ","
    ,    ":"
    ,    ";"
    ,    "abs"
    ,    "rem"
    ,    "mod"
    ,    "<="
    ,    ">="
    ,    "shift_left"
    ,    "shift_right"
    ,    "rotate_left"
    ,    "rotate_right"
    ,    "sll"
    ,    "srl"
    ,    "resize"
    ,    "to_integer"
    ,    "to_signed"
    ,    "to_unsigned"
    ,    "std_match"
    ,    "unsigned"
    ,    "signed"
    ]


isTokenToRemove :: String -> Bool
isTokenToRemove s = elem s tokensToRemove


