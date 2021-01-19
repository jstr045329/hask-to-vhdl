"""This script generates skeleton code for ScrapeInputs.hs"""


# TODO: Add time amounts to keywords - ns, ms, us. 
VHDL_KEYWORDS = [
        "abs"
    ,   "access"
    ,   "after"
    ,   "alias"
    ,   "all"
    ,   "and"
    ,   "architecture"
    ,   "array"
    ,   "assert"
    ,   "attribute"
    ,   "begin"
    ,   "block"
    ,   "body"
    ,   "buffer"
    ,   "bus"
    ,   "case"
    ,   "component"
    ,   "configuration"
    ,   "constant"
    ,   "disconnect"
    ,   "downto"
    ,   "else"
    ,   "elsif"
    ,   "end"
    ,   "entity"
    ,   "exit"
    ,   "file"
    ,   "for"
    ,   "function"
    ,   "generate"
    ,   "generic"
    ,   "group"
    ,   "guarded"
    ,   "if"
    ,   "impure"
    ,   "in"
    ,   "inertial"
    ,   "inout"
    ,   "is"
    ,   "label"
    ,   "library"
    ,   "linkage"
    ,   "literal"
    ,   "loop"
    ,   "map"
    ,   "mod"
    ,   "nand"
    ,   "new"
    ,   "next"
    ,   "nor"
    ,   "not"
    ,   "null"
    ,   "of"
    ,   "on"
    ,   "open"
    ,   "or"
    ,   "others"
    ,   "out"
    ,   "package"
    ,   "port"
    ,   "postponed"
    ,   "procedure"
    ,   "process"
    ,   "pure"
    ,   "range"
    ,   "record"
    ,   "register"
    ,   "reject"
    ,   "return"
    ,   "rol"
    ,   "ror"
    ,   "select"
    ,   "severity"
    ,   "signal"
    ,   "shared"
    ,   "sla"
    ,   "sli"
    ,   "sra"
    ,   "srl"
    ,   "subtype"
    ,   "then"
    ,   "to"
    ,   "transport"
    ,   "type"
    ,   "unaffected"
    ,   "units"
    ,   "until"
    ,   "use"
    ,   "variable"
    ,   "wait"
    ,   "when"
    ,   "while"
    ,   "with"
    ,   "xnor"
    ,   "xor"
    ]

VHDL_OPERATORS = [
      "<=" 
    , "=>" 
    , ":=" 
    , "/="
    , "--" # NOTE: Where the results of this script are being used, comments should already be filtered out. It should be okay to delete this branch from generated Hs.
    , "**"
    , "=" 
    , "<" 
    , ">" 
    , "(" 
    , ")" 
    , "+" 
    , "-" 
    , "*" 
    , "/" 
    , "," 
    , ":" 
    , ";" 
    ]  


if __name__ == "__main__":
    from NumericStd import NUMERIC_STD_FUNCTIONS
    def tab(n=1):
        return "    " * n

    for s in VHDL_KEYWORDS + VHDL_OPERATORS + NUMERIC_STD_FUNCTIONS:
        print(tab() + '| (((length los) > 1) && ((los !! 0) == "%s")) = []' % (s,))

    print("\n\n\n--            Skeleton For Filtering IN Above")
    print("--               Skeleton For Filtering OUT Below\n\n\n")
    print("tokensToRemove = [")
    for s in VHDL_KEYWORDS + VHDL_OPERATORS + NUMERIC_STD_FUNCTIONS:
        print(tab() + "," + tab() + '"' + s + '"')
    print("]")
    


