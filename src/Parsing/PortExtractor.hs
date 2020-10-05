module Parsing.PortExtractor where
import Data.List
import Data.Maybe
import Parsing.GuaranteeWhitespace
import Parsing.TokenMatchingTools
import Rendering.InfoTypes
import Tools.StringTools
import Tools.ListTools
import Text.Printf


thisLineContainsPort :: String -> Bool
thisLineContainsPort someStr
    | length someStr < 4                =   False
    | take 4 someStr == "port"          =   True
    | otherwise                         =   thisLineContainsPort (tail someStr)


lineContainsName :: String -> String -> Bool
lineContainsName name line
    -- Note that if you pass in "port map" or "generic map" as the thing to search for, 
    -- the top condition will take precedence. Some explicit false conditions are necessary
    -- however because if you search for "port" or "generic", the search could be thrown
    -- off by "port map" and "generic map"
    | (take (length name)) line == name =   True                                                  
    | (take 8 line) == "port map"       =   False
    | (take 11 line) == "generic map"   =   False
    | otherwise                         =   lineContainsName name (tail line)


-- Send in an entire file as a list of strings (where each string is 1 token), 
-- and this returns everything after the keyword. If there is no port 
-- keyword, this returns an empty list.
locateKeyword :: String -> [String] -> [String]
locateKeyword keyword tokList
    | length tokList == 0               =   []
    -- Note: Insert any special cases here:
    | keyword == "port map" && 
      (tokList !! 1) == "port" && 
      (tokList !! 2) == "map"           =   (tail $ tail tokList)
    | keyword == "generic map" && 
      (tokList !! 1) == "generic" && 
      (tokList !! 2) == "map"           =   (tail $ tail tokList)
    | (head tokList) == keyword         =   tail tokList
    | otherwise                         =   locateKeyword keyword (tail tokList)


-- Pass in a list of tokens, and this will return the same list up until a 
-- closing parenthesis has been encountered.
stopAtClosingParen :: [String] -> Int -> Int -> [String]
stopAtClosingParen [] _ _ = []
stopAtClosingParen (x:xs) numOpening numClosing = 
    if (x == "(")
        then [x] ++ stopAtClosingParen xs (numOpening + 1) numClosing
        else if (x == ")")
                then if ((numOpening-1) == numClosing)
                        then [x] 
                        else [x] ++ stopAtClosingParen xs numOpening (numClosing + 1)
                else [x] ++ stopAtClosingParen xs numOpening numClosing


testTokens :: [String]
testTokens =   ["entity", "entity_name", "is", 
                "generic", "(", "width", ":", "integer", ":=", "8", ";",
                "depth", ":", "std_ulogic", ":=", "64", ";",
                "rst_val", ":", "std_logic_vector", "(", "7", "downto", "0", ")", ";",
                "other_val", ":", "std_logic", ":=", "'1'", ")", ";",
                "port", "(", "clk", ":", "in", "std_logic", ";",
                "rst", ":", "in", "std_logic", ":=", "'0'", ";",
                "p", ":", "out", "std_logic_vector", "(", "width", "-", "1", "downto", "0", ")", ";",
                "r", ":", "out", "std_logic_vector", "(", "width", "-", "1", "downto", "0", ")", ";",
                "q", ":", "out", "std_logic_vector", "(", "width", "-", "1", "downto", "0", ")", ")", ";",
                "end", "entity_name", ";",
                "architecture", "behavioral", "of", "entity_name", "is",
                "signal", "q0", ":", "std_logic", ";",
                "signal", "q1", ":", "unsigned", "(", "7", "downto", "0", ")", ";",
                "signal", "q2", ":", "signed", "(", "31", "downto", "0", ")", ";",
                "begin",
                "process", "(", "clk", ")",
                "var", "x", "std_logic_vector", ":=", "\"0000001010101111\"", ";",
                "begin",
                "x", "<=", "y", ";",
                "end", "process", ";",
                "end", "behavioral", ";"]


-- Use this function to search for "port" or "generic", then return everything
-- after that until the closing parenthesis. Note that 1st and last tokens are 
-- parentheses. 
extractDeclaration :: String -> [String] -> [String]
extractDeclaration keyword tokList = stopAtClosingParen (locateKeyword keyword tokList) 0 0


-- Returns index of the first semicolon found in a list of strings.
-- Returns -1 if no semicolon is found.
firstSemicolon :: [String] -> Int -> Int
firstSemicolon [] n = (-1)
firstSemicolon (x:xs) n = if (x == ";") then n
                                        else firstSemicolon xs (n+1)


-- Returns index of first := operator found in a list of strings.
-- Returns -1 if no such token is found.
firstDefaultAssignment :: [String] -> Int -> Int
firstDefaultAssignment [] n = (-1)
firstDefaultAssignment (x:xs) n 
    | (x == ";") = (-1)
    | (x == ":=") = n
    | otherwise = firstDefaultAssignment xs (n+1)


-- Assume this function is fed by extractDeclaration, above, except outermost ('s & )'s
-- have been removed:
genericHasDefault :: [String] -> Bool
genericHasDefault xs
    | (length xs <= 6)              = False
    | (firstDefaultAssignment xs 0 == (-1)) = False
    | (firstSemicolon xs 0 == (-1)) = (firstDefaultAssignment xs 0) > 0
    | otherwise                     = (firstSemicolon xs 0) > (firstDefaultAssignment xs 0) 


portHasDefault :: [String] -> Bool
portHasDefault xs 
    | (length xs <= 7)              = False
    | (firstDefaultAssignment xs 0 == (-1)) = False
    | (firstSemicolon xs 0 == (-1)) = (firstDefaultAssignment xs 0) > 0
    | otherwise                     = (firstSemicolon xs 0) > (firstDefaultAssignment xs 0) 


-- This function assumes the head of the list is the name of a declaration,
-- either generic or port. This function will locate the semicolon, and 
-- remove everything up to and including the semicolon. If the semicolon is
-- not found, this function returns an empty list.
remove1Declaration :: [String] -> [String]
remove1Declaration []               = []
remove1Declaration xs
    | (firstSemicolon xs 0) < 0     = []
    | otherwise                     = repTail n xs where
                                      n = (firstSemicolon xs 0) + 1


extract1Declaration :: [String] -> [String]
extract1Declaration []              = []
extract1Declaration xs
    | (firstSemicolon xs 0) < 0     = []
    | otherwise                     = take n xs where
                                      n = (firstSemicolon xs 0) + 1


containsSubstr :: String -> String -> Bool
containsSubstr "" _ = False
containsSubstr _ "" = False
containsSubstr someStr searchFor
    | (take (length searchFor) someStr) == searchFor = True
    | otherwise = containsSubstr (tail someStr) searchFor


-- This function assumes that tokens early in the [String] describe
-- an integer. This function's job is to help you figure out if you're
-- dealing with a constrained integer or an unconstrained one. If 
-- constrained, this function extracts the limits. 
resolveConstrainedness :: [String] -> DataType
resolveConstrainedness tokList
    | elem "downto" (take 3 tokList)        = ConstrainedInt 0 0 
    | elem "to" (take 3 tokList)            = ConstrainedInt 0 0 
    | otherwise                             = UnconstrainedInt


inferDatatype :: [String] -> DataType
inferDatatype (oneTok:moreTokens)
    | containsSubstr oneTok "std_logic_vector" = StdLogicVector
    | containsSubstr oneTok "std_ulogic_vector" = StdULogicVector
    | containsSubstr oneTok "std_logic"     = StdLogic
    | containsSubstr oneTok "std_ulogic"    = StdULogic
    | containsSubstr oneTok "unsigned"      = Unsigned
    | containsSubstr oneTok "signed"        = Signed
    | containsSubstr oneTok "bit"           = Bit
    | containsSubstr oneTok "integer"       = resolveConstrainedness moreTokens
    | otherwise                             = UserDefinedDataType oneTok


is0Thru9 :: Char -> Bool
is0Thru9 c = elem c ['0'..'9']


tokenContainsInt :: String -> Bool
tokenContainsInt ""                         = False
tokenContainsInt s
    | (length s) < 1                        = False
    | (s !! 0) == '-' && is0Thru9 (s !! 1)  = True
    | is0Thru9 (s !! 0)                     = True
    | otherwise                             = False


-- This function starts at opening paren, and searches until "downto".
-- Excludes paren and "downto":
findParenToDownto :: [String] -> [String]
findParenToDownto los = afterKeyword (untilKeyword los ["downto"] []) ["("]


-- This function starts at "to" and continues to the closing paren.
-- Excludes "to" and paren.
findToToParen :: [String] -> [String]
findToToParen los = afterKeyword (untilKeyword los [")"] []) ["to"]


-- NOTE: This function fails when last token is not a semicolon.
extractWidthExpression :: [String] -> [String]
extractWidthExpression [] = []
extractWidthExpression xs
    | (usesTo xs)                           = findToToParen xs
    | (usesDownto xs)                       = findParenToDownto xs
    | otherwise                             = extractWidthExpression (tail xs)


startsWithNum :: String -> Bool
startsWithNum s = elem (head s) ['0'..'9']


-- This function decides whether a string is a variable (or port, signal, generic, etc.)
-- soley on whether the first character in the string is a letter. 
-- This function will break if you feed it a hex literal, but since nobody uses hex literals
-- to define vector widths in VHDL, I'm not worried about it. 
isVariable :: String -> Bool
isVariable s = not (startsWithNum s)


-- This drops the last token in a list if that token is an operator that is allowed in range expressions.
-- The purpose for doing this is so that if you have:
--      w - 1
-- and the 1 is stripped off, the - will be stripped off as well. 
dropLastTokIfOp :: [String] -> [String]
dropLastTokIfOp [] = []
dropLastTokIfOp los
    | (isRangeOperator (last los)) = dropLast los
    | otherwise = los


extractWidthVariable :: [String] -> String
extractWidthVariable [] = "baloney"
extractWidthVariable los = 
    if (length (extractWidthExpression los)) > 0 
        --then (intercalate " " (extractWidthExpression los))
        then (head (extractWidthExpression los))
        else "this line failed" 
    --intercalate " " 
        --(dropLastTokIfOp [s | s <- (extractWidthExpression los), (Parsing.PortExtractor.isVariable s) || (isRangeOperator s)])
    

usesTo :: [String] -> Bool
usesTo los = elem "to" (untilKeywordIncEnd los [";"] [])


usesDownto :: [String] -> Bool
usesDownto los = elem "downto" (untilKeywordIncEnd los [";"] [])


rangeToken :: String -> Bool
rangeToken "downto"                         = True
rangeToken "to"                             = True
rangeToken _                                = False


endOfDeclaration :: String -> Bool
endOfDeclaration ";"                        = True
endOfDeclaration _                          = False

--untilClosingParen [] n = 
--    if (n > 1)
--        then error "Unbalanced parentheses"
--        else []

untilClosingParen :: [String] -> Int -> [String]
untilClosingParen [] _ = []
untilClosingParen los n
    -- TODO: Check whether it is possible for function to terminate with 
    -- unbalanced parentheses without throwing error
    | (n < 0) = error "Unbalanced parentheses"
    | ((head los) == ";") = []
--    | (((head los) /= "(") && (n == 0)) = untilClosingParen (tail los) 0
    | ((head los) == "(") = [head los] ++ (untilClosingParen (tail los) (n + 1))
    | (((head los) == ")") && (n == 1)) = [head los]
    | ((head los) == ")") = [head los] ++ (untilClosingParen (tail los) (n - 1))
    | otherwise = [head los] ++ (untilClosingParen (tail los) n)


-- Define a list of operators that are allowed in range declarations:
rangeOperatorList :: [String]
rangeOperatorList = ["+", "-", "*", "/", "**"]


-- Make a descriptive name to determine if a token is a range operator:
isRangeOperator :: String -> Bool
isRangeOperator s = elem s rangeOperatorList


-- Returns True if a list of strings is all numbers or range operators.
-- Returns False otherwise. 
-- allNumericOrOpToks :: [String] -> Bool
-- allNumericOrOpToks [] = True
-- allNumericOrOpToks los
    -- | (not (isRangeOperator (head los))) && (not (is0Thru9 (head (head los)))) = False
    -- | otherwise = allNumericOrOpToks (tail los)


terminateOnMinus :: [String] -> [String]
terminateOnMinus [] = []
terminateOnMinus los
    | (head los) == "-" = []
    | otherwise = [head los] ++ terminateOnMinus (tail los)

-- This function drops tokens until it encounters an opening parenthesis. 
-- Then it builds a list until closing parenthesis is found. 
-- If your VHDL file is syntactically correct, first and last tokens should
-- always be ()'s. 
extractWidthTo' :: [String] -> [String]
extractWidthTo' [] = []
extractWidthTo' (x:xs)
    | (x == "(") = 
        if (usesTo (x:xs))
            then dropLast (skipN (untilClosingParen (x:xs) 0) 3)
            else tail (terminateOnMinus (untilKeyword (x:xs) ["downto"] []))
    | (x == ";") = []
    | otherwise = extractWidthTo' xs


containsParens :: [String] -> Bool
containsParens [] = False
containsParens (x:xs)
    | (x == "(") = True
    | (x == ")") = True
    | otherwise = containsParens xs
 

tokList2Width :: [String] -> Width
tokList2Width [] = Soft "Soft, Soft Baloney" -- TODO: Delete this line. Delete all baloney references
tokList2Width los = Soft (extractWidthVariable los)

skipDataType :: [String] -> [String]
skipDataType los = afterAny los 
    [ ["std_logic_vector"]
    , ["std_ulogic_vector"]
    , ["std_logic"]
    , ["std_ulogic"]
    , ["signed"]
    , ["unsigned"]
    , ["integer"]
    ]


extractWidth :: [String] -> Width
extractWidth xs
    | (length xs) < 2                       = WidthNotSpecified
    | (not (containsParens (untilKeyword xs [";"] []))) = WidthNotSpecified
    | (usesTo (untilKeywordIncEnd xs [";"] []))   = 
        tokList2Width (["(", "0", "to"] ++ (afterKeyword (untilKeywordIncEnd xs [";"] []) ["to"]))

    | (usesDownto (untilKeywordIncEnd xs [";"] [])) = 
        tokList2Width (untilKeywordIncEnd (skipDataType xs) [";"] [])

    | otherwise                             = extractWidth (tail xs)


-- Note: This breaks under this condition:
-- extractPortDefault  (tokenize ["r : out std_logic_vector := (others => '1')))))))))))))))))))))));"])
extractPortDefault :: [String] -> DefaultValue
extractPortDefault xs
    | portHasDefault xs                     = Specified (xs !! 5)
    | otherwise                             = Unspecified


-- Note: This breaks under this condition:
-- extractGenericDefault  (tokenize ["r : out std_logic_vector := (others => '1')))))))))))))))))))))));"])
extractGenericDefault :: [String] -> DefaultValue
extractGenericDefault xs
    | genericHasDefault xs                  = Specified (xs !! 4)
    | otherwise                             = Unspecified


-- Assume this function is fed by extractDeclaration "generic", above, and surrounding 
-- parentheses have been removed.
extractGenerics :: [String] -> [Information]
extractGenerics x 
    | (length x) < 2                    = []
    | otherwise = [Generic {  nomen     =   x !! 0
                           , dataType  =   inferDatatype (tail (tail x))
                           , width     =   extractWidth x
                           , sDefault  =   (extractGenericDefault x)
                           , comments  =   [""]}
                           ] ++ extractGenerics (remove1Declaration x)

-- TODO:
--      2. Fix conversion of std_logic generic to std_logic_vector constant

extractPorts :: [String] -> [Information]
extractPorts x
    | (length x) < 2    = []
    | otherwise         = [Port {   nomen = x !! 0
                                ,   dataType = inferDatatype (tail (tail (tail x)))
                                ,   width = extractWidth x
                                ,   direction = if ((x !! 2) == "in")
                                                    then In
                                                    else Out
                                ,   sDefault = (extractPortDefault x)
                                ,   sReset = (makeResetVal (inferDatatype (tail (tail (tail x)))))
                                ,   clocked = Nothing
                                ,   comments = []
                                ,   assertionLevel = Nothing
                                }] ++ extractPorts (remove1Declaration x) 


-- TODO: Replace nameIsInList with elem
nameIsInList :: String -> [String] -> Bool
nameIsInList _ []           = False
nameIsInList name (x:xs)    = if (name == x) then True
                              else nameIsInList name xs


isPort :: String -> [String] -> Bool
isPort name tokList = nameIsInList name $ extractDeclaration "port" tokList


isGeneric :: String -> [String] -> Bool
isGeneric name tokList = nameIsInList name $ extractDeclaration "generic" tokList


isSignal :: String -> [String] -> Bool
isSignal _ [] = False
isSignal name tokList
    | (head tokList) == "signal" = if (name == (tokList !! 1))
                                        then True
                                        else isSignal name (tail tokList)
    | otherwise = isSignal name (tail tokList)


nameOneSig :: String -> Integer -> String
nameOneSig stub d = "s__" ++ stub ++ "__d" ++ (printf "%06d" d)


nameOneVar :: String -> String
nameOneVar stub = "v__" ++ stub 


nameOneInput :: String -> Integer -> String
nameOneInput stub d = "i__" ++ stub ++ "__d" ++ (printf "%06d" d)


nameOneOutput :: String -> Integer -> String
nameOneOutput stub d = "o__" ++ stub ++ "__d" ++ (printf "%06d" d)


isSigName :: String -> Bool
isSigName s = (take 3 s) == "s__"


isVarName :: String -> Bool
isVarName s = (take 3 s) == "v__"


isInputName :: String -> Bool
isInputName s = (take 3 s) == "i__"


isOutputName :: String -> Bool
isOutputName s = (take 3 s) == "o__"


hasDelay :: String -> Bool
hasDelay s = containsSubStr s "__d"


findDelayIdx :: String -> Maybe Integer
findDelayIdx s = findSubStrIdx s "__d" 0


extractDelay :: String -> Maybe Integer
extractDelay s
    | findDelayIdx s == Nothing = Nothing
    | otherwise = Just (read (skipNChars s (3 + (fromMaybe 0 (findDelayIdx s)))) :: Integer)


