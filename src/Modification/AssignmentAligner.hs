module Modification.AssignmentAligner where
import Tools.LogicTools
import Parsing.TokenMatchingTools


data ReasonToStartLine = ReasonToStartLine [String] Int deriving (Eq, Show)


newLineCauses :: [ReasonToStartLine]
newLineCauses = [
      ReasonToStartLine ["entity", "", "is"] 0
    , ReasonToStartLine ["entity", "", "is"] 1
    , ReasonToStartLine ["architecture", "", "of", "", "is"] 0
    , ReasonToStartLine ["architecture", "", "of", "", "is"] 1
    , ReasonToStartLine ["process", "("] 1
    , ReasonToStartLine ["generic", "("] 1
    , ReasonToStartLine ["port", "("] 1
    , ReasonToStartLine [")", ";"] 1
    , ReasonToStartLine [","] 0
    , ReasonToStartLine [";"] 0
    , ReasonToStartLine ["begin"] 0
    , ReasonToStartLine ["begin"] 1
    , ReasonToStartLine ["then"] 0
    ]


matchOneWord :: String -> String -> Bool
matchOneWord textTok matchTok
    | matchTok == "" = True
    | otherwise = textTok == matchTok


matchOnePhrase :: [String] -> [String] -> Bool
matchOnePhrase textTokens matchTokens = logicalAnd (map (\(x, y) -> matchOneWord x y) (zip textTokens matchTokens))


tryOneNewLineCause :: [String] -> ReasonToStartLine -> Bool 
tryOneNewLineCause los (ReasonToStartLine tokList n)
    | los == [] = False
    | tokList == [] = False
    | matchOnePhrase (take (length tokList) los) tokList = True
    | otherwise = tryOneNewLineCause los (ReasonToStartLine tokList n)


reasonMatches :: [String] -> ReasonToStartLine -> [ReasonToStartLine]
reasonMatches [] _ = []
reasonMatches los (ReasonToStartLine matchTokens n)
    | matchOnePhrase los matchTokens = [ReasonToStartLine matchTokens n]
    | otherwise = []


findFirstMatch :: [String] -> [ReasonToStartLine] -> [ReasonToStartLine]
findFirstMatch [] _ = []
findFirstMatch _ [] = []
findFirstMatch tokList (x:xs)
    | (reasonMatches tokList x) /= [] = reasonMatches tokList x
    | otherwise = findFirstMatch tokList xs                        


addNewlines :: [String] -> [String]
addNewlines los
    | los == [] = []
    | (logicalOr (map (\x -> tryOneNewLineCause (take 10 los) x) newLineCauses)) =
        ["\n"] ++ (take lengthOfMatch los) ++ addNewlines (skipNTokens los lengthOfMatch)
    | otherwise = [head los] ++ addNewlines (tail los)
        where
            x0 = head (findFirstMatch los newLineCauses)
            ReasonToStartLine matchTokens n = x0
            lengthOfMatch = length matchTokens


dontDoubleUpAfter :: [ReasonToStartLine]
dontDoubleUpAfter = [
      ReasonToStartLine [""] 0
    , ReasonToStartLine [""] 0
    ]


data ReasonToAppendSpace = ReasonToAppendSpace [String] Int deriving (Eq, Show)


appendSpaceCauses :: [ReasonToAppendSpace]
appendSpaceCauses = [
      ReasonToAppendSpace ["<="] 1
    , ReasonToAppendSpace [":="] 1
    , ReasonToAppendSpace [">="] 1
    , ReasonToAppendSpace ["/="] 1
    , ReasonToAppendSpace [","] 1
    , ReasonToAppendSpace [">"] 1
    , ReasonToAppendSpace ["<"] 1
    , ReasonToAppendSpace ["="] 1
    , ReasonToAppendSpace ["&"] 1
    , ReasonToAppendSpace ["*"] 1
    , ReasonToAppendSpace ["/"] 1
    , ReasonToAppendSpace ["-"] 1
    , ReasonToAppendSpace ["+"] 1
    , ReasonToAppendSpace [")"] 1
    , ReasonToAppendSpace [":"] 1
    , ReasonToAppendSpace [";"] 1
    ]


data ReasonToPrependSpace = ReasonToPrependSpace [String] Int deriving (Eq, Show)


prependSpaceCauses :: [ReasonToPrependSpace]
prependSpaceCauses = [
      ReasonToPrependSpace ["<="] 1
    , ReasonToPrependSpace [":="] 1
    , ReasonToPrependSpace [">="] 1
    , ReasonToPrependSpace [":"] 1
    , ReasonToPrependSpace ["="] 1
    , ReasonToPrependSpace ["*"] 1
    , ReasonToPrependSpace ["+"] 1
    , ReasonToPrependSpace ["-"] 1
    , ReasonToPrependSpace ["/"] 1
    , ReasonToPrependSpace ["<"] 1
    , ReasonToPrependSpace [">"] 1
    , ReasonToPrependSpace ["&"] 1
    ]


data ThingToAlign = ThingToAlign [String] deriving (Eq, Show)


alignCauses :: [ThingToAlign]
alignCauses = [
      ThingToAlign [":="]
    , ThingToAlign ["<="]
    , ThingToAlign [":"]
    ]


-- Note that changes in tab must not take effect until after the upcoming line ending. 
-- Note that if the same line has a reason to increase and a reason to decrease, the two
-- should cancel each other out. For example, elsif <condition> then. 
data ReasonToIncreaseTab = ReasonToIncreaseTab [String] deriving (Eq, Show)

increaseTabCauses :: [ReasonToIncreaseTab]
increaseTabCauses = [
      ReasonToIncreaseTab ["begin"]
    , ReasonToIncreaseTab ["for", "", "to"]
    , ReasonToIncreaseTab ["for", "", "downto"]
    , ReasonToIncreaseTab [""]
    , ReasonToIncreaseTab [""]
    , ReasonToIncreaseTab [""]
    ]

-- Note that changes in tab must not take effect until after the upcoming line ending. 
-- Note that if the same line has a reason to increase and a reason to decrease, the two
-- should cancel each other out. For example, elsif <condition> then. 
data ReasonToDecreaseTab = ReasonToDecreaseTab [String] deriving (Eq, Show)


decreaseTabCauses = [
      ReasonToDecreaseTab ["end"]
    , ReasonToDecreaseTab ["elsif"]
    , ReasonToDecreaseTab [""]
    , ReasonToDecreaseTab [""]
    ]


insertLineEndings :: [String] -> [String]
insertLineEndings _ = [""]


alignAssignments :: [String] -> [ThingToAlign] -> [String]
alignAssignments _ _ = [""]







