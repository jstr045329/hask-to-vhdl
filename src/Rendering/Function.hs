module Rendering.Function where
import Rendering.InfoTypes
import Rendering.VhdMath
import Rendering.Assignment
import Tools.WhiteSpaceTools


-- fInputs should be VhdSig's
data Function = FunctionDef {
      fNomen :: String
    , fInputs :: [Information]
    , fConstants :: [Information]
    , fVariables :: [Information]
    , returnTypeAs :: Information
    , fLines :: [String]
    }

    | FunctionCall {
      fNomen :: String
    , fInputs :: [Information]
    , returnDest :: Information
    } deriving (Eq, Show)


renderFunction :: Function -> [String]
renderFunction (FunctionCall _ _ _) = error "This should never happen"
renderFunction fDef =
    ["function " ++ (fNomen fDef) ++ "("] ++ 
    (zipTab (declareBatch (fInputs fDef))) ++
    [(tab 1) ++ " return " ++ (datatypeToStr (dataType (returnTypeAs fDef)) (width (returnTypeAs fDef))) ++ " is"] ++
    (zipTab (fLines fDef)) ++
    ["end function;"]


getFuncInputList :: [Information] -> String
getFuncInputList fInList
    | fInList == [] = ""
    | (length fInList) == 1 = (nomen (head fInList))
    | otherwise = (nomen (head fInList)) ++ ", " ++ (getFuncInputList (tail fInList))


callFunction :: Function -> [String]
callFunction (FunctionDef _ _ _ _ _ _) = error "This should never happen"
callFunction fCall =
    [(nomen rDest) ++ " " ++ (assignmentOperator rDest) ++ " " ++ (fNomen fCall) ++
    "(" ++ (getFuncInputList (fInputs fCall)) ++ ");"]
    where
        rDest = returnDest fCall

 
