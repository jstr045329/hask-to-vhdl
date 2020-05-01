module Rendering.Procedure where
import Rendering.InfoTypes
import Rendering.VhdMath
import Tools.WhiteSpaceTools


data Procedure = ProcedureDef {
      pNomen :: String
    , pInputs :: [Information]
    , pOutputs :: [Information]
    , pInouts :: [Information]
    , pConstants :: [Information]
    , pVariables :: [Information]
    , returnTypeAs :: Maybe Information
    , pLines :: [String]
    , explicitSig :: Maybe [Bool] -- Nothing means make no inputs explicitly a signal
    , aspireToSynth :: Bool
    }   

    | ProcedureCall {
      pNomen :: String
    , pInputs :: [Information]
    , pOutputs :: [Information]
    , pInouts :: [Information]
    , returnDest :: Assignment
    , aspireToSynth :: Bool
    } deriving (Eq, Show)


renderProcedure :: Procedure -> [String]
renderProcedure (ProcedureCall _ _ _ _ _ _) = error "This should never happen"
renderProcedure fDef =
    ["procedure " ++ (pNomen fDef) ++ "("] ++  
    (zipTab (declareBatch (pInputs fDef))) ++ 
--    [(tab 1) ++ " return " ++ (datatype2Str (dataType (returnTypeAs fDef)) (width (returnTypeAs fDef))) ++ " is"] ++
    (zipTab (pLines fDef)) ++
    ["end procedure;"]

-- TODO: Decide if this file is worth fleshing out

