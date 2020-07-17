module Rendering.Process where
import Rendering.Assignment
import Rendering.Condition
import Rendering.InfoTypes
import Rendering.VhdMath
import Tools.ListTools
import Tools.WhiteSpaceTools
import Rendering.SemicolonTools
import Rendering.FilterUnique
import Rendering.SetRenderingTools


data ClkEdge = Rising | Falling deriving (Eq, Show)


data Process = Process {
          pNomen :: String
        , pClk :: Information
        , pRst :: Information
        , syncronousReset :: Bool
        , clkEdge :: ClkEdge
        , sensitivityList :: [Information]
        , inputSignals :: [Information]
        , variables :: [Information]
        , internalState :: [Information]
        , outputSignals :: [Information]
        , assignments :: [(Condition, [Assignment], [Assignment])]
        } deriving (Eq, Show)


-- Generates the header for a process, with optional name:
processHeader :: String -> [Information] -> String
processHeader "" sigList = "process(" ++ (joinWithCommas (Prelude.map (\x -> nomen x) sigList)) ++ ")"
processHeader pName sigList = pName ++ ": " ++ (processHeader "" sigList)


processFooter :: String -> [String]
processFooter "" = ["end process;"]
processFooter s = ["end process " ++ s ++ ";"]


ifClockEdge :: ClkEdge -> Information -> String
ifClockEdge Rising someClk = "if rising_edge(" ++ (nomen someClk) ++ ") then"
ifClockEdge Falling someClk = "if falling_edge(" ++ (nomen someClk) ++ ") then"


endClockEdge :: [String]
endClockEdge = ["end if;"]


resetStructure :: Information -> [Information] -> [String]
resetStructure rstSig sigList =
    ["if " ++ (nomen rstSig) ++ " = " ++ (liftAssertion (assertionLevel rstSig)) ++ " then"] ++ 
    (zipTab (resetBatch (filterUnique sigList))) ++ 
    ["else"]


endResetStructure :: [String]
endResetStructure = ["end if;"]


handleClockAndReset :: Process -> [String]
handleClockAndReset p =
    if (syncronousReset p)
        then
            [(ifClockEdge (clkEdge p) (pClk p))] ++ 
            (zipTab (resetStructure (pRst p) ((internalState p) ++ (outputSignals p))))

        else 
            (resetStructure (pRst p) ((internalState p) ++ (outputSignals p))) ++ 
            [(tab 1) ++ (ifClockEdge (clkEdge p) (pClk p))]


endClockAndReset :: [String]
endClockAndReset = [(tab 1) ++ "end if;", "end if;"]


unpackElse :: [Assignment] -> [String]
unpackElse [] = []
unpackElse aList = assignBatch aList


unpackOneCondition :: Condition -> [Assignment] -> [Assignment] -> [String]
unpackOneCondition JustTrue aList _ = semiWrapBatch (assignBatch aList)
unpackOneCondition c aListT aListF = 
    ["if " ++ (cond2Str c) ++ " then"] ++ 
    (zipTab (semiWrapBatch (assignBatch aListT))) ++
    (if (length (unpackElse aListF) > 0)
        then (["else"] ++ (semiWrapBatch (zipTab (unpackElse aListF))))
        else []) ++ 
    ["end if;"]


unpackConditions :: [(Condition, [Assignment], [Assignment])] -> [String]
unpackConditions [] = []
unpackConditions cList =
    (unpackOneCondition c aT aF) ++ unpackConditions (tail cList) where
        (c, aT, aF) = head cList


-- TODO: Make inclusion of rst in sensitivity list contingent on async reset
renderProcess :: Process -> [String]
renderProcess p = 
    [(processHeader (pNomen p) (sensitivityList p))] ++
    (declareBatch (variables p)) ++ 
    ["begin"] ++ 
    (zipTab ((handleClockAndReset p) ++ 
                (zipTab (zipTab (unpackConditions (assignments p)))) ++
                endClockAndReset)) ++ 
    (processFooter (pNomen p))
    


inputsFromCondition :: Condition -> [Information]
inputsFromCondition JustTrue = []
inputsFromCondition (Not c) = inputsFromCondition c
inputsFromCondition (JustInfo i) = [i]

inputsFromCondition (Princess []) = []
inputsFromCondition (Pauper []) = []
inputsFromCondition (And []) = []
inputsFromCondition (Or []) = []
inputsFromCondition (Xor []) = []
inputsFromCondition (Nand []) = []
inputsFromCondition (Nor []) = []
inputsFromCondition (Xnor []) = []

inputsFromCondition (Princess cList) = union (inputsFromCondition (head cList)) (inputsFromCondition (Princess (tail cList)))
inputsFromCondition (Pauper cList) = union (inputsFromCondition (head cList)) (inputsFromCondition (Pauper (tail cList)))
inputsFromCondition (And cList) = union (inputsFromCondition (head cList)) (inputsFromCondition (And (tail cList)))
inputsFromCondition (Or cList) = union (inputsFromCondition (head cList)) (inputsFromCondition (Or (tail cList)))
inputsFromCondition (Xor cList) = union (inputsFromCondition (head cList)) (inputsFromCondition (Xor (tail cList)))
inputsFromCondition (Nand cList) = union (inputsFromCondition (head cList)) (inputsFromCondition (Nand (tail cList)))
inputsFromCondition (Nor cList) = union (inputsFromCondition (head cList)) (inputsFromCondition (Nor (tail cList)))
inputsFromCondition (Xnor cList) = union (inputsFromCondition (head cList)) (inputsFromCondition (Xnor (tail cList)))
inputsFromCondition (GreaterT i1 i2) = [i1, i2]
inputsFromCondition (LesserT i1 i2) = [i1, i2]
inputsFromCondition (EqualTo i1 i2) = [i1, i2]
inputsFromCondition (GreaterTEq i1 i2) = [i1, i2]
inputsFromCondition (LessTEq i1 i2) = [i1, i2]

inputFromAssignment :: Assignment -> Information
inputFromAssignment (Assignment _ i) = i


outputFromAssignment :: Assignment -> Information
outputFromAssignment (Assignment i _) = i


getInputs' :: (Condition, [Assignment], [Assignment]) -> [Information]
getInputs' (oneCondition, aList1, aList2) = 
    union
        ((Prelude.map inputFromAssignment aList1) ++ (Prelude.map inputFromAssignment aList2))
        (inputsFromCondition oneCondition)


getOutputs' :: (Condition, [Assignment], [Assignment]) -> [Information]
getOutputs' (_, aList1, aList2) = filterUnique ((Prelude.map outputFromAssignment aList1) ++ (Prelude.map outputFromAssignment aList2))


getInputs :: [(Condition, [Assignment], [Assignment])] -> [Information]
getInputs [] = []
getInputs someList 
    | ((length someList) == 1) = getInputs' (head someList)
    | otherwise = union (getInputs' (head someList)) (getInputs (tail someList))


getOutputs :: [(Condition, [Assignment], [Assignment])] -> [Information]
getOutputs [] = []
getOutputs someList 
    | ((length someList) == 1) = getOutputs' (head someList)
    | otherwise = union (getOutputs' (head someList)) (getOutputs (tail someList))


getState :: [(Condition, [Assignment], [Assignment])] -> [Information]
getState someList = intersection'
                        (getInputs someList)
                        (getOutputs someList)


easyProcess :: String -> [(Condition, [Assignment], [Assignment])] -> Process
easyProcess nm aList = Process {
      pNomen = nm
    , pClk = easyClk
    , pRst = easyRst
    , syncronousReset = True
    , clkEdge = Rising
    , sensitivityList = [easyClk]
    , inputSignals = getInputs aList
    , variables = []
    , internalState = getState aList
    , outputSignals = getOutputs aList
    , assignments = aList
    }


addSensitivity :: Process -> Information -> Process
addSensitivity p i = Process {
      pNomen = pNomen p
    , pClk = pClk p
    , pRst = pRst p
    , syncronousReset = syncronousReset p
    , clkEdge = clkEdge p
    , sensitivityList = [i] ++ sensitivityList p
    , inputSignals = inputSignals p
    , variables = variables p
    , internalState = internalState p
    , outputSignals = outputSignals p
    , assignments = assignments p
    }


makeFalling :: Process -> Process
makeFalling p = Process {
      pNomen = pNomen p
    , pClk = pClk p
    , pRst = pRst p
    , syncronousReset = syncronousReset p
    , clkEdge = Falling
    , sensitivityList = sensitivityList p
    , inputSignals = inputSignals p
    , variables = variables p
    , internalState = internalState p
    , outputSignals = outputSignals p
    , assignments = assignments p
    }


asyncReset :: Process -> Process
asyncReset p = Process {
      pNomen = pNomen p
    , pClk = pClk p
    , pRst = pRst p
    , syncronousReset = False
    , clkEdge = clkEdge p
    , sensitivityList = sensitivityList p
    , inputSignals = inputSignals p
    , variables = variables p
    , internalState = internalState p
    , outputSignals = outputSignals p
    , assignments = assignments p
    }


addAssignment :: Process -> (Condition, [Assignment], [Assignment]) -> Process
addAssignment p newAssignment = Process {
      pNomen = pNomen p
    , pClk = pClk p
    , pRst = pRst p
    , syncronousReset = syncronousReset p
    , clkEdge = clkEdge p
    , sensitivityList = sensitivityList p
    , inputSignals = getInputs ([newAssignment] ++ assignments p)
    , variables = variables p
    , internalState = getState ([newAssignment] ++ assignments p)
    , outputSignals = getOutputs ([newAssignment] ++ assignments p)
    , assignments = [newAssignment] ++ assignments p
    }


-- Use this function as a template for modifying processes:
modProcess p = Process {
      pNomen = pNomen p
    , pClk = pClk p
    , pRst = pRst p
    , syncronousReset = syncronousReset p
    , clkEdge = clkEdge p
    , sensitivityList = sensitivityList p
    , inputSignals = inputSignals p
    , variables = variables p
    , internalState = internalState p
    , outputSignals = outputSignals p
    , assignments = assignments p
    }




