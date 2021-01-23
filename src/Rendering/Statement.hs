------------------------------------------------------------------------------------------------------------------------
--                                                Statement Data Type 
------------------------------------------------------------------------------------------------------------------------
module Rendering.Statement where
import Rendering.Assignment
import Rendering.PortMap
import Rendering.Condition
import Rendering.InfoTypes
import Tools.WhiteSpaceTools
import Rendering.ElseTerm
import Tools.ListTools


------------------------------------------------------------------------------------------------------------------------
--                                          Represent Concurrent Statements 
------------------------------------------------------------------------------------------------------------------------
data ConcurrentStatement =
        ConBatch [Assignment]
    |   Instances [PortMap]
        deriving (Eq, Show)


------------------------------------------------------------------------------------------------------------------------
--                                           Render Concurrent Statements 
------------------------------------------------------------------------------------------------------------------------
renderConcurrent :: ConcurrentStatement -> [String]
renderConcurrent (ConBatch []) = []
renderConcurrent (ConBatch assignments) = assignBatch assignments
renderConcurrent (Instances []) = []
renderConcurrent (Instances portMaps) = (renderPortMap (head portMaps)) ++ (renderConcurrent (Instances (tail portMaps)))
        

------------------------------------------------------------------------------------------------------------------------
--                                          Represent Sequential Statements 
------------------------------------------------------------------------------------------------------------------------
data SequentialStatement = 
        SeqBatch [Assignment]
    |   IfStatement [(Condition, [Assignment], [SequentialStatement])]
    |   CaseStatement Information [(Information, [Assignment], [SequentialStatement])]
    |   ForLoop Int Int [Assignment] [SequentialStatement]
    |   WhileLoop Condition [Assignment] [SequentialStatement]
    |   RawSequentialVhd [String]
        deriving (Eq, Show)


------------------------------------------------------------------------------------------------------------------------
--                                              Render One If Statement 
------------------------------------------------------------------------------------------------------------------------
renderSequential :: SequentialStatement -> Int -> [String]

renderSequential (IfStatement []) _ = ["end if;"]

renderSequential (IfStatement [(TerminalElse, assignmentList, sequentialStatements)]) _ = 
    ["else"] ++ 
    (zipTab (assignBatch assignmentList)) ++ 
    (flattenShallow (map (\x -> (zipTab (renderSequential x 0))) sequentialStatements))

renderSequential (IfStatement someList) whichIfTerm =
    [(elseTerm whichIfTerm) ++ "if " ++ (cond2Str oneCond) ++ " then"] ++
    (zipTab (assignBatch assignmentList)) ++ 
    (flattenShallow (map (\x -> (zipTab (renderSequential x 0))) sequentialStatements)) ++ 
    (renderSequential (IfStatement (tail someList)) (whichIfTerm+1)) ++ 
    ["end if;"]
        where 
            (oneCond, assignmentList, sequentialStatements) = head someList


