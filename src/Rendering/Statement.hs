------------------------------------------------------------------------------------------------------------------------
--                                                Statement Data Type 
------------------------------------------------------------------------------------------------------------------------
module Rendering.Statement where
import Rendering.Assignment
import Rendering.PortMap
import Rendering.Condition
import Rendering.InfoTypes
import Tools.WhiteSpaceTools


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
        deriving (Eq, Show)


------------------------------------------------------------------------------------------------------------------------
--                                           Render Sequential Statements 
------------------------------------------------------------------------------------------------------------------------
-- renderSequential :: SequentialStatement -> Int -> [String]


-- renderSequential (SeqBatch []) _ = []
-- renderSequential (SeqBatch assignments) numTabs = 
    -- nZipTab numTabs (assignBatch assignments)


-- -- renderSequential (IfStatement []) _ = []
-- -- renderSequential (IfStatement ifStatementList) numTabs = 
    -- -- (nZipTab 
        -- -- numTabs 
        -- -- []
        -- -- ) ++ 


-- renderSequential (CaseStatement _ []) _ = []
-- renderSequential (CaseStatement switchSig moreSequentialStatements) numTabs =
    -- nZipTab numTabs
    

-- renderSequential (ForLoop _ _ [] _) _ = []
-- renderSequential (ForLoop idxLo idxHi someAssignments moreSequentialStatements) numTabs =
    -- nZipTab numTabs


-- renderSequential (WhileLoop _ [] _) _ = []
-- renderSequential (WhileLoop someCondition  someAssignments moreSequentialStatements) numTabs =
    -- nZipTab numTabs

