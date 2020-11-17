-- This module is an experiment in wrapping things in monads, 
-- using generated code as a side effect
module Rendering.MonadTrial where 
import Rendering.InfoTypes
import Rendering.InfiniteSigList
import Rendering.Assignment
import Rendering.Statement


oneInput = Port {
        nomen = "fred"
    ,   dataType = StdLogic
    ,   width = Hard 1
    ,   direction = In
    ,   sDefault = Unspecified
    ,   sReset = "'0'"
    ,   clocked = Nothing
    ,   comments = []
    ,   assertionLevel = Nothing
    }


-- Make an infinite list of signals:    
sigList = endlessSignals oneInput


-- Make an infinite list of assignments:
-- assignmentList = assignSignalChainWithInput (head sigList)


-- renderAssignments :: [Statement] -> ProjectParameters -> [Assignment]


-- gleanPorts :: [Statement] -> ProjectParameters -> [Information]


-- gleanGenerics :: [Statement] -> ProjectParameters -> [Information]


-- gleanSignals :: [Statement] -> ProjectParameters -> [Information]


-- gleanProcesses :: [Statement] -> ProjectParameters -> [Process]


