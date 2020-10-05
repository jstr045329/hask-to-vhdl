module Rendering.FiniteStateMachine where
import Rendering.InfoTypes
import Rendering.Condition
import Data.Hashable


data FsmInput = FsmInput Information deriving (Eq, Show)


data FsmState = FsmState String deriving (Eq, Show)


data FsmEdge = FsmEdge FsmState FsmState deriving (Eq, Show)


data FsmOutput = MooreOutput {
                        fsmOutputNomen :: String
                    ,   fsmState :: FsmState
                    ,   assertionVal :: Maybe Information
                    -- ,   mutateVal :: <insert type here>
                    ,   defaultVal :: Information
                    ,   driveVal :: Information -- Whatever port or signal you drive with this output.
                    } 

                |   MealyOutput {
                        fsmOutputNomen :: String
                    ,   fsmState :: FsmState
                    ,   mealyConditions :: [Condition]    
                    ,   mealyInputs :: [[Information]] -- Sum of Products. Inner list is ANDed together. Outer list is OR'd together. 
                    ,   mealyComparisonValues :: [[Information]] -- Values to which mealyInputs are compared in order for a condition to be true. 
                    ,   assertionVal :: Maybe Information
                    ,   defaultVal :: Information
                    ,   driveVal :: Information -- Whatever port or signal you drive with this output.
                    } deriving (Eq, Show)


data FiniteStateMachine = FiniteStateMachine {
        fsmNomen :: String
    ,   fsmInputs :: [Information]
    ,   fsmStates :: [FsmState]
    ,   fsmEdges :: [FsmEdge]
    ,   fsmOutputs :: [FsmOutput]
    } deriving (Eq, Show)
