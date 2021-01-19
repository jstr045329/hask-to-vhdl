------------------------------------------------------------------------------------------------------------------------
--                                               Define The TUI State 
--
-- This module defines the TuiState type, which is central to the Text User Interface (TUI). 
--
------------------------------------------------------------------------------------------------------------------------
module Rendering.RapidProjectGen.RapidTuiState where
import Rendering.RapidProjectGen.GeneratorState


data TuiState =
  TuiState 
    {   
        -- These should appear on the left: 
        _entityTree :: [String]

        -- These should appear on the right:
    ,   _genericList :: [String]
    ,   _portList :: [String]
    ,   _signalList :: [String]

        -- This should appear in the middle:
    ,   _renderedCode :: [String]

        -- This should appear on the bottom:
    ,   _commandHistory :: [String]
    ,   _newCommand :: String
    ,   _userHints :: [String]

        -- This stores the state of the code generator:
    ,   generatorState :: GeneratorState
    } deriving (Show, Eq) 


