-- This module implements the finite state machine that decides what box keystrokes apply to.
module Rendering.RapidProjectGen.RapidGenFSM where


stateEntities :: Int
stateEntities = 1000


stateRenderedCode :: Int
stateRenderedCode = 2000


stateGenerics :: Int
stateGenerics = 3000


statePorts :: Int
statePorts = 4000


stateSignals :: Int
stateSignals = 5000


stateCommandHistory :: Int
stateCommandHistory = 6000


stateNewCommand :: Int
stateNewCommand = 7000


-- This function decides which box should be selected next when moving clockwise. 
decodeClockwise :: Int -> Int
decodeClockwise x
    | (x == stateEntities) = stateRenderedCode
    | (x == stateRenderedCode) = stateGenerics
    | (x == stateGenerics) = statePorts
    | (x == statePorts) = stateSignals
    | (x == stateSignals) = stateCommandHistory
    | (x == stateCommandHistory) = stateNewCommand
    | otherwise = stateEntities


-- This function decides which box should be selected next when moving counter-clockwise. 
decodeCounterClockwise :: Int -> Int
decodeCounterClockwise x
    | (x == stateEntities) = stateNewCommand
    | (x == stateRenderedCode) = stateEntities
    | (x == stateGenerics) = stateRenderedCode
    | (x == statePorts) = stateGenerics
    | (x == stateSignals) = statePorts
    | (x == stateCommandHistory) = stateSignals
    | otherwise = stateCommandHistory


-- decodeOneKeyStroke :: Event -> 


