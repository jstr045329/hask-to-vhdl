module Rendering.Clock where


-- Represent Clock Frequency:
data Freq =     OneFreqOnly -- For situations where frequency is irrelevant
            |   FreqInHz Double 
            |   FreqInMHz Double
            deriving (Eq, Show)


data Phase =    OnePhaseOnly 
            |   PhaseDatum -- More descriptive way of saying "Phase 0"
            |   LagDatumByDeg Double -- For simplicity, we only represent phases by degrees lagging. 
            |   PhaseByNum Int -- Phase 0, Phase 1, Phase 2, etc. 
            deriving (Eq, Show)


-- Represent what we ordinarily care about in a clock:
--      * Its frequency, and
--      * Whether 
data Clock = Clock Freq Phase deriving (Eq, Show)


-- Represent the relationship between 2 clocks:
data ClockRelationship = 
                SyncSameFreq
            |   SyncInteger Int
            |   SyncFractional Int Int
            |   AsyncCloseInFreq 
            |   AsyncFarInFreq
            |   Unpredictable
            deriving (Eq, Show)


-- Represent Clock Phase Uncertainty in dbc/Hz:
data ClockJitter = ClockJitter Double deriving (Eq, Show)

