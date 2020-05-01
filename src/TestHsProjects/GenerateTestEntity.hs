----------------------------------------------------------------------------------------------------
--                                  Right Now The Goal Is
--
-- To write < 100 lines of Haskell in GenerateTestEntity that renders >> 100 lines of VHDL. 
----------------------------------------------------------------------------------------------------
module TestHsProjects.GenerateTestEntity where
import Rendering.InfoTypes
import Rendering.VhdMath
import Rendering.Process
import Rendering.Literals


----------------------------------------------------------------------------------------------------
--                                  Create Some Essentials
----------------------------------------------------------------------------------------------------
-- Anything that's nearly universal should go here:
clk = easyClk
rst = easyRst


----------------------------------------------------------------------------------------------------
--                              Create a Bipolar PWM Generator
----------------------------------------------------------------------------------------------------
-- Generics:
counterWidth = easyGenericInt "COUNTER_WIDTH" (Specified "16") ["width of counter"]

-- Ports:
pwmThresh = easyInUnsigned "i_pwm_thresh" (Hard 10) ["PWM Threshold"]
pwmOut = easyOutSl "o_pwm_out" ["Turns transistor on and off"]


-- Signals:
pwmCounter = easySig "s_pwmCounter" StdLogicVector (Hard 10) ["Counter for PWM generator"]


----------------------------------------------------------------------------------------------------
--                                Create a PID Controller
----------------------------------------------------------------------------------------------------
adcWidth = easyGenericInt "ADC_WIDTH" (Specified "12") ["width of ADC output"]
dacWidth = easyGenericInt "DAC_WIDTH" (Specified "10") ["width of ADC output"]
gainWidth = easyGenericInt "GAIN_WIDTH" (Specified "16") ["width of PID gains"]
gainP = easyGenericInt "GAIN_P" (Specified "256") []
gainI = easyGenericInt "GAIN_I" (Specified "256") []
gainD = easyGenericInt "GAIN_D" (Specified "256") []


-- Create some signals:
errorSig = easyInUnsigned "i_error" 
x0 = easySig "x0" Unsigned (Hard 10) [""]
x1 = easySig "x1" Unsigned (Hard 10) [""]
x2 = easySig "x2" Signed (Hard 12) [""]
x3 = easySig "x3" Signed (Hard 12) [""]
x4 = easySig "x4" StdLogic (Soft "w") [""]
x5 = easySig "x5" StdLogic (Soft "w") [""]
x6 = easySig "x6" Unsigned (Hard 10) [""]
x7 = easySig "x7" Unsigned (Hard 10) [""]
x8 = easySig "x8" Signed (Hard 12) [""]
x9 = easySig "x9" Signed (Hard 12) [""]
x10 = easySig "x10" StdLogic (Soft "w") [""]
x11 = easySig "x11" StdLogic (Soft "w") [""]
comparison0 = GreaterT x0 x1
comparison1 = LesserT x2 x3

const0 = Literal {
              dataType = StdLogicVector
            , width = Hard 12
            , sDefault = Specified (repeat1s 12)
            , comments = []}

const1 = Literal {
              dataType = StdLogicVector
            , width = Hard 12
            , sDefault = Specified (repeat0s 12)
            , comments = []}

comparison2 = Princess [comparison0, comparison1]
comparison3 = Pauper [comparison0, comparison1]

a0 = Assignment x0 x1
a1 = Assignment x2 x3
a2 = Assignment x4 x5
a3 = Assignment x5 x10
a4 = Assignment x6 x11
a5 = Assignment x7 x8
a6 = Assignment x8 x9
a7 = Assignment x9 x10

p0 = Process {
              pNomen = "joe_process_01"
            , pClk = clk
            , pRst = rst
            , syncronousReset = True
            , clkEdge = Rising
            , sensitivityList = [clk] -- TODO: See if I can delete this field
            , inputSignals = []
            , variables = []
            , internalState = [x0, x1, x2, x3]
            , outputSignals = [x4, x5]
            , assignments = [(comparison0, [a0], [a1, a2]), 
                (comparison1, [a4], [a5, a6]), 
                (JustTrue, [a6, a7], [a0, a1, a2]), 
                (comparison2, [a0, a1, a6, a7], [a0]), 
                (comparison3, [a1, a2], [a5, a6, a7])]
            }


p1 = easyProcess "harry_proc" 
                [(comparison0, [a0], [a1, a2]), 
                (comparison1, [a4], [a5, a6]), 
                (JustTrue, [a6, a7], [a0, a1, a2]), 
                (comparison2, [a0, a1, a6, a7], [a0]), 
                (comparison3, [a1, a2], [a5, a6, a7])]


bigSigList :: Int -> [Information]
bigSigList 0 = []
bigSigList n = [easySig ("joe_422_" ++ (show n)) StdLogicVector (Hard 10) []] ++ bigSigList (n-1) 


sList = bigSigList 200


condList :: [Information] -> [Condition]
condList someList 
    | (length someList) <= 1 = []
    | otherwise = [GreaterT (someList !! 0) (someList !! 1)] ++ (condList (tail someList))


assignments' :: [Condition] -> [Assignment] -> [Assignment] -> [(Condition, [Assignment], [Assignment])]
assignments' [] _ _ = []
assignments' _ [] _ = []
assignments' _ _ [] = []
assignments' cList aList1 aList2 = [( (head cList), [head aList1], [head aList2])]
                                    ++ assignments' (tail cList) (tail aList1) (tail aList2)


aList :: [Information] -> [Assignment]
aList someList
    | ((length someList) <= 1) = []
    | otherwise = [Assignment (someList !! 0) (someList !! 1)] ++ (aList (tail someList))


p2 = easyProcess "joe_422_driver"
        (assignments' 
            (take 100 (condList sList)) 
            (take 100 (aList sList))
            (take 100 (aList (tail (tail (tail (tail (tail sList))))))))

----------------------------------------------------------------------------------------------------
--                          Create a Module that Wraps Everything Up
----------------------------------------------------------------------------------------------------
-- Create a top level synth module:


----------------------------------------------------------------------------------------------------
--                                   Create a Testbench
----------------------------------------------------------------------------------------------------

-- TODO: Make it possible for generic to be reset value for a signal or output
-- TODO: Make it possible for generic to be default value for a signal or port
-- TODO: Make it possible for generic to be width of signal or port
-- TODO: Make it possible for constant to set width for signal/port
-- TODO: make it possible for constant to set default/reset value for signals/ports






