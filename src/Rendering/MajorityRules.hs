module Rendering.MajorityRules where
import Rendering.BitCountingTools
import Rendering.ZeroPad
import Tools.WhiteSpaceTools
import Rendering.IntToBin


-- Skips the "else" for the first "if" term. 
-- Inserts "else" for every term thereafter. 
elseTerm :: Int -> String
elseTerm 0 = ""
elseTerm _ = "els"


generateOneRow :: Int -> Int -> Int -> [String]
generateOneRow n min1s numBits
    | n >= 2^numBits                    = []
    | count1s (intToBin n) >= min1s     = [(elseTerm n) ++ "if x = \"" ++ (zeroPad numBits (intToBin n)) ++ "\" then"] ++
                                          [tab 1 ++ "return '1';"] ++ 
                                          generateOneRow (n + 1) min1s numBits

    | otherwise                         = [(elseTerm n) ++ "if x = \"" ++ (zeroPad numBits (intToBin n)) ++ "\" then"] ++
                                          [tab 1 ++ "return '0';"] ++ 
                                          generateOneRow (n + 1) min1s numBits


generateFunction :: Int -> Int -> [String]
generateFunction min1s numBits =
    [ "function majority_rules ("
    , (tab 1) ++ "x : std_logic_vector(" ++ (show (numBits-1)) ++ " downto 0) return std_logic is"
    , "begin"] ++
      zipTab (generateOneRow 0 min1s numBits) ++ 
    [ (tab 1) ++ "else"
    , (tab 2) ++ "return '0';"
    , (tab 1) ++ "end if;"
    , "end function;"]


