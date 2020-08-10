-- This module generates a VHDL package representing f(x) = 1/x.
-- This module prefers to make monotonically increasing input lead to 
-- monotonically decreasing output, rather than make the most accurate 
-- binary representation possible. The reason for that is so that you 
-- can do >/< comparisons with the output and get correct answers. 
--
-- At this time, this module assumes 32 bits integer, 32 bits fractional. 
-- If someone wants to volunteer to generalize this to M bits integer and N bits, 
-- fractional, the community will be most grateful.
module Cookbook.OneOverX where 
import Rendering.BitCrunchingTools
import Rendering.WrapInParens
import Rendering.ZeroPad
import Rendering.ElseClause
import Tools.WhiteSpaceTools
import Data.Bits


calculateSlope :: (Fractional a) => a -> a -> a
calculateSlope x0 x1 = abs((y1 - y0) / (x1 - x0)) where
    y0 = 1 / x0
    y1 = 1 / x1
    

calculateBias :: (Fractional a) => a -> a
calculateBias x0 = 1 / x0


-- This function attempts to find x such that:
--      x > tinyX
--      x < bigX
--      1/x > tinyY
--      1/x < bigY
-- The motivation for doing this is to have a few spare MSB's & LSB's 
-- in both argument and output so that function is reasonably accurate. 
findXStartingPlace :: (Fractional a, Ord a, Show a) => a -> a -> a -> a -> a -> a -> a
findXStartingPlace x tinyX tinyY bigX bigY xGrowthRate
    | (x > bigX) = error "Change a parameter. X is too big."
    | ((1 / x) > bigY) = error "Change a parameter. Y is too big."
    | ((1 / x) < tinyY) = error "Change a parameter. Y is too small."
    | ((x > tinyX) && ((1 / x) < bigY)) = x
    | otherwise = findXStartingPlace (x * xGrowthRate) tinyX tinyY bigX bigY xGrowthRate


-- This function has unused inputs so that it has the same interface as findXStartingPlace
shouldXStop :: (Fractional a, Ord a, Show a) => a -> a -> a -> a -> a -> a -> Bool
shouldXStop x tinyX tinyY bigX bigY xGrowthRate
    | ((1 / x) < tinyY) = True
    | (x > bigX) = True
    | otherwise = False


generateOneChunk :: Integer -> Integer -> Integer -> [String]
generateOneChunk n slope bias = 
    [(elseClause n) ++ "if x > " ++ (int2Str bias 64) ++ " then"] ++ 
    [(tab 1) ++ "bias := " ++ wrapInParens (int2Str bias 64) ++ ";"] ++ 
    [(tab 1) ++ "slope := " ++ wrapInParens (int2Str bias 64) ++ ";"] ++ 
    [(tab 1) ++ "prod := slope * bias;" ++ ";"] ++ 
    [(tab 1) ++ "y := " ++ (int2Str bias 64) ++ " - prod(127 downto 64);"]
    
    -- TODO: Write a function in BitCrunchingTools that performs multiplacation on binary strings

-- We need a function that performs 1/x, but regards x as fixed point, having 32 bits of integer and 
-- 32 bits of fractional. 
oneOverX' :: (Integral a, Floating b) => a -> b
oneOverX' x = (2**32) / (fromIntegral x)


oneOverX :: (Integral a) => a -> a
oneOverX x = floor(oneOverX' x)


makeSlope :: (Data.Bits.Bits a, Integral a) => a -> a -> a
makeSlope x0 x1 = quot ((oneOverX x1) - (oneOverX x0)) (x1 - x0)


makeBias :: (Data.Bits.Bits a, Integral a) => a -> a
makeBias x = oneOverX x 


generateOneOverXPkg' :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> [String]
generateOneOverXPkg' n x deltaX tinyX tinyY bigX bigY 
    | ((x == 0) && (deltaX == 0)) = error "You did not imply division by 0 and an infinite loop at the same time. Nope. I didn't see that. See what? Nothing, exactly."
    | (x == 0) = generateOneOverXPkg' n (x + deltaX) deltaX tinyX tinyY bigX bigY
    | (x > bigX) = []
    | (x > tinyX) =
        (generateOneChunk n (makeSlope x (x+deltaX)) (makeBias x)) ++
        (generateOneOverXPkg' (n + 1) (x + deltaX) deltaX tinyX tinyY bigX bigY)
        
    | otherwise = generateOneOverXPkg' n (x + deltaX) deltaX tinyX tinyY bigX bigY
    
    
    
    
    
    
    
    
-- TODO: PICK UP HERE: Work towards this:

-- TODO: Figure out how to generate 1 line such that monotonically decreasing output is guaranteed.

-- The interface I ultimately want to use is something like this:
--generateOneOverXPkg :: (Fractional a, Ord a, Show a, Num a) => a -> a -> a -> a -> a -> a -> a -> a
--generateOneOverXPkg tinyX tinyY bigX bigY numRows nStages desiredError


