-- This module generates a VHDL package representing f(x) = 1/x.
-- This module prefers to make monotonically increasing input lead to 
-- monotonically decreasing output, rather than make the most accurate 
-- binary representation possible. The reason for that is so that you 
-- can do >/< comparisons with the output and get correct answers. 
--
-- At this time, this module assumes 32 bits integer, 64 bits fractional. 
-- If someone wants to volunteer to generalize this to M bits integer and N bits, 
-- fractional, the community will be most grateful.
--
-- Note that if you generate too many rows, the topmost rows of your if statement
-- are guaranteed to return all 0's. If you want the capability of producing all 0 
-- output, you want the topmost row to do that. Anything beyond that is superfluous. 
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


generateOneChunk :: Integer -> Integer -> Integer -> Integer -> [String]
generateOneChunk n xThresh slope bias = 
    [(elseClause n) ++ "if x > " ++ wrapInParens (int2Str xThresh 128) ++ " then"] ++ 
    [(tab 1) ++ "delta := x - " ++ wrapInParens (int2Str xThresh 128) ++ ";"] ++
    [(tab 1) ++ "slope := " ++ wrapInParens (int2Str slope 128) ++ ";"] ++ 
    [(tab 1) ++ "bias := " ++ wrapInParens (int2Str bias 128) ++ ";"] ++ 
    [(tab 1) ++ "prod := slope * delta;"] ++ 
    [(tab 1) ++ "y := bias - prod(255 downto 128);"] ++ 
    ["", ""] 
    

makeSlope :: (Data.Bits.Bits a, Integral a) => Double -> Double -> a
makeSlope x0 x1 = abs(floor((((2**64)/x1) - ((2**64)/x0)) / (x1 - x0)))


makeBias :: (Data.Bits.Bits a, Integral a) => Double -> a
makeBias x = floor((2**64)/x)


makeXThresh :: (Data.Bits.Bits a, Integral a) => Double -> a
makeXThresh x = floor((2**96) * x)


xExpGrowthRate :: Double
xExpGrowthRate = 1.12


makeDeltaX :: Double -> Double
makeDeltaX x = x * xExpGrowthRate


generateOneOverXIfStmt :: Integer -> Double -> Double -> Integer -> [String]
generateOneOverXIfStmt n x deltaX numRows
    | ((x == 0) && (deltaX == 0)) = error "You did not imply division by 0 and an infinite loop at the same time. Nope. I didn't see that. See what? Nothing, exactly."
    | (x == 0) = generateOneOverXIfStmt n (x + deltaX) deltaX numRows
    | (n > numRows) = []
    | otherwise =
        (generateOneOverXIfStmt (n + 1) (makeDeltaX x) deltaX numRows) ++ 
        (generateOneChunk (numRows-n) (makeXThresh x) (makeSlope x (x+deltaX)) (makeBias x))
        
-- TODO: Write a module that breaks the generated lines up into many small functions, then pipelines them. 