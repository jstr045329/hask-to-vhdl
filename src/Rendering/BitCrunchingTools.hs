module Rendering.BitCrunchingTools (
      str2Int
    , str2Frac
    , int2Str
    , decrementBitString
    , incrementBitString
    , decreaseBitString
    , increaseBitString
    ) where
import Rendering.ZeroPad
import Data.Bits


-- Converts 0 to '0', anything else to '1'. 
-- No, I have not succumbed to that regrettable habit of putting "words" in "quotes" "unnecessarily". 
-- This function converts (Num a) => 0 :: a to '0' :: Char,
-- anything else to '1' :: Char 
char2Int :: (Num a) => Char -> a
char2Int '0' = 0
char2Int _ = 1


str2Int :: (Num a) => String -> a
str2Int "" = 0
str2Int s = ((2 ^ ((length s) - 1)) * char2Int (head s)) + str2Int (tail s)


str2Frac' :: (Num a, Fractional a, Floating a) => String -> a -> a
str2Frac' "" _ = 0
str2Frac' s n = (2.0 ** ((-1) * n) * char2Int (head s)) + str2Frac' (tail s) (n + 1)


-- Like str2Int but assumes radix point is all the way to the left. 
str2Frac :: (Num a, Fractional a, Floating a) => String -> a
str2Frac "" = 0
str2Frac s = str2Frac' s 1


int2Str' :: (Num a, Integral a, Ord a, Bits a) => a -> String
int2Str' x
    | (x < 0) = error "negative numbers not supported at this time"
    | (x == 0) = "0"
    | otherwise = (int2Str' (x `shiftR` 1)) ++ [newChar] where 
        newChar = 
            if ((x `mod` 2) == 0)
                then '0'
                else '1'


int2Str :: (Num a, Integral a, Ord a, Bits a) => a -> Int -> String
int2Str n w = rightJustifyBitString (int2Str' n) w 


decrementBitString :: String -> String
decrementBitString bs = int2Str (((str2Int bs) - 1) :: Integer) (length bs)


incrementBitString :: String -> String
incrementBitString bs = int2Str (((str2Int bs) + 1) :: Integer) (length bs)


-- Note: the following function can get expensive for n >> 10. 
decreaseBitString :: String -> Int -> String
decreaseBitString s 0 = s
decreaseBitString s n = decrementBitString (decreaseBitString s (n - 1))


-- Note: the following function can get expensive for n >> 10. 
increaseBitString :: String -> Int -> String
increaseBitString s 0 = s
increaseBitString s n = incrementBitString (increaseBitString s (n - 1))

