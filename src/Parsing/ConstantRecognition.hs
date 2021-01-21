------------------------------------------------------------------------------------------------------------------------
--                                         Recognize Constants And Generics 
--
-- BY CONVENTION, words in all caps LIKE_THIS are a constant or generic. There's nothing about the VHDL language 
-- that requires this. Note that this module cannot tell if the user prefers a NAME_IN_ALL_CAPS to be a generic as 
-- opposed to a constant, or vice versa. Therefore, the two exported functions are equivalent. The same function is 
-- provided with 2 names in case that helps the code be more expressive. Obviously, this expression will always evaluate
-- to False:
--
--      (isGeneric s) && (not (isConstant s))
--
-- Likewise, this one will always evaluate to True:
--
--      (isGeneric s) || (not (isConstant s))
--
-- so it is suggested to avoid writing redundant and/or useless code.
--
------------------------------------------------------------------------------------------------------------------------
module Parsing.ConstantRecognition (
        isConstant
    ,   isGeneric
    ) where
import Parsing.VhdlTokens
import Data.HashSet


constantBrains :: String -> Bool
constantBrains s
    | (s == "") = True
    | (elem (head s) (fromList "abcdefghijklmnopqrstuvwxyz")) = False
    | (elem (head s) (fromList "ABCDEFGHIJKLMNOPQRSTUVWXYZ123456789_")) = constantBrains (tail s)
    | (isVhdlToken [head s]) = False
    | otherwise = constantBrains (tail s)


isConstant :: String -> Bool
isConstant s
    | (s == "") = False
    | otherwise = constantBrains s


-- Provide a more descriptive name if user is looking for 
isGeneric = isConstant

