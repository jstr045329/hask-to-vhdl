module Rendering.Literals where
import Rendering.InfoTypes


slOne :: Information
slOne = Literal {
              dataType = StdLogic
            , width = Hard 1
            , sDefault = Specified "'1'"
            , comments = []
            }


slZero :: Information
slZero = Literal {
              dataType = StdLogic
            , width = Hard 1
            , sDefault = Specified "'0'"
            , comments = []
            }


repeat1s :: Integer -> String
repeat1s n = take (fromInteger n) (repeat '1')


repeat0s :: Integer -> String
repeat0s n = take (fromInteger n) (repeat '0')


wrapSlvLiteral :: String -> String
wrapSlvLiteral s = "\"" ++ s ++ "\""


allOnes :: Integer -> String
allOnes n = wrapSlvLiteral (repeat1s n)


allZeros :: Integer -> String
allZeros n = wrapSlvLiteral (repeat0s n)


slvAllOnesStr :: Information -> String
slvAllOnesStr (Port _ _ (Hard n) _ _ _ _ _ _) = wrapSlvLiteral (repeat1s n)
slvAllOnesStr (Generic _ _ (Hard n) _ _) = wrapSlvLiteral (repeat1s n)
slvAllOnesStr (VhdSig _ _ (Hard n) _ _ _ _ _) = wrapSlvLiteral (repeat1s n)
slvAllOnesStr (Variable _ _ (Hard n) _ _ _ _) = wrapSlvLiteral (repeat1s n)
slvAllOnesStr _ = error "Only Ports, Generics, VhdSigs, and Variables with hard coded widths allowed"


slvAllZerosStr :: Information -> String
slvAllZerosStr (Port _ _ (Hard n) _ _ _ _ _ _) = "\"" ++ (repeat0s n) ++ "\""
slvAllZerosStr (Generic _ _ (Hard n) _ _) = wrapSlvLiteral (repeat0s n)
slvAllZerosStr (VhdSig _ _ (Hard n) _ _ _ _ _) = wrapSlvLiteral (repeat0s n)
slvAllZerosStr (Variable _ _ (Hard n) _ _ _ _) = wrapSlvLiteral (repeat0s n)
slvAllZerosStr _ = error "Only Ports, Generics, VhdSigs, and Variables with hard coded widths allowed"


slvAllOnes :: Information -> Information
slvAllOnes someSig = Literal {
              dataType = dataType someSig
            , width = width someSig
            , sDefault = Specified (slvAllOnesStr someSig)
            , comments = []}


slvAllZeros :: Information -> Information
slvAllZeros someSig = Literal {
              dataType = dataType someSig
            , width = width someSig
            , sDefault = Specified (slvAllZerosStr someSig)
            , comments = []}

-- TODO: Figure out whether these functions will work with Signed, Unsigned, etc.....





