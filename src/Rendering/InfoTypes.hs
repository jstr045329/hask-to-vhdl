{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Rendering.InfoTypes where
import qualified Data.Hashable as H
import GHC.Generics (Generic)
import Data.Hashable
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import Foreign.Ptr (castPtr)
import Tools.StringTools
import Data.List
import Text.Printf
import Rendering.InfoNameTools


------------------------------------------------------------------------------------------------------------------------
--                                    Data Types for Ports, Signals and Generics
------------------------------------------------------------------------------------------------------------------------
data Direction =          In
                        | Out 
                        | InOut
                          deriving (Eq, Show)


showDir :: Direction -> String
showDir In = "in"
showDir Out = "out"


data DefaultValue =       Specified String
                        | Unspecified
                          deriving (Eq, Show)


data Width =              Hard Integer
                        | Soft String
                        | WidthNotSpecified
                        | WidthNotRelevant
                        | WidthInfinite
                          deriving (Eq, Show)


data DataType =           StdLogic
                        | StdULogic
                        | StdLogicVector
                        | StdULogicVector
                        | Signed
                        | Unsigned
                        | Bit
                        | UnconstrainedInt
                        | ConstrainedInt Int Int
                        | Record String [Information]
                        | UserDefinedDataType String 
                          deriving (Eq, Show)


data Information = 
                  Port {
                  nomen                 :: String
                , dataType              :: DataType
                , width                 :: Width
                , direction             :: Direction
                , sDefault              :: DefaultValue
                , sReset                :: String
                , clocked               :: Maybe Bool -- Nothing for inputs, Just True/False for outputs
                , comments              :: [String]
                , assertionLevel        :: Maybe String
                }

            |     Generic {
                  nomen                 :: String
                , dataType              :: DataType
                , width                 :: Width
                , sDefault              :: DefaultValue
                , comments              :: [String]
                }

            |     VhdSig {
                  nomen                 :: String
                , dataType              :: DataType
                , width                 :: Width
                , sDefault              :: DefaultValue
                , sReset                :: String
                , clocked               :: Maybe Bool -- Nothing == Not decided. Just True/False = Decided
                , comments              :: [String]
                , assertionLevel        :: Maybe String
                }

            |     Variable {
                  nomen                 :: String
                , dataType              :: DataType
                , width                 :: Width
                , sDefault              :: DefaultValue
                , sReset                :: String
                , comments              :: [String]
                , assertionLevel        :: Maybe String
                } 

            |     Constant {
                  nomen                 :: String
                , dataType              :: DataType
                , width                 :: Width
                , sDefault              :: DefaultValue
                , comments              :: [String]
                }

            |     Literal {
                  dataType              :: DataType
                , width                 :: Width
                , sDefault              :: DefaultValue
                , comments              :: [String]
                } 
                
                deriving (Eq, Show, Generic)


instance Hashable Information where
    hashWithSalt s (Port nm dt w dir0 sDef sRst clk'd c aL) =
        s `hashWithSalt`
        nm `hashWithSalt`
        (show dt) `hashWithSalt`
        (show w) `hashWithSalt`
        (show dir0) `hashWithSalt`
        (show sDef) `hashWithSalt`
        (show sRst) `hashWithSalt`
        (show clk'd) `hashWithSalt`
        (show aL)


    hashWithSalt s (Generic nm dt w sDef c) =
        s `hashWithSalt`
        nm `hashWithSalt`
        (show dt) `hashWithSalt`
        (show w) `hashWithSalt`
        (show sDef) 


    hashWithSalt s (VhdSig nm dt w sDef sRst clk'd c aL) =
        s `hashWithSalt`
        nm `hashWithSalt`
        (show dt) `hashWithSalt`
        (show w) `hashWithSalt`
        (show sDef) `hashWithSalt`
        (show sRst) `hashWithSalt`
        (show clk'd) `hashWithSalt`
        (show aL)


    hashWithSalt s (Variable nm dt w sDef sRst c aL) = 
        s `hashWithSalt`
        nm `hashWithSalt`
        (show dt) `hashWithSalt`
        (show w) `hashWithSalt`
        (show sDef) `hashWithSalt`
        (show sRst) `hashWithSalt`
        (show aL)


    hashWithSalt s (Constant nm dt w sDef c) = 
        s `hashWithSalt`
        nm `hashWithSalt`
        (show dt) `hashWithSalt`
        (show w) `hashWithSalt`
        (show sDef) 


    hashWithSalt s (Literal dt w sDef c) =
        s `hashWithSalt`
        (show dt) `hashWithSalt`
        (show w) `hashWithSalt`
        (show sDef)



------------------------------------------------------------------------------------------------------------------------
--                                     Check What Information Is By Constructor
--
-- See InfoNameTools.hs for tools that determine what information is by its name.
------------------------------------------------------------------------------------------------------------------------
isPort :: Information -> Bool
isPort (Port _ _ _ _ _ _ _ _ _) = True
isPort _ = False

-- Only applicable for ports:
isInput :: Information -> Bool
isInput (Port _ _ _ In _ _ _ _ _) = True
isInput _ = False


-- Only applicable for ports:
isOutput :: Information -> Bool
isOutput (Port _ _ _ Out _ _ _ _ _) = True
isOutput _ = False


-- Only applicable for ports:
isInOut :: Information -> Bool
isInOut (Port _ _ _ InOut _ _ _ _ _) = True
isInOut _ = False


isGeneric :: Information -> Bool
isGeneric (Generic _ _ _ _ _) = True
isGeneric _ = False


isVhdSig :: Information -> Bool
isVhdSig (VhdSig _ _ _ _ _ _ _ _) = True
isVhdSig _ = False


isVariable :: Information -> Bool
isVariable (Variable _ _ _ _ _ _ _) = True
isVariable _ = False


isConstant :: Information -> Bool
isConstant (Constant _ _ _ _ _) = True
isConstant _ = False


isLiteral :: Information -> Bool
isLiteral (Literal _ _ _ _) = True
isLiteral _ = False

------------------------------------------------------------------------------------------------------------------------
--                               Extract List of Information from List of Information
--
-- For example, extract only the inputs, only the signals, etc.
------------------------------------------------------------------------------------------------------------------------
getGenerics :: [Information] -> [Information]
getGenerics iList = [x | x <- iList, isGeneric x]


getPorts :: [Information] -> [Information]
getPorts iList = [x | x <- iList, isPort x]


getInputPorts :: [Information] -> [Information]
getInputPorts iList = [x | x <- iList, isInput x]


getOutputPorts :: [Information] -> [Information]
getOutputPorts iList = [x | x <- iList, isOutput x]


getInOutPorts :: [Information] -> [Information]
getInOutPorts iList = [x | x <- iList, isInOut x]


getSignals :: [Information] -> [Information]
getSignals iList = [x | x <- iList, isVhdSig x]


getVariables :: [Information] -> [Information]
getVariables iList = [x | x <- iList, isVariable x]


getConstants :: [Information] -> [Information]
getConstants iList = [x | x <- iList, isConstant x]


getLiterals :: [Information] -> [Information]
getLiterals iList = [x | x <- iList, isLiteral x]


------------------------------------------------------------------------------------------------------------------------
--                                   Lift Optional Assertion Value from Signal Map
--
-- Assertion levels are optional, so you may need a way to check whether assertion level was defined and, if so,
-- to lift it from the fearsome monad.
------------------------------------------------------------------------------------------------------------------------
hasAssertion :: Maybe String -> Bool
hasAssertion Nothing = False
hasAssertion _ = True


liftAssertion :: Maybe String -> String
liftAssertion Nothing = "" 
liftAssertion (Just s) = s


------------------------------------------------------------------------------------------------------------------------
--                                               Peek At Default Value
------------------------------------------------------------------------------------------------------------------------
default2Str :: DefaultValue -> String
default2Str Unspecified = ""
default2Str (Specified s) = " := " ++ s


------------------------------------------------------------------------------------------------------------------------
--                                            Convert Datatype to String
------------------------------------------------------------------------------------------------------------------------
datatypeToStr :: DataType -> Width -> String
datatypeToStr StdLogic _ = "std_logic"
datatypeToStr StdULogic _ = "std_ulogic"
datatypeToStr StdLogicVector (Hard n) = "std_logic_vector(" ++ (show (n-1)) ++ " downto 0)"
datatypeToStr StdLogicVector (Soft s) = "std_logic_vector(" ++ s ++ "-1 downto 0)"
datatypeToStr StdULogicVector (Hard n) = "std_ulogic_vector(" ++ (show (n-1)) ++ " downto 0)"
datatypeToStr StdULogicVector (Soft s) = "std_ulogic_vector(" ++ s ++ "-1 downto 0)"
datatypeToStr Signed (Hard n)   = "signed(" ++ (show (n-1)) ++ " downto 0)"
datatypeToStr Signed (Soft s)   = "signed(" ++ s ++ "-1 downto 0)"
datatypeToStr Unsigned (Hard n)   = "unsigned(" ++ (show (n-1)) ++ " downto 0)"
datatypeToStr Unsigned (Soft s)   = "unsigned(" ++ s ++ "-1 downto 0)"
datatypeToStr UnconstrainedInt _ = "integer"
datatypeToStr (ConstrainedInt x y) _ = "integer range " ++ (show x) ++ " to " ++ (show y)
datatypeToStr (Record s infoList) _ =
    "type " ++ s ++ " is record\n" ++ 
    (intercalate 
        "\n"
        (map (\x -> ("    " ++ (declareOneThing x))) infoList)) ++
    "end record " ++ s ++ ";"
datatypeToStr (UserDefinedDataType s) _ = s
datatypeToStr StdLogicVector WidthNotSpecified = "std_logic_vector"
datatypeToStr StdLogicVector WidthNotRelevant = "std_logic_vector"
datatypeToStr StdULogicVector WidthNotSpecified = "std_ulogic_vector"
datatypeToStr StdULogicVector WidthNotRelevant = "std_ulogic_vector"
datatypeToStr Signed WidthNotSpecified   = "signed"
datatypeToStr Signed WidthNotRelevant   = "signed"
datatypeToStr Unsigned WidthNotSpecified   = "unsigned"
datatypeToStr Bit _ = error "Do not use bit"


------------------------------------------------------------------------------------------------------------------------
--                                                Make A Reset Value
------------------------------------------------------------------------------------------------------------------------
makeResetVal :: DataType -> String
makeResetVal StdLogic = "'0'"
makeResetVal StdULogic = "'0'"
makeResetVal UnconstrainedInt = "0"
makeResetVal (ConstrainedInt a _) = show a
makeResetVal _ = "(others => '0')"


------------------------------------------------------------------------------------------------------------------------
--                                          Set And Modify Assertion Levels
------------------------------------------------------------------------------------------------------------------------
invertAssertionLevel :: Maybe String -> Maybe String
invertAssertionLevel Nothing = Nothing
invertAssertionLevel (Just "'0'") = Just "'1'"
invertAssertionLevel (Just _) = Just "'0'"


negativeAssertionLevel :: Maybe String -> Maybe String
negativeAssertionLevel Nothing = Nothing
negativeAssertionLevel _ = Just "'0'"


positiveAssertionLevel :: Maybe String -> Maybe String
positiveAssertionLevel Nothing = Nothing
positiveAssertionLevel _ = Just "'1'"


negativePortAssertionLevel :: Information -> Information
negativePortAssertionLevel p = Port {
      nomen = nomen p
    , dataType = dataType p
    , width = width p
    , direction = direction p
    , sDefault = sDefault p
    , sReset = sReset p
    , clocked = clocked p
    , comments = comments p
    , assertionLevel = negativeAssertionLevel (assertionLevel p)
    }

------------------------------------------------------------------------------------------------------------------------
--                                          Lift Assertion Level From Monad
------------------------------------------------------------------------------------------------------------------------
-- TODO: Unify this with liftAssertion.
liftAssertionLevel :: Maybe String -> String
liftAssertionLevel Nothing = ""
liftAssertionLevel (Just s) = s


------------------------------------------------------------------------------------------------------------------------
--                        Make Signal/Port/Generic Creation Easier by Assuming Common Values
------------------------------------------------------------------------------------------------------------------------
easySig :: String -> DataType -> Width -> [String] -> Information
easySig nm dt (Hard w) c = VhdSig {
                            nomen = nm,
                            dataType = if w == 1
                                        then StdLogic
                                        else StdLogicVector,
                            width = Hard w,
                            sDefault = Unspecified,
                            clocked = Nothing,
                            comments = c,
                            sReset = makeResetVal dt,
                            assertionLevel = Just "'1'"
                        }


easySig nm dt (Soft s) c = VhdSig {
                            nomen = nm,
                            dataType = if s == ""
                                        then StdLogic
                                        else StdLogicVector,
                            width = Soft s,
                            sDefault = Unspecified,
                            clocked = Nothing,
                            comments = c,
                            sReset = makeResetVal dt,
                            assertionLevel = Just "'1'"
                        }


easyUnclockedSL :: String -> Information
easyUnclockedSL nm = VhdSig {
              nomen = nm
            , dataType = StdLogic
            , width = Hard 1
            , sDefault = Specified "'0'"
            , sReset = "'0'"
            , clocked = Just False
            , comments = [""]
            , assertionLevel = Just "'1'"
            }


easyClockedSL :: String -> Information
easyClockedSL nm = VhdSig {
              nomen = nm
            , dataType = StdLogic
            , width = Hard 1
            , sDefault = Specified "'0'"
            , sReset = "'0'"
            , clocked = Just True
            , comments = [""]
            , assertionLevel = Just "'1'"
            }


hardWidthSLV :: String -> Integer -> Information
hardWidthSLV nm w = VhdSig {
              nomen = nm
            , dataType = StdLogic
            , width = Hard w
            , sDefault = Specified "(others => '0')"
            , sReset = "(others => '0')"
            , clocked = Just True
            , comments = [""]
            , assertionLevel = Just "'1'"
            }


hardWidthUnsigned :: String -> Integer -> Information
hardWidthUnsigned nm w = VhdSig {
              nomen = nm
            , dataType = Unsigned
            , width = Hard w
            , sDefault = Specified "(others => '0')"
            , sReset = "(others => '0')"
            , clocked = Just True
            , comments = [""]
            , assertionLevel = Just "'1'"
            }


hardWidthSigned :: String -> Integer -> Information
hardWidthSigned nm w = VhdSig {
              nomen = nm
            , dataType = Signed
            , width = Hard w
            , sDefault = Specified "(others => '0')"
            , sReset = "(others => '0')"
            , clocked = Just True
            , comments = [""]
            , assertionLevel = Just "'1'"
            }

softWidthSLV :: String -> String -> Information
softWidthSLV nm softWidth = VhdSig {
              nomen = nm
            , dataType = StdLogic
            , width = Soft softWidth
            , sDefault = Specified "(others => '0')"
            , sReset = "(others => '0')"
            , clocked = Just True
            , comments = [""]
            , assertionLevel = Just "'1'"
            }


easyGenericInt :: String -> DefaultValue -> [String] -> Information
easyGenericInt nm def c = Generic {
      nomen = nm
    , dataType = UnconstrainedInt
    , width = WidthNotRelevant
    , sDefault = def 
    , comments = c 
    }


easyInSl :: String -> [String] -> Information
easyInSl nm c = Port {
      nomen = nm
    , dataType = StdLogic
    , width = Hard 1
    , direction = In
    , sDefault = Unspecified
    , sReset = "'0'"
    , clocked = Nothing
    , comments = c 
    , assertionLevel = Just "'1'"
    }   


easyInSlv :: String -> Width -> [String] -> Information
easyInSlv nm w c = Port {
      nomen = nm
    , dataType = StdLogicVector
    , width = w -- Can be hard or soft coded
    , direction = In
    , sDefault = Unspecified
    , sReset = "(others => '0')"
    , clocked = Nothing
    , comments = c 
    , assertionLevel = Nothing
    }   


easyInUnsigned :: String -> Width -> [String] -> Information
easyInUnsigned nm w c = Port {
      nomen = nm
    , dataType = Unsigned
    , width = w -- Can be hard or soft coded
    , direction = In
    , sDefault = Unspecified
    , sReset = "(others => '0')"
    , clocked = Nothing
    , comments = c
    , assertionLevel = Nothing
    }


easyOutSl :: String -> [String] -> Information
easyOutSl nm c = Port {
      nomen = nm
    , dataType = StdLogic
    , width = Hard 1
    , direction = Out
    , sDefault = Unspecified
    , sReset = "'0'"
    , clocked = Just True
    , comments = c
    , assertionLevel = Just "'1'"
    }


easyOutSlv :: String -> Width -> [String] -> Information
easyOutSlv nm w c = Port {
      nomen = nm
    , dataType = StdLogicVector
    , width = w -- Can be hard or soft coded
    , direction = Out
    , sDefault = Unspecified
    , sReset = "(others => '0')"
    , clocked = Just True
    , comments = c
    , assertionLevel = Nothing
    }


easyOutUnsigned :: String -> Width -> [String] -> Information
easyOutUnsigned nm w c = Port {
      nomen = nm
    , dataType = Unsigned
    , width = w -- Can be hard or soft coded
    , direction = Out
    , sDefault = Unspecified
    , sReset = "(others => '0')"
    , clocked = Just True
    , comments = c
    , assertionLevel = Nothing
    }


------------------------------------------------------------------------------------------------------------------------
--                                           Port Versions of Clk and Rst
------------------------------------------------------------------------------------------------------------------------
easyClk :: Information
easyClk = easyInSl "clk" []


easyClkF :: Int -> Information
easyClkF f = easyInSl ("clk_" ++ show f ++ "") [show f ++ " MHz clock"]


easyRst :: Information
easyRst = easyInSl "rst" []


easyRstN :: Information
easyRstN = negativePortAssertionLevel (easyInSl "rst_n" [])


------------------------------------------------------------------------------------------------------------------------
--                                          Signal Versions of Clk and Rst
------------------------------------------------------------------------------------------------------------------------
easyClkSig :: Information
easyClkSig = easyUnclockedSL "s_clk_p"


easyRstSig :: Information
easyRstSig = easyUnclockedSL "s_rst_p"


easyClkSigN :: Information
easyClkSigN = easyUnclockedSL "s_clk_n"


easyRstSigN :: Information
easyRstSigN = easyUnclockedSL "s_rst_n"


------------------------------------------------------------------------------------------------------------------------
--                                  Exchange Information Between Ports and Signals
--
-- Converts names of this format:
--      i_descriptive_name__d0000
--      o_descriptive_name__d0000
-- to names of this format:
--      s_descriptive_name__d0000
------------------------------------------------------------------------------------------------------------------------

-- TODO: See if nomenNeedsUndy can be deleted:
nomenNeedsUndy :: Char -> Bool
nomenNeedsUndy 'i' = True
nomenNeedsUndy 'o' = True
nomenNeedsUndy 'u' = True
nomenNeedsUndy 's' = True
nomenNeedsUndy _ = False


nomenCommando :: Char -> Bool
nomenCommando c = not (nomenNeedsUndy c)


------------------------------------------------------------------------------------------------------------------------
--                                  Decide If Signal/Port Follows Naming Convention
------------------------------------------------------------------------------------------------------------------------

-- TODO: Check if this is consistent with InfoNameTools.hs. If not, update. 
nomenIsFormatted :: String -> Bool
nomenIsFormatted s = startingLetter && firstUndy && lastUndy && dLocation && endNumber where
    exempt = nomenCommando (head s)
    startingLetter = elem (head s) "isovgu"
    firstUndy = (s !! 1) == '_'
    lastUndy = ((s !! ((findNumberIdx s) - 2)) == '_') || exempt
    dLocation = ((s !! ((findNumberIdx s) - 1)) == 'd') || exempt
    endNumber = (endsInNumbers 4 s) || exempt


-- Assumes name is already formatted:
nomen2SigName :: String -> String
nomen2SigName s = "s" ++ (tail s)


-- Assumes name is already formatted:
nomen2Input :: String -> String
nomen2Input s = "i" ++ (tail s)


-- Assumes name is already formatted:
nomen2Inout :: String -> String
nomen2Inout s = "u" ++ (tail s)


-- Assumes name is already formatted:
nomen2Output :: String -> String
nomen2Output s = "o" ++ (tail s)


-- Assumes name is already formatted:
nomen2Var :: String -> String
nomen2Var s = "v" ++ (tail s)


-- Assumes name is already formatted:
nomen2Generic :: String -> String
nomen2Generic s = "g" ++ (tail s)


------------------------------------------------------------------------------------------------------------------------
--                                                 Extract Name Stub
------------------------------------------------------------------------------------------------------------------------

-- TODO: Move to InfoNameTools.hs.
extractStub :: String -> Maybe String
extractStub s
    | not (nomenIsFormatted s) = Nothing
    | otherwise = Just (tail (tail (take (numIdx-2) s)))
    where
        numIdx = findNumberIdx s



------------------------------------------------------------------------------------------------------------------------
--                                          Assign Reset and Default Values
------------------------------------------------------------------------------------------------------------------------
resetBatch :: [Information] -> [String]
resetBatch sigList = (map (\x -> (nomen x) ++ " <= " ++ (sReset x) ++ ";") sigList)


-- On the occasion you want a signal initialized to something other than all 0's, 
-- use resetAll1s or customResetVal:
resetAll1s :: Information -> Information
resetAll1s x = VhdSig {
              nomen = nomen x
            , dataType = dataType x
            , width = width x
            , sDefault = sDefault x
            , sReset = "(others => '1');"
            , clocked = clocked x
            , comments = comments x
            , assertionLevel = assertionLevel x
            }


customResetVal :: Information -> String -> Information
customResetVal x s = VhdSig {
              nomen = nomen x
            , dataType = dataType x
            , width = width x
            , sDefault = if (sDefault x == Unspecified)
                                then Unspecified
                                else Specified s
            , sReset = s
            , clocked = clocked x
            , comments = comments x
            , assertionLevel = assertionLevel x
            }


------------------------------------------------------------------------------------------------------------------------
--                                             Declare Information Types
------------------------------------------------------------------------------------------------------------------------
declareOneThing :: Information -> String
declareOneThing (Port nm dt w pDir sDef _ _ _ _) = 
    nm ++ " : " ++ (showDir pDir) ++ " " ++ (datatypeToStr dt w) ++ (default2Str sDef) ++ ";"
declareOneThing (Generic nm dt w sDef _) = nm ++ " : " ++ (datatypeToStr dt w) ++ (default2Str sDef) ++ ";"
declareOneThing (VhdSig nm dt w sDef _ _ _ _) = nm ++ " : " ++ (datatypeToStr dt w) ++ (default2Str sDef) ++ ";"
declareOneThing (Variable nm dt w sDef _ _ _) = nm ++ " : " ++ (datatypeToStr dt w) ++ (default2Str sDef) ++ ";"
declareOneThing (Constant nm dt w sDef _) = nm ++ " : " ++ (datatypeToStr dt w) ++ (default2Str sDef) ++ ";"
declareOneThing (Literal _ _ _ _) = error "You do not declare a literal"


declareWithKind :: Information -> String
declareWithKind (Port nm dt w pDir sDef _ _ c _) = declareOneThing (Port nm dt w pDir sDef "" Nothing c Nothing)
declareWithKind (Generic nm dt w sDef c) = declareOneThing (Generic nm dt w sDef c)
declareWithKind (VhdSig nm dt w sDef _ _ c aL) = 
    "signal " ++ declareOneThing (VhdSig nm dt w sDef "" Nothing c aL)
declareWithKind (Variable nm dt w sDef _ c aL) = 
    "variable " ++ declareOneThing (Variable nm dt w sDef "" c aL)
declareWithKind (Constant nm dt w sDef c) = 
    "constant " ++ declareOneThing (Constant nm dt w sDef c)
declareWithKind (Literal _ _ _ _) = error "You do not declare a literal"


declareBatch :: [Information] -> [String]
declareBatch sList = map declareWithKind sList


------------------------------------------------------------------------------------------------------------------------
--                                     Extract Names from A List of Informations
--
-- The parameter is named sigList, but the list can be any combination of signals, ports, generics, constants, etc.
-- If you pass any Literals into this, you will get an error (as well you should).
--
------------------------------------------------------------------------------------------------------------------------
getNames :: [Information] -> [String]
getNames sigList = map (\x -> nomen x) sigList


convertGen2Const :: Information -> Information
convertGen2Const p = 
    Constant {  nomen = nomen p
            ,   dataType = dataType p
            ,   width = width p
            ,   sDefault = sDefault p
            ,   comments = comments p
            }


------------------------------------------------------------------------------------------------------------------------
--                                              Convert Port to Signal
------------------------------------------------------------------------------------------------------------------------
port2Sig :: Information -> Information
port2Sig p = 
    VhdSig  {   nomen = portName2SigName(nomen p) 0
            ,   dataType = dataType p
            ,   width = width p
            ,   sDefault = sDefault p
            ,   sReset = sReset p
            ,   clocked = clocked p
            ,   comments = (if (direction p == Out)
                                        then ["Driven by " ++ (nomen p)]
                                        else ["Drives " ++ (nomen p)]) ++ (comments p)
            ,   assertionLevel = assertionLevel p
            }


port2SigN :: Information -> Integer -> Information
port2SigN p n = 
    VhdSig  {   nomen = portName2SigName(nomen p) n
            ,   dataType = dataType p
            ,   width = width p
            ,   sDefault = sDefault p
            ,   sReset = sReset p
            ,   clocked = clocked p
            ,   comments = (if (direction p == Out)
                                        then ["Driven by " ++ (nomen p)]
                                        else ["Drives " ++ (nomen p)]) ++ (comments p)
            ,   assertionLevel = assertionLevel p
            }


batchPortToSig :: [Information] -> [(Information, Information)]
batchPortToSig [] = []
batchPortToSig (x:xs) = [(onePort, oneSignal)] ++ batchPortToSig xs where
    onePort = x
    oneSignal = port2Sig x


mapPortToSig :: [Information] -> [(Information, Information)]
mapPortToSig [] = []
mapPortToSig (x:xs) = [(onePort, oneSignal)] ++ mapPortToSig xs where
    onePort = x
    oneSignal = VhdSig {
                      nomen = nomen2SigName (nomen x)
                    , dataType = dataType x
                    , width = width x
                    , sDefault = sDefault x
                    , sReset = makeResetVal (dataType x)
                    , clocked = clocked x
                    , comments = if (direction x == In)
                                        then ["Driven by " ++ (nomen x)]
                                        else ["Drives " ++ (nomen x)]
                    , assertionLevel = assertionLevel x
                    }

