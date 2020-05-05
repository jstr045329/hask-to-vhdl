module Rendering.PopulateTemplates where
import Data.List
import Tools.WhiteSpaceTools
import Rendering.InfoTypes


data SimType = UnboundedInt | BoundedInt | SimBool deriving (Eq, Show)


data Settings = Settings {
      modName               ::      String
    , defaultWidth          ::      Width
    , defaultBusWidth       ::      Width
    , defaultSynthType      ::      DataType
    , defaultSimType        ::      SimType
    , clockName             ::      String
    , resetName             ::      String
    , clkEdge               ::      Bool
    , resetEdge             ::      Bool
    , syncronousReset       ::      Bool
    } deriving (Eq, Show)


vanillaSettings :: String -> Settings
vanillaSettings myModName = Settings {
      modName               =       myModName
    , defaultWidth          =       Hard 1
    , defaultBusWidth       =       Soft "width"
    , defaultSynthType      =       Unsigned
    , defaultSimType        =       UnboundedInt
    , clockName             =       "clk"
    , resetName             =       "rst"
    , clkEdge               =       True
    , resetEdge             =       True
    , syncronousReset       =       True
    }


entityHeader :: [String] -> Settings -> [String]
entityHeader _ settings =   ["entity "] ++ [modName settings] ++ [" is"]


entityFooter :: [String] -> Settings -> [String]
entityFooter _ settings = ["end "] ++ [modName settings] ++ [";"]


architectureHeader :: [String] -> Settings -> [String]
architectureHeader _ settings = ["architecture behavioral of "] ++ [modName settings] ++ [" is"]


architectureFooter :: [String] -> Settings -> [String]
architectureFooter _ _ = ["end behavioral;"]


getMsb :: Width -> String
getMsb (Hard a) = show (a-1)
getMsb (Soft s) = s ++ "-1"


getBitRange :: Width -> String
getBitRange w = (getMsb w) ++ " downto 0"


getWrappedBitRange :: Width -> String
getWrappedBitRange w = "(" ++ (getBitRange w) ++ ")"


wrapGenericSection :: [String] -> [String]
wrapGenericSection [] = []
wrapGenericSection los = ["generic ("] ++ (zipTab los) ++ [");"]


wrapPortSection :: [String] -> [String]
wrapPortSection [] = []
wrapPortSection los = ["port ("] ++ (zipTab los) ++ [");"]


populateEntityTemplate :: [String] -> [String] -> [String] -> [String] -> Settings -> [String]
populateEntityTemplate genericDecs portDecs signalDecs logic settings =
    (entityHeader [] settings) ++
    (zipTab (wrapGenericSection genericDecs)) ++ 
    (zipTab (wrapPortSection portDecs)) ++ 
    (entityFooter [] settings) ++ 
    (architectureHeader [] settings) ++ 
    (zipTab signalDecs) ++ 
    ["begin"] ++ 
    (zipTab logic) ++ 
    (architectureFooter [] settings)


populatePackageTemplate :: String -> [String] -> [String] -> [String]
populatePackageTemplate nm declarationLines bodyLines =
    [ "package " ++ nm ++ " is"] ++
    declarationLines ++ 
    [ "end package " ++ nm ++ ";"] ++
    [ ""
    , ""
    , "package body " ++ nm ++ " is"] ++
    bodyLines ++
    [ "end package body " ++ nm ++ ";"]


-- Appends a colon to a string, but only if the string is not empty. 
-- Gives user the option to name a process or not. 
addColon :: String -> String
addColon "" = ""
addColon nm = nm ++ ": " 


populateProcess :: String -> Information -> Information -> [Information] -> [String] -> [String] -> [String]
populateProcess nm clk rst sigList varLines bodyLines = 
    [ (addColon nm) ++ "process(" ++ (nomen clk) ++ ")"] ++
      varLines ++
    [ "begin"
    , tab 1 ++ "if rising_edge(" ++ (nomen clk) ++ " then"
    , tab 2 ++ "if " ++ (nomen rst) ++ " = '1' then"] ++
    (nZipTab 3 (mapM (\x -> (nomen x) ++ " <= " ++ (sReset x) ++ ";") sigList)) ++
    [ tab 2 ++ "else"] ++
    nZipTab 3 bodyLines ++
    [ tab 2 ++ "end if;"
    , tab 1 ++ "end if;"
    , "end process;"]


