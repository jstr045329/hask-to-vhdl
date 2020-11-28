{-# LANGUAGE FlexibleContexts #-}
module Rendering.RapidProjectGen.RapidProjectGen where 
import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad.Identity (Identity)
import Rendering.Entity
import Rendering.InfoTypes
import Parsing.ParsecExample
import Control.Monad.State
import Rendering.ProjectParameters
import Parsing.GuaranteeWhitespace
import Parsing.TokenMatchingTools


startsWith :: String -> String -> Bool
startsWith txtBody lookFor
    | (parse (Parsec.string lookFor) txtBody) == Right lookFor = True 
    | otherwise = False


data GeneratorState = GeneratorState {
        keepGoing :: Bool
    ,   showVhd :: Bool 
    ,   showHs :: Bool 
    ,   formingEntity :: Bool 
    ,   useConcurrent :: Bool
    ,   useSequential :: Bool
    ,   drinkingVhd :: Bool 
    ,   drinkingProcess :: Bool 
    ,   uniqueNameInt :: Integer
    ,   projectParameters :: ProjectParameters
    ,   childEntities :: [Entity]
    ,   presentEntity :: Entity
    ,   parentEntity :: [Entity]
    ,   parentState :: [GeneratorState]
    ,   defaultClk :: Information
    ,   defaultRst :: Information
    ,   defaultDataType :: DataType
    ,   defaultWidth :: Width
    } deriving (Eq, Show)


defaultGeneratorState :: GeneratorState 
defaultGeneratorState = GeneratorState {
        keepGoing = True
    ,   showVhd = True
    ,   showHs = False
    ,   formingEntity = False
    ,   useConcurrent = True
    ,   useSequential = False 
    ,   drinkingVhd = False 
    ,   drinkingProcess = False 
    ,   uniqueNameInt = 495137688
    ,   projectParameters = easyProjParams
    ,   childEntities = []
    ,   presentEntity = defaultEntity
    ,   parentEntity = [TopLevelEntity]
    ,   parentState = []
    ,   defaultClk = easyClk
    ,   defaultRst = easyRst
    ,   defaultDataType = StdLogic
    ,   defaultWidth = Hard 1 
    }


makeOneNewSignal :: String -> GeneratorState -> Information
makeOneNewSignal oneStr generatorState = 
    let tokList = tokenize'' oneStr
    in VhdSig {
            nomen = tokList !! 1
        ,   dataType = defaultDataType generatorState
        ,   width = if (length (afterKeyword tokList ["width", "="]) > 0)
                        then Hard (read (head (afterKeyword tokList ["width", "="])) :: Integer)
                        else defaultWidth generatorState
        ,   sDefault = Unspecified
        ,   sReset = makeResetVal (defaultDataType generatorState)
        ,   clocked = Nothing
        ,   comments = []
        ,   assertionLevel = Nothing
        }


makeOneNewPort :: String -> GeneratorState -> Bool -> Information
makeOneNewPort oneStr generatorState isInput = 
    let tokList = tokenize'' oneStr
    in Port {
            nomen = tokList !! 1
        ,   dataType = defaultDataType generatorState
        ,   width = if (length (afterKeyword tokList ["width", "="]) > 0)
                        then Hard (read (head (afterKeyword tokList ["width", "="])) :: Integer)
                        else defaultWidth generatorState
        ,   direction = if isInput
                            then In 
                            else Out
        ,   sDefault = Unspecified
        ,   sReset = makeResetVal (defaultDataType generatorState)
        ,   clocked = Nothing
        ,   comments = []
        ,   assertionLevel = Nothing
        }


decodeOneStr :: String -> GeneratorState -> GeneratorState
decodeOneStr oneStr presentState
    | (startsWith oneStr ":done") = presentState
    
    | (startsWith oneStr "<ent>") = presentState { formingEntity = True}
    
    | (startsWith oneStr "</ent>") = presentState { formingEntity = False}
    
    | (startsWith oneStr "sig ") = 
        presentState {
            presentEntity = (presentEntity presentState) {
                signals = (signals (presentEntity presentState)) ++ [makeOneNewSignal oneStr presentState]
                }
            }

    | (startsWith oneStr "in ") = 
        presentState {
            presentEntity = (presentEntity presentState) {
                ports = (ports (presentEntity presentState)) ++ [makeOneNewPort oneStr presentState True]
                }
            }
    
    | (startsWith oneStr "out ") = 
        presentState {
            presentEntity = (presentEntity presentState) {
                ports = (ports (presentEntity presentState)) ++ [makeOneNewPort oneStr presentState False]
                }
            }
        
    | (startsWith oneStr "<vhd>") = presentState { drinkingVhd = True }
        
    | (startsWith oneStr "</vhd>") = presentState { drinkingVhd = False }
    
    | (startsWith oneStr "<proc>") = presentState { drinkingProcess = True }
    
    | (startsWith oneStr "</proc>") = presentState { drinkingProcess = False }

    | (startsWith oneStr "show vhd") = presentState {showVhd = True, showHs = False}
    
    | (startsWith oneStr "show hs") = presentState {showVhd = False, showHs = True}

    | otherwise = 
        if (drinkingVhd presentState)
            then presentState {
                    presentEntity = (presentEntity presentState) { 
                        literalVhdLines = (literalVhdLines (presentEntity presentState)) ++ [oneStr]
                        }
                    }
            
            else presentState

-- TODO: Make all signals in an entity available to child entities. 
-- Automatically wire up port map. 

-- TODO: Put the "undo" function in something closer to the top. 
-- projGenEventLoop :: IO ()
-- projGenEventLoop = do
    
-- TODO: Add HashSet for infinite signal stubs 
-- TODO: Add [[Information]] for infinite signal lists 

-- TODO: Add HashSet's for libraries needed



