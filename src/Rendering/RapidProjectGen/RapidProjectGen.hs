{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Rendering.RapidProjectGen.RapidProjectGen where 
import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad.Identity (Identity)
import Rendering.Entity
import Rendering.InfoTypes
import Rendering.Process
import Rendering.ProjectParameters
import Parsing.ParsecExample
import Control.Monad.State
import Rendering.ProjectParameters
import Parsing.GuaranteeWhitespace
import Parsing.TokenMatchingTools
import Control.Monad.IO.Class
import System.Exit
import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Brick.Util
import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes
import Cursor.Simple.List.NonEmpty
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Txt
import Tools.ListTools
import Control.Lens hiding (element)
import Control.Lens.TH
import Tools.WhiteSpaceTools
import Rendering.RapidProjectGen.ScreenParameters
import Rendering.RapidProjectGen.GeneratorState
import Rendering.RapidProjectGen.ScreenParameters
import Rendering.RapidProjectGen.CommandDecoder
import Tools.StringTools
import Rendering.CommentTools
import Data.List
import Rendering.EntityTree
import Rendering.InterspersedCode
import Rendering.RapidProjectGen.DecodeOneString


------------------------------------------------------------------------------------------------------------------------
--                                              Define TUI Entry Point 
------------------------------------------------------------------------------------------------------------------------
tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  print endState


------------------------------------------------------------------------------------------------------------------------
--                                              Define TUI Entry State 
------------------------------------------------------------------------------------------------------------------------
data TuiState =
  TuiState 
    {
        -- These should appear on the left: 
        _entityTree :: [String]

        -- These should appear on the right:
    ,   _genericList :: [String]
    ,   _portList :: [String]
    ,   _signalList :: [String]

        -- This should appear in the middle:
    ,   _renderedCode :: [String]

        -- This should appear on the bottom:
    ,   _commandHistory :: [String]
    ,   _newCommand :: String
    ,   _userHints :: [String]

        -- This stores the state of the code generator:
    ,   generatorState :: GeneratorState
    } deriving (Show, Eq) 


------------------------------------------------------------------------------------------------------------------------
--                                                   Boiler Plate 
------------------------------------------------------------------------------------------------------------------------
data ResourceName =
  ResourceName
  deriving (Show, Eq, Ord)


------------------------------------------------------------------------------------------------------------------------
--                                                  Define TUI App 
------------------------------------------------------------------------------------------------------------------------
tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty [("selected", fg red)]
    }   


------------------------------------------------------------------------------------------------------------------------
--                                            Center A String In A Field 
------------------------------------------------------------------------------------------------------------------------
ctrString :: String -> Int -> String
ctrString s n = take n ((take n' infiniteSpaces) ++ s ++ infiniteSpaces) where
    n' = div (n - (length s)) 2


------------------------------------------------------------------------------------------------------------------------
--                                       Draw Entity Hierarchy From TUI State 
------------------------------------------------------------------------------------------------------------------------
drawEntHierarchy :: TuiState -> [String]
drawEntHierarchy ts = 
    [ctrString "Entities" sideColumn] ++ (showEntityTree (entTree (generatorState ts)) 0)

-- showEntityTree :: EntityTree -> Int -> [String]

-- showEntityHierarchy (entTree (generatorState ts)) 0

-- showEntityHierarchy :: Entity -> Int -> [String
--     [ctrString "Entities" sideColumn] ++ (showEntityTree (entTree (generatorState ts)) 0)


-- TODO: Think about whether to delete EntityTree.
--      Is it really necessary?
--      If not, delete the file, and all references to it.
--      Replace with new functions. 

------------------------------------------------------------------------------------------------------------------------
--                                            Decide How Tall Windows Are
------------------------------------------------------------------------------------------------------------------------
genericsToShow :: Int
genericsToShow = 5


portsToShow :: Int 
portsToShow = 20


signalsToShow :: Int
signalsToShow = 20


renderedLinesToShow :: Int
renderedLinesToShow = 27


commandHistoryTraceback :: Int
commandHistoryTraceback = 10


------------------------------------------------------------------------------------------------------------------------
--                                             Define Initial TUI State 
------------------------------------------------------------------------------------------------------------------------
buildInitialState :: IO TuiState
buildInitialState = 
    pure TuiState {
                _entityTree = [ctrString "Entities" sideColumn]
            ,   _genericList = [ctrString "Generics" sideColumn] ++ (take genericsToShow blankLines)
            ,   _portList = [ctrString "Ports" sideColumn] ++ (take portsToShow blankLines)
            ,   _signalList = [ctrString "Signals" sideColumn] ++ (take signalsToShow blankLines)
            ,   _renderedCode = [ctrString "Rendered VHDL" middleColumn] ++ (take renderedLinesToShow blankLines)
            ,   _commandHistory = take commandHistoryTraceback blankLines
            ,   _newCommand = cmdArrows 
            ,   _userHints = ["\n"]
            ,   generatorState = defaultGeneratorState
            }




--drawTui :: TuiState -> [Widget ResourceName]
--drawTui ts = [
--    vBox [
--        hBox [
----                vBox $ concat [map str (drawEntHierarchy ts)]
--               vBox $ concat [map str (gleanRenderedCode ts)]
----            ,   vBox $ concat [
----                            map str (gleanGenerics ts)
----                        ,   map str (gleanPorts ts)
----                        ,   map str (gleanSignals ts)
----                        ]
--            ]
----    ,   vBox $ concat [map str (makeVisibleCommandHistory (_commandHistory ts))]
--    ,   vBox [str (_newCommand ts)]
--    ,   vBox $ concat [map str (_userHints ts)]
--    ,   vBox $ concat [map str (displayPresentEnt ts)]
--    ]
--    ]

------------------------------------------------------------------------------------------------------------------------
--                                           Display Most Recent Commands 
------------------------------------------------------------------------------------------------------------------------
makeVisibleCommandHistory :: [String] -> [String]
makeVisibleCommandHistory los = [ctrString "Command History" wholeScreen] ++ (take commandHistoryTraceback ((lastN los commandHistoryTraceback) ++ blankLines))


-----------------------------------------------------------------------------------------------------------------------
--                                    Apply Attribute To Top Of List, If Selected 
------------------------------------------------------------------------------------------------------------------------
drawOneLine :: Int -> Bool -> String -> Widget n
drawOneLine n b = 
    (if (b && (n == 0))
        then withAttr "selected"
            else id ) .
    str


------------------------------------------------------------------------------------------------------------------------
--                                            Convert Data Type To String 
------------------------------------------------------------------------------------------------------------------------
dt2Str :: DataType -> String
dt2Str StdLogic = "SL"
dt2Str StdLogicVector = "SL Vec"
dt2Str StdULogic = "SUL"
dt2Str StdULogicVector = "SUL Vec"
dt2Str Signed = "Signed"
dt2Str Unsigned = "Unsigned"
dt2Str Bit = "Bit"
dt2Str UnconstrainedInt = "Int"
dt2Str (ConstrainedInt a b) = "Int " ++ (show a) ++ " " ++ (show b)
dt2Str _ = ""


------------------------------------------------------------------------------------------------------------------------
--                                                Define Field Widths 
------------------------------------------------------------------------------------------------------------------------
nomenWidth :: Int
nomenWidth = 20


dtWidth :: Int
dtWidth = 10


widthWidth :: Int
widthWidth = 10


defaultStrWidth :: Int
defaultStrWidth = 10


------------------------------------------------------------------------------------------------------------------------
--                                           Convert Information To String 
------------------------------------------------------------------------------------------------------------------------
showOneInfo :: Information -> String
showOneInfo (Port n dt w dir _ _ _ _ _) = intercalate " " [ljs n nomenWidth, ljs (dt2Str dt) dtWidth, ljs (show w) 10, show dir]
showOneInfo (VhdSig n dt w _ _ _ _ _) = intercalate " " [ljs n nomenWidth, ljs (dt2Str dt) dtWidth, ljs (show w) 10]
showOneInfo (Generic n dt w (Specified dV) _) = intercalate " " [ljs n nomenWidth, ljs dV defaultStrWidth, ljs (dt2Str dt) dtWidth, ljs (show w) 10]
showOneInfo (Generic n dt w Unspecified _) = intercalate " " [ljs n nomenWidth, ljs "" defaultStrWidth, ljs (dt2Str dt) dtWidth, ljs (show w) 10]
showOneInfo _ = ""


sideColDashes :: String
sideColDashes = ctrString (take (sideColumn - 2) dashes) sideColumn


-----------------------------------------------------------------------------------------------------------------------
--                                         Get Present Entity From TUI State 
------------------------------------------------------------------------------------------------------------------------
pEnt :: TuiState -> String
pEnt ts = last (pathToPresent (generatorState ts))


------------------------------------------------------------------------------------------------------------------------
--                                              Display Present Entity 
------------------------------------------------------------------------------------------------------------------------
displayPresentEnt :: TuiState -> [String]
displayPresentEnt ts = ["Present Entity: " ++ (pEnt ts)]


------------------------------------------------------------------------------------------------------------------------
--                                        Glean Information's From TUI State 
------------------------------------------------------------------------------------------------------------------------
numGenericsToDisplay :: Int
numGenericsToDisplay = 12

numPortsToDisplay :: Int
numPortsToDisplay = 18

numSignalsToDisplay :: Int
numSignalsToDisplay = 18


------------------------------------------------------------------------------------------------------------------------
--                                           Glean Generics from TuiState 
------------------------------------------------------------------------------------------------------------------------
gleanGenerics :: TuiState -> [String]
gleanGenerics ts = 
    take numGenericsToDisplay
        ([ctrString "Generics" sideColumn, sideColDashes] ++ 
        (if (length (getNodesWithName (pEnt ts) (entTree (generatorState ts))) > 0)
            then map showOneInfo (generics (head (getNodesWithName (pEnt ts) (entTree (generatorState ts)))))
            else []) ++
        blankLines)


------------------------------------------------------------------------------------------------------------------------
--                                             Glean Ports from TuiState 
------------------------------------------------------------------------------------------------------------------------
gleanPorts :: TuiState -> [String]
gleanPorts ts = 
    take numPortsToDisplay
        ([ctrString "Ports" sideColumn, sideColDashes] ++ 
        (if (length (getNodesWithName (pEnt ts) (entTree (generatorState ts))) > 0)
            then map showOneInfo (ports (head (getNodesWithName (pEnt ts) (entTree (generatorState ts)))))
            else []) ++
        blankLines)


------------------------------------------------------------------------------------------------------------------------
--                                            Glean Signals from TuiState 
------------------------------------------------------------------------------------------------------------------------
gleanSignals :: TuiState -> [String]
gleanSignals ts = 
    take numSignalsToDisplay
        ([ctrString "Signals" sideColumn, sideColDashes] ++ 
        (if (length (getNodesWithName (pEnt ts) (entTree (generatorState ts))) > 0)
            then map showOneInfo (signals (head (getNodesWithName (pEnt ts) (entTree (generatorState ts)))))
            else []) ++
        blankLines)

------------------------------------------------------------------------------------------------------------------------
--                                         Glean Rendered Code from TuiState 
------------------------------------------------------------------------------------------------------------------------
renderedCodeWidth :: Int
renderedCodeWidth = 75


bedOfProcrustes :: String -> String
bedOfProcrustes s = take renderedCodeWidth (s ++ (repeat ' '))


allProcessLines :: Entity -> [String]
allProcessLines someEnt = flattenShallow (map (\x -> renderProcess x easyProjParams) (processes someEnt))


gleanRenderedCode :: TuiState -> [String]
gleanRenderedCode ts = [titleLine] ++ take renderedLinesToShow perfectLines where
    oneEntTree = entTree (generatorState ts)
    oneEnt = head (fetchOneEntity (pEnt ts) oneEntTree)
    rawLines = (addToVhdBody oneEnt) ++ (allProcessLines oneEnt) ++ blankLines
    perfectLines = map bedOfProcrustes rawLines
    titleLine = ctrString "Rendered Code" renderedCodeWidth


------------------------------------------------------------------------------------------------------------------------
--                                                     Draw TUI 
------------------------------------------------------------------------------------------------------------------------
drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = [
    vBox [
        hBox [
                vBox $ concat [map str (drawEntHierarchy ts)]
            ,   vBox $ concat [map str (gleanRenderedCode ts)]
            ,   vBox $ concat [
                            map str (gleanGenerics ts)
                        ,   map str (gleanPorts ts)
                        ,   map str (gleanSignals ts)
                        ]
            ]
    ,   vBox $ concat [map str (makeVisibleCommandHistory (_commandHistory ts))]
    ,   vBox [str (_newCommand ts)]
    ,   vBox $ concat [map str (_userHints ts)]
    ,   vBox $ concat [map str (displayPresentEnt ts)]
    ]
    ]


------------------------------------------------------------------------------------------------------------------------
--                                         Add And Remove The Command Prompt 
------------------------------------------------------------------------------------------------------------------------
eraseArrows :: String -> String
eraseArrows s = "  " ++ (tail (tail s))


cmdArrows :: String
cmdArrows = ">>   "


------------------------------------------------------------------------------------------------------------------------
--                                                 Handle TUI Events 
------------------------------------------------------------------------------------------------------------------------
handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey KEnter [] -> do 
            let s' = s {
                _commandHistory = (_commandHistory s) ++ [eraseArrows (_newCommand s)]
            }

            let s'' = s' {
                _newCommand = cmdArrows
            }

            let trimmedCommand = trimLeadingSpaces (eraseArrows (_newCommand s))

            let s''' = s'' {
                generatorState = decodeOneStr trimmedCommand (generatorState s)
            }
            continue s'''

        EvKey (KChar 'q') [MCtrl] -> halt s


        EvKey (KChar 's') [MCtrl] -> do
            -- TODO: save files
            -- Could be as simple as saving command history
            continue s

        EvKey (KChar c) [] -> do
            let s' = s {
                _newCommand = (_newCommand s) ++ [c]
            }
            continue s'

        EvKey KBS [] -> do
            let s' = s {
                _newCommand = dropLast (_newCommand s) 
            }
            continue s'

        _ -> continue s
 
    _ -> continue s


