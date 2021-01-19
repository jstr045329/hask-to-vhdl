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
import Parsing.SourceSinkParser
import qualified Data.HashSet as HashSet
import Rendering.RapidProjectGen.RapidTuiState
import Rendering.RapidProjectGen.TuiParameters
import Rendering.RapidProjectGen.DisplayDataTypes
import Rendering.RapidProjectGen.DisplayInformation
import Rendering.RapidProjectGen.SideColDashes
import Rendering.RapidProjectGen.PresentEntity
import Rendering.RapidProjectGen.ExtractParsedNames


------------------------------------------------------------------------------------------------------------------------
--                                              Define TUI Entry Point 
------------------------------------------------------------------------------------------------------------------------
tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  print endState


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
--                                       Draw Entity Hierarchy From TUI State 
------------------------------------------------------------------------------------------------------------------------
drawEntHierarchy :: TuiState -> [String]
drawEntHierarchy ts = 
    [ctrString "Entities" sideColumn] ++ (showEntityTree (entTree (generatorState ts)) 0)


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
--                                                Glean User Messages 
--
-- Extract user messages from TuiState. 
--
------------------------------------------------------------------------------------------------------------------------
gleanUserMessages :: TuiState -> [String]
gleanUserMessages ts = map getOneUserMessage (userMessages (generatorState ts))


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
    ,   vBox $ concat [map str (gleanUserMessages ts)] -- [map str (_userHints ts)]
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


incRendCodeStartLoc :: GeneratorState -> GeneratorState
incRendCodeStartLoc gS 
    | ((renderedCodeStartLoc gS) < 2147483640) = gS { renderedCodeStartLoc = (renderedCodeStartLoc gS) + 1}
    | otherwise = gS


decRendCodeStartLoc :: GeneratorState -> GeneratorState
decRendCodeStartLoc gS 
    | ((renderedCodeStartLoc gS) > 0) = gS { renderedCodeStartLoc = (renderedCodeStartLoc gS) - 1}
    | otherwise = gS


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

        EvKey (KChar 'u') [MCtrl] -> do
            let s' = s {
                generatorState = decRendCodeStartLoc (generatorState s) 
            }
            continue s'

        EvKey (KChar 'd') [MCtrl] -> do
            let s' = s {
                generatorState = incRendCodeStartLoc (generatorState s) 
            }
            continue s'

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


