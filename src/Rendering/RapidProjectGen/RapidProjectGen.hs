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


tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  print endState


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

        -- This provides some space between lists:
    ,   emptySpace :: [String]

        -- This stores the state of the code generator:
    ,   generatorState :: GeneratorState
    } deriving (Show, Eq) 


data ResourceName =
  ResourceName
  deriving (Show, Eq, Ord)


tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty [("selected", fg red)]
    }   


ctrString :: String -> Int -> String
ctrString s n = take n ((take n' infiniteSpaces) ++ s ++ infiniteSpaces) where
    n' = div (n - (length s)) 2


buildInitialState :: IO TuiState
buildInitialState = 
    pure TuiState {
                _entityTree = [ctrString "Entities" sideColumn]
            ,   _genericList = [ctrString "Generics" sideColumn] ++ (take 5 blankLines)
            ,   _portList = [ctrString "Ports" sideColumn] ++ (take 20 blankLines)
            ,   _signalList = [ctrString "Signals" sideColumn] ++ (take 20 blankLines)
            ,   _renderedCode = [ctrString "Rendered VHDL" middleColumn] ++ (take 30 blankLines)
            ,   _commandHistory = take 10 blankLines
            ,   _newCommand = cmdArrows -- ">>   "
            ,   _userHints = ["\n"]
            ,   emptySpace = ["\n"]
            ,   generatorState = defaultGeneratorState
            }


commandHistoryTraceback :: Int
commandHistoryTraceback = 10


makeVisibleCommandHistory :: [String] -> [String]
makeVisibleCommandHistory los = [ctrString "Command History" wholeScreen] ++ (take commandHistoryTraceback ((lastN los commandHistoryTraceback) ++ blankLines))


-- Applies the selected attribute to the top of a list if it is selected:
drawOneLine :: Int -> Bool -> String -> Widget n
drawOneLine n b = 
    (if (b && (n == 0))
        then withAttr "selected"
            else id ) .
    str


drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = [
    vBox [
        hBox [
                vBox $ concat [map str (_entityTree ts)]
            ,   vBox $ concat [map str (_renderedCode ts)]
            ,   vBox $ concat [
                            map str (_genericList ts)
                        ,   map str (_portList ts)
                        ,   map str (_signalList ts)
                        ]
            ]
    ,   vBox $ concat [map str (makeVisibleCommandHistory (_commandHistory ts))]
    ,   vBox [str (_newCommand ts)]
    ,   vBox $ concat [map str (_userHints ts)]
    ]
    ]


eraseArrows :: String -> String
eraseArrows s = "  " ++ (tail (tail s))


cmdArrows :: String
cmdArrows = ">>   "


handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey KEnter [] -> do 
            -- TODO: PICK UP HERE: use string to modify generatorState
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
-- decodeOneStr :: String -> GeneratorState -> GeneratorState

        EvKey (KChar 'q') [MCtrl] -> halt s


        EvKey (KChar 's') [MCtrl] -> do
            -- TODO: save files
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






