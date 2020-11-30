module Rendering.Entity where
import Rendering.InfoTypes
import Rendering.Function
import Rendering.Procedure
import Rendering.Process
import Rendering.PortMap


data Entity = Entity {
            entNomen :: String
        ,   generics :: [Information]
        ,   ports :: [Information]
        ,   signals :: [Information]
        ,   functions :: [Function]
        ,   procedures :: [Procedure]
        ,   nestedEntities :: [Entity]
        ,   childInstances :: [PortMap]
        ,   processes :: [Process]
        ,   inputBuffLayers :: Int
        ,   outputBuffLayers :: Int
        ,   recursionDepth :: Int
        ,   maxRecursionDepth :: Int -- For recursive entities, choose a termination depth
        ,   literalVhdLines :: [String]
        ,   literalHsLines :: [String]
        ,   renderedHsLines :: [String]
        ,   tuiCommands :: [String]
    } | TopLevelEntity 
        deriving (Eq, Show)


defaultEntity :: Entity 
defaultEntity = Entity {
        entNomen = "Give Me A Name"
    ,   generics = []
    ,   ports = []
    ,   signals = []
    ,   functions = []
    ,   procedures = []
    ,   nestedEntities = []
    ,   childInstances = []
    ,   processes = []
    ,   inputBuffLayers = 0
    ,   outputBuffLayers = 0
    ,   recursionDepth = 0
    ,   maxRecursionDepth = 0
    ,   literalVhdLines = []
    ,   literalHsLines = []
    ,   renderedHsLines = []
    ,   tuiCommands = []
    }
