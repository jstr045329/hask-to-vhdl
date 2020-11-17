module Rendering.Entity where
import Rendering.InfoTypes
import Rendering.Function
import Rendering.Procedure
import Rendering.Process


data Entity = Entity {
            entNomen :: String
        ,   generics :: [Information]
        ,   ports :: [Information]
        ,   signals :: [Information]
        ,   functions :: [Function]
        ,   procedures :: [Procedure]
        ,   nestedEntities :: [Entity]
        ,   processes :: [Process]
        ,   inputBuffLayers :: Integer
        ,   outputBuffLayers :: Integer
        ,   overallLatency :: Integer
        ,   latencyDatum :: Maybe Information
        ,   latencyRelevant :: Bool
        ,   maxRecursionDepth :: Integer -- For recursive entities, choose a termination depth
    } deriving (Eq, Show)

