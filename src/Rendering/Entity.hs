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
    } deriving (Eq, Show)

