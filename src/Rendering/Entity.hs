module Rendering.Entity where
import Rendering.InfoTypes
import Rendering.Function
import Rendering.Procedure
import Rendering.Process
import Rendering.PortMap
import Rendering.InterspersedCode
import Tools.WhiteSpaceTools
import Tools.ListTools
import Parsing.SourceSinkParser


data Entity = Entity {
            entNomen :: String
        ,   generics :: [Information]
        ,   ports :: [Information]
        ,   signals :: [Information]
        ,   functions :: [Function]
        ,   procedures :: [Procedure]
        ,   routeInfinitelyUp :: [Information]
        ,   routeInfinitelyDown :: [Information]
        ,   nestedEntities :: [Entity]
        ,   childInstances :: [PortMap]
        ,   processes :: [Process]
        ,   inputBuffLayers :: Int
        ,   outputBuffLayers :: Int
        ,   recursionDepth :: Int
        ,   maxRecursionDepth :: Int -- For recursive entities, choose a termination depth
        ,   interspersedCode :: [InterspersedCode]
        ,   addToVhdBody :: [String] -- VHDL Literals
        ,   parsedNames :: InfoPack -- This struct contains names parsed from addToVhdBody, organized by signal, input, output, etc.
    } | TopLevelEntity 
        deriving (Eq, Show)


defaultEntity :: Entity 
defaultEntity = Entity {
        entNomen = "Unnamed"
    ,   generics = []
    ,   ports = []
    ,   signals = []
    ,   functions = []
    ,   procedures = []
    ,   routeInfinitelyUp = []
    ,   routeInfinitelyDown = []
    ,   nestedEntities = []
    ,   childInstances = []
    ,   processes = []
    ,   inputBuffLayers = 0
    ,   outputBuffLayers = 0
    ,   recursionDepth = 0
    ,   maxRecursionDepth = 0
    ,   interspersedCode = []
    ,   addToVhdBody = []
    ,   parsedNames = blankInfoPack
    }


showEntityHierarchy :: Entity -> Int -> [String]
showEntityHierarchy e n
    | (length (nestedEntities e) == 0) = [(tab n) ++ (entNomen e)]
    | otherwise = [(tab n) ++ (entNomen e)] ++ (flattenShallow (map (\x -> showEntityHierarchy x (n+1)) (nestedEntities e)))

