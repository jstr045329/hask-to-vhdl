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
import Data.HashSet


data Entity = Entity {
            entNomen :: String

        -- These lists contain lists of informations the user has declared:
        ,   generics :: [Information]
        ,   ports :: [Information]
        ,   signals :: [Information]
        ,   functions :: [Function]
        ,   procedures :: [Procedure]
        ,   routeInfinitelyUp :: HashSet Information
        ,   routeInfinitelyDown :: HashSet Information
        ,   nestedEntities :: [Entity]
        ,   childInstances :: [PortMap]
        ,   processes :: [Process]
        ,   inputBuffLayers :: Int
        ,   outputBuffLayers :: Int
        ,   recursionDepth :: Int
        ,   maxRecursionDepth :: Int -- For recursive entities, choose a termination depth
        ,   interspersedCode :: [InterspersedCode]
        ,   addToVhdBody :: [String] -- VHDL Literals

        -- This struct contains names parsed from addToVhdBody, organized by signal, input, etc. 
        -- parsedNames is naive, in that all it knows are names. Contains no information about 
        -- width, datatype, etc. 
        ,   parsedNames :: InfoPack 

        -- These lists contain default Information's for every name in parsedNames:
        ,   parsedInputs :: HashSet Information
        ,   parsedOutputs :: HashSet Information
        ,   parsedSignals :: HashSet Information
        ,   parsedGenerics :: HashSet Information

        -- These are the finalized lists of Information's:
        -- Anything the user has declared takes precedence. 
        -- Anything in parsedNames that has not been declared also goes to the appropriate list:
        ,   aggInputs :: HashSet Information
        ,   aggOutputs :: HashSet Information
        ,   aggSignals :: HashSet Information
        ,   aggGenerics :: HashSet Information
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
    ,   routeInfinitelyUp = empty
    ,   routeInfinitelyDown = empty
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
    ,   parsedInputs = empty
    ,   parsedOutputs = empty
    ,   parsedSignals = empty
    ,   parsedGenerics = empty
    ,   aggInputs = empty
    ,   aggOutputs = empty
    ,   aggSignals = empty
    ,   aggGenerics = empty
    }


-- showEntityHierarchy :: Entity -> Int -> [String]
-- showEntityHierarchy e n
--     | (length (nestedEntities e) == 0) = [(tab n) ++ (entNomen e)]
--    | otherwise = [(tab n) ++ (entNomen e)] ++ (flattenShallow (map (\x -> showEntityHierarchy x (n+1)) (nestedEntities e)))

