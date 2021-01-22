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

        -- These lists contain lists of informations the user has declared:
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

        -- This struct contains names parsed from addToVhdBody, organized by signal, input, etc. 
        -- parsedNames is naive, in that all it knows are names. Contains no information about 
        -- width, datatype, etc. 
        ,   parsedNames :: InfoPack 

        -- These lists contain default Information's for every name in parsedNames:
        ,   parsedInputs :: [Information]
        ,   parsedOutputs :: [Information]
        ,   parsedSignals :: [Information]
        ,   parsedGenerics :: [Information]

        -- These are the finalized lists of Information's:
        -- Anything the user has declared takes precedence. 
        -- Anything in parsedNames that has not been declared also goes to the appropriate list:
        ,   aggInputs :: [Information]
        ,   aggOutputs :: [Information]
        ,   aggSignals :: [Information]
        ,   aggGenerics :: [Information]
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
    ,   parsedInputs = []
    ,   parsedOutputs = []
    ,   parsedSignals = []
    ,   parsedGenerics = []
    ,   aggInputs = []
    ,   aggOutputs = []
    ,   aggSignals = []
    ,   aggGenerics = []
    }


showEntityHierarchy :: Entity -> Int -> [String]
showEntityHierarchy e n
    | (length (nestedEntities e) == 0) = [(tab n) ++ (entNomen e)]
    | otherwise = [(tab n) ++ (entNomen e)] ++ (flattenShallow (map (\x -> showEntityHierarchy x (n+1)) (nestedEntities e)))

