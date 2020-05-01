module Rendering.DeclarePorts where
import Rendering.InfoTypes
import Tools.ListTools


portDirToStr :: Direction -> String
portDirToStr In = "in"
portDirToStr Out = "out"


declareOnePort :: Information -> String
declareOnePort p = (nomen p) ++ " : " ++ (portDirToStr (direction p)) ++ " " ++ (datatypeToStr (dataType p) (width p)) ++ (default2Str (sDefault p)) ++ ";"


declarePortBatch :: [Information] -> [String]
declarePortBatch portList
    | length portList == 0 = []
    | length portList == 1 = [dropLast (declareOnePort (head portList))]
    | otherwise = [declareOnePort (head portList)] ++ declarePortBatch (tail portList)


