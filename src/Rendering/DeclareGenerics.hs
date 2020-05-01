module Rendering.DeclareGenerics where
import Rendering.InfoTypes
import Tools.ListTools


declareOneGeneric :: Information -> String
declareOneGeneric g = (nomen g) ++ " : " ++ (datatypeToStr (dataType g) (width g)) ++ (default2Str (sDefault g)) ++ ";"


declareGenericBatch :: [Information] -> [String]
declareGenericBatch genList
    | length genList == 0 = []
    | length genList == 1 = [dropLast (declareOneGeneric (head genList))]
    | otherwise = [declareOneGeneric (head genList)] ++ declareGenericBatch (tail genList)


