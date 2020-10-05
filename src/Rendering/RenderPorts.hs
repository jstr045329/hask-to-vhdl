module Rendering.RenderPorts where
import Rendering.InfoTypes 


-- Pass in a port, and this will create 1/2 a line of port map:
mapHalfPort :: Information -> String
mapHalfPort p = (nomen p) ++ " => "


-- Pass in a port and a signal, and this will create 1 line of port map
-- Also works for generics
mapOnePort :: Information -> Information -> String
mapOnePort p s = (nomen p) ++ " => " ++ (nomen s) ++ ","


-- This function should work for both signals AND generics :)
mapPorts2SimilarNames :: [Information] -> [String]
mapPorts2SimilarNames portList
    | portList == [] = []
    | otherwise = [mapOnePort onePort oneSig] ++ mapPorts2SimilarNames (tail portList) where
        onePort = head portList
        oneSig = port2Sig onePort

