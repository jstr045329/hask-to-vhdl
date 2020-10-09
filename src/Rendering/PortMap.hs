module Rendering.PortMap where
import Rendering.InfoTypes
import Rendering.VhdMath
import Rendering.Assignment
import Tools.WhiteSpaceTools


data PortMap = PortMap {
      instNomen :: String
    , modName :: String
    , assignments :: [Assignment]
    }


pMapColon :: String -> String
pMapColon "" = ""
pMapColon s = s ++ ": "


hasGenerics :: PortMap -> Bool
hasGenerics pMap
    | ((getGenerics (map getLeftSide (assignments pMap))) == []) = False
    | otherwise = True


renderGenericMap :: PortMap -> [String]
renderGenericMap pMap
    | hasGenerics pMap = [(tab 1) ++ "generic map("] ++
      (nZipTab 2 (assignBatch [x | x <- (assignments pMap), isGeneric (getLeftSide x)])) ++ 
      [(tab 1) ++ ")"]
    | otherwise = []


renderPortMap :: PortMap -> [String]
renderPortMap pMap = 
    [(pMapColon (instNomen pMap)) ++ (modName pMap)] ++
    renderGenericMap pMap ++
    [(tab 1) ++ "port map("] ++ 
    (nZipTab 2 (assignBatch [x | x <- (assignments pMap), isPort (getLeftSide x)])) ++ 
    [(tab 1) ++ ");"] ++
    ["",""]
    
-- TODO: Write a module for rendering function calls, and another for procedure calls

