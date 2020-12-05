-----------------------------------------------------------------------------------------------------------------------
--                                     Represent And Manipulate Entity Hierarchy 
------------------------------------------------------------------------------------------------------------------------
module Rendering.EntityTree where
import Rendering.Entity
import Tools.WhiteSpaceTools
import Tools.ListTools


------------------------------------------------------------------------------------------------------------------------
--                                            Represent Entity Hierarchy 
------------------------------------------------------------------------------------------------------------------------
data EntityTree = 
        EntityTree Entity [EntityTree] 
    |   TopLevel
        deriving (Eq, Show)


------------------------------------------------------------------------------------------------------------------------
--                                       Operate On One Entity If Name Matches 
------------------------------------------------------------------------------------------------------------------------
modEntMatchName :: String -> Entity -> (Entity -> Entity) -> Entity
modEntMatchName s e f 
    | (s == (entNomen e)) = f e
    | otherwise = e


------------------------------------------------------------------------------------------------------------------------
--                                        Change One Entity, Matched By Name 
-- 
-- Requires a function (typically anonymous, though it does not have to be) with type:
--      Entity -> Entity
--
-----------------------------------------------------------------------------------------------------------------------
changeOneEntity :: String -> EntityTree -> (Entity -> Entity) -> EntityTree
changeOneEntity oneEntNomen (EntityTree oneEntity []) someFunc = 
    EntityTree 
        (modEntMatchName oneEntNomen oneEntity someFunc) 
        []
changeOneEntity oneEntNomen (EntityTree oneEntity children) someFunc = 
    EntityTree 
        (modEntMatchName oneEntNomen oneEntity someFunc) 
        (map (\x -> changeOneEntity oneEntNomen x someFunc) children) where


------------------------------------------------------------------------------------------------------------------------
--                              Append One Entity As Child Of Another, Matched By Name 
------------------------------------------------------------------------------------------------------------------------
appendOneEntity :: String -> EntityTree -> Entity -> EntityTree
appendOneEntity parentNomen (EntityTree oneEntity children) oneNewEntity
    | (parentNomen == (entNomen oneEntity)) = 
        EntityTree
            oneEntity 
            (children ++ [EntityTree oneNewEntity []])
    | otherwise = 
        EntityTree
            oneEntity
            (map (\x -> appendOneEntity parentNomen x oneNewEntity) children)


------------------------------------------------------------------------------------------------------------------------
--                                             Display Entity Hierarchy 
------------------------------------------------------------------------------------------------------------------------
showEntityTree :: EntityTree -> Int -> [String]
showEntityTree (EntityTree e []) n = [(tab n) ++ (entNomen e)]
showEntityTree (EntityTree e c) n =
    [(tab n) ++ (entNomen e)] ++
    (flattenShallow (map (\x -> showEntityTree x (n+1)) c))


------------------------------------------------------------------------------------------------------------------------
--                                             Extract Entities By Name 
--
-- Returns a list of entities that match the string. If all entity names are unique, then the list contains exactly 
-- 1 element if a match is found, and 0 elements otherwise. If multiple entities match the name, you can get multiple 
-- items in the list. If using infinite recursion, you could get an infinite list, so it is important to ensure finite 
-- list length somewhere in your program. 
--
------------------------------------------------------------------------------------------------------------------------
getNodesWithName :: String -> EntityTree -> [Entity]
getNodesWithName s (EntityTree e [])
    | (s == (entNomen e)) = [e]
    | otherwise = []
getNodesWithName s (EntityTree e c)
    | (s == (entNomen e)) = [e] ++ flattenShallow (map (\x -> getNodesWithName s x) c)
    | otherwise = flattenShallow (map (\x -> getNodesWithName s x) c)


testChangeOneEntity = do
    putStrLn "Here is an entity hierarchy:"
    (mapM putStrLn (showEntityTree eT4 0)) 
    putStrLn "Now let's change the name of something:"
    (mapM putStrLn (showEntityTree eT5 0)) where
        x1 = EntityTree (defaultEntity {entNomen = "x1"}) []
        x2 = EntityTree (defaultEntity {entNomen = "x2"}) []
        x3 = defaultEntity {entNomen = "x3"}
        x4 = EntityTree (defaultEntity {entNomen = "x4"}) []
        x5 = EntityTree (defaultEntity {entNomen = "x5"}) []
        x6 = defaultEntity {entNomen = "x6"}
        x7 = EntityTree (defaultEntity {entNomen = "x7"}) []
        x8 = EntityTree (defaultEntity {entNomen = "x8"}) []
        x9 = defaultEntity {entNomen = "x9"}
        x10 = defaultEntity {entNomen = "x10"}
        x11 = defaultEntity {entNomen = "x11"}
        eT1 = EntityTree x3 [x1, x2]
        eT2 = EntityTree x6 [x4, x5]
        eT3 = EntityTree x9 [x7, x8]
        eT4 = EntityTree x10 [eT1, eT2, eT3]
        eT5 = changeOneEntity "x3" eT4 (\x -> x {entNomen = "Bob"})


