module Rendering.GlueSigNames where
import Rendering.InfoTypes


glueSigNames :: [Information] -> String
glueSigNames [] = ""
glueSigNames sigList
    | (length sigList) == 1 = nomen (head sigList) ++ ", "
    | otherwise = (nomen (head sigList)) ++ ", " ++ (glueSigNames (tail sigList))

