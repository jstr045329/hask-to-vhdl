module Rendering.SemicolonTools where


semiWrapLine :: String -> String
semiWrapLine s = s ++ ";"


semiWrapBatch :: [String] -> [String]
semiWrapBatch los = map semiWrapLine los


