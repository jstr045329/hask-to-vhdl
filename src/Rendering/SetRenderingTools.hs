module Rendering.SetRenderingTools where
import Rendering.InfoTypes
import Rendering.FilterUnique


union :: [Information] -> [Information] -> [Information]
union [] b = b 
union a [] = a 
union a b = filterUnique (a ++ b)


intersection' :: [Information] -> [Information] -> [Information]
intersection' b0 b1 = [x | x <- (union b0 b1), elem x b0, elem x b1] 


