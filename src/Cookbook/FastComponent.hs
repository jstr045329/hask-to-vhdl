-- This module assumes that the input file contains exactly 1 entity,
-- and writes a component declaration for that 1 entity.
module Cookbook.FastComponent (
    fastComponent
    ) where 
import Rendering.GenerateTestbench
import Parsing.GuaranteeWhitespace


makeLineEndings :: [String] -> String -> [String]
makeLineEndings [] underConst = [underConst]
makeLineEndings los underConst
    | (head los == ";") = [underConst ++ (head los)] ++ (makeLineEndings (tail los) "")
    | otherwise = makeLineEndings (tail los) (underConst ++ " " ++ (head los))


-- generateComponentDec :: [String] -> [String]
-- generateComponentDec los

fastComponent fileNameIn = do
    toks <- liftM (\s -> (makeLineEndings (generateComponentDec (tokenize'' s)) "")) (readFile fileNameIn)
    -- lines <- liftM (\s -> generateComponentDec rawToks
    mapM putStrLn toks


