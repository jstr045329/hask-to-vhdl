module Parsing.ScrapeOutputs (
    scrapeFormulaOutputs
    ) where 
import Parsing.LocateOutputs


scrapeFormulaOutputBrains :: [String] -> [String]
scrapeFormulaOutputBrains [] = []
scrapeFormulaOutputBrains los
    | (isAssignmentTarget los) = [head los] ++ (scrapeFormulaOutputBrains (tail los))
    | otherwise = scrapeFormulaOutputBrains (tail los)


scrapeFormulaOutputs :: [String] -> [String]
scrapeFormulaOutputs los = scrapeFormulaOutputBrains (removeIfStatements False los)
    

