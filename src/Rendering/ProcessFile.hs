module Rendering.ProcessFile where
import Parsing.GuaranteeWhitespace

glueStrings :: [String] -> String
glueStrings los
    | los == [] = ""
    | otherwise = (head los) ++ "\n" ++ (glueStrings (tail los))


glueStrings' :: [String] -> IO String
glueStrings' los = do
    return (glueStrings los)


--processFile :: FilePath -> FilePath -> ([String] -> [String]) -> IO ()
processFile fileNameIn fileNameOut businessLogic = do  
                x <- liftM tokenize' (readFile fileNameIn)
                y <- glueStrings' (businessLogic x)
                writeFile fileNameOut y


