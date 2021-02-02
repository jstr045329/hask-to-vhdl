module Rendering.RapidProjectGen.RunGeneratorScript where
import Rendering.RapidProjectGen.CommandDecoder
import Rendering.RapidProjectGen.DecodeOneString
import Rendering.RapidProjectGen.GeneratorState
import Rendering.RapidProjectGen.WriteEntityFiles
import Rendering.ProcessFile
import Control.Monad
import Tools.ListTools


feedOneLine :: [String] -> GeneratorState -> GeneratorState
feedOneLine [] gS = gS
feedOneLine los gS = feedOneLine (tail los) (decodeOneStr (head los) gS) 


executeOneFile' :: [String] -> IO GeneratorState
executeOneFile' los = dumpGeneratorStateToFile' (feedOneLine los defaultGeneratorState)


-- runScript :: String -> IO ()
runScript someName = do
--     inputStr <- readFile someName
    inputLines <- liftM lines (readFile someName)
    ioGS <- executeOneFile' inputLines 
    return ioGS

-- 142 dumpGeneratorStateToFile' gS =
-- 143     do
-- 144         dumpAllEntities (entTree gS)
-- 145         dumpAllTestbenches (entTree gS)
-- 146         dumpAllTcls (entTree gS)
-- 147         return gS


--    inputLines <- lines (readFile someFileName)
--    outputGS <- feedOneLine inputLines defaultGeneratorState
--    return dumpGeneratorStateToFile' outputGS


