module Cookbook.BlankEntity (
    blankEntity
    ) where 
import Rendering.ProcessFile
import Rendering.PopulateTemplates
import Rendering.InfoTypes


fullPath :: String -> String -> String
fullPath onePath entName = onePath ++ entName ++ ".vhd"


blankEntity :: String -> String -> IO ()
blankEntity onePath entName = do 
    -- easyInSl :: String -> [String] -> Information
    dump2File (fullPath onePath entName) 
        (populateEntityTemplate [] 
            (declareBatch [easyClk, easyRst]) [] [] (vanillaSettings entName))
    
