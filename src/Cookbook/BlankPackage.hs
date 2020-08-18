module Cookbook.BlankPackage (
    blankPackage
    ) where 
import Rendering.ProcessFile
import Rendering.PopulateTemplates
import Rendering.InfoTypes


fullPath :: String -> String -> String
fullPath onePath entName = onePath ++ entName ++ ".vhd"


blankPackage :: String -> String -> IO ()
blankPackage onePath pkgName = do 
    dump2File (fullPath onePath pkgName) (populatePackageTemplate pkgName [] [])
    
