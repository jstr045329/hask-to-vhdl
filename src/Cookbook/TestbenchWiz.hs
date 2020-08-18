-- This module converts the input filename from abc.vhd to abc_tb.vhd, then passes it 
-- through the generateTestbench funtion. 
module Cookbook.TestbenchWiz (
    magicTestbench
    ) where
import Tools.StringTools
import Rendering.ProcessFile
import Rendering.GenerateTestbench


convertFilename :: String -> String
convertFilename s = replaceSubStr s ".vhd" "_tb.vhd"


magicTestbench fName = processFile fName (convertFilename fName) generateTestbench

