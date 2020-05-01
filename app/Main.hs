module Main where
import Rendering.GenerateTestbench
import Rendering.ProcessFile

--main :: IO ()
--main = putStrLn "yo" 

inputFile = "/home/jamie/hask-to-vhdl/src/TestVhd/test.vhd"
outputFile = "/home/jamie/hask-to-vhdl/src/TestVhd/test_tb.vhd"

--main :: IO()
main = do
    putStrLn "Running main..."
    let los = generateTestbench
    processFile inputFile outputFile generateTestbench


