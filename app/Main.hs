module Main where
import Rendering.GenerateTestbench
import Rendering.ProcessFile


inputFile = "/path/to/some/vhd.vhd"
outputFile = "/path/to/some/other/vhd.vhd"

main = do
    putStrLn "Running main..."
    let los = generateTestbench
    processFile inputFile outputFile generateTestbench


