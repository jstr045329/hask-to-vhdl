module Rendering.VectorSubRange where 
import Rendering.InfoTypes


data VectorSubRange =     WholeVector Information
                        | SubRangeHard Integer Integer
                        | SubRangeSoft Information Information
                        | MsbDowntoX Information
                        | SubRangeXDownto0 Information
                          deriving (Eq, Show)

