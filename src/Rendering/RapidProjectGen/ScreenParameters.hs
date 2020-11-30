module Rendering.RapidProjectGen.ScreenParameters where


-- Decide how wide a column off to the side should be:
sideColumn :: Int
sideColumn = 45

-- Decide how wide the middle column should be:
middleColumn :: Int
middleColumn = 120

-- Decide how wide the entire window should be:
wholeScreen :: Int
wholeScreen = sideColumn*2 + middleColumn


