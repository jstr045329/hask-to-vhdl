module EasyTui.EasyTuiTools where 


-- ScreenSize's are always (width, height)
data ScreenSize = ScreenSize Integer Integer deriving (Eq, Show)


-- BoxDims are always (x_start, y_start, x_stop, y_stop)
-- where 0,0 is upper left hand corner of screen. 
data BoxCorners = BoxCorners Int Int Int Int deriving (Eq, Show)


data BoxOfStrings = BoxOfStrings [String]


infiniteDashes :: String 
infiniteDashes = ['-' | _ <- [0..]]


infiniteSpaces :: String 
infiniteSpaces = [' ' | _ <- [0..]]


spaces :: Int -> String 
spaces n 
    | (n <= 0) = ""
    | otherwise = take n infiniteSpaces


drawOneLine :: String -> BoxCorners -> String 
drawOneLine s (BoxCorners xStart _ xStop _) = 
    "| " ++ (take fieldWidth s) ++ (spaces (fieldWidth - (length s))) ++ " |" where 
        fieldWidth = (xStop - xStart - 4)


drawTitleLine :: String -> BoxCorners -> String 
drawTitleLine s (BoxCorners xStart _ xStop _) = 
    

-- Note that inputBox is ignored when lineNum == 0
drawBox :: [String] -> BoxCorners -> Int -> [String]
drawBox rawStrings (BoxCorners xStart yStart xStop yStop) lineNum 
    | (lineNum == 0) = [take (xStop - xStart) infiniteDashes] ++ (drawBox rawStrings (BoxCorners xStart yStart xStop yStop) (lineNum + 1)) 
    | (lineNum == yStop - yStart - 1) = [take (xStop - xStart) infiniteDashes]
    | (length rawStrings == 0) = 
        [drawOneLine "" (BoxCorners xStart yStart xStop yStop)] ++ 
        (drawBox [] (BoxCorners xStart yStart xStop yStop) (lineNum + 1))
    | otherwise = 
        [drawOneLine (head rawStrings) (BoxCorners xStart yStart xStop yStop)] ++ 
        (drawBox (tail rawStrings) (BoxCorners xStart yStart xStop yStop) (lineNum + 1))
        
    


