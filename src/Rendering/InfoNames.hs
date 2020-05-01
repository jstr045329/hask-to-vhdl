module Rendering.InfoNames where
import Text.Printf


formStr :: String
formStr = "%04d"


nameInput :: String -> Int -> String
nameInput stub d = "i_" ++ stub ++ "_d" ++ (printf formStr d)


nameOutput :: String -> Int -> String
nameOutput stub d = "o_" ++ stub ++ "_d" ++ (printf formStr d)


nameInout :: String -> Int -> String
nameInout stub d = "u_" ++ stub ++ "_d" ++ (printf formStr d)


nameSignal :: String -> Int -> String
nameSignal stub d = "s_" ++ stub ++ "_d" ++ (printf formStr d)


nameGeneric :: String -> Int -> String
nameGeneric stub _ = "g_" ++ stub


nameVar :: String -> Int -> String
nameVar stub _ = "v_" ++ stub


