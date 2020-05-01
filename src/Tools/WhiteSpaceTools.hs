module Tools.WhiteSpaceTools where


oneTab :: String
oneTab = "    "


tab :: Int -> String
tab n = concat $ take n $ repeat oneTab


zipTab :: [String] -> [String]
zipTab lines = map (\x -> (tab 1) ++ x) lines

nZipTab :: Int -> [String] -> [String]
nZipTab 0 s = s
nZipTab n s = zipTab (nZipTab (n - 1) s)


blankLine :: [String]
blankLine = [""]


eol :: String
eol = "\n"



