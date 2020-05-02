module Parsing.GuaranteeWhitespace where


twoCharTokens :: [String]
twoCharTokens = [
      "<=" 
    , "=>" 
    , ":=" 
    , "/="
    , "--"
    ]


oneCharTokens :: [String]
oneCharTokens = [
      "="
    , "<"
    , ">"
    , "("
    , ")"
    , "+"
    , "-"
    , "*"
    , "/"
    , ","
    , ":"
    , ";"
    ]


snipString :: String -> String -> String
snipString _ "" = ""
snipString s oneTok
    | length s < length oneTok = ""
    | otherwise = take (length oneTok) s


negNumberStart :: [String]
negNumberStart = map (\x -> "-" ++ [x]) ['0'..'9']


findFirstMatch :: String -> [String]
findFirstMatch s =
    let twoCharMatches = [a | a <- twoCharTokens, a == (take 2 s)]
        oneCharMatches = [a | a <- oneCharTokens, a == (take 1 s)]
        in if (length twoCharMatches) > 0
                then twoCharMatches
                else oneCharMatches


skipNChars :: String -> Integer -> String
skipNChars someStr n = iterate tail someStr !! (fromInteger n)


wrapInSpaces :: String -> String
wrapInSpaces s = " " ++ s ++ " "


idxFirstNonWhitespaceChar :: String -> Integer -> Integer
idxFirstNonWhitespaceChar "" n = n
idxFirstNonWhitespaceChar s n
    | (head s) == ' '   = idxFirstNonWhitespaceChar (tail s) (n + 1)
    | otherwise         = n


firstNonWhitespaceChar :: String -> String
firstNonWhitespaceChar "" = ""
firstNonWhitespaceChar s
    | (head s) == ' ' = firstNonWhitespaceChar (tail s)
    | otherwise = [head s]


leadsWithDigit :: String -> Bool
leadsWithDigit s
    | (firstNonWhitespaceChar s) == "" = False
    | otherwise = elem (head (firstNonWhitespaceChar s)) ['0'..'9']


gWhitespace :: String -> String
gWhitespace "" = ""
gWhitespace s
    | findFirstMatch s /= []        = (wrapInSpaces (head (findFirstMatch s))) ++ 
                                      gWhitespace (skipNChars s (fromIntegral (length (head (findFirstMatch s)))))

    | otherwise                     = [head s] ++ gWhitespace (tail s)


removeComments :: [String] -> [String]
removeComments [] = []
removeComments los
    | (head los) == "--" = []
    | otherwise = [head los] ++ (removeComments (tail los))


-- Each string is 1 line in this file:
tokenize :: [String] -> [String]
tokenize [] = []
tokenize los = (removeComments (words (gWhitespace (head los)))) ++ tokenize (tail los)


--tokenize' :: String -> [String]
--tokenize' s = tokenize (words s)


tokenize' :: String -> String -> [String]
tokenize' "" _ = []
tokenize' inStr underConstruction
    | ((head inStr) == '\n') = [underConstruction] ++ (tokenize' (tail inStr) "")
    | otherwise = tokenize' (tail inStr) (underConstruction ++ [head inStr])


tokenize'' :: String -> [String]
tokenize'' "" = []
tokenize'' s = tokenize (tokenize' s "")


liftM :: (a -> b) -> (IO a -> IO b)
liftM f action = do x <- action
                    return (f x)


-- Use this to print list of tokens:
-- liftM tokenize' (readFile "./src/test.vhd")
-- :t liftM tokenize' (readFile "./src/test.vhd") gives
-- IO [String]

tokenizeFile :: String -> IO [()]
tokenizeFile fileName = do 
                x <- liftM tokenize'' (readFile fileName) 
                mapM putStrLn x




