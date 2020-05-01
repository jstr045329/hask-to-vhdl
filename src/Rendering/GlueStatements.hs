module Rendering.GlueStatements where
import Parsing.TokenMatchingTools


glueOneLine :: [String] -> String
glueOneLine [] = ""
glueOneLine x = (head x) ++ " " ++ (glueOneLine (tail x))


locateStatementEnd :: [String] -> Int
locateStatementEnd [] = 0
locateStatementEnd x
    | (head x) == ";" = 0
    | (head x) == "," = 0
    | otherwise = 1 + locateStatementEnd (tail x)


glueStatements :: [String] -> [String]
glueStatements [] = []
glueStatements los =
    [glueOneLine (take statementLength los)] ++ (glueStatements (skipNTokens los statementLength))
        where
            statementLength = 1 + locateStatementEnd los

