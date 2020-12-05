module Rendering.InterspersedCode where 


data VhdLiteral = 
        VhdLiteral [String]
    |   NoVhd 
        deriving (Eq, Show)


data HsLiteral = 
        HsLiteral [String] 
    |   NoHs
        deriving (Eq, Show)


data InterspersedCode = InterspersedCode VhdLiteral HsLiteral deriving (Eq, Show)


