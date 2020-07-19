module Rendering.VhdLibraries where


data StandardLibs = 
            StdLogicLib
        |   StdArithLib
        |   NumericBitLib
        |   NumericStdLib
        |   SignedLib
        |   UnsignedLib
        |   RealLib
        |   ComplexLib
        |   TextIOLib
            deriving (Eq, Show)


data VhdLib = 
          UseAll String  
        | UseSome String [String]
          deriving (Eq, Show)


