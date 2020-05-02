module Rendering.DeclareSignals where
import Rendering.InfoTypes


declareOneSig :: Information -> String
declareOneSig s = "signal " ++ (nomen s) ++ " : " ++ (datatypeToStr (dataType s) (width s)) ++ ";"


-- Same as declareOneSig, but uses the default value if it exists:
declareSigWithDefault :: Information -> String
declareSigWithDefault s = 
    "signal " ++ (nomen s) ++ " : " ++ (datatypeToStr (dataType s) (width s)) ++ (default2Str (sDefault s)) ++ ";"


