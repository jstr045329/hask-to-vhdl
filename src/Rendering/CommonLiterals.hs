module Rendering.CommonLiterals where
import Rendering.InfoTypes


stdLogicHi :: Information
stdLogicHi = Literal {
        dataType = StdLogic
    ,   width = Hard 1
    ,   sDefault = Specified "'1'"
    ,   comments = []
    }
    

stdLogicLo :: Information
stdLogicLo = Literal {
        dataType = StdLogic
    ,   width = Hard 1
    ,   sDefault = Specified "'0'"
    ,   comments = []
    }
    
    
