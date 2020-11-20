module Rendering.ElseTerm where 


-- Skips the "else" for the first "if" term. 
-- Inserts "else" for every term thereafter. 
elseTerm :: Int -> String
elseTerm 0 = ""
elseTerm _ = "els"

