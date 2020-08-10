module Rendering.ElseClause where

elseClause :: (Integral a) => a -> String
elseClause 0 = ""
elseClause _ = "els"
