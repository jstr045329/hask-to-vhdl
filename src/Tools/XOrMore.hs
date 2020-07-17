module Tools.XOrMore where

-- Original impetus for this module is that we needed a type to express 
-- a number either being 1 or >1. If a number is > 1, we don't care how 
-- much; only that it is greater than 1.
data OneOrMore = OnlyOne | MoreThanOne deriving (Eq, Show)

-- Add a few others for the sake of completeness:
data ZeroOrMore = OnlyZero | MoreThanZero deriving (Eq, Show)

data XOrMore = OnlyX Double | MoreThanX Double deriving (Eq, Show)

