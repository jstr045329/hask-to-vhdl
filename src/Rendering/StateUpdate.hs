-- Contains a datatype for representing assignments of this form:
--
--      x1 <= x2;
--      v1 := v2;
--
-- The StateUpdate constructor does not care whether its first argument (assignment destination)
-- is a signal or variable. Code that renders the StateUpdate should handle either case. 
module Rendering.StateUpdate where
import Rendering.InfoTypes
import Data.Hashable


data StateUpdate = StateUpdate Information Information deriving (Eq, Show)

    --  TODO: Add a Concat constructor to Information datatype. 

instance Hashable StateUpdate where
    hashWithSalt s (StateUpdate i1 i2) = 
        s `hashWithSalt`
        i1 `hashWithSalt`
        i2

