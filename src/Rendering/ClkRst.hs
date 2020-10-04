module Rendering.ClkRst where
import Rendering.InfoTypes

------------------------------------------------------------------------------------------------------------------------
--                                      Define a Type to Convey Clock and Reset
--
-- Type signatures get cluttered up when you keep typing:
--
--      ... -> Information -> Information -> ...
--
-- over and over. Here is a type to wrap those up.
------------------------------------------------------------------------------------------------------------------------
data ClkRst = ClkRst Information Information deriving (Eq, Show)


------------------------------------------------------------------------------------------------------------------------
--                                Here Is A Function To Generate The Type In One Word
------------------------------------------------------------------------------------------------------------------------
easyClkRst :: ClkRst
easyClkRst = ClkRst easyClk easyRst
