------------------------------------------------------------------------------------------------------------------------
--                                       Define The Code Convention Interface 
--
-- This module defines the interface by which users can define any coding convention they can imagine, and automate
-- the transformation of code so that it conforms to that convention.
--
-- Code transformations accept the interface RenderedCodePack -> RenderedCodePack.
--
------------------------------------------------------------------------------------------------------------------------
module Rendering.CodeConventions.CodeConventions where
import Parsing.SourceSinkParser


data RenderedCodePack = RenderedCodePack {
        rawUserInput :: [String]
    ,   renderedLines :: [String]
    ,   portAndSignalNames :: InfoPack
    }


blankCodePack :: RenderedCodePack
blankCodePack = RenderedCodePack {
        rawUserInput = []
    ,   renderedLines = []
    ,   portAndSignalNames = blankInfoPack
    }


noCodeTransformation :: RenderedCodePack -> RenderedCodePack
noCodeTransformation rcp = rcp


