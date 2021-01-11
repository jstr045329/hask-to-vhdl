------------------------------------------------------------------------------------------------------------------------
--                                                 Interspersed Code 
--
-- This module represents VHDL, Hs, and hask-to-vhdl commands intermingled in a stream. 
--
------------------------------------------------------------------------------------------------------------------------
module Rendering.InterspersedCode where 


data VhdLiteral = 
        VhdLiteral [String]
    |   NoVhd 
        deriving (Eq, Show)


data HsLiteral = 
        HsLiteral [String] 
    |   NoHs
        deriving (Eq, Show)


-- hask-to-vhdl commands go here:
data H2VCommand = 
        H2VLiteral [String]
    |   NoH2V 
        deriving(Eq, Show)


------------------------------------------------------------------------------------------------------------------------
--                                           Wrap All These Types Together 
------------------------------------------------------------------------------------------------------------------------
data InterspersedCode = InterspersedCode VhdLiteral HsLiteral H2VCommand deriving (Eq, Show)


------------------------------------------------------------------------------------------------------------------------
--                                          Render Mixed Languages As VHDL 
--
-- NOTE: Right now, all code is converted to VHDL. When the team size grows larger than 1, we would like to parse 
-- the entire VHDL language, convert to Hs, and back again. This will allow an engineer to switch between VHDL and 
-- Hs views of the same project. The idea is that when you see something in VHDL you want to change, change it, and 
-- when you switch back to Hs view, Hs will reflect the change you just made. And likewise for Hs. 
--
------------------------------------------------------------------------------------------------------------------------
renderVhdToVhd :: VhdLiteral -> [String]
renderVhdToVhd (VhdLiteral los) = los
renderVhdToVhd NoVhd = []


-- TODO: PICK UP HERE: Run the Hs literals and return output
renderHsToVhd :: HsLiteral -> [String]
renderHsToVhd (HsLiteral los) = los
renderHsToVhd NoHs = []


-- TODO: Run the commands and return output
renderH2VToVhd :: H2VCommand -> [String]
renderH2VToVhd (H2VLiteral los) = los 
renderH2VToVhd NoH2V = []


interspersed2Vhd :: InterspersedCode -> [String]
interspersed2Vhd (InterspersedCode vLit hLit h2VLit) = (renderVhdToVhd vLit) ++ (renderHsToVhd hLit) ++ (renderH2VToVhd h2VLit)






