module Rendering.RegisteredGates where
import Rendering.InfoTypes
import Rendering.GlueSigNames
import Tools.ListTools


-- TODO: Add package declaration to output file

glue' :: Information -> Information -> [Information] -> [Information] -> String
glue' clk rst sigList results = (glueSigNames ([clk, rst] ++ sigList)) ++ (nomen (head results))
 
-- sigList is a list of signals you want ANDed together. 
-- results is a list of signals that will accept the output.
andSigs :: Information -> Information -> [Information] -> [Information] -> [String]
andSigs clk rst sigList results
    | ((length results) == 0) && ((length sigList) > 0) = error "results list must be at least as long as sigList"
    | (length sigList) < 3 = []
    | (length sigList) == 3 = ["registeredAND1 port map (" ++ (glue' clk rst sigList results) ++ ");"]
    | (length sigList) == 4 = ["registeredAND2 port map (" ++ (glue' clk rst sigList results) ++ ");"]
    | otherwise = ["registeredAND3 port map (" ++
        (glueSigNames ([clk, rst] ++ (take 3 sigList))) ++ (nomen (head results)) ++ ");"] ++
        (andSigs clk rst (skipN sigList 3) (tail results))


-- sigList is a list of signals you want ORed together. 
-- results is a list of signals that will accept the output.
orSigs :: Information -> Information -> [Information] -> [Information] -> [String]
orSigs clk rst sigList results
    | ((length results) == 0) && ((length sigList) > 0) = error "results list must be at least as long as sigList"
    | (length sigList) == 0 = []
    | (length sigList) == 1 = ["registeredOR1 port map (" ++
                                (glueSigNames ([clk, rst] ++ sigList)) ++ (nomen (head results)) ++ ");"]
    | (length sigList) == 2 = ["registeredOR2 port map (" ++
                                (glueSigNames ([clk, rst] ++ sigList)) ++ (nomen (head results)) ++ ");"]
    | otherwise = ["registeredOR3 port map (" ++
        (glueSigNames ([clk, rst] ++ (take 3 sigList))) ++ (nomen (head results)) ++ ");"] ++
        (orSigs clk rst (skipN sigList 3) (tail results))


-- sigList is a list of signals you want NANDed together. 
-- results is a list of signals that will accept the output.
nandSigs :: Information -> Information -> [Information] -> [Information] -> [String]
nandSigs clk rst sigList results
    | ((length results) == 0) && ((length sigList) > 0) = error "results list must be at least as long as sigList"
    | (length sigList) == 0 = []
    | (length sigList) == 1 = ["registeredNAND1 port map (" ++
                                (glueSigNames ([clk, rst] ++ sigList)) ++ (nomen (head results)) ++ ");"]
    | (length sigList) == 2 = ["registeredNAND2 port map (" ++
                                (glueSigNames ([clk, rst] ++ sigList)) ++ (nomen (head results)) ++ ");"]
    | otherwise = ["registeredNAND3 port map (" ++
        (glueSigNames ([clk, rst] ++ (take 3 sigList))) ++ (nomen (head results)) ++  ");"] ++
        (nandSigs clk rst (skipN sigList 3) (tail results))


-- sigList is a list of signals you want NORed together. 
-- results is a list of signals that will accept the output.
norSigs :: Information -> Information -> [Information] -> [Information] -> [String]
norSigs clk rst sigList results
    | ((length results) == 0) && ((length sigList) > 0) = error "results list must be at least as long as sigList"
    | (length sigList) == 0 = []
    | (length sigList) == 1 = ["registeredNOR1 port map (" ++
                                (glueSigNames ([clk, rst] ++ sigList)) ++ (nomen (head results)) ++ ");"]
    | (length sigList) == 2 = ["registeredNOR2 port map (" ++
                                (glueSigNames ([clk, rst] ++ sigList)) ++ (nomen (head results)) ++ ");"]
    | otherwise = ["registeredNOR3 port map (" ++
        (glueSigNames ([clk, rst] ++ (take 3 sigList))) ++ (nomen (head results)) ++ ");"] ++
        (norSigs clk rst (skipN sigList 3) (tail results))


-- sigList is a list of signals you want XORed together. 
-- results is a list of signals that will accept the output.
xorSigs :: Information -> Information -> [Information] -> [Information] -> [String]
xorSigs clk rst sigList results
    | ((length results) == 0) && ((length sigList) > 0) = error "results list must be at least as long as sigList"
    | (length sigList) == 0 = []
    | (length sigList) == 1 = ["registeredXOR1 port map (" ++
                                (glueSigNames ([clk, rst] ++ sigList)) ++ (nomen (head results)) ++ ");"]
    | (length sigList) == 2 = ["registeredXOR2 port map (" ++
                                (glueSigNames ([clk, rst] ++ sigList)) ++ (nomen (head results)) ++ ");"]
    | otherwise = ["registeredXOR3 port map (" ++
        (glueSigNames ([clk, rst] ++ (take 3 sigList))) ++ (nomen (head results)) ++ ");"] ++
        (xorSigs clk rst (skipN sigList 3) (tail results))

