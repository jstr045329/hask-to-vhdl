module Rendering.RegisteredGates where
import Rendering.InfoTypes
import Rendering.GlueSigNames
import Tools.ListTools


-- sigList is a list of signals you want ANDed together. 
-- results is a list of signals that will accept the output.
andSigs :: [Information] -> [String]
andSigs sigList results
    | ((length results) == 0) && ((length sigList) > 0) = error "results list must be at least as long as sigList"
    | (length sigList) == 0 = []
    | (length sigList) == 1 = ["registeredAND1 port map (" ++
                                (glueSigNames ([clk, rst] ++ sigList ++ (nomen (head results)))) ++ ");"]
    | (length sigList) == 2 = ["registeredAND2 port map (" ++
                                (glueSigNames ([clk, rst] ++ sigList ++ (nomen (head results)))) ++ ");"]
    | otherwise = ["registeredAND3 port map (" ++
        (glueSigNames ([clk, rst] ++ (take 3 sigList) ++ (nomen (head results)))) ++ ");"] ++
        (andSigs (skipN sigList 3) (tail results))


-- sigList is a list of signals you want ORed together. 
-- results is a list of signals that will accept the output.
orSigs :: [Information] -> [String]
orSigs sigList results
    | ((length results) == 0) && ((length sigList) > 0) = error "results list must be at least as long as sigList"
    | (length sigList) == 0 = []
    | (length sigList) == 1 = ["registeredOR1 port map (" ++
                                (glueSigNames ([clk, rst] ++ sigList ++ (nomen (head results)))) ++ ");"]
    | (length sigList) == 2 = ["registeredOR2 port map (" ++
                                (glueSigNames ([clk, rst] ++ sigList ++ (nomen (head results)))) ++ ");"]
    | otherwise = ["registeredOR3 port map (" ++
        (glueSigNames ([clk, rst] ++ (take 3 sigList) ++ (nomen (head results)))) ++ ");"] ++
        (orSigs (skipN sigList 3) (tail results))


-- sigList is a list of signals you want NANDed together. 
-- results is a list of signals that will accept the output.
nandSigs :: [Information] -> [String]
nandSigs sigList results
    | ((length results) == 0) && ((length sigList) > 0) = error "results list must be at least as long as sigList"
    | (length sigList) == 0 = []
    | (length sigList) == 1 = ["registeredNAND1 port map (" ++
                                (glueSigNames ([clk, rst] ++ sigList ++ (nomen (head results)))) ++ ");"]
    | (length sigList) == 2 = ["registeredNAND2 port map (" ++
                                (glueSigNames ([clk, rst] ++ sigList ++ (nomen (head results)))) ++ ");"]
    | otherwise = ["registeredNAND3 port map (" ++
        (glueSigNames ([clk, rst] ++ (take 3 sigList) ++ (nomen (head results)))) ++ ");"] ++
        (nandSigs (skipN sigList 3) (tail results))


-- sigList is a list of signals you want NORed together. 
-- results is a list of signals that will accept the output.
norSigs :: [Information] -> [String]
norSigs sigList results
    | ((length results) == 0) && ((length sigList) > 0) = error "results list must be at least as long as sigList"
    | (length sigList) == 0 = []
    | (length sigList) == 1 = ["registeredNOR1 port map (" ++
                                (glueSigNames ([clk, rst] ++ sigList ++ (nomen (head results)))) ++ ");"]
    | (length sigList) == 2 = ["registeredNOR2 port map (" ++
                                (glueSigNames ([clk, rst] ++ sigList ++ (nomen (head results)))) ++ ");"]
    | otherwise = ["registeredNOR3 port map (" ++
        (glueSigNames ([clk, rst] ++ (take 3 sigList) ++ (nomen (head results)))) ++ ");"] ++
        (norSigs (skipN sigList 3) (tail results))


-- sigList is a list of signals you want XORed together. 
-- results is a list of signals that will accept the output.
xorSigs :: [Information] -> [String]
xorSigs sigList results
    | ((length results) == 0) && ((length sigList) > 0) = error "results list must be at least as long as sigList"
    | (length sigList) == 0 = []
    | (length sigList) == 1 = ["registeredXOR1 port map (" ++
                                (glueSigNames ([clk, rst] ++ sigList ++ (nomen (head results)))) ++ ");"]
    | (length sigList) == 2 = ["registeredXOR2 port map (" ++
                                (glueSigNames ([clk, rst] ++ sigList ++ (nomen (head results)))) ++ ");"]
    | otherwise = ["registeredXOR3 port map (" ++
        (glueSigNames ([clk, rst] ++ (take 3 sigList) ++ (nomen (head results)))) ++ ");"] ++
        (xorSigs (skipN sigList 3) (tail results))

