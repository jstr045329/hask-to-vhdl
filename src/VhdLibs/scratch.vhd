
sl2Bool(not (bool2SL(francine_01 > joe_47) and (bool2SL(francine_01 < harold_96) and bool2SL(joe_47 = harold_96))))

sl2Bool(not (bool2SL(francine_01 > joe_47) or (bool2SL(francine_01 < harold_96) or bool2SL(joe_47 = harold_96))))

sl2Bool(bool2SL(francine_01 > joe_47) xor (bool2SL(francine_01 < harold_96) xor bool2SL(joe_47 = harold_96)))

sl2Bool(not (bool2SL(francine_01 > joe_47) xor (bool2SL(francine_01 < harold_96) xor bool2SL(joe_47 = harold_96))))

sl2Bool(
    bool2SL(
        sl2Bool(
            bool2SL(francine_01 > joe_47) or 
                (bool2SL(francine_01 < harold_96) or 
                (bool2SL(joe_47 = harold_96) or 
                (bool2SL(jose_42 = '1') or 
                bool2SL(geraldo_87 = '0')))))) 
    xor 
        bool2SL(
            sl2Bool(
                not 
                    (bool2SL(francine_01 > joe_47) and 
                    (bool2SL(francine_01 < harold_96) and 
                    (bool2SL(joe_47 = harold_96) and 
                    (bool2SL(jose_42 = '1') and 
                    bool2SL(geraldo_87 = '0'))))))))
