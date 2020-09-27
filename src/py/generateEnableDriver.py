"""This script generates a one-hot clock enable process."""

PERIOD_IN_CLKS = 8
OUTPUT_TAPS = list(range(PERIOD_IN_CLKS))
NAME_STUB = "s_soft_enable_%d"


assert(max(OUTPUT_TAPS) < PERIOD_IN_CLKS)


def makeSignalNames():
    y = []
    for i in range(len(OUTPUT_TAPS)):
        y.append(NAME_STUB % i)
    return y


def declareSignals():
    return ["signal %s : std_logic;" % s for s in makeSignalNames()] + \
        ["signal s_enable_counter : integer;", 
         "signal s_enable_other_enables : std_logic;"]


def resetSignals():
    return ["%s <= '0';" % s for s in makeSignalNames()] + \
        ["s_enable_other_enables <= '0';",
         "s_enable_counter <= 0;"]


def tab(n=1):
    return "    " * n


def zipTab(los, n=1):
    return [tab(n) + s for s in los]


process_header = """ENABLE_DRIVER: process(clk)
begin
    if rising_edge(clk) then 
        if rst = '1' then
"""
# Then reset the signals 

            # s_enable_counter <= 0;
            # s_soft_enable_0 <= '0';
            # s_soft_enable_1 <= '0';
            # s_soft_enable_2 <= '0';
            # s_soft_enable_3 <= '0';
            # s_enable_other_enables <= '0';
        
cycle_0_chunk = ["""if s_enable_counter = %d then""",
"""    s_enable_counter <= 0;""",
"""    s_soft_enable_0 <= '1';""",
"""    s_enable_other_enables <= '1';""",
"""else""",
"""    s_enable_counter <= s_enable_counter + 1;""",
"""    s_soft_enable_0 <= '0';""",
"""end if;""",
"",]


def populateCycle0Chunk():
    y = []
    for line in cycle_0_chunk:
        if "%d" in line:
            y.append(line % PERIOD_IN_CLKS)
        else:
            y.append(line)
    return y


def wrapInSEnableOtherEnables(los):
    return ["""if s_enable_other_enables = '1' then """] + zipTab(los) + \
            ["""end if;"""]


cycle_n_chunk = ["""if s_enable_counter = %d then""",
tab() + """%s <= '1';""",
"""else""",
tab() + """%s <= '0';""",
"""end if;""",
]


def makeOneChunk(sigName, cycleNum):
    y = []
    for line in cycle_n_chunk:
        if "%s" in line:
            y.append(line % sigName)
        elif "%d" in line:
            y.append(line % cycleNum)
        else:
            y.append(line)
    return y


def main():
    y = []
    y.extend(declareSignals())
    y.append("")
    y.append("")
    y.append(process_header)
    y.extend(zipTab(resetSignals(), 3))
    y.append("")
    y.append("")
    y.append(tab(2) + "else")
    y.extend(zipTab(populateCycle0Chunk(), 3))
    chunks = []
    sigNames = makeSignalNames()
    for i in range(len(OUTPUT_TAPS)):
        oneTap = OUTPUT_TAPS[i]
        chunks.extend(makeOneChunk(sigNames[i], oneTap))
        chunks.extend(["",""])
    
    y.extend(zipTab(wrapInSEnableOtherEnables(chunks), 3))
    y.append(tab(2) + "end if;")
    y.append(tab(1) + "end if;")
    y.append("end process;")
    y.append("")
    y.append("")
    
    for line in y:
        print(line)
    
if __name__ == "__main__":
    main()

