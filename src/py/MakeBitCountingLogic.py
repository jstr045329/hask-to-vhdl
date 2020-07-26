# This script generates a function that counts the number of 1's in 
# a std_logic_vector.
INPUT_FILE = "../VhdTemplates/CountBits8Template.vhd"
OUTPUT_FILE = "../VhdLibs/CountBits8.vhd"
NUM_INPUT_BITS = 8
NUM_OUTPUT_BITS = 4

def int2Bin(x):
    return "{0:b}".format(x)


def zeroPad(s, w):
    num0s = w - len(s)
    return "0"*num0s + s


def wrapNum(s):
    return '"' + s + '"'


def tab(n=1):
    return "    "*n


def ziptab(los, n=1):
    if n == 1:
        return [tab() + s for s in los]
    return [tab() + s for s in ziptab(los, n-1)]


def count1s(binStr):
    y = 0
    for c in binStr:
        if c == '1':
            y += 1
    return y


def main():
    y = []
    y.append("function count_bits(")
    y.append(tab() + ("x : std_logic_vector(%d downto 0)" % (NUM_INPUT_BITS-1,)))
    y.append(tab() + ") return std_logic_vector is")
    y.append("variable y : std_logic_vector(%d downto 0);" % (NUM_OUTPUT_BITS-1,))
    y.append("begin")
    y.append(tab() + "y := (others => '0');")
    y.append(tab() + "case x is")
    for i in range(2**NUM_INPUT_BITS):
        oneBinStr = int2Bin(i)
        s1 = tab() + "when " + wrapNum(zeroPad(oneBinStr, NUM_INPUT_BITS)) + " =>"
        s2 = tab(2) + "y := " + wrapNum(zeroPad(int2Bin(count1s(oneBinStr)), 4)) + ";"
        y.append(s1)
        y.append(s2)
    y.append(tab() + "when others =>")
    y.append(tab(2) + "y := (others => '0');")
    y.append(tab() + "end case;")
    y.append(tab() + "return y;")
    y.append("end function;")
    y.append("")
    return y


if __name__ == "__main__":
    logicLines = main()
    templateLines = []
    with open(INPUT_FILE, 'r') as f:
        templateLines.extend(f.readlines())
    y = []
    for line in templateLines:
        los = line.split()
        if len(los) > 0 and los[0] == "<insert_logic_here>":
            y.extend([s + "\n" for s in logicLines])
        else:
            y.append(line)

    with open(OUTPUT_FILE, 'w') as f:
        for line in y:
            f.write(line)




