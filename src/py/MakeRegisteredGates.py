"""This script takes OneRegisteredGate.vhd and transforms it into a library of registered gates."""


if __name__ == "__main__":
    INPUT_FILE = "../VhdTemplates/OneRegisteredGate.vhd"
    OUTPUT_FILE = "../VhdLibs/RegisteredGatesPkg.vhd"
    POS_RESET = False
    with open(INPUT_FILE, 'r') as f:
        inputLines = f.readlines()


def makeTagMap(f1, f2, f3, f4, f5, f6, f7, f8):
    tags = {
        "<entity_name_here>": f1,
        "<input_0_here>": f2,
        "<input_1_here>": f3,
        "<input_2_here>": f4,
        "<input_3_here>": f5,
        "<input_4_here>": f5,
        "<input_5_here>": f5,
        "<input_6_here>": f5,
        "<insert_logic_here>": f6,
        "<reset_name_here>": f7,
        "<test_reset_here>": f8,
        }
    return tags


def use_input_0(**kwargs):
    return True


def use_input_1(**kwargs):
    if "input_1" in kwargs.keys():
        return kwargs["input_1"]
    return False


def use_input_2(**kwargs):
    if "input_2" in kwargs.keys():
        return kwargs["input_2"]
    return False


def use_input_3(**kwargs):
    if "input_3" in kwargs.keys():
        return kwargs["input_3"]
    return False


def reset_name(**kwargs):
    if kwargs["posReset"]:
        return "reset"
    return "reset_n"


def test_reset(**kwargs):
    if kwargs["posReset"]:
        return "if reset = '1' then"
    return "if reset_n = '0' then"
    

def andPack():
    def makeEntityName(**kwargs):
        y = "registeredAND"
        y += kwargs["numInputs"]
        return y
    def logic_function(**kwargs):
        return " and "
    return makeTagMap(makeEntityName, use_input_0, use_input_1, use_input_2, use_input_3, logic_function,\
            reset_name, test_reset)


def orPack():
    def makeEntityName(**kwargs):
        y = "registeredOR"
        y += kwargs["numInputs"]
        return y
    def logic_function(**kwargs):
        return " or "
    return makeTagMap(makeEntityName, use_input_0, use_input_1, use_input_2, use_input_3, logic_function,\
            reset_name, test_reset)


def nandPack():
    def makeEntityName(**kwargs):
        y = "registeredNAND"
        y += kwargs["numInputs"]
        return y
    def logic_function(**kwargs):
        return " nand "
    return makeTagMap(makeEntityName, use_input_0, use_input_1, use_input_2, use_input_3, logic_function,\
            reset_name, test_reset)


def norPack():
    def makeEntityName(**kwargs):
        y = "registeredNOR"
        y += kwargs["numInputs"]
        return y
    def logic_function(**kwargs):
        return " nor "
    return makeTagMap(makeEntityName, use_input_0, use_input_1, use_input_2, use_input_3, logic_function,\
            reset_name, test_reset)


def xorPack():
    def makeEntityName(**kwargs):
        y = "registeredXOR"
        y += kwargs["numInputs"]
        return y
    def logic_function(**kwargs):
        return " xor "
    return makeTagMap(makeEntityName, use_input_0, use_input_1, use_input_2, use_input_3, logic_function,\
            reset_name, test_reset)


def glueNames(nameList, functionStr):
    return functionStr.join(nameList)


TWO_INPUT_INFIX_OPERATORS = ["nand", "nor"]


def wrapInSpaces(s):
    return " " + s + " "


def leftAssociateParens(s):
    """Takes this:
            x nand y nand z
    And turns it into this:
            ((x nand y) nand z)
    In other words, it provides a precedence order for infix operators that are only defined
    for 2 inputs."""
    los = s.split()
    if len(los) < 5:
        # 3 is the minimum length at which this function will not cause an error.
        # But 5 is the minimum length for which you would want to use this.
        return None
    y = []
    numParens = 0
    if los[1] in TWO_INPUT_INFIX_OPERATORS:
        y.append("(")
        numParens += 1
    y.append(los[0])
    y.append(wrapInSpaces(los[1]))
    youWantToUseThis = False
    for i in range(2, len(los)-1, 2):
        thisTok = los[i] # Even numbered token. Should always be an input name.
        nextTok = None
        if len(los)-1 > i:
            nextTok = los[i+1] # Odd numbered token. Should always be an infix operator.
        if nextTok is not None and nextTok in TWO_INPUT_INFIX_OPERATORS:
            y.append("(")
            numParens += 1
            youWantToUseThis = True
        y.append(thisTok)
        if nextTok is not None:
            y.append(wrapInSpaces(nextTok))
        else:
            break
    y.append(los[-1])
    y.extend([")"]*numParens)
    if not youWantToUseThis:
        return None
    return ''.join(y)


def populateTemplateOnce(los, onePack, numInputs, posReset):
    y = []
#    print(onePack)
    for line in los:
        appendLine = True
        for oneKey in onePack.keys():
            if oneKey in line:
                input_1 = numInputs > 1
                input_2 = numInputs > 2
                input_3 = numInputs > 3
                input_4 = numInputs > 4
                input_5 = numInputs > 5
                input_6 = numInputs > 6
                nameList = []
                numParens = 0
                infixOpDefFor2InputsOnly = False
                if oneKey == "<insert_logic_here>" and onePack["<insert_logic_here>"]() in [" nand "]:
                    infixOpDefFor2INputsOnly = True
                nameList.append("a0")
                if input_1:
                    if infixOpDefFor2InputsOnly:
                        nameList.append("(")
                        numParens += 1
                    nameList.append("a1")
                if input_2:
                    if infixOpDefFor2InputsOnly:
                        nameList.append("(")
                        numParens += 1
                    nameList.append("a2")    
                if input_3:
                    if infixOpDefFor2InputsOnly:
                        nameList.append("(")
                        numParens += 1
                    nameList.append("a3")    
                if input_4:
                    if infixOpDefFor2InputsOnly:
                        nameList.append("(")
                        numParens += 1
                    nameList.append("a4")    
                if input_5:
                    if infixOpDefFor2InputsOnly:
                        nameList.append("(")
                        numParens += 1
                    nameList.append("a5")    
                if input_6:
                    if infixOpDefFor2InputsOnly:
                        nameList.append("(")
                        numParens += 1
                    nameList.append("a6")    
                
                if oneKey == "<input_0_here>":
                    line = line.replace(oneKey, "a0 : in std_logic;")
                elif oneKey == "<input_1_here>" and input_1:
                    line = line.replace(oneKey, "a1 : in std_logic;")
                elif oneKey == "<input_2_here>" and input_2:
                    line = line.replace(oneKey, "a2 : in std_logic;")
                elif oneKey == "<input_3_here>" and input_3:
                    line = line.replace(oneKey, "a3 : in std_logic;")
                elif oneKey == "<input_4_here>" and input_3:
                    line = line.replace(oneKey, "a4 : in std_logic;")
                elif oneKey == "<input_5_here>" and input_3:
                    line = line.replace(oneKey, "a5 : in std_logic;")
                elif oneKey == "<input_6_here>" and input_3:
                    line = line.replace(oneKey, "a6 : in std_logic;")
                elif oneKey == "<insert_logic_here>":
                    logicStr = glueNames(nameList, onePack["<insert_logic_here>"]())
                    logicWithParens = leftAssociateParens(logicStr)
                    if logicWithParens is not None:
                        line = line.replace(oneKey, logicWithParens)
                    else:
                        line = line.replace(oneKey, logicStr)
                elif oneKey == "<reset_name_here>":
                    line = line.replace(oneKey, reset_name(posReset=posReset))
                elif oneKey == "<test_reset_here>":
                    line = line.replace(oneKey, test_reset(posReset=posReset))
                else:
                    line = line.replace(oneKey, onePack["<entity_name_here>"](numInputs=str(numInputs)))

                if oneKey == "<input_1_here>" and not input_1:
                    appendLine = False
                if oneKey == "<input_2_here>" and not input_2:
                    appendLine = False
                if oneKey == "<input_3_here>" and not input_3:
                    appendLine = False
                if oneKey == "<input_4_here>" and not input_4:
                    appendLine = False
                if oneKey == "<input_5_here>" and not input_5:
                    appendLine = False
                if oneKey == "<input_6_here>" and not input_6:
                    appendLine = False

        if appendLine:
            y.append(line)
    return y 


def main(input_los, posReset):
    y = []
    for pack in [andPack(), orPack(), nandPack(), norPack(), xorPack()]:
        for numInputs in range(1, 8):
            y.extend(populateTemplateOnce(input_los, pack, numInputs, posReset))
    return y
    

if __name__ == "__main__":
    newLib = main(inputLines, POS_RESET)
    with open(OUTPUT_FILE, 'w') as f:
        for line in newLib:
            f.write(line)


