"""This script takes OneRegisteredGate.vhd and transforms it into a library of registered gates."""


if __name__ == "__main__":
    INPUT_FILE = "./OneRegisteredGate.vhd"
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
                nameList = ["a0"]
                if input_1:
                    nameList.append("a1")    
                if input_2:
                    nameList.append("a2")    
                if input_3:
                    nameList.append("a3")    
                if input_4:
                    nameList.append("a4")    
                if input_5:
                    nameList.append("a5")    
                if input_6:
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
                    line = line.replace(oneKey, glueNames(nameList, onePack["<insert_logic_here>"]()))
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
    
# TODO: PICK UP HERE: Make entity names correct

# TODO: Parse input for a random seed, & choose a default if none selected.

# TODO: Add optional random 

# TODO: Give user option to enable random trials, generate the same module 1000x,
#       and choose the one with the lowest latency.

# TODO: Give user the option to use random trials, and output a module with an 
#       exact latency, with a limit on # of trials, wall time, etc.  
#       This option makes it easier to achieve higher clock speeds. 

# TODO: Give user the option to prepend 1+ input dffs for each bit, intentionally
#       add 1+ delays to latency, and then prefer smaller numbers of inputs. 
#       This option makes it easier to achieve higher clock speeds. 

if __name__ == "__main__":
    newLib = main(inputLines, POS_RESET)
    with open(OUTPUT_FILE, 'w') as f:
        for line in newLib:
            f.write(line)


