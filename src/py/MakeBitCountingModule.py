# Takes the template for counting bits and populates reset name & reset level.
INPUT_FILENAME = "../VhdTemplates/CountBitsTemplate.vhd"
OUTPUT_FILENAME = "../VhdLibs/CountBits8.vhd"


USE_POSITIVE_RESET = True


def rst_name():
    if USE_POSITIVE_RESET:
        return "reset"
    return "reset_n"


def rst_level():
    if USE_POSITIVE_RESET:
        return "'1'"
    return "'0'"


def main():
    y = []
    with open(INPUT_FILENAME, 'r') as f:
        los = f.readlines()
    for line in los:
        s = line
        s = s.replace("<reset_name_here>", rst_name())
        s = s.replace("<reset_level_here>", rst_level())
        y.append(s)
    with open(OUTPUT_FILENAME, 'w') as f:
        for line in y:
            f.write(line)

if __name__ == "__main__":
    main()


