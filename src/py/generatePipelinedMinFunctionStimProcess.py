"""This script generates a stim process for MinFunction_xxxx_tb.vhd.
Note this script doesn't generate the entire process - just the signal stim part."""
NUM_INPUTS = 64
NUM_EXPERIMENTS = 128
BITS_PER_NUMBER = 20
from secrets import randbits


def oneInputName(n):
    return "i_distance_%04d" % n


def makeUniqueNumbers():
    nums = set()
    for i in range(NUM_INPUTS):
        oneNum = randbits(BITS_PER_NUMBER)
        while oneNum in nums:
            oneNum = randbits(BITS_PER_NUMBER)
        nums.add(oneNum)
    return list(nums)


def main():
    y = []
    input_names = [oneInputName(n) for n in range(NUM_INPUTS)]
    for i in range(NUM_EXPERIMENTS):
        num_batch = makeUniqueNumbers()
        correct_answer = min(num_batch)
        for name, num in zip(input_names, num_batch):
            y.append(name + " <= " + str(num) + ";")
        y.append("correct_answer <= " + str(correct_answer) + ";")
        y.append("sync_wait_rising(clk, 10);")
        y.append("")
    for line in y:
        print(line)

if __name__ == "__main__":
    main()
