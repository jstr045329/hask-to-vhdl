"""This function generates a pipelined minimum function"""
NUM_INPUTS = 64
INPUTS_PER_INTERMEDIATE = 4
TEMPLATE_FILE = "../VhdTemplates/MinFunctionTemplate.vhd"
RESULTS_FILE = "../VhdLibs/MinFunction_%04d.vhd"
from numpy import ceil


def tab(n=1):
    return "    " * n


def declareOneInput(n):
    return "i_distance_%04d : in integer := max_integer;" % n
    

def declareOneIntermediate(n):
    return "signal s_smallest_intermediate_%04d : integer;" % n


def resetOneIntermediate(n):
    return "s_smallest_intermediate_%04d <= max_integer;" % n


def oneIntermediateName(n):
    return "s_smallest_intermediate_%04d" % n


def declareOneVariable(n):
    return "variable v_smallest_intermediate_%04d : integer;" % n


def initializeFirstStageVariable(n):
    return tab(3) + "v_smallest_intermediate_%04d := max_integer;" % (n,)


def initializeUpperStageVariable(n_out):
    return initializeFirstStageVariable(n_out)
    

def assignIntermediate(n):
    return tab(3) + "s_smallest_intermediate_%04d <= v_smallest_intermediate_%04d;" % (n, n)


def applyIfFormatString(s, n):
    if "%04d" in s:
        return s % n
    return s


def makeOneIfStatement(n_in, n_out):
    return [ \
        """if i_distance_%04d < v_smallest_intermediate_%04d then""" % (n_in, n_out),
        tab(1) + """v_smallest_intermediate_%04d := i_distance_%04d;""" % (n_out, n_in),
        """end if;""",
        """""",
        ]


def makeOneIntermediateIfStatement(n_in, n_out):
    return [ \
        """if s_smallest_intermediate_%04d < v_smallest_intermediate_%04d then""" % (n_in, n_out),
        tab(1) + """v_smallest_intermediate_%04d := s_smallest_intermediate_%04d;""" % (n_out, n_in),
        """end if;""",
        """""",
        ]


def mapInputs2Intermediates():
    """Takes all the inputs and assigns their values to intermediate variables (which in turn 
    source intermediate signals) when an input is a candidate for new minimum."""
    input_decs = []
    signal_decs = []
    signal_resets = []
    variable_decs = []
    variable_initializations = []
    variable_assignments = []
    intermediate_assignments = []
    signal_assignments = []

    one_assignment_batch = []
    input_list = list(range(NUM_INPUTS))
    intermediate_num = 0
    signal_decs.append(declareOneIntermediate(intermediate_num))
    variable_decs.append(declareOneVariable(intermediate_num))
    variable_initializations.append(initializeFirstStageVariable(intermediate_num))
    signal_assignments.append(assignIntermediate(intermediate_num))
    signal_num = 0
    while len(input_list) > 0:
        if (signal_num % INPUTS_PER_INTERMEDIATE == 0) and (signal_num > 0):
            intermediate_num += 1
            signal_decs.append(declareOneIntermediate(intermediate_num))
            variable_decs.append(declareOneVariable(intermediate_num))
            variable_initializations.append(initializeFirstStageVariable(intermediate_num))
            signal_assignments.append(assignIntermediate(intermediate_num))
            signal_resets.append(resetOneIntermediate(intermediate_num))
        n = input_list[0]
        input_decs.append(declareOneInput(n))
        one_assignment_batch.extend(makeOneIfStatement(n, intermediate_num))
        del(input_list[0])
        signal_num += 1
    intermediate_assignments.append(one_assignment_batch)

    return input_decs, signal_decs, signal_resets, variable_decs, variable_initializations, intermediate_assignments, signal_assignments


def mapOneBatch(sig_num_list, startingIntermediate):
    intermediate_num = startingIntermediate
    signal_num = 0
    one_assignment_batch = []
    new_signal_decs = []
    new_variable_decs = []
    new_variable_initializations = []
    new_signal_assignments = []
    new_signal_resets = [] # todo: populate this
    new_intermediate_assignments = [] # todo: populate this
    new_signal_names = []
    
    # Map 0 -> 15 to 16 -> 19
    
    while len(sig_num_list) > 0:
        if (signal_num % INPUTS_PER_INTERMEDIATE == 0):
            new_signal_decs.append(declareOneIntermediate(intermediate_num))
            new_variable_decs.append(declareOneVariable(intermediate_num))
            new_variable_initializations.append(initializeUpperStageVariable(intermediate_num))
            
            # the following needs to be wrapped in an if statement?
            new_signal_assignments.append(assignIntermediate(intermediate_num))
            new_signal_resets.append(resetOneIntermediate(intermediate_num))
            # new_intermediate_assignments.append(
            new_signal_names.append(oneIntermediateName(intermediate_num))
        n = sig_num_list[0]
        
        # makeOneIntermediateIfStatement(n_in, n_out):
        one_assignment_batch.extend(makeOneIntermediateIfStatement(n, intermediate_num))
        signal_num += 1
        del(sig_num_list[0])
        if (signal_num % INPUTS_PER_INTERMEDIATE == 0):
            intermediate_num += 1
    new_intermediate_assignments.append(one_assignment_batch)
    return new_signal_decs, \
            new_signal_resets, \
            new_variable_decs, \
            new_variable_initializations, \
            new_intermediate_assignments, \
            new_signal_assignments, \
            intermediate_num, \
            new_signal_names


def main():
    input_decs, \
    signal_decs, \
    signal_resets, \
    variable_decs, \
    variable_initializations, \
    intermediate_assignments, \
    signal_assignments = mapInputs2Intermediates()
    
    latency = 1
    
    lower_idx = 0
    num_sigs = len(signal_assignments)
    #print("the very first num sigs: ", num_sigs)
    #for s in signal_assignments:
    #    print(s)
    upper_idx = lower_idx + num_sigs
    sig_num_list = list(range(lower_idx, upper_idx))
    num_new_sigs = 1000000000000
    startingIntermediate = len(signal_assignments)
    #print("First lower idx: ", lower_idx)
    #print("First upper idx: ", upper_idx)
    
    while num_new_sigs > 1:        
        
        new_signal_decs, \
        new_signal_resets, \
        new_variable_decs, \
        new_variable_initializations, \
        new_intermediate_assignments, \
        new_signal_assignments, \
        intermediate_num, \
        new_signal_names = mapOneBatch(sig_num_list, startingIntermediate)

        signal_decs.extend(new_signal_decs)
        signal_resets.extend(new_signal_resets)
        variable_decs.extend(new_variable_decs)
        variable_initializations.extend(new_variable_initializations)
        intermediate_assignments.extend(new_intermediate_assignments)
        signal_assignments.extend(new_signal_assignments)
        latency += 1
        
        # Set up for next iteration:
        lower_idx = upper_idx
        num_new_sigs = len(new_signal_decs)
        upper_idx += num_new_sigs
        sig_num_list = list(range(lower_idx, upper_idx))
        startingIntermediate += num_new_sigs
        #print(num_new_sigs, str(sig_num_list))


    # Read template:
    with open(TEMPLATE_FILE, 'r') as f:
        lines = f.readlines()

    # Interleave generated code with template lines:
    y = []
    for line in lines:
        if "<inputs_here>" in line:
            y.extend([tab(2) + s for s in input_decs])
        elif "<signals_here>" in line:
            y.extend(signal_decs)
        elif "<variables_here>" in line:
            y.extend([tab(1) + s for s in variable_decs])
        elif "<reset_intermediates_here>" in line:
            y.extend([tab(3) + s for s in signal_resets])
        elif "<business_logic_here>" in line:
            y.extend(variable_initializations)
            for one_list in intermediate_assignments:
                y.extend([tab(3) + s for s in one_list])
            y.extend(signal_assignments)
        elif "<latency_here>" in line:
            y.append("o_latency <= %d;" % latency)
        elif "<drive_output_here>" in line:
            y.append("o_min <= " + new_signal_names[-1] + ";")
        elif "<num_inputs_here>" in line:
            #print("found num inputs")
            #print("old line: ", line)
            line = line.replace("<num_inputs_here>", "%04d" % NUM_INPUTS)
            #print("new line: ", line)
            y.append(line)
        else:
            y.append(line)
    #print(TEMPLATE_FILE)

    # Write output lines to results file:
    with open(RESULTS_FILE % NUM_INPUTS, 'w') as f:
        for oneLine in y:
            if isinstance(oneLine, list):
                #print(oneLine)
                if len(oneLine) > 0:
                    for s in oneLine:
                        f.write(oneLine)
                        f.write("\n")
            else:
                f.write(oneLine)
                if oneLine[-1] != "\n":
                    f.write("\n")


if __name__ == "__main__":
    main()

