
''' Script to compare the output files of LFRic '''

import sys
import os
import math


def main():
    ''' Compare the two provided LFRic output files. '''

    # Parse input arguments
    if len(sys.argv) - 1 != 2:
        sys.exit("This script expects exactly 2 arguments with the files to compare.")
    filename1 = sys.argv[1]
    filename2 = sys.argv[2]
    if not os.path.isfile(filename1):
        sys.exit(f"The first argument '{filename1}' must point to a file.")
    if not os.path.isfile(filename2):
        sys.exit(f"The second argument '{filename2}' must point to a file.")

    # Compare filename1 with filename2
    with open(filename1, 'r') as file1, open(filename2, 'r') as file2:
        line_f1 = file1.readline()
        line_f2 = file2.readline()
        while line_f1 and line_f2:
            if "Conservation" in line_f1:  # or "Residual" in line_f1 or "Min/max" in line_f1:
                print(f"Comparing:\n{line_f1}{line_f2}")
                value_f1 = float(line_f1.split()[-1])
                value_f2 = float(line_f2.split()[-1])
                if not math.isclose(value_f1, value_f2, rel_tol=1e-04):
                    sys.exit("The values are not equal")

            if " timestep" in line_f1:
                print(line_f1)
            line_f1 = file1.readline()
            line_f2 = file2.readline()

        if file1.readline() or file2.readline():
            sys.exit("The files have a different number of lines")

    sys.exit(0) # Successful comparison

if __name__ == "__main__":
    main()
