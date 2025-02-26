# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2025, Science and Technology Facilities Council
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Author: S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Script to compare NEMO run.stat output files. '''

import sys
import os
import math

TOLERANCE = 1e-07


def is_float(x):
    ''' Check if the given value is a float. '''
    try:
        _ = float(x)
        return True
    except ValueError:
        return False


def main():
    ''' Compare the two provided NEMO run.stat output files, it checks that the
    values are equivalent (within a tolerance). '''

    # Parse input arguments
    if len(sys.argv) - 1 != 2:
        sys.exit(f"This script expects exactly 2 arguments with the two files "
                 f"to compare, but got: {sys.argv}")
    filename1 = sys.argv[1]
    filename2 = sys.argv[2]
    if not os.path.isfile(filename1):
        sys.exit(f"The first argument '{filename1}' must point to a file.")
    if not os.path.isfile(filename2):
        sys.exit(f"The second argument '{filename2}' must point to a file.")

    # Compare filename1 with filename2
    with open(filename1, 'r', encoding="utf-8") as file1, \
         open(filename2, 'r', encoding="utf-8") as file2:
        line_f1 = file1.readline()
        line_f2 = file2.readline()
        while line_f1 and line_f2:
            # Get all numbers (rhs of each :)
            list_f1 = [x.replace('D', 'E') for x in line_f1.split(' ')]
            list_f2 = [x.replace('D', 'E') for x in line_f2.split(' ')]
            values1 = [float(x) for x in list_f1 if is_float(x)]
            values2 = [float(x) for x in list_f2 if is_float(x)]
            if len(values1) != 6 or len(values2) != 6:
                sys.exit(f"One of the lines does not have the expected 6 "
                         f"output numbers:\n{line_f1}{line_f2}")
            for value1, value2 in zip(values1, values2):
                if not math.isclose(value1, value2, rel_tol=TOLERANCE):
                    sys.exit(f"The values are not equal:\n{line_f1}{line_f2}")

            # Get next lines
            line_f1 = file1.readline()
            line_f2 = file2.readline()

        if file1.readline() or file2.readline():
            sys.exit("The files have a different number of lines")

    sys.exit(0)  # Successful comparison


if __name__ == "__main__":
    main()
