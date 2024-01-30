# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council
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

''' Script to compare LFRic output files. '''

import sys
import os
import math

TOLERANCE = 1e-04


def main():
    ''' Compare the two provided LFRic output files, it checks that the
    Conservation and Residual values are equivalent (within a tolerance). '''

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
            if (
                "Conservation" in line_f1
                or "Residual" in line_f1
            ):
                value_f1 = float(line_f1.split()[-1])
                value_f2 = float(line_f2.split()[-1])
                if not math.isclose(value_f1, value_f2, rel_tol=TOLERANCE):
                    sys.exit(f"The values are not equal:\n{line_f1}{line_f2}")

            # Get next lines
            line_f1 = file1.readline()
            line_f2 = file2.readline()

        if file1.readline() or file2.readline():
            sys.exit("The files have a different number of lines")

    sys.exit(0)  # Successful comparison


if __name__ == "__main__":
    main()
