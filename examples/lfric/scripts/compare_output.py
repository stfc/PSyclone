# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Script to compare LFRic output files. '''

import sys
import os

TOLERANCE_DIGITS = 8


def main():
    ''' Compare the two provided LFRic output files, it checks that the
    checksum values are equivalent (within a tolerance). '''

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
            value1 = line_f1.split('=')[-1][:TOLERANCE_DIGITS]
            value2 = line_f2.split('=')[-1][:TOLERANCE_DIGITS]
            if value1 != value2:
                sys.exit(f"The values are not equal:\n{line_f1}{line_f2}")

            # Get next lines
            line_f1 = file1.readline()
            line_f2 = file2.readline()

        if file1.readline() or file2.readline():
            sys.exit("The files have a different number of lines")

    sys.exit(0)  # Successful comparison


if __name__ == "__main__":
    main()
