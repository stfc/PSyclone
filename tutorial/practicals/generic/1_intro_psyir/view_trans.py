# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''A transformation script that simply prints the PSyIR of the input file
to stdout. No actual transformations are performed.

In order to use this script you must first install PSyclone. See
README.md in the top-level psyclone directory.

Once you have psyclone installed, this may be used by doing:

 $ psyclone -s ./view_trans.py some_source_file.f90 -o /dev/null

'''


def trans(psyir):
    ''' Prints the PSyIR of the input file to stdout.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`
    '''
    print(psyir.view())
