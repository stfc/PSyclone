# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''A transformation script that adds profiling information.

In order to use this script you must first install PSyclone. See
README.md in the top-level psyclone directory.

Once you have psyclone installed, this may be used by doing:

 $ psyclone -s ./profile_trans.py some_source_file.f90

'''

from psyclone.psyir.transformations import ProfileTrans
from psyclone.psyir.nodes import Routine


def trans(psyir):
    ''' Adds profiling to each Routine.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`
    '''

    p_trans = ProfileTrans()

    for subroutine in psyir.walk(Routine):
        p_trans.apply(subroutine)

    # Display PSyIR tree
    print(psyir.view())
