# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''A transformation script that adds profiling information with user-provided
region names.

In order to use this script you must first install PSyclone. See
README.md in the top-level psyclone directory.

Once you have psyclone installed, this may be used by doing:

 $ psyclone -s ./profile_trans.py some_source_file.f90

'''

from psyclone.psyir.transformations import ProfileTrans
from psyclone.psyir.nodes import Loop

# Set up some loop_type inference rules in order to reference useful domain
# loop constructs by name
Loop.set_loop_type_inference_rules({"levels": {"variable": "jk"}})


def trans(psyir):
    ''' Adds profiling around loops over levels.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`
    '''

    p_trans = ProfileTrans()

    loops = psyir.walk(Loop)
    loop_counter = 0
    for loop in loops:
        if loop.loop_type == "levels":
            # We only put profiling around loops over levels
            loop_counter += 1
            p_trans.apply(loop, {"region_name": ("kloop", f"{loop_counter}")})

    print(psyir.view())
