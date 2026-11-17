# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''A simple transformation script for the introduction of OpenMP with PSyclone.
In order to use it you must first install PSyclone. See README.md in the
top-level psyclone directory.

Once you have PSyclone installed, this script may be used by doing:

 >>> psyclone -s ./omp_levels_trans.py traldf_iso.F90

This should produce a lot of output, ending with generated
Fortran.
'''

from psyclone.psyGen import TransInfo
from psyclone.transformations import TransformationError
from psyclone.psyir.nodes import Loop

# Set up some loop_type inference rules in order to reference useful domain
# loop constructs by name
Loop.set_loop_type_inference_rules({
        "lon": {"variable": "ji"},
        "lat": {"variable": "jj"},
        "levels": {"variable": "jk"},
        "tracers": {"variable": "jt"}
})


def trans(psyir):
    ''' Transform a specific Schedule by making all loops
    over levels OpenMP parallel.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    # Get the transformation we will apply
    ompt = TransInfo().get_trans_name('OMPParallelLoopTrans')
    # Apply it to each loop over levels containing a kernel
    for loop in psyir.walk(Loop):
        if loop.loop_type == "levels":
            try:
                ompt.apply(loop)
            except TransformationError as error:
                print(str(error))
                continue
