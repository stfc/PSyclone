# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''A transformation script that adds a KERNELS region plus LOOP COLLAPSE
directives to the tracer-advection mini-app.  In order to use it you
must first install PSyclone. See README.md in the top-level psyclone
directory.

Once you have psyclone installed, this may be used by doing:

 $ psyclone -s ./kernels_trans.py some_source_file.f90

This should produce a lot of output, ending with generated
Fortran. Note that the Fortran source files provided to PSyclone must
have already been preprocessed (if required).

'''

from psyclone.psyir.nodes import Loop
from psyclone.psyir.transformations import ACCKernelsTrans, ACCLoopTrans
from psyclone.transformations import (
    ACCDataTrans, TransformationError)


Loop.set_loop_type_inference_rules({
        "lon": {"variable": "ji"},
        "lat": {"variable": "jj"},
        "levels": {"variable": "jk"},
        "tracers": {"variable": "jt"}
})

# Get the PSyclone transformations we will use
ACC_DATA_TRANS = ACCDataTrans()
ACC_KERNELS_TRANS = ACCKernelsTrans()
ACC_LOOP_TRANS = ACCLoopTrans()


def trans(psyir):
    '''A PSyclone-script compliant transformation function that is
    bespoke for the tracer-advection mini-app. It encloses the
    body of the iteration loop within a KERNELS region and then
    applies COLLAPSE(2) to every latitude-longitude loop nest
    within that.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`
    '''
    subroutine = psyir.children[0].children[0]

    # Find the outer, 'iteration' loop
    tloop = None
    for node in subroutine.children:
        if isinstance(node, Loop) and node.loop_type == "tracers":
            tloop = node
            break
    ACC_KERNELS_TRANS.apply(tloop.loop_body)

    loops = tloop.walk(Loop)
    for loop in loops:
        if loop.loop_type == "lat":
            try:
                ACC_LOOP_TRANS.apply(loop, options={"collapse": 2})
            except TransformationError:
                pass

    # Finally, enclose the whole of the 'iteration' loop within
    # a data region
    ACC_DATA_TRANS.apply(tloop)

    print(psyir.view())
