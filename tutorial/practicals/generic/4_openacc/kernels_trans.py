# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''A skeleton transformation script that acts as a starting point for
adding OpenACC KERNELS directives to NEMO style code.  In order to use it you
must first install PSyclone. See README.md in the top-level psyclone directory.

Once you have psyclone installed, this may be used by doing:

 $ psyclone -s ./kernels_trans.py some_source_file.f90

This should produce a lot of output, ending with generated
Fortran. Note that the Fortran source files provided to PSyclone must
have already been preprocessed (if required).

'''

from psyclone.psyir.nodes import Loop, Assignment
from psyclone.psyir.transformations import ACCKernelsTrans
from psyclone.transformations import TransformationError, ACCDataTrans


# Get the PSyclone transformations we will use
ACC_DATA_TRANS = ACCDataTrans()
ACC_KERNELS_TRANS = ACCKernelsTrans()

# loops of interest.
Loop.set_loop_type_inference_rules({"tracers": {"variable": "jt"}})

def trans(psyir):
    '''A PSyclone-script compliant transformation function.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`
    '''

    # Find the outer, 'iteration' loop
    tloop = None
    for node in psyir.walk(Loop, stop_type=Loop):
        if node.loop_type == "tracers":
            tloop = node
            break

    # Loop through the children of the loop body and transform those
    # that are over levels

    print(psyir.view())
