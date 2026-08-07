# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2024-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''A simple generic transformation script to apply omp parallel do.
'''

from psyclone.transformations import OMPParallelLoopTrans, TransformationError
from psyclone.psyir.nodes import FileContainer, Loop

# Set up some loop_type inference rules in order to reference useful domain
# loop constructs by name
Loop.set_loop_type_inference_rules({
        "lon": {"variable": "i"},
        "lat": {"variable": "j"},
})


def trans(psyir: FileContainer) -> None:
    ''' Transform a specific Schedule by making all loops
    over latitudes OpenMP parallel do.

    :param psyir: the PSyIR of the provided file.

    '''
    # Get the transformation we will apply
    ompt = OMPParallelLoopTrans()
    # Apply it to each loop over latitude containing a kernel
    for loop in psyir.walk(Loop):
        print("loop", loop.loop_type)
        if loop.loop_type == "lat":
            try:
                ompt.apply(loop)
            except TransformationError as error:
                print(str(error))
                continue
