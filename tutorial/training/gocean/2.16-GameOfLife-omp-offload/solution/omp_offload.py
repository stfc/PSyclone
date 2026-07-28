# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
Python script intended to be passed to PSyclone via the -s option.
It adds OpenMP offload directives to all kernels.
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.gocean1p0 import GOKern
from psyclone.psyir.nodes import Directive, FileContainer, Loop, Routine
from psyclone.psyir.transformations import TransformationError, OMPTargetTrans
from psyclone.transformations import OMPLoopTrans
from psyclone.psyir.transformations import OMPDeclareTargetTrans
from fuse_loops import trans as fuse_trans


def trans(psyir: FileContainer) -> None:
    '''
    Take the supplied psyir object, apply kernel inlining and loop fusion,
    then add OpenMP offloading directives.

    :param psyir: the PSyIR layer to transform.

    '''

    declare_target = OMPDeclareTargetTrans()

    # Use existing fuse script to fuse all loops
    fuse_trans(psyir)

    # Module inline all kernels (so they can be modified)
    # Then add an OpenMP routine statement to each of them:
    module_inline = KernelModuleInlineTrans()
    for kern in psyir.walk(GOKern):
        module_inline.apply(kern)
        # Put a ``declare target`` directive inside each kernel
        try:
            declare_target.apply(kern)
        except TransformationError as err:
            print(f"Failed to annotate '{kern.name}' with "
                  f"GPU-enabled directive due to:\n"
                  f"{err.value}")

    loop_offloading = OMPLoopTrans(
        omp_directive="teamsdistributeparalleldo",
        omp_schedule="none")
    target_trans = OMPTargetTrans()

    for subroutine in psyir.walk(Routine):
        for loop in subroutine.walk(Loop):
            if loop.loop_type == "outer":
                loop_offloading.apply(loop)
                target_trans.apply(loop.ancestor(Directive))
