# flake8: noqa
# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2024-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
This example inlines all kernels, fuses loops together, applies OpenMP
parallelisation, and then tiles the fused loops.
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.psyir.transformations import MoveTrans, TransformationError
from psyclone.transformations import OMPLoopTrans
from psyclone.psyir.transformations import (InlineTrans, LoopFuseTrans,
                                            LoopTiling2DTrans,
                                            OMPParallelTrans)
from psyclone.psyir.nodes import (Assignment, Call, FileContainer, Loop,
                                  Reference)


def trans(psyir: FileContainer) -> None:
    """
    A complex program that inline all loops, moves the scalar assignment to
    the top so that all loops are next to each other. This allows loops to
    be fused then. After fusion, OpenMP parallelisation is added. Once
    parallelisation has been added, apply loop tiling.

    :param psyir: the PSyIR of the provided file.

    """

    # First inline all kernels. We first need to 'module inline' each
    # subroutine, i.e. copy the subroutine into the current module using
    # the KernelModuleInlineTrans. Once this is done, we can use the
    # inlining transformation:
    module_inline = KernelModuleInlineTrans()
    inline = InlineTrans()

    for call in psyir.walk(Call):
        if call.routine.name != #TODO: not for output_field
            print("Inlining", call.routine)
            TODO appy inlining, first module_inline, then inline

    # Study the output code - and find a way to add openmp - ideally
    # by using `openmp parallel` only once around all loops.
    # There is an easy solution, but a more complicated one will
    # allow you to fuse loop.
    # Alternatively/additionally, try to apply LoopTiling
