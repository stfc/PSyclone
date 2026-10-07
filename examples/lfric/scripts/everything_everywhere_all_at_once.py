# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''PSyclone transformation script for the LFRic API to apply all the
DistributedMemory, OpenMP coloring and serial transformations possible.

'''
from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.transformations import (
    LFRicRedundantComputationTrans)
from psyclone.lfric import LFRicHaloExchange, LFRicHaloExchangeStart
from psyclone.psyir.transformations import Matmul2CodeTrans, OMPParallelTrans
from psyclone.psyir.nodes import IntrinsicCall, KernelSchedule
from psyclone.psyGen import InvokeSchedule
from psyclone.transformations import (
    LFRicColourTrans, LFRicOMPLoopTrans, LFRicAsyncHaloExchangeTrans)
from psyclone.psyir.transformations import MoveTrans, TransformationError

ENABLE_REDUNDANT_COMPUTATION = True
ENABLE_ASYNC_HALOS = False  # TODO #2903: Async fails with FFSL
ENABLE_OMP_COLOURING = True
ENABLE_INTRINSIC_INLINING = True
# LFRicLoopFuseTrans and LFRicKernelConstTrans could also be included but there
# are some issues to overcome, e.g. TODO #2232


def trans(psyir):
    ''' Apply all possible LFRic transformations.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    rtrans = LFRicRedundantComputationTrans()
    ctrans = LFRicColourTrans()
    otrans = LFRicOMPLoopTrans()
    oregtrans = OMPParallelTrans()
    inline_trans = KernelModuleInlineTrans()
    matmul_trans = Matmul2CodeTrans()
    const = LFRicConstants()
    ahex_trans = LFRicAsyncHaloExchangeTrans()
    mtrans = MoveTrans()

    for subroutine in psyir.walk(InvokeSchedule):
        if ENABLE_REDUNDANT_COMPUTATION:
            # Make setval_* compute redundantly to the level 1 halo if it
            # is in its own loop and is not restricted to owned dofs only.
            for loop in subroutine.loops():
                if loop.iteration_space == "dof":
                    if len(loop.kernels()) == 1:
                        if loop.kernels()[0].name in ["setval_c", "setval_x"]:
                            rtrans.apply(loop, options={"depth": 1})

        if ENABLE_ASYNC_HALOS:
            # This transformation splits all synchronous halo exchanges
            for h_ex in subroutine.walk(LFRicHaloExchange):
                ahex_trans.apply(h_ex)

            # This transformation moves the start of the halo exchanges as
            # far as possible offering the potential for overlap between
            # communication and computation
            location_cursor = 0
            for ahex in subroutine.walk(LFRicHaloExchangeStart):
                if ahex.position <= location_cursor:
                    continue
                try:
                    mtrans.apply(ahex, subroutine.children[location_cursor])
                    location_cursor += 1
                except TransformationError:
                    pass

        if ENABLE_OMP_COLOURING:
            # Colour loops over cells unless they are on discontinuous
            # spaces or over dofs
            for loop in subroutine.loops():
                if loop.iteration_space.endswith("cell_column") \
                    and loop.field_space.orig_name \
                        not in const.VALID_DISCONTINUOUS_NAMES:
                    ctrans.apply(loop)

            # Add OpenMP to loops unless they are over colours or are null
            for loop in subroutine.loops():
                if loop.loop_type not in ["colours", "null"]:
                    oregtrans.apply(loop)
                    otrans.apply(loop, options={"reprod": True})

            # Transformations that modify kernel code will need to have the
            # kernels inlined first
            if ENABLE_INTRINSIC_INLINING:
                for kernel in subroutine.coded_kernels():
                    try:
                        inline_trans.apply(kernel)
                    except TransformationError:
                        pass

    # Then transform all the kernels inlined into the module
    for kschedule in psyir.walk(KernelSchedule):
        if ENABLE_INTRINSIC_INLINING:
            # Expand MATMUL intrinsic
            for icall in kschedule.walk(IntrinsicCall):
                if icall.intrinsic == IntrinsicCall.Intrinsic.MATMUL:
                    try:
                        matmul_trans.apply(icall)
                    except TransformationError:
                        pass
