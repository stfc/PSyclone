# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Authors: R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#          I. Kavcic, Met Office
#          J. Henrichs, Bureau of Meteorology
# Modified: O. Brunt, Met Office

'''PSyclone transformation script for the LFRic API to apply all the
DistibutedMemory, OpenMP coloring and serial transformations possible.

'''
from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.domain.lfric import LFRicConstants
from psyclone.dynamo0p3 import LFRicHaloExchange, LFRicHaloExchangeStart
from psyclone.psyir.transformations import Matmul2CodeTrans
from psyclone.psyir.nodes import IntrinsicCall, Container, KernelSchedule
from psyclone.transformations import Dynamo0p3ColourTrans, \
                                     Dynamo0p3OMPLoopTrans, \
                                     OMPParallelTrans, \
                                     Dynamo0p3RedundantComputationTrans, \
                                     Dynamo0p3AsyncHaloExchangeTrans, \
                                     MoveTrans, \
                                     TransformationError

ENABLE_REDUNDANT_COMPUTATION = True
ENABLE_ASYNC_HALOS = True
ENABLE_OMP_COLOURING = True
ENABLE_INTRINSIC_INLINING = True
# LFRicLoopFuseTrans and DynKernelConstTrans could also be included but there
# are some issues to overcome, e.g. TODO #2232


def trans(psy):
    ''' Apply all possible LFRic transformations. '''
    rtrans = Dynamo0p3RedundantComputationTrans()
    ctrans = Dynamo0p3ColourTrans()
    otrans = Dynamo0p3OMPLoopTrans()
    oregtrans = OMPParallelTrans()
    inline_trans = KernelModuleInlineTrans()
    matmul_trans = Matmul2CodeTrans()
    const = LFRicConstants()
    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    mtrans = MoveTrans()

    # Loop over all of the Invokes in the PSy object
    for invoke in psy.invokes.invoke_list:
        schedule = invoke.schedule

        if ENABLE_REDUNDANT_COMPUTATION:
            # Make setval_* compute redundantly to the level 1 halo if it
            # is in its own loop
            for loop in schedule.loops():
                if loop.iteration_space == "dof":
                    if len(loop.kernels()) == 1:
                        if loop.kernels()[0].name in ["setval_c", "setval_x"]:
                            rtrans.apply(loop, options={"depth": 1})

        if ENABLE_ASYNC_HALOS:
            # This transformation splits all synchronous halo exchanges
            for h_ex in schedule.walk(LFRicHaloExchange):
                ahex_trans.apply(h_ex)

            # This transformation moves the start of the halo exchanges as
            # far as possible offering the potential for overlap between
            # communication and computation
            location_cursor = 0
            for ahex in schedule.walk(LFRicHaloExchangeStart):
                if ahex.position <= location_cursor:
                    continue
                try:
                    mtrans.apply(ahex, schedule.children[location_cursor])
                    location_cursor += 1
                except TransformationError:
                    pass

        if ENABLE_OMP_COLOURING:
            # Colour loops over cells unless they are on discontinuous
            # spaces or over dofs
            for loop in schedule.loops():
                if loop.iteration_space == "cell_column" \
                    and loop.field_space.orig_name \
                        not in const.VALID_DISCONTINUOUS_NAMES:
                    ctrans.apply(loop)

            # Add OpenMP to loops unless they are over colours or are null
            for loop in schedule.loops():
                if loop.loop_type not in ["colours", "null"]:
                    oregtrans.apply(loop)
                    otrans.apply(loop, options={"reprod": True})

        # Transformations that modify kernel code will need to have the
        # kernels inlined first
        if ENABLE_INTRINSIC_INLINING:
            for kernel in schedule.coded_kernels():
                try:
                    inline_trans.apply(kernel)
                except TransformationError:
                    pass

    # Then transform all the kernels inlined into the module
    if psy.invokes.invoke_list:
        root = psy.invokes.invoke_list[0].schedule.ancestor(Container)
        for kschedule in root.walk(KernelSchedule):
            if ENABLE_INTRINSIC_INLINING:
                # Expand MATMUL intrinsic
                for icall in kschedule.walk(IntrinsicCall):
                    if icall.intrinsic == IntrinsicCall.Intrinsic.MATMUL:
                        try:
                            matmul_trans.apply(icall)
                        except TransformationError:
                            pass

    return psy
