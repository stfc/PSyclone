#!/usr/bin/env python
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
# Authors: S. Siso, STFC Daresbury Lab

''' PSyclone transformation script showing the introduction of OpenACC loop
directives into Nemo code. '''

from utils import insert_explicit_loop_parallelism, normalise_loops, \
    enhance_tree_information, add_profiling
from psyclone.psyir.nodes import Call, Loop
from psyclone.transformations import ACCParallelTrans, ACCLoopTrans
from psyclone.transformations import ACCRoutineTrans

PROFILING_ENABLED = True


def trans(psy):
    ''' Add OpenACC Parallel and Loop directives to all loops, including the
    implicit ones, to parallelise the code and execute it in an acceleration
    device.

    :param psy: the PSy object which this script will transform.
    :type psy: :py:class:`psyclone.psyGen.PSy`
    :returns: the transformed PSy object.
    :rtype: :py:class:`psyclone.psyGen.PSy`

    '''
    acc_region_trans = ACCParallelTrans(default_present=False)
    acc_loop_trans = ACCLoopTrans()

    print(f"Invokes found in {psy.name}:")
    for invoke in psy.invokes.invoke_list:
        print(invoke.name)

        if PROFILING_ENABLED:
            add_profiling(invoke.schedule.children)

        # TODO #2317: Has structure accesses that can not be offloaded and has
        # a problematic range to loop expansion of (1:1)
        if psy.name.startswith("psy_obs_"):
            print("Skipping", invoke.name)
            continue

        # TODO #1841: These files have a bug in the array-range-to-loop
        # transformation. One leads to the following compiler error
        # NVFORTRAN-S-0083-Vector expression used where scalar expression
        # required, the other to an incorrect result.
        if invoke.name in ("trc_oce_rgb", ):
            print("Skipping", invoke.name)
            continue

        # This are functions with scalar bodies, we don't want to parallelise
        # them, but we could:
        # - Inine them
        # - Annotate them with 'omp declare target' and allow to call from gpus
        if invoke.name in ("q_sat", "sbc_dcy", "gamma_moist", "cd_neutral_10m",
                           "psi_h", "psi_m"):

            print("Skipping", invoke.name)
            continue

        # OpenACC fails in the following routines with the Compiler error:
        # Could not find allocated-variable index for symbol - xxx
        # This all happen on characters arrays, e.g. cd_nat
        if invoke.name in ("lbc_nfd_2d_ptr", "lbc_nfd_3d_ptr",
                           "lbc_nfd_4d_ptr", "bdy_dyn"):
            print("Skipping", invoke.name)
            continue

        enhance_tree_information(invoke.schedule)

        normalise_loops(
                invoke.schedule,
                hoist_local_arrays=True,
                convert_array_notation=True,
                convert_range_loops=True,
                hoist_expressions=True
        )

        # For performance in lib_fortran, mark serial routines as GPU-enabled
        if psy.name == "psy_lib_fortran_psy":
            if not invoke.schedule.walk(Loop):
                calls = invoke.schedule.walk(Call)
                if all(call.is_available_on_device() for call in calls):
                    # SIGN_ARRAY_1D has a CodeBlock because of a WHERE without
                    # array notation. (TODO #717)
                    ACCRoutineTrans().apply(invoke.schedule,
                                            options={"force": True})
                    continue

        insert_explicit_loop_parallelism(
            invoke.schedule,
            region_directive_trans=acc_region_trans,
            loop_directive_trans=acc_loop_trans,
            # Collapse is necessary to give GPUs enough parallel items
            collapse=True,
        )

    return psy
