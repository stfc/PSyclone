#!/usr/bin/env python
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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

''' PSyclone transformation script showing the introduction of OpenMP for GPU
directives into Nemo code. '''

from psyclone.psyGen import TransInfo
from psyclone.psyir.nodes import (
    Call, Loop, Directive, Assignment, OMPAtomicDirective)
from psyclone.psyir.transformations import OMPTargetTrans
from psyclone.transformations import OMPDeclareTargetTrans

from utils import insert_explicit_loop_parallelism, normalise_loops, \
    enhance_tree_information, add_profiling

PROFILING_ENABLED = True


def trans(psy):
    ''' Add OpenMP Target and Loop directives to all loops, including the
    implicit ones, to parallelise the code and execute it in an acceleration
    device.

    :param psy: the PSy object which this script will transform.
    :type psy: :py:class:`psyclone.psyGen.PSy`
    :returns: the transformed PSy object.
    :rtype: :py:class:`psyclone.psyGen.PSy`

    '''
    omp_target_trans = OMPTargetTrans()
    omp_loop_trans = TransInfo().get_trans_name('OMPLoopTrans')
    omp_loop_trans.omp_directive = "loop"

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

        enhance_tree_information(invoke.schedule)

        normalise_loops(
                invoke.schedule,
                hoist_local_arrays=True,
                convert_array_notation=True,
                loopify_array_intrinsics=True,
                convert_range_loops=True,
                hoist_expressions=True
        )

        # For performance in lib_fortran, mark serial routines as GPU-enabled
        if psy.name == "psy_lib_fortran_psy":
            if not invoke.schedule.walk(Loop):
                calls = invoke.schedule.walk(Call)
                if all(call.is_available_on_device() for call in calls):
                    OMPDeclareTargetTrans().apply(invoke.schedule)
                    continue

        # For now this is a special case for stpctl.f90 because it forces
        # loops to parallelise without many safety checks
        # TODO #2446: This needs to be generalised and probably be done
        # from inside the loop transformation when the race condition data
        # dependency is found.
        if psy.name == "psy_stpctl_psy":
            for loop in invoke.schedule.walk(Loop):
                # Skip if an outer loop is already parallelised
                if loop.ancestor(Directive):
                    continue
                omp_loop_trans.apply(loop, options={"force": True})
                omp_target_trans.apply(loop.parent.parent)
                assigns = loop.walk(Assignment)
                if len(assigns) == 1 and assigns[0].lhs.symbol.name == "zmax":
                    stmt = assigns[0]
                    if OMPAtomicDirective.is_valid_atomic_statement(stmt):
                        parent = stmt.parent
                        atomic = OMPAtomicDirective()
                        atomic.children[0].addchild(stmt.detach())
                        parent.addchild(atomic)
            continue

        insert_explicit_loop_parallelism(
                invoke.schedule,
                region_directive_trans=omp_target_trans,
                loop_directive_trans=omp_loop_trans,
                # Collapse is necessary to give GPUs enough parallel items
                collapse=True
        )

    return psy
