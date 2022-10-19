#!/usr/bin/env python
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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

from utils import insert_explicit_loop_parallelism, normalise_loops, \
    enhance_tree_information, add_profiling
from psyclone.psyGen import TransInfo
from psyclone.psyir.nodes import Call, Loop
from psyclone.psyir.transformations import OMPTargetTrans
from psyclone.transformations import OMPDeclareTargetTrans

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

        # Has structure accesses that can not be offloaded
        if psy.name.startswith("psy_obs_"):
            print("Skipping", invoke.name)
            continue
        if psy.name in ("psy_diaobs_psy", "psy_stopar_psy", "psy_diawri_psy"):
            print("Skipping", invoke.name)
            continue

        # diaptr ptr_sf is considered and array instead of a function call
        # because it is an interface.
        # sbc_dyc is is considered and array instead of a function call because
        # it is imported.
        if psy.name in ("psy_diaptr_psy", "psy_sbccpl_psy"):
            print("Skipping", invoke.name)
            continue

        # TODO 1837: Has a TRIM intrinsic that can not be offloaded
        if invoke.name in ("cpl_oasis3_cpl_freq"):
            print("Skipping", invoke.name)
            continue

        # TODO #1841:
        # NVFORTRAN-S-0083-Vector expression used where scalar expression required
        if invoke.name in ("blk_oce"):
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
            if not invoke.schedule.walk((Loop, Call)):
                OMPDeclareTargetTrans().apply(invoke.schedule)
                continue

        insert_explicit_loop_parallelism(
                invoke.schedule,
                region_directive_trans=omp_target_trans,
                loop_directive_trans=omp_loop_trans,
                collapse=True,
                exclude_calls=psy.name != "psy_lib_fortran_psy",
        )

    return psy
