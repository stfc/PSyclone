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

''' PSyclone transformation script to insert OpenMP for CPU
directives into Nemo code. Tested with ECMWF Nemo 4.0 code. '''

from utils import (insert_explicit_loop_parallelism, normalise_loops,
                   enhance_tree_information, add_profiling)
from psyclone.transformations import OMPLoopTrans

PROFILING_ENABLED = False


def trans(psy):
    ''' Add OpenMP Parallel and Do directives to all loops, including the
    implicit ones.

    :param psy: the PSy object which this script will transform.
    :type psy: :py:class:`psyclone.psyGen.PSy`

    :returns: the transformed PSy object.
    :rtype: :py:class:`psyclone.psyGen.PSy`

    '''
    omp_parallel_trans = None
    omp_loop_trans = OMPLoopTrans(omp_schedule="static")
    omp_loop_trans.omp_directive = "paralleldo"

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

        enhance_tree_information(invoke.schedule)

        if invoke.name in ("eos_rprof"):
            # TODO #1959: This subroutines make the ECMWF compilation fail
            # because it moves a statement function outside of the
            # specification part.
            print("Skipping normalisation for ", invoke.name)

        elif invoke.name in (
                "trc_oce_rgb",  # Produces incorrect results
                "removepoints"  # Compiler error: The shapes of the array
                                # expressions do not conform
                ):
            # TODO #1841: These subroutines have a bug in the
            # array-range-to-loop transformation.
            print("Skipping normalisation for ", invoke.name)
        else:
            normalise_loops(
                    invoke.schedule,
                    hoist_local_arrays=False,
                    convert_array_notation=True,
                    convert_range_loops=True,
                    hoist_expressions=False
            )

        insert_explicit_loop_parallelism(
                invoke.schedule,
                region_directive_trans=omp_parallel_trans,
                loop_directive_trans=omp_loop_trans,
                # Collapse may be useful in some architecture/compiler
                collapse=False,
        )

    return psy
