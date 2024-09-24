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

from utils import (
    insert_explicit_loop_parallelism, normalise_loops, add_profiling,
    enhance_tree_information, NOT_PERFORMANT, NOT_WORKING)
from psyclone.psyir.nodes import Routine
from psyclone.transformations import OMPLoopTrans

PROFILING_ENABLED = False

# List of all files that psyclone will skip processing (the NOT_PERFORMANT
# list also has files that fail for NEMOv5)
FILES_TO_SKIP = NOT_PERFORMANT + NOT_WORKING + [
    "lbclnk.f90",  # TODO #2685: effective shape bug
    "asminc.f90",
    "trosk.f90",  # TODO #1254
    "vremap.f90",  # Bulk assignment of a structure component
    "ldfslp.f90",  # Dependency analysis mistake? see Cray compiler comment
]


def trans(psyir):
    ''' Add OpenMP Parallel and Do directives to all loops, including the
    implicit ones.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    omp_parallel_trans = None
    omp_loop_trans = OMPLoopTrans(omp_schedule="static")
    omp_loop_trans.omp_directive = "paralleldo"

    # TODO #2317: Has structure accesses that can not be offloaded and has
    # a problematic range to loop expansion of (1:1)
    if psyir.name.startswith("obs_"):
        print("Skipping file", psyir.name)
        return

    for subroutine in psyir.walk(Routine):
        print(f"Adding OpenMP threading to subroutine: {subroutine.name}")

        if PROFILING_ENABLED:
            add_profiling(subroutine.children)

        enhance_tree_information(subroutine)

        if subroutine.name in ("eos_rprof", "load_nml", "prt_ctl_write_sum",
                               "sbc_blk"):
            # TODO #1959: 'eos_rprof' make the ECMWF compilation fail
            # because it moves a statement function outside of the
            # specification part.
            # The rest are due to Subroutine wrongly parsed as Arrays?
            print("Skipping normalisation for ", subroutine.name)

        else:
            normalise_loops(
                    subroutine,
                    hoist_local_arrays=False,
                    convert_array_notation=True,
                    convert_range_loops=True,
                    hoist_expressions=False
            )

        insert_explicit_loop_parallelism(
                subroutine,
                region_directive_trans=omp_parallel_trans,
                loop_directive_trans=omp_loop_trans,
                # Collapse may be useful in some architecture/compiler
                collapse=False,
                privatise_arrays=(psyir.name in ["zdftke.f90", ])
        )
