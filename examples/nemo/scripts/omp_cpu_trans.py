#!/usr/bin/env python
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
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

import os
from utils import (
    insert_explicit_loop_parallelism, normalise_loops, add_profiling,
    enhance_tree_information, PARALLELISATION_ISSUES, NEMO_MODULES_TO_IMPORT,
    PRIVATISATION_ISSUES)
from psyclone.psyir.nodes import Routine
from psyclone.transformations import OMPLoopTrans

# Enable the insertion of profiling hooks during the transformation script
PROFILING_ENABLED = False

# List of all module names that PSyclone will chase during the creation of the
# PSyIR tree in order to use the symbol information from those modules
RESOLVE_IMPORTS = NEMO_MODULES_TO_IMPORT

# A environment variable can inform if this is targeting NEMOv4, in which case
# array privatisation is disabled.
NEMOV4 = os.environ.get('NEMOV4', False)

# List of all files that psyclone will skip processing
FILES_TO_SKIP = []

if PROFILING_ENABLED:
    # Fails with profiling enabled. issue #2723
    FILES_TO_SKIP.append("mppini.f90")


def trans(psyir):
    ''' Add OpenMP Parallel and Do directives to all loops, including the
    implicit ones.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    # If the environemnt has ONLY_FILE defined, only process that one file and
    # nothing else. This is useful for file-by-file exhaustive tests.
    only_do_file = os.environ.get('ONLY_FILE', False)
    if only_do_file and psyir.name != only_do_file:
        return

    omp_parallel_trans = None
    omp_loop_trans = OMPLoopTrans(omp_schedule="static")
    omp_loop_trans.omp_directive = "paralleldo"

    for subroutine in psyir.walk(Routine):
        print(f"Adding OpenMP threading to subroutine: {subroutine.name}")

        if PROFILING_ENABLED:
            add_profiling(subroutine.children)

        enhance_tree_information(subroutine)

        normalise_loops(
                subroutine,
                hoist_local_arrays=False,
                convert_array_notation=True,
                convert_range_loops=True,
                scalarise_loops=False,
                increase_array_ranks=False,
                hoist_expressions=False,
        )

        if psyir.name not in PARALLELISATION_ISSUES:
            insert_explicit_loop_parallelism(
                    subroutine,
                    region_directive_trans=omp_parallel_trans,
                    loop_directive_trans=omp_loop_trans,
                    collapse=False,
                    privatise_arrays=(not NEMOV4 and
                                      psyir.name not in PRIVATISATION_ISSUES)
            )
