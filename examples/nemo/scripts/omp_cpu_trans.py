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
    enhance_tree_information, PARALLELISATION_ISSUES,
    NEMO_MODULES_TO_IMPORT)
from psyclone.psyir.nodes import Routine
from psyclone.transformations import OMPLoopTrans

# Enable the insertion of profiling hooks during the transformation script
PROFILING_ENABLED = False

# Whether to chase the imported modules to improve symbol information (it can
# also be a list of module filenames to limit the chasing to only specific
# modules). This has to be used in combination with '-I' command flag in order
# to point to the module location directory. We also strongly recommend using
# the '--enable-cache' flag to reduce the performance overhead.
RESOLVE_IMPORTS = NEMO_MODULES_TO_IMPORT

# A environment variable can inform if this is targeting NEMOv4, in which case
# array privatisation is disabled.
NEMOV4 = os.environ.get('NEMOV4', False)

# By default, allow optimisations that may change the results, e.g. reductions
REPRODUCIBLE = os.environ.get('REPRODUCIBLE', False)

# List of all files that psyclone will skip processing
FILES_TO_SKIP = []
if not NEMOV4:
    # TODO #3112: These produce diverging run.stat results in gcc NEMOv5 BENCH
    FILES_TO_SKIP = [
        "dynhpg.f90",
        "dynspg_ts.f90",
        "sbcssm.f90",
        "tramle.f90",
        "trazdf.f90",
    ]

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

    # Parallelising this file currently causes a noticeable slowdown
    if psyir.name.startswith("icethd"):
        return

    # This file fails for gcc NEMOv5 BENCH
    if not NEMOV4 and psyir.name == "icedyn_rhg_evp.f90":
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
                # See issue #3022
                loopify_array_intrinsics=psyir.name != "getincom.f90",
                convert_range_loops=True,
                hoist_expressions=False,
                scalarise_loops=False
        )

        if psyir.name not in PARALLELISATION_ISSUES:
            insert_explicit_loop_parallelism(
                    subroutine,
                    region_directive_trans=omp_parallel_trans,
                    loop_directive_trans=omp_loop_trans,
                    collapse=False,
                    privatise_arrays=not NEMOV4,
                    enable_reductions=not REPRODUCIBLE,
            )
