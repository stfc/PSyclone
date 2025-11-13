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

''' PSyclone transformation script showing the introduction of OpenMP for GPU
directives into Nemo code. '''

import os
from utils import (
    add_profiling, inline_calls, insert_explicit_loop_parallelism,
    normalise_loops, enhance_tree_information, PARALLELISATION_ISSUES,
    NEMO_MODULES_TO_IMPORT)
from psyclone.psyir.nodes import Routine
from psyclone.psyir.transformations import (
    OMPTargetTrans, OMPDeclareTargetTrans)
from psyclone.transformations import (
    OMPLoopTrans, TransformationError)


# This environment variable informs if profiling hooks have to be inserted.
PROFILING_ENABLED = os.environ.get('ENABLE_PROFILING', False)

# By default, we don't do module inlining as it's still under development.
INLINING_ENABLED = os.environ.get('ENABLE_INLINING', False)

# By default, we allow all device intrinsics (not only the reproducible ones)
REPRODUCIBLE = os.environ.get('REPRODUCIBLE', False)

# This environment variable informs if this is targeting NEMOv4, in which case
# array privatisation is disabled and some more files excluded
NEMOV4 = os.environ.get('NEMOV4', False)

# This environment variable informs if we're enabling asynchronous
# parallelism.
ASYNC_PARALLEL = os.environ.get('ASYNC_PARALLEL', False)

# Whether to chase the imported modules to improve symbol information (it can
# also be a list of module filenames to limit the chasing to only specific
# modules). This has to be used in combination with '-I' command flag in order
# to point to the module location directory. We also strongly recommend using
# the '--enable-cache' flag to reduce the performance overhead.
RESOLVE_IMPORTS = NEMO_MODULES_TO_IMPORT

# List of all files that psyclone will skip processing
FILES_TO_SKIP = [
    "icefrm.f90",  # Has an unsupported implicit symbol declaration
]

NEMOV5_EXCLUSIONS = []

NEMOV4_EXCLUSIONS = [
    "dynspg_ts.f90",
    "tranxt.f90",
]

SKIP_FOR_PERFORMANCE = [
    "iom.f90",
    "iom_nf90.f90",
    "iom_def.f90",
    "timing.f90",
    "histcom.f90",
]

OFFLOADING_ISSUES = [
    # Produces different output results
    "zdftke.f90",
    # The following issues only affect BENCH (because ice is enabled?)
    # Runtime Error: Illegal address during kernel execution
    "trcrad.f90",
    # Signal 11 issues
    "trcbbl.f90",
    "bdyice.f90",
    "sedfunc.f90",
    "stpmlf.f90",
    "trddyn.f90",
    "trczdf.f90",
    "trcice_pisces.f90",
    "dtatsd.f90",
    # Runtime Error: Illegal address during kernel execution with
    # asynchronicity.
    "fldread.f90",
    "trcatf.f90",
    "zdfiwm.f90",
    "zdfsh2.f90",
]

if ASYNC_PARALLEL:
    # Runtime Error: (CUDA_ERROR_LAUNCH_FAILED): Launch failed
    # (often invalid pointer dereference) in get_cstrgsurf
    OFFLOADING_ISSUES.append("sbcclo.f90")
    OFFLOADING_ISSUES.append("trcldf.f90")


def trans(psyir):
    ''' Add OpenMP Target and Loop directives to all loops, including the
    implicit ones, to parallelise the code and execute it in an acceleration
    device.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    # If the environemnt has ONLY_FILE defined, only process that one file and
    # known-good files that need a "declare target" inside. This is useful for
    # file-by-file exhaustive tests.
    only_do_file = os.environ.get('ONLY_FILE', False)
    if only_do_file and psyir.name not in (only_do_file,
                                           "lib_fortran.f90",
                                           "solfrac_mod.f90"):
        return

    omp_target_trans = OMPTargetTrans()
    if NEMOV4:
        # TODO #2895: Explore why loop/teams loop diverge for NEMOv4
        omp_gpu_loop_trans = OMPLoopTrans(omp_schedule="none")
        omp_gpu_loop_trans.omp_directive = "loop"
    else:
        omp_gpu_loop_trans = OMPLoopTrans(omp_schedule="none")
        omp_gpu_loop_trans.omp_directive = "teamsloop"
    omp_cpu_loop_trans = OMPLoopTrans(omp_schedule="static")
    omp_cpu_loop_trans.omp_directive = "paralleldo"

    disable_profiling_for = []

    for subroutine in psyir.walk(Routine):

        # The exclusion below could be in the FILES_TO_SKIP global parameter,
        # but in this script, for testing purposes, we exclude them here so the
        # PSyclone frontend and backend are still tested and it also allows to
        # insert profiling hooks later on.
        if psyir.name in SKIP_FOR_PERFORMANCE:
            continue
        if NEMOV4 and psyir.name in NEMOV4_EXCLUSIONS:
            continue
        if not NEMOV4 and psyir.name in NEMOV5_EXCLUSIONS:
            continue
        # ICE routines do not perform well on GPU, so we skip them
        if psyir.name.startswith("ice"):
            continue
        # Many of the obs_ files have problems to be offloaded to the GPU
        if psyir.name.startswith("obs_"):
            continue
        # Skip initialisation and diagnostic subroutines
        if (subroutine.name.endswith('_alloc') or
                subroutine.name.endswith('_init') or
                subroutine.name.startswith('Agrif') or
                subroutine.name.startswith('dia_') or
                subroutine.name == 'dom_msk' or
                subroutine.name == 'dom_zgr' or
                subroutine.name == 'dom_ngb'):
            continue

        enhance_tree_information(subroutine)
        normalise_loops(
                subroutine,
                hoist_local_arrays=False,
                convert_array_notation=True,
                # See issue #3022
                loopify_array_intrinsics=psyir.name != "getincom.f90",
                convert_range_loops=True,
                increase_array_ranks=not NEMOV4,
                hoist_expressions=True
        )
        # Perform module-inlining of called routines.
        if INLINING_ENABLED:
            inline_calls(subroutine)

        # These are functions that are called from inside parallel regions,
        # annotate them with 'omp declare target'
        if (
            subroutine.name.lower().startswith("sign_")
            or subroutine.name.lower() == "solfrac"
            # Important for performance but causes SIGNAL 11 in some cases
            # or (psyir.name == "sbc_phy.f90" and not subroutine.walk(Loop))
        ):
            try:
                OMPDeclareTargetTrans().apply(subroutine)
                print(f"Marked {subroutine.name} as GPU-enabled")
            except TransformationError as err:
                print(err)
            # We continue parallelising inside the routine, but this could
            # change if the parallelisation directives added below are not
            # nestable, in that case we could add a 'continue' here
            disable_profiling_for.append(subroutine.name)

        if NEMOV4:
            # For nemo4 always offload but without privatisation
            print(f"Adding OpenMP offloading to subroutine: {subroutine.name}")
            insert_explicit_loop_parallelism(
                    subroutine,
                    region_directive_trans=omp_target_trans,
                    loop_directive_trans=omp_gpu_loop_trans,
                    collapse=True,
                    privatise_arrays=False,
                    asynchronous_parallelism=ASYNC_PARALLEL,
                    uniform_intrinsics_only=REPRODUCIBLE,
                    enable_reductions=not REPRODUCIBLE
            )
        elif psyir.name not in PARALLELISATION_ISSUES + OFFLOADING_ISSUES:
            print(f"Adding OpenMP offloading to subroutine: {subroutine.name}")
            insert_explicit_loop_parallelism(
                    subroutine,
                    region_directive_trans=omp_target_trans,
                    loop_directive_trans=omp_gpu_loop_trans,
                    collapse=True,
                    privatise_arrays=True,
                    asynchronous_parallelism=ASYNC_PARALLEL,
                    uniform_intrinsics_only=REPRODUCIBLE,
                    enable_reductions=not REPRODUCIBLE
            )
        elif psyir.name not in PARALLELISATION_ISSUES:
            # This have issues offloading, but we can still do OpenMP threading
            print(f"Adding OpenMP threading to subroutine: {subroutine.name}")
            # If asynchronous parallelism is enabled, these subroutines in
            # sbcclo.f90 fail if they're parallelised on the CPU.
            if (ASYNC_PARALLEL and subroutine.name in
                    ("get_cssrcsurf", "get_cstrgsurf")):
                continue
            insert_explicit_loop_parallelism(
                    subroutine,
                    loop_directive_trans=omp_cpu_loop_trans,
                    privatise_arrays=True,
                    asynchronous_parallelism=ASYNC_PARALLEL
            )

    # Iterate again and add profiling hooks when needed
    for subroutine in psyir.walk(Routine):
        if psyir.name in SKIP_FOR_PERFORMANCE:
            continue
        if PROFILING_ENABLED and subroutine.name not in disable_profiling_for:
            print(f"Adding profiling hooks to subroutine: {subroutine.name}")
            add_profiling(subroutine.children)
