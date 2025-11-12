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
import sys
from utils import (
    add_profiling, inline_calls, insert_explicit_loop_parallelism,
    normalise_loops, enhance_tree_information, NEMO_MODULES_TO_IMPORT)
from psyclone.psyir.nodes import Routine, Loop
from psyclone.psyir.transformations import (
    OMPTargetTrans, OMPDeclareTargetTrans)
from psyclone.transformations import (
    OMPLoopTrans, TransformationError)
from psyclone.transformations import (
    ACCParallelTrans, ACCLoopTrans, ACCRoutineTrans)


# This environment variable informs if this is targeting NEMOv4
NEMOV4 = os.environ.get('NEMOV4', False)

# This environment variable informs which parallelisation directives to use
# It supports acc_offloading, omp_offloading and omp_threading
# They can be combined, e.g PARALLEL_DIRECTIVES='omp_offloading+omp_threading'
PARALLEL_DIRECTIVES = os.environ.get('PARALLEL_DIRECTIVES', '')

# By default, allow optimisations that may change the results, e.g. reductions,
# offloading instrinsics without math_uniform, ...
REPRODUCIBLE = os.environ.get('REPRODUCIBLE', False)

# This environment variable informs if profiling hooks have to be inserted.
PROFILING_ENABLED = os.environ.get('ENABLE_PROFILING', False)

# By default, we don't do module inlining as it's still under development.
INLINING_ENABLED = os.environ.get('ENABLE_INLINING', False)

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
FILES_TO_SKIP = []

NEMOV5_EXCLUSIONS = [
    # Fail in gcc NEMOv5 BENCH
    "dynhpg.f90",
    "dynspg_ts.f90",
    "sbcssm.f90",
    "tramle.f90",
    "trazdf.f90",
    # Fail in nvfortran when enabling seaice
    "icefrm.f90",  # Has unsupported implicit symbol declaration
    "icerst.f90"
]

NEMOV4_EXCLUSIONS = [
    "dynspg_ts.f90",
]

if NEMOV4:
    FILES_TO_SKIP.extend(NEMOV4_EXCLUSIONS)
else:
    FILES_TO_SKIP.extend(NEMOV5_EXCLUSIONS)

SKIP_FOR_PERFORMANCE = [
    "iom.f90",
    "iom_nf90.f90",
    "iom_def.f90",
    "timing.f90",
    "lbclnk.f90",
    "histcom.f90",
]

# These files change the results from the baseline when psyclone adds
# parallelisation dirctives
PARALLELISATION_ISSUES = []
if not NEMOV4:
    PARALLELISATION_ISSUES.extend([
        "ldfc1d_c2d.f90",
        "tramle.f90",
        "traqsr.f90",
    ])

# These files change the results from the baseline when psyclone adds
# offloading dirctives
OFFLOADING_ISSUES = []
if not NEMOV4:
    OFFLOADING_ISSUES.extend([
        # Produces different output results
        "zdftke.f90",
        # The following issues only affect BENCH (because ice is enabled?)
        # Runtime Error: Illegal address during kernel execution
        "trcrad.f90",
        # nvhpc > 24.11 - Signal 11 issues
        "trcbbl.f90",
        "bdyice.f90",
        "sedfunc.f90",
        "stpmlf.f90",
        "trddyn.f90",
        "trczdf.f90",
        "trcice_pisces.f90",
        "dtatsd.f90",
        "trcatf.f90",
        "stp2d.f90",
    ])

if not NEMOV4 and "acc_offloading" in PARALLEL_DIRECTIVES:
    OFFLOADING_ISSUES.extend([
        # Fail in OpenACC ORCA2_ICE_PISCES
        "dynzdf.f90",
        "trabbl.f90",
        "trazdf.f90",
        "zdfsh2.f90",
    ])

ASYNC_ISSUES = [
    # Runtime Error: (CUDA_ERROR_LAUNCH_FAILED): Launch failed
    # (often invalid pointer dereference) in get_cstrgsurf
    "sbcclo.f90",
    "trcldf.f90",
    # Runtime Error: Illegal address during kernel execution with
    # asynchronicity.
    "zdfiwm.f90",
    "zdfsh2.f90",
    # Diverging results with asynchronicity
    "traadv_fct.f90",
    "bdy_oce.f90",
]


def select_transformations():
    '''
    Use the PARALLEL_DIRECTIVES global to select what specific transformations
    to apply to insert the desired directives.
    '''
    process_directives = PARALLEL_DIRECTIVES

    if 'omp_offloading' in process_directives:
        offload_region_trans = OMPTargetTrans()
        mark_for_gpu_trans = OMPDeclareTargetTrans()
        if NEMOV4:
            # TODO #2895: Explore why loop/teams loop diverge for NEMOv4
            gpu_loop_trans = OMPLoopTrans(omp_schedule="none")
            gpu_loop_trans.omp_directive = "loop"
        else:
            gpu_loop_trans = OMPLoopTrans(omp_schedule="none")
            gpu_loop_trans.omp_directive = "teamsloop"
        process_directives = process_directives.replace('omp_offloading', '')
    elif 'acc_offloading' in process_directives:
        offload_region_trans = ACCParallelTrans(default_present=False)
        mark_for_gpu_trans = ACCRoutineTrans()
        gpu_loop_trans = ACCLoopTrans()
        process_directives = process_directives.replace('acc_offloading', '')
    else:
        offload_region_trans = None
        mark_for_gpu_trans = None
        gpu_loop_trans = None

    if 'omp_threading' in process_directives:
        cpu_loop_trans = OMPLoopTrans(omp_schedule="static")
        cpu_loop_trans.omp_directive = "paralleldo"
        process_directives = process_directives.replace('omp_threading', '')
    else:
        cpu_loop_trans = None

    process_directives = process_directives.replace('+', '')
    if process_directives != '':
        sys.exit(f"Unkown PARALLEL_DIRECTIVES: {process_directives}")

    return (offload_region_trans, mark_for_gpu_trans,
            gpu_loop_trans, cpu_loop_trans)


def filter_files_by_name(name: str) -> bool:
    '''
    :returns: whether to transform a file with the given name. Contrary to
        FILES_TO_SKIP, this will still run the files through psyclone.
    '''
    # The two options below are useful for file-by-file exhaustive tests.
    # If the environemnt has ONLY_FILE defined, only process that one file and
    # known-good files that need a "declare target" inside.
    only_file = os.environ.get('ONLY_FILE', False)
    if only_file:
        files_to_do = [only_file]
        if "offloading" in PARALLEL_DIRECTIVES:
            files_to_do.extend(["lib_fortran.f90", "solfrac_mod.f90"])
        if name in files_to_do:
            return True
    # If the environemnt has ALL_BUT_FILE defined, process all files but
    # the one named file.
    all_but_file = os.environ.get('ALL_BUT_FILE', False)
    if all_but_file and name == all_but_file:
        return True

    if name in SKIP_FOR_PERFORMANCE:
        return True

    # Parallelising this file currently causes a noticeable slowdown
    # if name.startswith("icethd"):
    if not NEMOV4 and name.startswith("ice"):
        return True
    if name.startswith("icb"):
        return True

    # This file fails for gcc NEMOv5 BENCH
    if not NEMOV4 and name == "icedyn_rhg_evp.f90":
        return True

    # Many of the obs_ files have problems to be offloaded to the GPU
    if name.startswith("obs_"):
        return True

    return False


def trans(psyir):
    ''' Normalise and add directives to all possible loops, including the
    implicit ones.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    if filter_files_by_name(psyir.name):
        return

    (offload_region_trans, mark_for_gpu_trans, gpu_loop_trans,
     cpu_loop_trans) = select_transformations()

    disable_profiling_for = []
    enable_async = ASYNC_PARALLEL and psyir.name not in ASYNC_ISSUES

    for subroutine in psyir.walk(Routine):

        # Skip initialisation subroutines
        if (subroutine.name.endswith('_alloc') or
                subroutine.name.endswith('_init') or
                subroutine.name.startswith('init_') or
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
            mark_for_gpu_trans and
            (subroutine.name.lower().startswith("sign_")
             or subroutine.name.lower() == "solfrac"
             or (psyir.name == "sbc_phy.f90" and not subroutine.walk(Loop)))
        ):
            try:
                mark_for_gpu_trans.apply(subroutine)
                print(f"Marked {subroutine.name} as GPU-enabled")
            except TransformationError as err:
                print(err)
            # We continue parallelising inside the routine, but this could
            # change if the parallelisation directives added below are not
            # nestable, in that case we could add a 'continue' here
            disable_profiling_for.append(subroutine.name)

        elif (psyir.name not in PARALLELISATION_ISSUES + OFFLOADING_ISSUES
              and gpu_loop_trans):
            print(
                f"Adding offload directives to subroutine: {subroutine.name}")
            insert_explicit_loop_parallelism(
                    subroutine,
                    region_directive_trans=offload_region_trans,
                    loop_directive_trans=gpu_loop_trans,
                    collapse=True,
                    privatise_arrays=not NEMOV4,
                    enable_reductions=not REPRODUCIBLE,
                    uniform_intrinsics_only=REPRODUCIBLE,
                    asynchronous_parallelism=enable_async,
            )
        elif psyir.name not in PARALLELISATION_ISSUES and cpu_loop_trans:
            # These have issues offloading, but we can still do threading
            print(f"Adding OpenMP threading to subroutine: {subroutine.name}")
            insert_explicit_loop_parallelism(
                    subroutine,
                    loop_directive_trans=cpu_loop_trans,
                    privatise_arrays=not NEMOV4,
                    enable_reductions=not REPRODUCIBLE,
                    asynchronous_parallelism=enable_async,
            )

    # Iterate again and add profiling hooks when needed
    for subroutine in psyir.walk(Routine):
        if PROFILING_ENABLED and subroutine.name not in disable_profiling_for:
            print(f"Adding profiling hooks to subroutine: {subroutine.name}")
            add_profiling(subroutine.children)
