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
    insert_explicit_loop_parallelism, normalise_loops, add_profiling,
    enhance_tree_information, PARALLELISATION_ISSUES,
    NEMO_MODULES_TO_IMPORT, PRIVATISATION_ISSUES)
from psyclone.psyir.nodes import Loop, Routine
from psyclone.psyir.transformations import OMPTargetTrans
from psyclone.transformations import (
    OMPLoopTrans, OMPDeclareTargetTrans, TransformationError)

PROFILING_ENABLED = os.environ.get('ENABLE_PROFILING', False)

# List of all module names that PSyclone will chase during the creation of the
# PSyIR tree in order to use the symbol information from those modules
RESOLVE_IMPORTS = NEMO_MODULES_TO_IMPORT

# List of all files that psyclone will skip processing
FILES_TO_SKIP = []

NEMOV5_EXCLUSIONS = [
    "sbcblk.f90",   # Compiler error: Vector expression used where scalar
                    # expression required
    "sbcflx.f90",  # NEMOv4 sbc_dyc causes NVFORTRAN-S-0083-Vector expression
                    # used where scalar expression required
    "fldread.f90",  # Wrong runtime results
    "zdfddm.f90",  # Wrong results
    "zdfiwm.f90",  # Wrong results
    "zdfswm.f90",  # fort2 terminated by signal 11
    # The files below fail with Metoffice ORCA2
    "domhgr.f90",
    "domutl.f90",
    "domzgr.f90",
    "icbutl.f90",
    "sbcrnf.f90",
    "sbcssm.f90",
    "stp2d.f90",
    "stprk3.f90",
    "stprk3_stg.f90",
    "trasqr.f90",
]

NEMOV4_EXCLUSIONS = [
    # TODO 2895: Maybe because NEMOv4 is compiled with aggressive opt flags?
    # "dynvor.f90",
    "tranxt.f90",
    "trabbl.f90",
    "trabbc.f90",
    "step.f90",
    "dynldf.f90",  # fails with PROFILING_ENABLED
    "fldread.f90",
]

SKIP_FOR_PERFORMANCE = [
    "iom.f90",
    "iom_nf90.f90",
    "iom_def.f90",
    "timing.f90",
    "prtctl.f90",
    "trazdf.f90",
    "dynzdf.f90",
]

DONT_HOIST = [
    # Incorrect hoisting
    "lbcnfd.f90",
]

OFFLOADING_ISSUES = [
    "trcrad.f90",  # Illegal address during kernel execution, unless the
                   # dimensions are small
    "traatf_qco.f90",  # Runtime: Failed to find device function (BENCH)
    "lbclnk.f90",  # Improve performance until #2751
    "traqsr.f90",
    "ldftra.f90",  # Wrong runtime results
    "geo2ocean.f90",  # Uses MATH function calls (EXCLUDE FOR TESTING #2856)
    "zdftke.f90",  # Uses MATH function calls (EXCLUDE FOR TESTING #2856)
]

# A environment variable can inform if this is targeting NEMOv4, in which case
# array privatisation is disabled and some more files excluded
NEMOV4 = os.environ.get('NEMOV4', False)


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

    # The exclusion below could be in the FILES_TO_SKIP global parameter, but
    # in this script, for testing purposes, we exclude them here so PSyclone
    # frontend and backend are still tested for this files (making passthrough
    # redundant). It also allows us to insert profiling hooks.
    if psyir.name in SKIP_FOR_PERFORMANCE:
        for subroutine in psyir.walk(Routine):
            print(f"Adding profiling hooks to subroutine: {subroutine.name}")
            add_profiling(subroutine.children)
        return
    if NEMOV4 and psyir.name in NEMOV4_EXCLUSIONS:
        for subroutine in psyir.walk(Routine):
            print(f"Adding profiling hooks to subroutine: {subroutine.name}")
            add_profiling(subroutine.children)
        return
    if not NEMOV4 and psyir.name in NEMOV5_EXCLUSIONS:
        for subroutine in psyir.walk(Routine):
            print(f"Adding profiling hooks to subroutine: {subroutine.name}")
            add_profiling(subroutine.children)
        return

    # ICE routines do not perform well on GPU, so we skip them
    if psyir.name.startswith("ice"):
        return

    # Many of the obs_ files have problems to be offloaded to the GPU
    if psyir.name.startswith("obs_"):
        return

    for subroutine in psyir.walk(Routine):

        # Skip things from the initialisation
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
                hoist_local_arrays=True,
                convert_array_notation=True,
                loopify_array_intrinsics=True,
                convert_range_loops=True,
                hoist_expressions=(psyir.name not in DONT_HOIST)
        )

        # These are functions that are called from inside parallel regions,
        # annotate them with 'omp declare target'
        if (
            subroutine.name.lower().startswith("sign_") or
            subroutine.name.lower() == "solfrac" or
            (psyir.name == "sbc_phy.f90" and not subroutine.walk(Loop))
        ):
            try:
                OMPDeclareTargetTrans().apply(subroutine)
                print(f"Marked {subroutine.name} as GPU-enabled")
            except TransformationError as err:
                print(err)
            # We continue parallelising inside the routine, but this could
            # change if the parallelisation directives added below are not
            # nestable, in that case we could add a 'continue' here
        elif PROFILING_ENABLED:
            # We annotate the rest with profiling hooks if requested
            print(f"Adding profiling hooks to subroutine: {subroutine.name}")
            add_profiling(subroutine.children)

        if NEMOV4 or psyir.name not in PARALLELISATION_ISSUES + OFFLOADING_ISSUES:
            print(f"Adding OpenMP offloading to subroutine: {subroutine.name}")
            insert_explicit_loop_parallelism(
                    subroutine,
                    region_directive_trans=omp_target_trans,
                    loop_directive_trans=omp_gpu_loop_trans,
                    collapse=True,
                    privatise_arrays=(psyir.name not in PRIVATISATION_ISSUES)
            )
        elif psyir.name not in PARALLELISATION_ISSUES:
            # This have issues offloading, but we can still do OpenMP threading
            print(f"Adding OpenMP threading to subroutine: {subroutine.name}")
            insert_explicit_loop_parallelism(
                    subroutine,
                    loop_directive_trans=omp_cpu_loop_trans,
                    privatise_arrays=(not NEMOV4 and
                                      psyir.name not in PRIVATISATION_ISSUES)
            )
