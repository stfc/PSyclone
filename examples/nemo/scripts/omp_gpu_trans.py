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

# import os
from utils import (
    insert_explicit_loop_parallelism, normalise_loops, add_profiling,
    enhance_tree_information, PASSTHROUGH_ISSUES, PARALLELISATION_ISSUES)
from psyclone.psyir.nodes import (
    Loop, Routine, Directive, Assignment, OMPAtomicDirective)
from psyclone.psyir.transformations import OMPTargetTrans
from psyclone.transformations import (
    OMPLoopTrans, OMPDeclareTargetTrans, TransformationError)

PROFILING_ENABLED = False

# List of all files that psyclone will skip processing
FILES_TO_SKIP = PASSTHROUGH_ISSUES + [
    "iom.f90",
    "iom_nf90.f90",
    "iom_def.f90",
    "timing.f90",   # Compiler error: Subscript, substring, or argument illegal
    "lbcnfd.f90",   # Illegal address during kernel execution
                    # - line 1012: lbc_nfd_dp
    "lib_mpp.f90",  # Compiler error: Illegal substring expression
    "prtctl.f90",   # Compiler error: Illegal substring expression
    "sbcblk.f90",   # Compiler error: Vector expression used where scalar
                    # expression required
]

OFFLOADING_ISSUES = [
    "trcrad.f90",  # Illegal address during kernel execution, unless the
                   # dimensions are small
    "tranxt.f90",  # String comparison not allowed inside omp teams
                   # (this worked fine with omp loop)
    "trazdf.f90",  # String comparison not allowed inside omp teams
    "crsdom.f90",  # String comparison not allowed inside omp teams
    "zdftke.f90",  # returned error 700 (CUDA_ERROR_ILLEGAL_ADDRESS):
                   # Illegal address during kernel execution
    "dynzdf.f90",  # returned error 700 (CUDA_ERROR_ILLEGAL_ADDRESS)
]

PRIVATISATION_ISSUES = [
    "ldftra.f90",  # Wrong runtime results
]


def trans(psyir):
    ''' Add OpenMP Target and Loop directives to all loops, including the
    implicit ones, to parallelise the code and execute it in an acceleration
    device.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    # if psyir.name not in (os.environ['ONLY_FILE'], "lib_fortran.f90"):
    #     return
    omp_target_trans = OMPTargetTrans()
    omp_gpu_loop_trans = OMPLoopTrans(omp_schedule="none")
    omp_gpu_loop_trans.omp_directive = "teamsdistributeparalleldo"
    omp_cpu_loop_trans = OMPLoopTrans(omp_schedule="static")
    omp_cpu_loop_trans.omp_directive = "paralleldo"

    # Many of the obs_ files have problems to be offloaded to the GPU
    if psyir.name.startswith("obs_"):
        return

    # ICE routines do not perform well on GPU, so we skip them
    if psyir.name.startswith("ice"):
        return

    for subroutine in psyir.walk(Routine):

        if PROFILING_ENABLED:
            add_profiling(subroutine.children)

        print(f"Adding OpenMP offloading to subroutine: {subroutine.name}")

        enhance_tree_information(subroutine)

        normalise_loops(
                subroutine,
                hoist_local_arrays=True,
                convert_array_notation=True,
                loopify_array_intrinsics=True,
                convert_range_loops=True,
                hoist_expressions=True
        )

        # Thes are functions that are called from inside parallel regions,
        # annotate them with 'omp declare target'
        if subroutine.name.lower().startswith("sign_"):
            OMPDeclareTargetTrans().apply(subroutine)
            print(f"Marked {subroutine.name} as GPU-enabled")
            # We continue parallelising inside the routine, but this could
            # change if the parallelisation directives added below are not
            # nestable, in that case we could add a 'continue' here

        # For now this is a special case for stpctl.f90 because it forces
        # loops to parallelise without many safety checks
        # TODO #2446: This needs to be generalised and probably be done
        # from inside the loop transformation when the race condition data
        # dependency is found.
        if psyir.name == "stpctl.f90":
            for loop in subroutine.walk(Loop):
                # Skip if an outer loop is already parallelised
                if loop.ancestor(Directive):
                    continue
                try:
                    omp_gpu_loop_trans.apply(loop, options={"force": True})
                except TransformationError:
                    continue
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

        if psyir.name not in PARALLELISATION_ISSUES + OFFLOADING_ISSUES:
            insert_explicit_loop_parallelism(
                    subroutine,
                    region_directive_trans=omp_target_trans,
                    loop_directive_trans=omp_gpu_loop_trans,
                    collapse=True,
                    privatise_arrays=(psyir.name not in PRIVATISATION_ISSUES)
            )
        elif psyir.name not in PARALLELISATION_ISSUES:
            # This have issues offloading, but we can still do OpenMP threading
            insert_explicit_loop_parallelism(
                    subroutine,
                    loop_directive_trans=omp_cpu_loop_trans,
                    privatise_arrays=(psyir.name not in PRIVATISATION_ISSUES)
            )
