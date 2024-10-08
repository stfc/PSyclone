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

from utils import (
    insert_explicit_loop_parallelism, normalise_loops, add_profiling,
    enhance_tree_information, NOT_PERFORMANT)
from psyclone.psyir.nodes import (
    Loop, Routine, Directive, Assignment, OMPAtomicDirective)
from psyclone.psyir.transformations import OMPTargetTrans
from psyclone.transformations import (
    OMPLoopTrans, OMPDeclareTargetTrans, TransformationError)

PROFILING_ENABLED = False

# List of all files that psyclone will skip processing
FILES_TO_SKIP = NOT_PERFORMANT + [
    "asminc.f90",
    "trosk.f90",    # TODO #1254
    "vremap.f90",   # Bulk assignment of a structure component
    "ldfslp.f90",   # Dependency analysis mistake? see Cray compiler comment
    "lib_mpp.f90",  # Compiler Error: Illegal substring expression
    "prtctl.f90",   # Compiler Error: Illegal substring expression
    "sbcblk.f90",   # Compiler Error: Vector expression used where scalar
                    # expression required
    "diadct.f90",   # Compiler Error: Wrong number of arguments in reshape
    "stpctl.f90",
    "lbcnfd.f90",
    "flread.f90",
    "sedini.f90",
    "diu_bulk.f90",  # Linking undefined reference
    "bdyini.f90",    # Linking undefined reference
    "trcrad.f90",
]


def trans(psyir):
    ''' Add OpenMP Target and Loop directives to all loops, including the
    implicit ones, to parallelise the code and execute it in an acceleration
    device.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''

    omp_target_trans = OMPTargetTrans()
    omp_loop_trans = OMPLoopTrans(omp_schedule="static")
    omp_loop_trans.omp_directive = "loop"

    # TODO #2317: Has structure accesses that can not be offloaded and has
    # a problematic range to loop expansion of (1:1)
    if psyir.name.startswith("obs_"):
        print("Skipping file", psyir.name)
        return

    for subroutine in psyir.walk(Routine):

        if PROFILING_ENABLED:
            add_profiling(subroutine.children)

        print(f"Transforming subroutine: {subroutine.name}")

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
                    omp_loop_trans.apply(loop, options={"force": True})
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

        insert_explicit_loop_parallelism(
                subroutine,
                region_directive_trans=omp_target_trans,
                loop_directive_trans=omp_loop_trans,
                # Collapse is necessary to give GPUs enough parallel items
                collapse=True,
        )
