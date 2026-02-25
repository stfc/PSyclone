#!/usr/bin/env python
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
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

import os
import pathlib
import sys
from typing import List, Union
from psyclone.psyir.nodes import (
    Assignment, IfBlock, Node, OMPDirective, OMPTargetDirective, ProfileNode,
    Routine, Schedule)
from psyclone.psyir.transformations import OMPTargetTrans, ProfileTrans
from psyclone.transformations import OMPLoopTrans, TransformationError

# Add examples/nemo/scripts to python path; needed to import utils.py
SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
NEMO_SCRIPTS_DIR = SCRIPT_DIR.parent / "scripts"
if str(NEMO_SCRIPTS_DIR) not in sys.path:
    sys.path.insert(0, str(NEMO_SCRIPTS_DIR))


PROFILING_ENABLED = os.environ.get("ENABLE_PROFILING", False)


def add_omp_region_profiling_markers(children: Union[List[Node], Schedule]):
    """Insert profiling markers around all top-level OpenMP directives.

    :param children: a Schedule or sibling nodes in the PSyIR to which to
        attempt to add profiling regions.
    """
    if children and isinstance(children, Schedule):
        # If we are given a Schedule, we look at its children.
        children = children.children
    # If we are given an empty list, we return.
    if not children:
        return
    # We do not want profiling calipers inside functions (such as the
    # PSyclone-generated comparison functions).
    parent_routine = children[0].ancestor(Routine)
    if parent_routine and parent_routine.return_symbol:
        return
    profile_trans = ProfileTrans()

    # Iterate over the children and wrap top-level OpenMP directives.
    for child in children[:]:
        if isinstance(child, OMPTargetDirective):
            # Only wrap top-level OpenMP target directives and not nested
            # directives or existing profiling markers.
            if (not child.ancestor(OMPDirective) and
                    not child.ancestor(ProfileNode)):
                try:
                    profile_trans.apply([child])
                except TransformationError as err:
                    print(
                        "Failed to add profiling around OMP target region "
                        f"in '{parent_routine.name}': {err}"
                    )
        if isinstance(child, IfBlock):
            # Recursively wrap any nested OpenMP kernels in if/else constructs.
            add_omp_region_profiling_markers(child.if_body)
            add_omp_region_profiling_markers(child.else_body)
        elif not isinstance(child, Assignment):
            add_omp_region_profiling_markers(child.children)


def trans(psyir):
    """Apply OpenMP offloading and insert profiling around target regions."""
    from utils import normalise_loops, insert_explicit_loop_parallelism

    omp_target_trans = OMPTargetTrans()
    omp_loop_trans = OMPLoopTrans(omp_schedule="none")
    omp_loop_trans.omp_directive = "teamsloop"

    for subroutine in psyir.walk(Routine):
        normalise_loops(
            subroutine,
            hoist_local_arrays=False,
            convert_array_notation=True,
            loopify_array_intrinsics=True,
            convert_range_loops=True,
            increase_array_ranks=False,
            hoist_expressions=True
        )
        insert_explicit_loop_parallelism(
            subroutine,
            region_directive_trans=omp_target_trans,
            loop_directive_trans=omp_loop_trans,
            collapse=True,
            enable_reductions=True
        )
        if PROFILING_ENABLED:
            add_omp_region_profiling_markers(subroutine.children)
