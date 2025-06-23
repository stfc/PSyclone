#!/usr/bin/env python
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2025, Science and Technology Facilities Council.
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

''' PSyclone transformation script showing the introduction of OpenACC loop
directives into Nemo code. '''

from utils import (
    insert_explicit_loop_parallelism, normalise_loops, add_profiling,
    enhance_tree_information, NOT_PERFORMANT, NEMO_MODULES_TO_IMPORT)
from psyclone.psyir.nodes import Routine
from psyclone.transformations import (
    ACCParallelTrans, ACCLoopTrans, ACCRoutineTrans)

# Enable the insertion of profiling hooks during the transformation script
PROFILING_ENABLED = True

# Whether to chase the imported modules to improve symbol information (it can
# also be a list of module filenames to limit the chasing to only specific
# modules). This has to be used in combination with '-I' command flag in order
# to point to the module location directory. We also strongly recommend using
# the '--enable-cache' flag to reduce the performance overhead.
RESOLVE_IMPORTS = NEMO_MODULES_TO_IMPORT

# List of all files that psyclone will skip processing
FILES_TO_SKIP = NOT_PERFORMANT


def trans(psyir):
    ''' Add OpenACC Parallel and Loop directives to all loops, including the
    implicit ones, to parallelise the code and execute it in an acceleration
    device.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    acc_region_trans = ACCParallelTrans(default_present=False)
    acc_loop_trans = ACCLoopTrans()

    # TODO #2317: Has structure accesses that can not be offloaded and has
    # a problematic range to loop expansion of (1:1)
    if psyir.name.startswith("obs_"):
        print("Skipping", psyir.name)
        return

    for subroutine in psyir.walk(Routine):
        print(f"Transforming subroutine: {subroutine.name}")

        if PROFILING_ENABLED:
            add_profiling(subroutine.children)

        # S-0074-Illegal number or type of arguments to ubound [and lbound]
        # - keyword argument array; and  NVFORTRAN-S-0082-Illegal substring
        # expression for variable filtide
        if subroutine.name in ("bdytide_init", "sbc_cpl_init"):
            print("Skipping", subroutine.name)
            continue

        # OpenACC fails in the following routines with the Compiler error:
        # Could not find allocated-variable index for symbol - xxx
        # This all happen on characters arrays, e.g. cd_nat
        if subroutine.name in ("lbc_nfd_2d_ptr", "lbc_nfd_3d_ptr",
                               "lbc_nfd_4d_ptr", "bdy_dyn", "dia_obs_init"):
            print("Skipping", subroutine.name)
            continue

        enhance_tree_information(subroutine)

        normalise_loops(
                subroutine,
                hoist_local_arrays=True,
                convert_array_notation=True,
                convert_range_loops=True,
                hoist_expressions=True
        )

        # These are functions that are called from inside parallel regions,
        # annotate them with 'acc routine'
        if subroutine.name.lower().startswith("sign_"):
            ACCRoutineTrans().apply(subroutine)
            print(f"Marked {subroutine.name} as GPU-enabled")
            continue

        insert_explicit_loop_parallelism(
            subroutine,
            region_directive_trans=acc_region_trans,
            loop_directive_trans=acc_loop_trans,
            # Collapse is necessary to give GPUs enough parallel items
            collapse=True,
        )
