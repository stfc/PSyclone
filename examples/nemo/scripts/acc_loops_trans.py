#!/usr/bin/env python
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
    insert_explicit_loop_parallelism, normalise_loops,
    enhance_tree_information, add_profiling)
from psyclone.psyir.nodes import Loop, Routine
from psyclone.transformations import (
    ACCParallelTrans, ACCLoopTrans, ACCRoutineTrans, TransformationError)

PROFILING_ENABLED = True

# Files that PSyclone could process but would reduce the performance.
NOT_PERFORMANT = [
    "bdydta.f90", "bdyvol.f90",
    "fldread.f90",
    "icbclv.f90", "icbthm.f90", "icbdia.f90", "icbini.f90",
    "icbstp.f90",
    "iom.f90", "iom_nf90.f90",
    "obs_grid.f90", "obs_averg_h2d.f90", "obs_profiles_def.f90",
    "obs_types.f90", "obs_read_prof.f90", "obs_write.f90",
    "tide_mod.f90", "zdfosm.f90",
]

# Files that we won't touch at all, either because PSyclone actually fails
# or because it produces incorrect Fortran.
NOT_WORKING = [
    # TODO #717 - array accessed inside WHERE does not use array notation
    "diurnal_bulk.f90",
    # TODO #1902: Excluded to avoid HoistLocalArraysTrans bug
    "mpp_ini.f90",
]

# List of all files that psyclone will skip processing
FILES_TO_SKIP = NOT_PERFORMANT + NOT_WORKING


def trans(psyir):
    ''' Add OpenACC Parallel and Loop directives to all loops, including the
    implicit ones, to parallelise the code and execute it in an acceleration
    device.

    :param psyir: the PSyIR representing the provided file.
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

        # TODO #1841: These files have a bug in the array-range-to-loop
        # transformation. One leads to the following compiler error
        # NVFORTRAN-S-0083-Vector expression used where scalar expression
        # required, the other to an incorrect result.
        if subroutine.name in ("trc_oce_rgb", ):
            print("Skipping", subroutine.name)
            continue

        # This are functions with scalar bodies, we don't want to parallelise
        # them, but we could:
        # - Inline them
        # - Annotate them with 'omp declare target' and allow to call from gpus
        if subroutine.name in ("q_sat", "sbc_dcy", "gamma_moist",
                               "cd_neutral_10m", "psi_h", "psi_m"):
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

        # For performance in lib_fortran, mark serial routines as GPU-enabled
        if psyir.name == "lib_fortran":
            if not subroutine.walk(Loop):
                try:
                    # We need the 'force' option.
                    # SIGN_ARRAY_1D has a CodeBlock because of a WHERE without
                    # array notation. (TODO #717)
                    ACCRoutineTrans().apply(subroutine,
                                            options={"force": True})
                except TransformationError as err:
                    print(err)

        insert_explicit_loop_parallelism(
            subroutine,
            region_directive_trans=acc_region_trans,
            loop_directive_trans=acc_loop_trans,
            # Collapse is necessary to give GPUs enough parallel items
            collapse=True,
        )
