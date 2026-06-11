# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2026, Science and Technology Facilities Council.
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
# Authors: A. R. Porter, STFC Daresbury Lab
#          R. W. Ford, STFC Daresbury Lab
#          S. Siso, STFC Daresbury Lab
#          L. Mosimann, NVIDIA.

'''PSyclone transformation script for LFRic to apply GPU offloading directives.
Also adds redundant computation to the level-1 halo for setval_* generically.

'''
import os
import sys
from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.lfric_builtins import LFRicBuiltIn
from psyclone.domain.lfric.transformations import (
    LFRicRedundantComputationTrans)
from psyclone.psyir.nodes import (
    Call, Directive, IntrinsicCall, Loop, Routine, Schedule)
from psyclone.psyir.transformations import (
    ACCKernelsTrans, Matmul2CodeTrans, OMPTargetTrans, TransformationError,
    OMPDeclareTargetTrans, OMPParallelTrans, InlineTrans)
from psyclone.transformations import (
    LFRicColourTrans, LFRicOMPLoopTrans,
    ACCParallelTrans, ACCRoutineTrans,
    OMPLoopTrans)
from psyclone.psyir.transformations import ACCLoopTrans


# Names of any Algorithm module that we won't add any GPU offloading
ALG_EXCLUSIONS = ('diagnostic_', )

# We won't attempt to inline calls to routines with names that contain
# these strings (because they're not computationally important).
INLINE_EXCLUSIONS = ["abort", "logging"]

OFFLOAD_DIRECTIVES = os.getenv('LFRIC_OFFLOAD_DIRECTIVES', "none")

# Ideally this script would not need the list of manually annotated symbols if
# resolving the imports retrieved the information, but currently:
# 1) The lfric build system does not have this in the working directory at
# psycloning-time, or they still have preprocessor macros.
# 2) Psyclone needs to parse comments/directive and understand them
RESOLVE_IMPORTS = ['constants_mod', 'finite_element_config_mod']
# Alternatively, we have to list by name every symbol that is GPU-ready
MANUALLY_ANNOTATED = [
    # Read-only Globals
    'geometry', 'topology', 'geometry_spherical', 'geometry_planar',
    'scaled_radius',
    # Constants
    'cpv', 'eps', 'dl_type_latitude', 'lf', 'cl', 'recip_epsilon', 'dl_type',
    'large_real_positive', 'eps_r_tran', 'ci', 'lv0', 'small_r_tran', 'pi',
    'rv', 'lv',
    # Functions/subroutines
    'chi2llr', 'chi2xyz', 'chi2abr', 'xyz2ll', 'xyz2llr', 'alphabetar2xyz',
    'coord_system', 'coord_system_xyz', 'cart2sphere_vector',
    'cart2sphere_scalar',
    'coordinate_jacobian', 'coordinate_jacobian_inverse',
    'pointwise_coordinate_jacobian', 'pointwise_coordinate_jacobian_inverse',

    'cross_product',
    'matrix_invert_lu',
    'crosses_panel_edge', 'crosses_rotated_panel_edge',
    'panel_neighbour', 'rotated_panel_neighbour',
    'qsaturation', 'interpolate_u_to_x',
    'rotation_vector_fplane', 'vert_vector_sphere', 'rotation_vector_sphere',

    # Subroutines with GPU annotation but that cause runtime faliures
    # 'native_jacobian',  # Runtime invalid address access
    # 'rehabilitate',     # Different results
]

def _replace_matmuls(sched: Schedule):
    '''
    Attempts to replace all MATMUL intrinsic calls with inline
    code.

    :param sched: schedule to transform.

    '''
    matrans = Matmul2CodeTrans()

    for call in sched.walk(Call):
        call: Call
        # The NVIDIA compiler (as at 25.3) will sometimes fail to compile
        # code with calls to MATMUL with a claim that they are not
        # available on the device, e.g.:
        # Call to NVHPC runtime function not supported -
        #   pgf90_matmul_real4_i8
        # Therefore, if we are unable to replace a MATMUL by generic code,
        # the resulting TransformationError will signal (to the calling
        # routine) that we are not to mark this kernel for offload.
        if (isinstance(call, IntrinsicCall) and
                call.intrinsic == IntrinsicCall.Intrinsic.MATMUL):
            matrans.apply(call)


def trans(psyir):
    '''Applies PSyclone colouring and GPU offloading transformations. Any
    kernels that cannot be offloaded to GPU are parallelised using OpenMP
    on the CPU.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    rtrans = LFRicRedundantComputationTrans()
    ctrans = LFRicColourTrans()
    otrans = LFRicOMPLoopTrans()
    const = LFRicConstants()
    cpu_parallel = OMPParallelTrans()
    mod_inline_trans = KernelModuleInlineTrans()
    inline_trans = InlineTrans()

    if OFFLOAD_DIRECTIVES == "omp":
        # Use OpenMP offloading
        loop_offloading_trans = OMPLoopTrans(
            omp_directive="teamsdistributeparalleldo",
            omp_schedule="none"
        )
        # OpenMP does not have a kernels parallelism directive equivalent
        # to OpenACC 'kernels'
        kernels_trans = None
        gpu_region_trans = OMPTargetTrans()
        gpu_annotation_trans = OMPDeclareTargetTrans()
    elif OFFLOAD_DIRECTIVES == "acc":
        # Use OpenACC offloading
        loop_offloading_trans = ACCLoopTrans()
        kernels_trans = ACCKernelsTrans()
        gpu_region_trans = ACCParallelTrans(default_present=False)
        gpu_annotation_trans = ACCRoutineTrans()
    else:
        print(f"The PSyclone transformation script expects the "
              f"LFRIC_OFFLOAD_DIRECTIVES to be set to 'omp' or 'acc' "
              f"but found '{OFFLOAD_DIRECTIVES}'.")
        sys.exit(-1)

    if psyir.name.lower().startswith(ALG_EXCLUSIONS):
        print(f"Not adding GPU offloading to psy: '{psyir.name}'")
        offload = False
    else:
        print(f"Attempting to offload psy: '{psyir.name}'")
        offload = True


    for subroutine in psyir.walk(Routine):

        print(f"Transforming invoke '{subroutine.name}' ...")

        # Make setval_c compute redundantly to the level 1 halo if it
        # is in its own loop and only if it has an iteration space that is
        # *not* restricted to owned dofs.
        for loop in subroutine.loops():
            if loop.iteration_space == "dof":
                if len(loop.kernels()) == 1:
                    if loop.kernels()[0].name in ["setval_c"]:
                        rtrans.apply(loop, options={"depth": 1})

        # Keep a record of any kernels we fail to offload.
        failed_inline = set()
        failed_to_offload = set()

        # Colour loops over cells unless they are on discontinuous spaces
        # (alternatively we could annotate the kernels with atomics)
        for loop in subroutine.loops():
            if loop.iteration_space.endswith("cell_column"):
                if (loop.field_space.orig_name not in
                        const.VALID_DISCONTINUOUS_NAMES):
                    ctrans.apply(loop)

        # We need to Module-inline all Kernels inside the loops and then mark
        # them with GPU-enabling directive or Inline them.
        for loop in subroutine.loops():
            for kern in loop.kernels():
                if not offload or isinstance(kern, LFRicBuiltIn):
                    # BuiltIns and kernels inside excluded invokes do
                    # not need inlining/annotations.
                    continue

                if kern.name == "compute_total_aam_code":
                    failed_inline.add(kern.name.lower())
                    failed_to_offload.add(kern.name.lower())
                    continue
                # Attempt to module-inline the kernel.
                try:
                    mod_inline_trans.apply(kern)
                    print(f"Module-inline successful for kernel "
                          f"'{kern.name}'")
                except TransformationError as err:
                    failed_inline.add(kern.name.lower())
                    print(f"Module-inline failed for kernel "
                          f"'{kern.name}' due to:\n{err.value}")
                    kern.append_preceding_comment(f"{err.value}")
                    loop.append_preceding_comment(f"{err.value}")

                # Attempt to fully inline the kernel
                try:
                    # Ensure any MATMULs within the kernel are also inlined
                    for routine in kern.get_callees():
                        _replace_matmuls(routine)

                    inline_trans.apply(kern)
                    print(f"Inline successful for kernel "
                          f"'{kern.name}'")
                    continue
                except TransformationError as err:
                    failed_inline.add(kern.name.lower())
                    print(f"Inline failed for kernel "
                          f"'{kern.name}' due to:\n{err.value}")

                    # If it cannot be inlined, fallback to annotate the
                    # kernel with GPU routine directives.
                    try:
                        gpu_annotation_trans.apply(
                            kern,
                            assume_valid_on_device=MANUALLY_ANNOTATED
                        )
                        print(f"Annotation successful for kernel "
                              f"'{kern.name}'")
                    except TransformationError as err:
                        failed_to_offload.add(kern.name.lower())
                        print(f"Annotation failed for kernel '{kern.name}' "
                              f"due to:\n{err.value}")
                        kern.append_preceding_comment(f"{err.value}")
                        loop.append_preceding_comment(f"{err.value}")

        # Add GPU offloading to loops
        for loop in subroutine.walk(Loop):
            kernel_names = [k.name.lower() for k in loop.kernels()]
            if offload and all(name not in failed_to_offload for name in
                               kernel_names):
                try:
                    if loop.loop_type == "colours":
                        pass
                    if loop.loop_type == "cells_in_colour":
                        loop_offloading_trans.apply(
                            loop, options={"independent": True})
                        gpu_region_trans.apply(loop.ancestor(Directive))
                        print(f"Offload with cell colouring: {kernel_names}")
                    if loop.loop_type == "":
                        loop_offloading_trans.apply(
                            loop, options={"independent": True})
                        gpu_region_trans.apply(loop.ancestor(Directive))
                        print(f"Offload independent loop: {kernel_names}")
                    if loop.loop_type == "dof":
                        # Loops over dofs can contains reductions
                        if kernels_trans:
                            # If kernel offloading is available it should
                            # manage them
                            kernels_trans.apply(loop)
                        else:
                            # Otherwise, if the reductions exists, they will
                            # be detected by the dependencyAnalysis and raise
                            # a TransformationError captured below
                            loop_offloading_trans.apply(
                                loop, options={"independent": True})
                            gpu_region_trans.apply(loop.ancestor(Directive))
                        print(f"Offload with dof loop: {kernel_names}")
                        # Alternatively we could use loop parallelism with
                        # reduction clauses
                except TransformationError as err:
                    print(f"Failed to offload loop with {kernel_names} "
                          f"because: {err}")

        # Apply OpenMP thread parallelism for any kernels we've not been able
        # to offload to GPU.
        for loop in subroutine.walk(Loop):
            if not offload or any(kern.name.lower() in failed_to_offload for
                                  kern in loop.kernels()):
                if loop.loop_type not in ["colours", "null"]:
                    cpu_parallel.apply(loop)
                    otrans.apply(loop, options={"reprod": True})
                    kernel_names = [kn.name for kn in loop.kernels()]
                    print(f"Added OMP threading to loop with {kernel_names}")
