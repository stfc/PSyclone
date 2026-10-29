# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''PSyclone transformation script for LFRic to apply GPU offloading directives.
Also adds redundant computation to the level-1 halo for setval_* generically.

'''
import os
import sys
from psyclone.domain.common.transformations import (
    KernelInlineTrans, KernelModuleInlineTrans)
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.lfric_builtins import LFRicBuiltIn
from psyclone.domain.lfric.transformations import (
    LFRicRedundantComputationTrans, LFRicLoopFuseTrans)
from psyclone.psyir.nodes import (
    Call, Directive, IntrinsicCall, Loop, Routine, Schedule)
from psyclone.psyir.transformations import (
    ACCKernelsTrans, Matmul2CodeTrans, OMPTargetTrans, TransformationError,
    OMPDeclareTargetTrans, OMPParallelTrans)
from psyclone.transformations import (
    LFRicColourTrans, LFRicOMPLoopTrans,
    ACCParallelTrans, ACCRoutineTrans,
    OMPLoopTrans)
from psyclone.psyir.transformations import ACCLoopTrans


# Names of any Algorithm module that we won't add any GPU offloading
ALG_EXCLUSIONS = ('diagnostic_', )

# We won't attempt to inline calls to routines or kernels with these names
# because they're not computationally important or cause runtime errors.
INLINE_EXCLUSIONS = [
    "abort", "logging",
    # TODO #3611: The problem could be the L/UBOUND offloaded intrinsics.
    "sample_eos_operators_code_inlined_",
    "apply_mixed_wp_operator_code_inlined_",
    "weighted_proj_2thetav_code_inlined_",
    "weighted_proj_theta2v_code_inlined_",
    "weighted_div_code_inlined_",
]

ENABLE_FUSING = False

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
    'monotonic_edge', 'fourth_order_vertical_edge', 'ffsl_flux_xy_1d',

    # Subroutines with GPU annotations but offloading them cause runtime issues
    # 'native_jacobian',  # Runtime invalid address access
    # 'rehabilitate',     # Different results
    # 'subgrid_quadratic_recon'  # Not enough heap memory on the gpu?
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
            try:
                matrans.apply(call)
            except TransformationError as err:
                print(f"Matmul2Code failed with: {err}")


def trans(psyir):
    '''Applies PSyclone colouring and GPU offloading transformations. Any
    kernels that cannot be offloaded to GPU are parallelised using OpenMP
    on the CPU.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    fusetrans = LFRicLoopFuseTrans()
    rtrans = LFRicRedundantComputationTrans()
    ctrans = LFRicColourTrans()
    otrans = LFRicOMPLoopTrans()
    const = LFRicConstants()
    cpu_parallel = OMPParallelTrans()
    mod_inline_trans = KernelModuleInlineTrans()
    inline_trans = KernelInlineTrans()

    if OFFLOAD_DIRECTIVES.startswith("omp"):
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
    elif OFFLOAD_DIRECTIVES.startswith("acc"):
        # Use OpenACC offloading
        loop_offloading_trans = ACCLoopTrans()
        kernels_trans = ACCKernelsTrans()
        gpu_region_trans = ACCParallelTrans(default_present=False)
        gpu_annotation_trans = ACCRoutineTrans()
    else:
        print(f"The PSyclone transformation script expects the "
              f"LFRIC_OFFLOAD_DIRECTIVES to be set to 'omp', 'omp_uniform', "
              f"'acc' or 'acc_uniform' but found '{OFFLOAD_DIRECTIVES}'.")
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

                # Ensure MATMULs within the kernel are also inlined
                for routine in kern.get_callees():
                    _replace_matmuls(routine)

                try:
                    # Attempt annotations first, since if inlining fails
                    # during lowering we can't fallback at that point
                    gpu_annotation_trans.apply(
                            kern,
                            assume_valid_on_device=MANUALLY_ANNOTATED
                    )
                    print(f"Annotation successful for kernel "
                          f"'{kern.name}'")
                    try:
                        if kern.name in INLINE_EXCLUSIONS:
                            continue
                        if OFFLOAD_DIRECTIVES.startswith("acc"):
                            # TODO #423: PSyclone doesn't support the OpenACC
                            # private clause yet, which we need for the inlined
                            # version
                            continue
                        # For the kernels that can be on the GPU, attempt to
                        # fully inline them to improve performance.
                        inline_trans.apply(kern)
                        print(f"Kernel '{kern.name}' marked for deferred "
                              f"inlining")
                    except TransformationError:
                        # If it fails continue as normal, as the kenrel will
                        # still be in the GPU (just not inlined)
                        continue
                except TransformationError as err:
                    failed_to_offload.add(kern.name.lower())
                    loop.append_preceding_comment(f"{err.value}")
                    print(f"Annotation failed for kernel '{kern.name}' "
                          f"due to:\n{err.value}")

        if ENABLE_FUSING:
            # Try to fuse all of the loops.
            loops = subroutine.loops()
            num_loops = len(loops)
            current_index = 0
            # Keep trying to fuse loops wherever possible
            while current_index < num_loops-1:
                try:
                    fusetrans.apply((loops[current_index],
                                     loops[current_index+1]))
                    kernel_names = [k.name.lower() for k in
                                    loops[current_index].kernels()]
                    # If fusion is successful then we have 1 less remaining
                    # loop and continue at the same index to keep trying to
                    # fuse.
                    num_loops = num_loops - 1
                    print(f"Fusion successful: {kernel_names}, "
                          f"{psyir.name}:{subroutine.name}")
                except TransformationError:
                    # If fusion fails we move to the next loop.
                    current_index = current_index + 1
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
