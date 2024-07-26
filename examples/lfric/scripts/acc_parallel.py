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
# Authors: A. R. Porter, STFC Daresbury Lab
#          L. Mosimann, NVIDIA

'''PSyclone transformation script for the LFRic API for OpenACC offload
to GPU by:

 * Applying the 'kernel constants' transformation;
 * Adding 'ACC routine' to kernels;
 * Module-inlining kernels;
 * Applying ACC parallel loop to loops;
 * Enclosing those loops in an ACC parallel region.

No explicit data movement directives are added so either the default OpenACC
data movement will be used or the code built to use Unified Memory.

'''
from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.transformations import (Dynamo0p3ColourTrans,
                                      Dynamo0p3RedundantComputationTrans)

from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.lfric_builtins import LFRicBuiltIn
from psyclone.psyir.nodes import ACCDirective, Loop
from psyclone.psyir.transformations import TransformationError, InlineTrans
from psyclone.transformations import (
    ACCParallelTrans, ACCLoopTrans, ACCRoutineTrans, Dynamo0p3KernelConstTrans)

# This could be done on an invoke basis rather than a whole PSy layer.
ACC_EXCLUSIONS = ["lfric_xios_setup_mod_psy",
                  "fem_constants_mod_psy",
                  "wt_advective_update_alg_mod_psy",
                  "field_vector_mod_psy",  # Reduction in dof loop
                  # Multiple invokes need to make use of the same
                  # module-inlined kernels.
                  "ffsl_1d_flux_alg_mod_psy",
                  # Different invokes in same file need to module inline the
                  # same kernel:
                  # Transformation Error: Cannot inline subroutine
                  # 'create_threshold_mask_code' because another, different,
                  # subroutine with the same name already exists and
                  # versioning of module-inlined subroutines is not
                  # implemented yet.
                  # "init_saturated_profile_alg_mod_psy",
                  # "moist_dyn_factors_alg_mod_psy",
                  # "limited_area_masks_alg_mod_psy",
                  # Module-inlined kernel calls a function ('qsaturation') and
                  # so shouldn't have been inlined in the first place!
                  "init_unsaturated_profile_alg_mod_psy",
                  # "intermesh_mappings_alg_mod_psy",
                  # "end_of_transport_step_alg_mod_psy",
                  # Module-inlined kernel calls a function
                  # ('compute_cubic_hermite_coeffs') and
                  # so shouldn't have been inlined in the first place!
                  "vertical_sl_conservative_alg_mod_psy",
                  "vertical_sl_advective_alg_mod_psy",
                  # Kernel calls 'coordinate_jacobian_quadrature_r_double'
                  "si_operators_alg_mod_psy",
                  # "smagorinsky_alg_mod_psy",
                  # Kernel calls 'held_suarez_damping'
                  "external_forcing_alg_mod_psy",
                  # Procedures called in a compute region must have acc
                  # routine information - qsaturation
                  # Module variables used in acc routine need to be in acc
                  # declare create() - p_zero (
                  # algorithm/slow_physics_alg_mod_psy.f90: 58)
                  # Module variables used in acc routine need to be in acc
                  # declare create() - recip_epsilon (algorithm/
                  # slow_physics_alg_mod_psy.f90: 47)
                  "slow_physics_alg_mod_psy",
                  # Module variables used in acc routine need to be in acc
                  # declare create() - recip_epsilon
                  "fast_physics_alg_mod_psy",
                  # Call to NVHPC runtime function not supported -
                  # pghpf_rnumd_i8
                  "init_thermo_profile_alg_mod_psy",
                  # Mod variables in acc routine
                  "galerkin_projection_algorithm_mod_psy",
                  # Mod variables in acc routine
                  "physics_mappings_alg_mod_psy",
                  # call to 'nodal_xyz_coordinates_code' with no acc routine
                  "diagnostic_alg_mod_psy",
                  # Module variables used in acc routine need to be in acc
                  # declare create() - chi2llr$sd
                  # Procedures called in a compute region must have acc
                  # routine info - coordinate_jacobian_evaluator_r_double
                  "physical_op_constants_mod_psy",
                  # Procedures called in a compute region must have acc
                  # routine information - interpolate_to_regular_grid
                  "reconstruct_w3_field_alg_mod_psy",
                  # Linker says it can't find compute_latlon_code and
                  # get_height_code
                  "geometric_constants_mod_psy"
                  ]


def trans(psy):
    '''Applies the PSyclone transformations.

    '''
    ctrans = Dynamo0p3ColourTrans()
    rtrans = Dynamo0p3RedundantComputationTrans()
    mod_inline_trans = KernelModuleInlineTrans()
    inline_trans = InlineTrans()
    const = LFRicConstants()
    loop_trans = ACCLoopTrans()
    parallel_trans = ACCParallelTrans(default_present=False)
    artrans = ACCRoutineTrans()
    const_trans = Dynamo0p3KernelConstTrans()

    setval_count = 0
    all_inlined_kernels = set()

    print(f"PSy name = '{psy.name}'")
    # Loop over all of the Invokes in the PSy object
    for invoke in psy.invokes.invoke_list:

        inlined_kernels = set()

        print("Transforming invoke '{0}' ...".format(invoke.name))
        schedule = invoke.schedule

        if psy.name.lower() in ACC_EXCLUSIONS:
            print(f"Not adding ACC to invoke in '{psy.name}'")
            apply_acc = False
        else:
            apply_acc = True
        kern_inline = False
        if "intermesh_constants" in psy.name:
            kern_inline = True
        # Keep a record of any kernels we fail to module inline as we can't
        # then add ACC ROUTINE to them.
        failed_inline = set()

        # Make setval_* compute redundantly to the level 1 halo if it
        # is in its own loop.
        for loop in schedule.loops():
            if loop.iteration_space == "dof":
                kernels = loop.kernels()
                if len(kernels) != 1:
                    raise Exception(f"Expecting loop to contain 1 call but "
                                    f"found '{len(kernels)}'")
                if kernels[0].name in ["setval_c", "setval_x"]:
                    setval_count += 1
                    rtrans.apply(loop, options={"depth": 1})

        # Colour loops over cells unless they are on discontinuous
        # spaces or over dofs
        for loop in schedule.loops():
            if loop.iteration_space == "cell_column":
                for kern in loop.kernels():
                    if kern.name.lower() not in (list(all_inlined_kernels) +
                                                 list(inlined_kernels)):
                        print(f"Module inlining kernel '{kern.name}'")
                        try:
                            mod_inline_trans.validate(kern)
                            if apply_acc and not kern_inline:
                                # At the moment any kernel transformations must
                                # be performed *before* it is module inlined.
                                const_trans.apply(kern,
                                                  {"element_order": 0,
                                                   "quadrature": False})
                                artrans.apply(kern)
                            mod_inline_trans.apply(kern)
                            if kern_inline:
                                inline_trans.apply(kern)
                            inlined_kernels.add(kern.name.lower())
                        except TransformationError as err:
                            failed_inline.add(kern.name.lower())
                            print(f"Module inlining kernel '{kern.name}' "
                                  f"failed:\n{err.value}")
                if (loop.field_space.orig_name not in
                        const.VALID_DISCONTINUOUS_NAMES):
                    ctrans.apply(loop)

        if not apply_acc:
            continue

        # Add OpenACC to loops unless they are over colours or are null.
        schedule = invoke.schedule
        for loop in schedule.walk(Loop):
            no_acc = False
            for kern in loop.kernels():
                if not (isinstance(kern, LFRicBuiltIn) or
                        kern.name.lower() in inlined_kernels):
                    # This kernel is not a BuiltIn and has not been inlined
                    # in this invoke().
                    no_acc = True
                    break
            if no_acc:
                print(f"Not adding OpenACC for kernels: "
                      f"{[kern.name for kern in loop.kernels()]}")
                continue

            try:
                if loop.loop_type == "colours":
                    loop_trans.apply(loop, options={"sequential": True})
                    parallel_trans.apply(loop.ancestor(ACCDirective))
                if loop.loop_type == "colour":
                    loop_trans.apply(loop, options={"independent": True})
                if loop.loop_type in ["", "dof"]:
                    loop_trans.apply(loop, options={"independent": True})
                    parallel_trans.apply(loop.ancestor(ACCDirective))
            except TransformationError as err:
                print(str(err))
                pass

        # Take a look at what we've done
        print("Found {0} setval calls".format(setval_count))
        print(schedule.view())

        # Add the list of kernels inlined in this invoke to those inlined for
        # the whole PSy layer module. (We have to do this as we can't currently
        # module inline the same kernel from different invokes as we do not
        # check that they are identical.)
        all_inlined_kernels.update(inlined_kernels)
    return psy
