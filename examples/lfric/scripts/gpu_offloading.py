# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2025, Science and Technology Facilities Council.
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

'''PSyclone transformation script for LFRic to apply colouring and GPU
offloading. Also adds redundant computation to the level-1 halo for
setval_* generically.

'''
import os
import sys
from psyclone.domain.lfric import LFRicConstants
from psyclone.psyir.nodes import Directive, Loop, Routine
from psyclone.psyir.transformations import (
    ACCKernelsTrans, TransformationError, OMPTargetTrans)
from psyclone.transformations import (
    Dynamo0p3ColourTrans, Dynamo0p3OMPLoopTrans,
    Dynamo0p3RedundantComputationTrans, OMPParallelTrans,
    ACCParallelTrans, ACCLoopTrans, ACCRoutineTrans,
    OMPDeclareTargetTrans, OMPLoopTrans)


# Names of any invoke that we won't add any GPU offloading
INVOKE_EXCLUSIONS = [
]

OFFLOAD_DIRECTIVES = os.getenv('LFRIC_OFFLOAD_DIRECTIVES', "none")


def trans(psyir):
    '''Applies PSyclone colouring and GPU offloading transformations. Any
    kernels that cannot be offloaded to GPU are parallelised using OpenMP
    on the CPU. Any setval_* kernels are transformed so as to compute
    into the L1 halos.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    rtrans = Dynamo0p3RedundantComputationTrans()
    ctrans = Dynamo0p3ColourTrans()
    otrans = Dynamo0p3OMPLoopTrans()
    const = LFRicConstants()
    cpu_parallel = OMPParallelTrans()

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

    print(f"PSy name = '{psyir.name}'")

    for subroutine in psyir.walk(Routine):

        print("Transforming invoke '{0}' ...".format(subroutine.name))

        # Make setval_* compute redundantly to the level 1 halo if it
        # is in its own loop
        for loop in subroutine.loops():
            if loop.iteration_space == "dof":
                if len(loop.kernels()) == 1:
                    if loop.kernels()[0].name in ["setval_c", "setval_x"]:
                        rtrans.apply(loop, options={"depth": 1})

        if psyir.name.lower() in INVOKE_EXCLUSIONS:
            print(f"Not adding GPU offloading to invoke '{psyir.name}'")
            offload = False
        else:
            offload = True

        # Keep a record of any kernels we fail to offload
        failed_to_offload = set()

        # Colour loops over cells unless they are on discontinuous spaces
        # (alternatively we could annotate the kernels with atomics)
        for loop in subroutine.loops():
            if loop.iteration_space.endswith("cell_column"):
                if (loop.field_space.orig_name not in
                        const.VALID_DISCONTINUOUS_NAMES):
                    ctrans.apply(loop)

        # Mark Kernels inside the loops over cells as GPU-enabled
        # (alternatively we could inline them)
        for loop in subroutine.loops():
            if loop.iteration_space.endswith("cell_column"):
                if offload:
                    for kern in loop.kernels():
                        try:
                            gpu_annotation_trans.apply(kern)
                        except TransformationError as err:
                            failed_to_offload.add(kern.name.lower())
                            print(f"Failed to annotate '{kern.name}' with "
                                  f"GPU-enabled directive due to:\n"
                                  f"{err.value}")
                        # For annotated or inlined kernels we could attempt to
                        # provide compile-time dimensions for the temporary
                        # arrays and convert to code unsupported intrinsics.

        # Add GPU offloading to loops unless they are over colours or are null.
        for loop in subroutine.walk(Loop):
            kernel_names = [k.name.lower() for k in loop.kernels()]
            if offload and all(name not in failed_to_offload for name in
                               kernel_names):
                try:
                    if loop.loop_type == "colours":
                        pass
                    if loop.loop_type == "colour":
                        loop_offloading_trans.apply(
                            loop, options={"independent": True})
                        gpu_region_trans.apply(loop.ancestor(Directive))
                    if loop.loop_type == "":
                        loop_offloading_trans.apply(
                            loop, options={"independent": True})
                        gpu_region_trans.apply(loop.ancestor(Directive))
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
                        # Alternatively we could use loop parallelism with
                        # reduction clauses
                    print(f"Successfully offloaded loop with {kernel_names}")
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
