# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2024, Science and Technology Facilities Council.
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
#          L. Mosimann, NVIDIA.

'''PSyclone transformation script for the lfric API to apply
colouring, OpenACC, OpenMP. Also adds redundant computation to the level-1
halo for setval_* generically.

'''
from psyclone.domain.lfric import LFRicConstants
from psyclone.psyir.nodes import ACCDirective, Loop
from psyclone.psyir.transformations import (
    ACCKernelsTrans, TransformationError)
from psyclone.transformations import (
    Dynamo0p3ColourTrans, Dynamo0p3OMPLoopTrans,
    Dynamo0p3RedundantComputationTrans, OMPParallelTrans,
    ACCParallelTrans, ACCLoopTrans, ACCRoutineTrans)


ACC_EXCLUSIONS = [
]


def trans(psy):
    '''Applies PSyclone colouring and OpenACC transformations. Any kernels that
    cannot be offloaded to GPU are parallelised using OpenMP on the CPU. Any
    setval_* kernels are transformed so as to compute into the L1 halos.

    '''
    rtrans = Dynamo0p3RedundantComputationTrans()
    ctrans = Dynamo0p3ColourTrans()
    otrans = Dynamo0p3OMPLoopTrans()
    const = LFRicConstants()
    loop_trans = ACCLoopTrans()
    ktrans = ACCKernelsTrans()
    parallel_trans = ACCParallelTrans(default_present=False)
    artrans = ACCRoutineTrans()
    oregtrans = OMPParallelTrans()

    print(f"PSy name = '{psy.name}'")

    # Loop over all of the Invokes in the PSy object
    for invoke in psy.invokes.invoke_list:

        print("Transforming invoke '{0}' ...".format(invoke.name))
        schedule = invoke.schedule

        # Make setval_* compute redundantly to the level 1 halo if it
        # is in its own loop
        for loop in schedule.loops():
            if loop.iteration_space == "dof":
                if len(loop.kernels()) == 1:
                    if loop.kernels()[0].name in ["setval_c", "setval_x"]:
                        rtrans.apply(loop, options={"depth": 1})

        if psy.name.lower() in ACC_EXCLUSIONS:
            print(f"Not adding ACC to invoke in '{psy.name}'")
            apply_acc = False
        else:
            apply_acc = True

        # Keep a record of any kernels we fail to module inline as we can't
        # then add ACC ROUTINE to them.
        failed_inline = set()

        # Colour loops over cells unless they are on discontinuous
        # spaces or over dofs
        for loop in schedule.loops():
            if loop.iteration_space == "cell_column":
                if apply_acc:
                    for kern in loop.kernels():
                        try:
                            artrans.apply(kern)
                        except TransformationError as err:
                            failed_inline.add(kern.name.lower())
                            print(f"Adding ACC Routine to kernel '{kern.name}'"
                                  f" failed:\n{err.value}")
                if (loop.field_space.orig_name not in
                        const.VALID_DISCONTINUOUS_NAMES):
                    ctrans.apply(loop)

        # Add OpenACC to loops unless they are over colours or are null.
        schedule = invoke.schedule
        for loop in schedule.walk(Loop):
            if not apply_acc or any(kern.name.lower() in failed_inline for
                                    kern in loop.kernels()):
                print(f"Not adding OpenACC for kernels: "
                      f"{[kern.name for kern in loop.kernels()]}")
                continue
            try:
                if loop.loop_type == "colours":
                    pass
                if loop.loop_type == "colour":
                    loop_trans.apply(loop, options={"independent": True})
                    parallel_trans.apply(loop.ancestor(ACCDirective))
                if loop.loop_type == "":
                    loop_trans.apply(loop, options={"independent": True})
                    parallel_trans.apply(loop.ancestor(ACCDirective))
                if loop.loop_type == "dof":
                    # We use ACC KERNELS for dof loops since they can contain
                    # reductions.
                    ktrans.apply(loop)
            except TransformationError as err:
                print(str(err))
                pass

        # Apply OpenMP thread parallelism for any kernels we've not been able
        # to offload to GPU.
        for loop in schedule.walk(Loop):
            if not apply_acc or any(kern.name.lower() in failed_inline for
                                    kern in loop.kernels()):
                if loop.loop_type not in ["colours", "null"]:
                    oregtrans.apply(loop)
                    otrans.apply(loop, options={"reprod": True})

    return psy
