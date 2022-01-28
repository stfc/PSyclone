# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council
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
# Author: R. W. Ford, STFC Daresbury Laboratory

'''File containing a PSyclone transformation script for the Dynamo0.3
API which removes specified halo exchanges associated with a field
being accessed within a kernel. Use of this script is entirely at the
risk of the user and will produce invalid code if used
incorrectly. The reason the script is useful is because PSyclone
assumption about continuous fields requiring a halo exchange is not
always the case (due to the way kernels are written internally).

'''
from psyclone.psyGen import Kern, HaloExchange

# TODO How to work with field vectors, A: remove them all
# TODO same kernel name in an invoke (how to determine which), A: optional? tuple with schedule position information
# TODO when things are not found? - list them at the end?

# Add all selected invoke/kernel/arg names here
invoke_info = {
    "invoke_0_w3_solver_kernel_type":
        {"solver_w3_code":
             ["chi"]},
    "invoke_bicg_group1":
        {"matrix_vector_mm_code":
             ["v", "lhs"]},
    "invoke_bicg_iterloop_group1":
        {"matrix_vector_mm_code":
             ["p"]}}

# Note, we could use the psy algorithm name as well to distinguish
# between algorithm files but the LFRic build system has support for
# scripts that are local to algorithms so this is not done.


def trans(psy):
    '''PSyclone transformation script for the Dynamo0.3 API to remove
    selective halo exchanges.

    '''
    for invoke in psy.invokes.invoke_list:
        if invoke.name in invoke_info:
            # Found a selected invoke
            kernel_info = invoke_info[invoke.name]
            schedule = invoke.schedule
            marked_halo_exchanges = []
            for kernel in schedule.walk(Kern):
                if kernel.name in kernel_info:
                    # Found a selected kernel
                    arg_info = kernel_info[kernel.name]
                    for arg in kernel.args:
                        if arg.name in arg_info:
                            # Found a selected field
                            print(f"Found '{invoke.name}':'{kernel.name}':"
                                  f"'{arg.name}'")

                            # Look for a preceding halo exchange for
                            # this field in the schedule
                            for node in kernel.preceding(reverse=True):
                                if isinstance(node, HaloExchange):
                                    # Found a preceding halo exchange
                                    if node.field.name == arg.name:
                                        print(f"  Found halo exchange for "
                                              f"'{arg.name}'")
                                        # Store the node to remove later
                                        marked_halo_exchanges.append(node)
                                        break
                                if isinstance(node, Kern):
                                    # Reached an earlier kernel before
                                    # finding a halo exchange so give
                                    # up.
                                    print("  Reached previous kernel")
                                    break
                            else:
                                # Reached the start of the schedule before
                                # finding a halo exchange
                                print("  Reached start of schedule")

            if marked_halo_exchanges:
                # Remove marked halo exchanges from this schedule
                schedule.view()
                # Reverse the walk as we remove nodes as we iterate
                # over them
                for halo_exchange in reversed(schedule.walk(HaloExchange)):
                    if halo_exchange in marked_halo_exchanges:
                        print(f"Removed halo exchange for "
                              f"'{halo_exchange.field.name}'")
                        halo_exchange.detach()
                schedule.view()
