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
from psyclone.psyir.transformations import TransformationError

# Add all selected invoke/kernel, position/arg names here. The kernel
# information has a name and a position, with the position being the
# nth time this kernel is found in a schedule, as a kernel may be used
# multiple times in a schedule.
invoke_info = {
    "invoke_0_w3_solver_kernel_type": {("solver_w3_code", 1): ["chi"]},
    "invoke_bicg_group1": {("matrix_vector_mm_code", 1): ["v", "lhs"]},
    "invoke_bicg_iterloop_group1": {("matrix_vector_mm_code", 1): ["p"]}}

# Note, we could use the psy algorithm name as well to distinguish
# between algorithm files but the LFRic build system has support for
# scripts that are local to algorithms so this is not done.


def trans(psy):
    '''PSyclone transformation script for the Dynamo0.3 API to remove
    selective halo exchanges.

    '''
    found_info = {}
    for invoke in psy.invokes.invoke_list:
        if invoke.name in invoke_info:
            # Found a selected invoke
            kernel_info = invoke_info[invoke.name]
            schedule = invoke.schedule
            marked_halo_exchanges = []
            kernel_count = {}
            for kernel in schedule.walk(Kern):
                # Update the number of times we have found this kernel
                # in the schedule
                try:
                    kernel_count[kernel.name] += 1
                except KeyError:
                    kernel_count[kernel.name] = 1
                kernel_lookup = (kernel.name, kernel_count[kernel.name])
                if kernel_lookup in kernel_info:
                    # Found a selected kernel
                    arg_info = kernel_info[kernel_lookup]
                    for arg in kernel.args:
                        if arg.name in arg_info:
                            # Found a selected field
                            print(f"Found '{invoke.name}':'{kernel_lookup}':"
                                  f"'{arg.name}'")
                            # Add this information to found_info so
                            # that later we can check whether all
                            # requested invokes/kernels/args are
                            # found.
                            try:
                                found_info[invoke.name]
                            except KeyError:
                                found_info[invoke.name] = {}
                            try:
                                found_info[invoke.name][kernel_lookup]
                            except KeyError:
                                found_info[invoke.name][kernel_lookup] = []
                            found_info[invoke.name][kernel_lookup].\
                                append(arg.name)

                            # Look for a preceding halo exchange for
                            # this field in the schedule
                            n_hex_found = 0
                            for node in kernel.preceding(reverse=True):
                                if isinstance(node, HaloExchange):
                                    # Found a preceding halo exchange
                                    if node.field.name == arg.name:
                                        n_hex_found += 1
                                        print(f"  Found halo exchange for "
                                              f"'{arg.name}'")
                                        # Store the node to remove later
                                        marked_halo_exchanges.append(node)
                                        # Continue until all halo
                                        # exchanges associated with a
                                        # vector field are found
                                        if n_hex_found == arg.vector_size:
                                            break
                                if isinstance(node, Kern):
                                    # Reached an earlier kernel before
                                    # finding halo exchange(s) so give
                                    # up.
                                    if n_hex_found > 0:
                                        raise TransformationError(
                                            "Found a subset of the halo "
                                            "exchanges for a field vector")
                                    print("  Reached previous kernel")
                                    break
                            else:
                                # Reached the start of the schedule before
                                # finding halo exchange(s)
                                if n_hex_found > 0:
                                    raise TransformationError(
                                        "Found a subset of the halo exchanges "
                                        "for a field vector")
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

    # Raise exception if any invoke/kernel/arg names were not found.
    message = ""
    for invoke_name in invoke_info:
        if invoke_name not in found_info:
            message += f"invoke '{invoke_name}' not found in schedule\n"
        else:
            for kernel_tuple in invoke_info[invoke_name]:
                if kernel_tuple not in found_info[invoke_name]:
                    message += (f"kernel '{kernel_tuple}' in invoke "
                                f"'{invoke_name}' not found in schedule\n")
                else:
                    for arg_name in invoke_info[invoke_name][kernel_tuple]:
                        if arg_name not in \
                           found_info[invoke_name][kernel_tuple]:
                            message += (
                                f"arg '{arg_name}' in kernel '{kernel_tuple}' "
                                f"in invoke '{invoke_name}' not found in "
                                f"schedule\n")
    if message:
        raise TransformationError(message)

    return psy
