# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council
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


'''PSyclone transformation script for the LFRic API to apply serial
optimisations: inline kernels into modules, expand intrinsics into code.
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.psyir.nodes import IntrinsicCall, Container, KernelSchedule
from psyclone.psyir.transformations import Matmul2CodeTrans
from psyclone.transformations import TransformationError


def trans(psy):
    '''Applies LFRic serial optimisations'''

    matmul_trans = Matmul2CodeTrans()
    inline_trans = KernelModuleInlineTrans()

    # Loop over all of the Invokes in the PSy object
    for invoke in psy.invokes.invoke_list:
        schedule = invoke.schedule

        # Try to Inline all kernels into the PSy module
        for kernel in schedule.coded_kernels():
            try:
                inline_trans.apply(kernel)
                print(f"Inline transformation was successful for "
                      f"'{kernel.name}' in '{invoke.name}'.")
            except TransformationError as err:
                print(f"Inline transformation failed for "
                      f"'{kernel.name}' in '{invoke.name}' because:")
                print(str(err))

    # Then we transform all the kernels inlined into the module
    if psy.invokes.invoke_list:
        root = psy.invokes.invoke_list[0].schedule.ancestor(Container)
        for kschedule in root.walk(KernelSchedule):
            # Expand MATMUL intrinsic
            for icall in kschedule.walk(IntrinsicCall):
                if icall.intrinsic == IntrinsicCall.Intrinsic.MATMUL:
                    try:
                        matmul_trans.apply(icall)
                    except TransformationError as err:
                        print(f"Inline MATMUL failed for '{kschedule.name}' "
                              f"because:")
                        print(str(err))

    return psy
