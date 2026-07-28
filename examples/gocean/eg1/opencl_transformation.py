# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2026, Science and Technology Facilities Council
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

''' Module providing a PSyclone transformation script that converts the
Schedule of each Invoke to use OpenCL. '''

from psyclone.psyGen import InvokeSchedule
from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.domain.gocean.transformations import (
    GOOpenCLTrans, GOMoveIterationBoundariesInsideKernelTrans)
from psyclone.psyir.nodes import FileContainer
from psyclone.transformations import KernelImportsToArguments


def trans(psyir: FileContainer):
    '''
    Transformation routine for use with PSyclone. Converts any imported-
    variable accesses into kernel arguments and then applies the OpenCL
    transformation to the PSy layer.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    # Get the necessary transformations
    import_trans = KernelImportsToArguments()
    mod_inline_trans = KernelModuleInlineTrans()
    move_boundaries_trans = GOMoveIterationBoundariesInsideKernelTrans()
    cltrans = GOOpenCLTrans()

    for schedule in psyir.walk(InvokeSchedule):
        print("Converting to OpenCL invoke: " + schedule.name)

        # Skip invoke_2 as its time_smooth_code kernel contains a
        # module variable (alpha) which is not dealt with by the
        # KernelImportsToArguments transformation, see issue #826.
        if schedule.name == "invoke_2":
            continue

        # Remove the imports from inside each kernel and move PSy-layer
        # loop boundaries inside the kernel as a mask. To do this we must
        # first module-inline the kernel into the PSy layer module.
        for kern in schedule.kernels():
            print("Update kernel: " + kern.name)
            mod_inline_trans.apply(kern)
            move_boundaries_trans.apply(kern)
            import_trans.apply(kern)

        # Transform invoke to OpenCL
        cltrans.apply(schedule)
