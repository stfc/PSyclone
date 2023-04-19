# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2021, Science and Technology Facilities Council.
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
# Authors: A. R. Porter and S. Siso, STFC Daresbury Lab

''' Module providing a transformation script that converts the Schedule of
    the first Invoke to use OpenCL. In order to do this, those kernels that
    access imported data are transformed so as to pass that data by argument.
'''

from psyclone.transformations import KernelImportsToArguments
from psyclone.domain.gocean.transformations import GOOpenCLTrans, \
    GOMoveIterationBoundariesInsideKernelTrans


def trans(psy):
    '''
    Transformation routine for use with PSyclone. Applies the OpenCL
    transform to the first Invoke in the psy object.

    :param psy: the PSy object which this script will transform.
    :type psy: :py:class:`psyclone.psyGen.PSy`
    :returns: the transformed PSy object.
    :rtype: :py:class:`psyclone.psyGen.PSy`

    '''

    # Get the Schedule associated with the first Invoke
    invoke = psy.invokes.invoke_list[0]
    sched = invoke.schedule

    # Convert any kernel accesses to imported data into arguments
    ktrans = KernelImportsToArguments()
    for kern in sched.kernels():
        ktrans.apply(kern)

    # Provide kernel-specific OpenCL optimization options
    move_boundaries_trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kern in sched.kernels():
        # Move the PSy-layer loop boundaries inside the kernel as a kernel
        # mask, this allows to iterate through the whole domain
        move_boundaries_trans.apply(kern)
        # Specify the OpenCL queue and workgroup size of the kernel
        kern.set_opencl_options({"queue_number": 1, 'local_size': 4})

    # Transform the Schedule
    cltrans = GOOpenCLTrans()
    cltrans.apply(sched, options={"end_barrier": True})

    return psy
