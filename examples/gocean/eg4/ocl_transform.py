# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2020, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab

''' Module providing a transformation script that converts the Schedule of
    the first Invoke to use OpenCL. In order to do this, those kernels that
    access global data are transformed so as to pass that data by argument. '''


def trans(psy):
    '''
    Transformation routine for use with PSyclone. Applies the OpenCL
    transform to the first Invoke in the psy object.

    :param psy: the PSy object which this script will transform.
    :type psy: :py:class:`psyclone.psyGen.PSy`
    :returns: the transformed PSy object.
    :rtype: :py:class:`psyclone.psyGen.PSy`

    '''
    from psyclone.transformations import OCLTrans, KernelGlobalsToArguments

    # Get the Schedule associated with the first Invoke
    invoke = psy.invokes.invoke_list[0]
    sched = invoke.schedule

    # Convert any kernel accesses to global data into arguments
    ktrans = KernelGlobalsToArguments()
    for kern in sched.kernels():
        ktrans.apply(kern)

    # Transform the Schedule
    cltrans = OCLTrans()
    cltrans.apply(sched, options={"end_barrier": True})

    # Provide kernel-specific OpenCL optimization options
    for kern in sched.kernels():
        kern.set_opencl_options({"queue_number": 1, 'local_size': 4})

    return psy
