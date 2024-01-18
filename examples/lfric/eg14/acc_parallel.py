# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Laboratory

'''File containing a PSyclone transformation script for the LFRic
API to apply OpenACC Kernels and Enter Data directives generically. Any
user-supplied kernels are also transformed through the addition of an OpenACC
Routine directive. PSyclone can apply this transformation script via its
 -s option.

'''
from psyclone.domain.lfric import LFRicConstants
from psyclone.psyGen import CodedKern
from psyclone.transformations import (
    ACCEnterDataTrans, ACCKernelsTrans, ACCRoutineTrans, Dynamo0p3ColourTrans)


def trans(psy):
    '''PSyclone transformation script for the LFRic API to apply OpenACC
    kernels and enter data directives generically. User-supplied kernels are
    transformed through the addition of a routine directive.

    :param psy: the PSy object containing the invokes to transform.
    :type psy: :py:class:`psyclone.dynamo0p3.DynamoPSy`

    :returns: the transformed PSy object.
    :rtype: :py:class:`psyclone.dynamo0p3.DynamoPSy`

    '''
    const = LFRicConstants()

    ctrans = Dynamo0p3ColourTrans()
    enter_data_trans = ACCEnterDataTrans()
    kernel_trans = ACCKernelsTrans()
    rtrans = ACCRoutineTrans()

    # Loop over all of the Invokes in the PSy object
    for invoke in psy.invokes.invoke_list:

        print("Transforming invoke '"+invoke.name+"'...")
        schedule = invoke.schedule

        # Colour loops over cells unless they are on discontinuous
        # spaces or over dofs
        for loop in schedule.loops():
            if loop.iteration_space == "cell_column":
                if (loop.field_space.orig_name not in
                        const.VALID_DISCONTINUOUS_NAMES):
                    ctrans.apply(loop)

        for loop in schedule.loops():
            if loop.loop_type not in ["colours", "null"]:
                kernel_trans.apply(loop)

        enter_data_trans.apply(schedule)

        # We transform every user-supplied kernel using ACCRoutineTrans. This
        # adds '!$acc routine' which ensures the kernel is compiled for the
        # OpenACC device.
        for kernel in schedule.walk(CodedKern):
            rtrans.apply(kernel)

    return psy
