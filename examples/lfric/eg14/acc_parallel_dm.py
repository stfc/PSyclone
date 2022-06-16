# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2022, Science and Technology Facilities Council.
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
API to apply OpenACC Loop, Parallel and Enter Data directives
generically in the presence of halo exchanges. Any user-supplied kernels
are also transformed through the addition of an OpenACC Routine directive.
The psyclone script can apply this transformation script via its -s option.

'''
from psyclone.psyGen import CodedKern
from psyclone.transformations import ACCEnterDataTrans, ACCParallelTrans, \
    ACCLoopTrans, ACCRoutineTrans


def trans(psy):
    '''PSyclone transformation script for the LFRic API to apply OpenACC loop,
    parallel and enter data directives generically. User-supplied kernels are
    transformed through the addition of a routine directive.

    '''
    loop_trans = ACCLoopTrans()
    parallel_trans = ACCParallelTrans()
    enter_data_trans = ACCEnterDataTrans()
    rtrans = ACCRoutineTrans()

    # Loop over all of the Invokes in the PSy object
    for invoke in psy.invokes.invoke_list:

        print("Transforming invoke '"+invoke.name+"'...")
        schedule = invoke.schedule
        for loop in schedule.loops():
            loop_trans.apply(loop)
            # The loop is now the child of the Directive's Schedule
            parallel_trans.apply(loop.parent.parent)
        enter_data_trans.apply(schedule)

        # We transform every user-supplied kernel using ACCRoutineTrans. This
        # adds '!$acc routine' which ensures the kernel is compiled for the
        # OpenACC device.
        for kernel in schedule.walk(CodedKern):
            rtrans.apply(kernel)

    return psy
