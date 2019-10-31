# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017, Science and Technology Facilities Council
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
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
# Author R. W. Ford, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''A PSyclone transformation script which performs redundant
computation to remove halo exchanges where possible and then moves the
remaining ones to the beginning of the loop thereby separating the
computation from the communication. The science code for which this
example script has been written is taken from the Met Office
repository but an operator has been replaced with a field in one of
the kernels to allow redundant computation'''


def trans(psy):
    '''removes the grad_p halo exchanges by redundant computation then
    moves the remaining halo exchanges to the beginning of the invoke
    call'''
    from psyclone.transformations import Dynamo0p3RedundantComputationTrans, \
        MoveTrans
    rc_trans = Dynamo0p3RedundantComputationTrans()
    m_trans = MoveTrans()
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    # redundant computation to remove grad_p halo exchanges
    schedule, _ = rc_trans.apply(schedule.children[5], {"depth": 2})
    schedule, _ = rc_trans.apply(schedule.children[0], {"depth": 2})

    # move remaining (potential) halo exchanges to start of the invoke
    schedule, _ = m_trans.apply(schedule.children[0], schedule.children[4])

    return psy
