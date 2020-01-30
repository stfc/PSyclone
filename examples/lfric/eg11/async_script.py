# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018, Science and Technology Facilities Council
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
# Authors: R. Ford and A. R. Porter, STFC Daresbury Laboratory


'''File containing a PSyclone transformation script for the Dynamo0p3
API to make asynchronous halo exchanges and overlap their
communication with computation. This can be applied via the -s option
in the generator.py script.

'''


def trans(psy):
    '''A sample transformation script to demonstrate the use of asynchronous
    halo exchanges with overlapping compute and communication for the
    most costly halo exchanges in the (current version of the) LFRic model.

    '''
    from psyclone.transformations import \
        Dynamo0p3RedundantComputationTrans, \
        Dynamo0p3AsyncHaloExchangeTrans, \
        MoveTrans

    schedule = psy.invokes.invoke_list[0].schedule
    schedule.view()

    # This transformation removes the halo exchange associated with
    # the grad_p field. This transformation is unnecessary if
    # annexed_dofs is set to True in the config file (although the
    # transformation still works).
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[0], {"depth": 1})
    schedule.view()

    # This transformation splits the three synchronous halo exchanges
    # (for fields p, hb_inv and u_normalisation) into asynchronous
    # (halo_exchange_start and halo_exchange_end) ones.
    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    for kern in schedule.children[3:0:-1]:
        schedule, _ = ahex_trans.apply(kern)
    schedule.view()

    # This transformation moves the start of the three halo exchanges
    # before the setval_c loop offering the potential for overlap
    # between communication and computation.
    mtrans = MoveTrans()
    for kern in schedule.children[5:0:-2]:
        schedule, _ = mtrans.apply(kern, schedule.children[0])
    schedule.view()

    return psy
