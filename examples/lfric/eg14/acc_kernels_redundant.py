# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council
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


'''File containing a PSyclone transformation script for the LFRic (Dynamo0p3)
API to apply redundant computation and OpenACC Kernels directives generically.
This can be applied via the -s option to the psyclone script.

'''
from __future__ import print_function
from psyclone.psyGen import HaloExchange
from psyclone.psyir.nodes import Loop
from psyclone.transformations import (ACCKernelsTrans, MoveTrans,
                                      Dynamo0p3RedundantComputationTrans)


def trans(psy):
    ''' PSyclone transformation script for the dynamo0p3 api to apply
    OpenACC Kernels directives generically in the presence of redundant
    computation..'''
    kernels_trans = ACCKernelsTrans()
    rc_trans = Dynamo0p3RedundantComputationTrans()
    m_trans = MoveTrans()

    # Loop over all of the Invokes in the PSy object
    for invoke in psy.invokes.invoke_list:

        print("Transforming invoke '"+invoke.name+"'...")
        schedule = invoke.schedule
        schedule.view()

        loops = schedule.walk(Loop)
        rc_trans.apply(loops[1], {"depth": 2})
        rc_trans.apply(loops[2], {"depth": 2})

        node_list = []
        for node in schedule.children:
            # We can't include halo exchange calls within a KERNELS region.
            if not isinstance(node, HaloExchange):
                node_list.append(node)
            else:
                if node_list:
                    kernels_trans.apply(node_list)
                node_list = []
                continue
        if node_list:
            kernels_trans.apply(node_list)

        schedule.view()

    return psy
