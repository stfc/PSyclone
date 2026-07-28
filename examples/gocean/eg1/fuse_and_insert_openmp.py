# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2026, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab

''' PSyclone script to fuse invoke_0 loops and insert OpenMP directives '''

from psyclone.psyir.nodes import FileContainer
from psyclone.psyGen import InvokeSchedule
from psyclone.psyGen import TransInfo


def trans(psyir: FileContainer):
    '''
    :param psyir: the PSyIR of the PSy-layer.

    '''
    invokes = psyir.walk(InvokeSchedule)

    # Print a list of all of the invokes found
    print([invoke.name for invoke in invokes])

    trans_info = TransInfo()
    print(trans_info.list)
    fuse_trans = trans_info.get_trans_name('LoopFuseTrans')
    omp_trans = trans_info.get_trans_name('GOceanOMPParallelLoopTrans')

    for invoke in invokes:
        if invoke.name == "invoke_0":
            # fuse all outer loops
            fuse_trans.apply(invoke.children[0], invoke.children[1])
            fuse_trans.apply(invoke.children[0], invoke.children[1])
            fuse_trans.apply(invoke.children[0], invoke.children[1])
            # fuse all inner loops
            fuse_trans.apply(invoke.children[0].loop_body[0],
                             invoke.children[0].loop_body[1])
            fuse_trans.apply(invoke.children[0].loop_body[0],
                             invoke.children[0].loop_body[1])
            fuse_trans.apply(invoke.children[0].loop_body[0],
                             invoke.children[0].loop_body[1])
            # Add OpenMP
            omp_trans.apply(invoke.children[0])
