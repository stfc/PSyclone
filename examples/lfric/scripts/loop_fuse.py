# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2025, Science and Technology Facilities Council
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
# Author: R. W. Ford, STFC Daresbury Lab
# Modified by I. Kavcic, Met Office
# Modified by J. Henrichs, Bureau of Meteorology
# Modified by S. Siso and N. Nobre, STFC Daresbury Lab

'''File containing a PSyclone transformation script for the LFRic
API to apply loop fusion generically. Fusion is attempted for all
adjacent loops at the top level of a schedule. It will not fuse loops
that are lower in the schedule e.g. coloured loops. This can be
applied via the -s option in the psyclone script.

'''
from psyclone.domain.lfric.transformations import LFRicLoopFuseTrans
from psyclone.transformations import TransformationError


def trans(psyir):
    '''PSyclone transformation script for the LFRic API to apply loop
    fusion generically to all top level loops.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    total_fused = 0
    lf_trans = LFRicLoopFuseTrans()

    for subroutine in psyir.children[0].children:
        # Loop over all nodes in reverse order
        idx = len(subroutine.children) - 1
        while idx > 0:
            node = subroutine.children[idx]
            prev_node = subroutine.children[idx-1]
            try:
                lf_trans.apply(prev_node, node, {"same_space": True})
                total_fused += 1
            except TransformationError:
                pass
            idx -= 1

    print(f"Fused {total_fused} loops")
