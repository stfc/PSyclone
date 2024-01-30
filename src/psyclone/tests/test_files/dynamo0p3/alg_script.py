# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
# Author R. W. Ford STFC Daresbury Lab

'''Null PSyclone transformation script that includes an example of the
optional 'trans_alg()' function. If included, the 'trans_alg()'
function allows modification of the algorithm layer.
'''


def trans_alg(psyir):
    '''Function to modify the algorithm layer PSyIR. This function is
    designed to be called by the psyclone script.

    :param psyir: algorithm-layer code represented in PSyIR.
    :type psyir: :class:py:`psyclone.psyir.nodes.Node`

    :returns: modified algorithm-layer code.
    :rtype: :class:py:`psyclone.psyir.nodes.Node`

    '''
    return psyir


def trans(psy):
    '''Function to modify the algorithm layer PSyIR. This function is
    designed to be called by the psyclone script.

    :param psy: PSyclone's representation of the PSy-layer code.
    :type psy: :class:py:`psyclone.dynamo0p3.DynamoPSy`

    :returns: modified algorithm-layer code.
    :rtype: :class:py:`psyclone.dynamo0p3.DynamoPSy`

    '''
    return psy
