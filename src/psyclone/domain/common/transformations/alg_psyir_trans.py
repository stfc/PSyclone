# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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

'''Specialise generic PSyIR representing an algorithm layer to a
PSyclone algorithm-layer-specific PSyIR which uses specialised classes.

'''
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import Call
from psyclone.domain.common.transformations import AlgInvokeTrans


class AlgPSyIRTrans(Transformation):
    '''Transform a generic PSyIR representation of the Algorithm layer to
    a PSyclone version with specialised domain-specific nodes. '''

    def __init__(self, invoke_name="invoke"):
        ''' xxx '''
        self._invoke_trans = AlgInvokeTrans()
        self._invoke_name = invoke_name

    def validate(self, node, options=None):
        ''' validate before applying the transformation '''
        # Check node is a top-level PSyIR node
        pass

    def trans(self, node, options=None):
        ''' apply transformation to the supplied node '''

        self._validate(node, options=options)
        for call in psyir.walk(Call):
            if call.routine.name.lower() == self._invoke_name:
                self._invoke_trans.apply(call, options=options)
