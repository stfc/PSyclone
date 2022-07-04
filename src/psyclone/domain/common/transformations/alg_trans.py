# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''Specialise generic PSyIR representing an algorithm layer to a
PSyclone algorithm-layer-specific PSyIR which uses specialised classes.

'''
from psyclone.domain.common.transformations import RaiseCall2InvokeTrans
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import Call, Routine, Container
from psyclone.psyir.transformations import TransformationError


class AlgTrans(Transformation):
    '''Transform a generic PSyIR representation of the Algorithm layer to
    a PSyclone version with specialised domain-specific nodes.

    '''
    def __init__(self):
        self._invoke_trans = RaiseCall2InvokeTrans()

    def validate(self, node, options=None):
        '''Validate the supplied PSyIR tree.

        :param node: a PSyIR node that is the root of a PSyIR tree.
        :type node: :py:class:`psyclone.psyir.node.Routine` or \
            :py:class:`psyclone.psyir.node.Container`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, str]]

        :raises TransformationError: if the supplied node argument is \
            not a Routine or a Container.
        :raises TransformationError: if the supplied node argument has \
            a parent.

        '''
        if not isinstance(node, (Routine, Container)):
            raise TransformationError(
                "Error in {0} transformation. The supplied call argument "
                "should be a Routine or Container node but found '{1}'."
                "".format(self.name, type(node).__name__))
        if node.parent:
            raise TransformationError(
                "Error in {0} transformation. The supplied node should be the "
                "root of a PSyIR tree but this node has a parent."
                "".format(self.name))

    def apply(self, psyir, options=None):
        ''' Apply transformation to the supplied PSyIR node.

        :param node: a PSyIR node that is the root of a PSyIR tree.
        :type node: :py:class:`psyclone.psyir.node.Routine` or \
            :py:class:`psyclone.psyir.node.Container`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        '''
        self.validate(psyir, options=options)
        idx = 0
        for call in psyir.walk(Call):
            if call.routine.name.lower() == "invoke":
                self._invoke_trans.apply(call, idx, options=options)
                idx += 1


__all__ = ['AlgTrans']
