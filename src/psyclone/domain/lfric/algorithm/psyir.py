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

'''This module contains algorithm-layer-specific PSyIR classes

'''
from psyclone.psyir.nodes import Call, Node


class AlgorithmInvokeCall(Call):
    '''An invoke call in the Algorithm layer'''
    _children_valid_format = "[KernelLayerCall]*"
    _text_name = "AlgorithmInvokeCall"
    _colour_key = "Container"  # green

    def __init__(self, routine, parent=None, description=None):
        self._description = description
        super(AlgorithmInvokeCall, self).__init__(routine, parent=parent)

    def node_str(self, colour=True):
        '''
        Construct a text representation of this node, optionally containing
        colour control codes.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this PSyIR node.
        :rtype: str

        '''
        return "{0}[description=\"{1}\"]".format(self.coloured_name(colour), self._description)

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :returns: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return isinstance(child, KernelLayerCall)


class KernelLayerCall(Call):
    '''A call to a coded kernel or builtin from an invoke call in the
    Algorithm layer

    '''
    _colour_key = "Container"  # green


class BuiltinCall(KernelLayerCall):
    '''A call to a builtin from an invoke call in the Algorithm layer'''
    _text_name = "BuiltinCall"


class CodedCall(KernelLayerCall):
    '''A call to a coded kernel from an invoke call in the Algorithm
    layer

    '''
    _text_name = "CodedCall"


class KernelLayerArgument(Node):
    '''A call to a coded kernel or builtin from an invoke call in the
    Algorithm layer

    '''
    pass
