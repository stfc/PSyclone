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

'''This module contains LFRic Algorithm-layer-specific PSyIR classes.

'''
from psyclone.domain.common.algorithm import (AlgorithmInvokeCall,
                                              KernelFunctor)


class LFRicAlgorithmInvokeCall(AlgorithmInvokeCall):
    '''An invoke call in an LFRic Algorithm layer.

    :param routine: the routine that this call calls.
    :type routine: py:class:`psyclone.psyir.symbols.RoutineSymbol`
    :param parent: parent of this node in the PSyIR.
    :type parent: sub-class of :py:class:`psyclone.psyir.nodes.Node`
    :param str description: xxx

    '''
    _text_name = "LFRicAlgorithmInvokeCall"

    def __init__(self, routine, parent=None, description=None):
        self._description = description
        super(LFRicAlgorithmInvokeCall, self).__init__(routine, parent=parent)

    @classmethod
    def create(cls, routine, arguments, description=None):
        '''Create an instance of class cls given valid instances of a routine
        symbol, a list of child nodes for its arguments and an
        optional name.

        :param routine: the routine that class cls calls.
        :type routine: py:class:`psyclone.psyir.symbols.RoutineSymbol`
        :param arguments: the arguments to this routine. These are \
            added as child nodes.
        :type arguments: list of :py:class:`psyclone.psyir.nodes.DataNode`
        :param description: a string describing the purpose of the \
            invoke or None if one is not provided. This is used to \
            create the name of the subroutine that replaces the \
            invoke. Defaults to None.
        :type name: str or NoneType

        :returns: an instance of cls.
        :rtype: \
            :py:class:`psyclone.psyir.nodes.LFRicAlgorithmInvokeCall` or a \
            subclass thereof.

        '''
        instance = super(LFRicAlgorithmInvokeCall, cls).create(
            routine, arguments)
        instance._description = description
        return instance

    def node_str(self, colour=True):
        '''Construct a text representation of this node, optionally
        containing colour control codes. Specialise as this node has
        an additional description argument.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this PSyIR node.
        :rtype: str

        '''
        return "{0}[description=\"{1}\"]".format(self.coloured_name(colour),
                                                 self._description)


class LFRicBuiltinFunctor(KernelFunctor):
    '''Object containing an LFRic builtin call, a description of its
    required interface and the arguments to be passed to it.

    '''
    _text_name = "LFRicBuiltinFunctor"


class LFRicKernelFunctor(KernelFunctor):
    '''Object containing an LFRic kernel call, a description of its
    required interface and the arguments to be passed to it.

    '''
    _text_name = "LFRicKernelFunctor"
