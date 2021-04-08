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

'''This module contains PSyclone Algorithm-layer-specific PSyIR classes.

'''
from psyclone.psyir.nodes import Call, Reference, DataNode
from psyclone.psyir.symbols import TypeSymbol
from psyclone.errors import GenerationError


class AlgorithmInvokeCall(Call):
    '''An invoke call in a PSyclone Algorithm layer.'''

    _children_valid_format = "[KernelFunctor]*"
    _text_name = "AlgorithmInvokeCall"
    _colour = "green"

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :returns: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return isinstance(child, KernelFunctor)


class KernelFunctor(Reference):
    '''Object containing a kernel call, a description of its required
    interface and the arguments to be passed to it.

    :param symbol: the functor symbol.
    :type symbol: :py:class:`psyclone.psyir.symbols.Symbol`
    :param parent: the parent node of this functor instance.
    :type parent: :py:class:`psyclone.psyir.nodes.Node` or NoneType

    '''
    _children_valid_format = "[DataNode]*"
    _text_name = "KernelFunctor"

    def __init__(self, symbol, parent=None):
        # pylint: disable=super-with-arguments
        super(KernelFunctor, self).__init__(symbol, parent=parent)

        if not isinstance(symbol, TypeSymbol):
            raise TypeError(
                "KernelFunctor symbol argument should be a TypeSymbol but "
                "found '{0}'.".format(type(symbol).__name__))

    @classmethod
    def create(cls, symbol, arguments):
        '''Create an instance of the calling class given valid instances of a
        TypeSymbol, and a list of child nodes for its arguments.

        :param symbol: the name of the kernel type that this object \
            references.
        :type symbol: py:class:`psyclone.psyir.symbols.TypeSymbol`
        :param arguments: the arguments to this routine. These are \
            added as child nodes.
        :type arguments: list of :py:class:`psyclone.psyir.nodes.DataNode`

        :returns: an instance of the calling class.
        :rtype: :py:class:`psyclone.psyir.nodes.Call` or subclass thereof.

        '''
        if not isinstance(symbol, TypeSymbol):
            raise GenerationError(
                "KernelFunctor create() symbol argument should be a "
                "TypeSymbol but found '{0}'.".format(type(symbol).__name__))
        if not isinstance(arguments, list):
            raise GenerationError(
                "KernelFunctor create() arguments argument should be a list "
                "but found '{0}'.".format(type(arguments).__name__))

        call = cls(symbol)
        call.children = arguments
        return call

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return isinstance(child, DataNode)

    def node_str(self, colour=True):
        '''
        Construct a text representation of this node, optionally containing
        colour control codes.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this PSyIR node.
        :rtype: str

        '''
        return "{0}[name='{1}']".format(
            self.coloured_name(colour), self.symbol.name)


__all__ = [
    'AlgorithmInvokeCall',
    'KernelFunctor']
