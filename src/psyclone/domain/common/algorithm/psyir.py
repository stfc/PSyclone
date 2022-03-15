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
# Author R. W. Ford STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology

'''This module contains PSyclone Algorithm-layer-specific PSyIR classes.

'''

from psyclone.errors import GenerationError, InternalError
from psyclone.psyir.nodes import Call, Reference, DataNode, Literal, \
    ArrayReference, Routine, Container, FileContainer
from psyclone.psyir.symbols import DataTypeSymbol, ContainerSymbol, \
    ImportInterface, RoutineSymbol


class AlgorithmInvokeCall(Call):
    '''An invoke call in a PSyclone Algorithm layer.

    :param invoke_routine_symbol: the routine that this call calls.
    :type invoke_routine_symbol: \
        py:class:`psyclone.psyir.symbols.RoutineSymbol`
    :param int index: the position of this invoke call relative to \
        other invokes in the algorithm layer.
    :param parent: optional parent of this node in the PSyIR. Defaults \
        to None.
    :type parent: sub-class of :py:class:`psyclone.psyir.nodes.Node` \
        or NoneType
    :param name: an optional name, describing the \
        AlgorithmInvokeCall. Defaults to None.

    :type name: str or NoneType

    :raises TypeError: if the index argument is not an integer.
    :raises ValueError: if the index argument is negative.

    '''
    _children_valid_format = "[KernelFunctor]*"
    _text_name = "AlgorithmInvokeCall"
    _colour = "green"

    def __init__(self, invoke_routine_symbol, index, parent=None, name=None):
        super().__init__(invoke_routine_symbol, parent=parent)

        if not isinstance(index, int):
            raise TypeError(
                f"AlgorithmInvokeCall index argument should be an int but "
                f"found '{type(index).__name__}'.")
        if index < 0:
            raise ValueError(
                f"AlgorithmInvokeCall index argument should be a non-negative "
                f"integer but found {index}.")
        if name and not isinstance(name, str):
            raise TypeError(
                f"AlgorithmInvokeCall name argument should be a str but "
                f"found '{type(name).__name__}'.")
        self._index = index
        # Keep the root names as these will also be needed by the
        # PSy-layer to use as tags to pull out the actual names from
        # the algorithm symbol table, once issue #753 is complete.
        self._psylayer_routine_root_name = None
        self._psylayer_container_root_name = None
        self._name = name

    @classmethod
    def create(cls, routine, arguments, index, name=None):
        # pylint: disable=arguments-differ
        '''Create an instance of the calling class given valid instances of a
        routine symbol, a list of child nodes for its arguments, an
        index and an optional name.

        :param routine: the routine that the calling class calls.
        :type routine: py:class:`psyclone.psyir.symbols.RoutineSymbol`
        :param arguments: the arguments to this routine. These are \
            added as child nodes.
        :type arguments: list of :py:class:`psyclone.psyir.nodes.DataNode`
        :param int index: the position of this invoke call relative to \
            other invokes in the algorithm layer.
        :param name: a string describing the purpose of the invoke or \
            None if one is not provided. This is used to create the \
            name of the routine that replaces the invoke. Defaults to \
            None.
        :type name: str or NoneType

        :raises GenerationError: if the arguments argument is not a \
            list.

        :returns: an instance of the calling class.
        :rtype: :py:class:`psyclone.psyir.nodes.AlgorithmInvokeCall` \
            or a subclass thereof.

        '''
        if not isinstance(arguments, list):
            raise GenerationError(
                f"AlgorithmInvokeCall create arguments argument should be a "
                f"list but found '{type(arguments).__name__}'.")

        call = cls(routine, index, name=name)
        call.children = arguments
        return call

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

    def node_str(self, colour=True):
        '''Construct a text representation of this node, optionally
        containing colour control codes. Specialise as this node has
        an additional name argument.

        :param bool colour: whether or not to include colour control \
            codes. Optional argument that defaults to True.

        :returns: description of this PSyIR node.
        :rtype: str

        '''
        return f"{self.coloured_name(colour)}[name=\"{self._name}\"]"

    @staticmethod
    def _def_container_root_name(node):
        '''
        :returns: the root name to use for the container.
        :rtype: str
        '''
        return f"psy_{node.name}"


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
        super().__init__(symbol, parent=parent)

        if not isinstance(symbol, DataTypeSymbol):
            raise TypeError(
                f"KernelFunctor symbol argument should be a DataTypeSymbol "
                f"but found '{type(symbol).__name__}'.")

    @classmethod
    def create(cls, symbol, arguments):
        '''Create an instance of the calling class given valid instances of a
        DataTypeSymbol and a list of child nodes for its arguments.

        :param symbol: the name of the kernel type that this object \
            references.
        :type symbol: py:class:`psyclone.psyir.symbols.DataTypeSymbol`
        :param arguments: the arguments to this routine. These are \
            added as child nodes.
        :type arguments: list of :py:class:`psyclone.psyir.nodes.DataNode`

        :returns: an instance of the calling class.
        :rtype: :py:class:`psyclone.psyir.nodes.Call` or subclass thereof.

        '''
        if not isinstance(symbol, DataTypeSymbol):
            raise GenerationError(
                f"KernelFunctor create() symbol argument should be a "
                f"DataTypeSymbol but found '{type(symbol).__name__}'.")
        if not isinstance(arguments, list):
            raise GenerationError(
                f"KernelFunctor create() arguments argument should be a list "
                f"but found '{type(arguments).__name__}'.")

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


__all__ = [
    'AlgorithmInvokeCall',
    'KernelFunctor']
