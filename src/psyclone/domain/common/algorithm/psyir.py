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
from psyclone.psyir.nodes import Call, Reference, DataNode, Literal, \
    ArrayReference
from psyclone.psyir.symbols import TypeSymbol, ContainerSymbol, \
    GlobalInterface, RoutineSymbol
from psyclone.errors import GenerationError


class AlgorithmInvokeCall(Call):
    '''An invoke call in a PSyclone Algorithm layer.

    :param routine: the routine that this call calls.
    :type routine: py:class:`psyclone.psyir.symbols.RoutineSymbol`
    :param parent: parent of this node in the PSyIR.
    :type parent: sub-class of :py:class:`psyclone.psyir.nodes.Node`

    '''
    _children_valid_format = "[KernelFunctor]*"
    _text_name = "AlgorithmInvokeCall"
    _colour = "green"

    def __init__(self, routine, parent=None):
        super(AlgorithmInvokeCall, self).__init__(routine, parent=parent)
        self._routine_symbol = None
        self._container_symbol = None

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

    def create_language_level_symbols(self, index):
        '''Creates subroutine and module symbols whose names are based on the
        position of this node (compared to other nodes of the same
        type) in the PSyIR tree.

        :param int index: the position of this invoke call relative to \
            other invokes in the algorithm layer.

        :raises TypeError: if the index argument is not an integer.
        :raises ValueError: if the index argument is negative.

        '''
        if not isinstance(index, int):
            raise TypeError(
                "AlgorithmInvokeCall index argument should be an int but "
                "found '{0}'.".format(type(index).__name__))
        if index < 0:
            raise ValueError(
                "AlgorithmInvokeCall index argument should be a non-negative "
                "integer but found {0}.".format(index))

        subroutine_root_name = self._def_sub_root_name(index)

        symbol_table = self.scope.symbol_table
        subroutine_name = symbol_table.next_available_name(
            root_name=subroutine_root_name)

        module_root_name = "{0}_mod".format(subroutine_name)
        module_name = symbol_table.next_available_name(
            root_name=module_root_name)

        self._container_symbol = ContainerSymbol(module_name)
        interface = GlobalInterface(self._container_symbol)
        self._routine_symbol = RoutineSymbol(
            subroutine_name, interface=interface)

    def _def_sub_root_name(self, index):
        '''Internal function that returns the proposed processed subroutine
        name given the index of this invoke.

        :param int index: the position of this invoke call relative to \
            other invokes in the algorithm layer.

        :returns: the proposed processed subroutine name for this \
            invoke.
        :rtype: str

        '''
        subroutine_root_name = "invoke_{0}".format(index)
        if len(self.children) == 1:
            # Add the name of the kernel if there is only one call
            subroutine_root_name += "_" + self.children[0].name
        return subroutine_root_name

    def lower_to_language_level(self):
        '''Transform this node and its children into an appropriate Call
        node.

        :raises GenerationError: if the create_language_level_symbols \
            method has not been called.

        '''
        if not self._container_symbol or not self._routine_symbol:
            raise GenerationError(
                "The 'create_language_level_symbols()' method must be called "
                "before calling 'lower_to_language_level()'.")
        arguments = []
        arguments_str = []
        for kern in self.children:
            for arg in kern.children:
                if isinstance(arg, Literal):
                    # Literals are not passed by argument.
                    pass
                elif isinstance(arg, (Reference, ArrayReference)):
                    # TODO #753 use a better check for equivalence (math_equal)
                    if str(arg).lower() not in arguments_str:
                        arguments_str.append(str(arg).lower())
                        arguments.append(arg.copy())
                else:
                    raise GenerationError(
                        "Expected Algorithm-layer kernel arguments to be "
                        "a literal, reference or array reference, but "
                        "found '{0}'.".format(type(arg).__name__))

        symbol_table = self.scope.symbol_table
        symbol_table.add(self._container_symbol)
        symbol_table.add(self._routine_symbol)
        call = Call.create(self._routine_symbol, arguments)
        self.replace_with(call)


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
        for child in call.children:
            child.parent = call
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
