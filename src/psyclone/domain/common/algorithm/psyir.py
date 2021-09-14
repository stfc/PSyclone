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
from __future__ import absolute_import
import six

from psyclone.psyir.nodes import Call, Reference, DataNode, Literal, \
    ArrayReference, Routine
from psyclone.psyir.symbols import DataTypeSymbol, ContainerSymbol, \
    ImportInterface, RoutineSymbol
from psyclone.errors import GenerationError


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
    :param description: an optional description of the \
        AlgorithmInvokeCall. Defaults to None.
    :type description: str or NoneType

    :raises TypeError: if the index argument is not an integer.
    :raises ValueError: if the index argument is negative.

    '''
    _children_valid_format = "[KernelFunctor]*"
    _text_name = "AlgorithmInvokeCall"
    _colour = "green"

    def __init__(self, invoke_routine_symbol, index, parent=None,
                 description=None):
        super(AlgorithmInvokeCall, self).__init__(
            invoke_routine_symbol, parent=parent)

        if not isinstance(index, int):
            raise TypeError(
                "AlgorithmInvokeCall index argument should be an int but "
                "found '{0}'.".format(type(index).__name__))
        if index < 0:
            raise ValueError(
                "AlgorithmInvokeCall index argument should be a non-negative "
                "integer but found {0}.".format(index))

        # In Python2 description may be unicode or str so check for
        # both (in a way that Python3 is happy with).
        if description and not isinstance(description, (str, six.text_type)):
            raise TypeError(
                "AlgorithmInvokeCall description argument should be a str but "
                "found '{0}'.".format(type(description).__name__))

        if description:
            # Avoid unicode issues with Python2
            description = str(description)
        self._index = index
        self.psylayer_routine_root_name = None
        self.psylayer_container_root_name = None
        self._description = description

    @classmethod
    def create(cls, routine, arguments, index, description=None):
        '''Create an instance of the calling class given valid instances of a
        routine symbol, a list of child nodes for its arguments, an
        index and an optional description.

        :param routine: the routine that the calling class calls.
        :type routine: py:class:`psyclone.psyir.symbols.RoutineSymbol`
        :param arguments: the arguments to this routine. These are \
            added as child nodes.
        :type arguments: list of :py:class:`psyclone.psyir.nodes.DataNode`
        :param int index: the position of this invoke call relative to \
            other invokes in the algorithm layer.
        :param description: a string describing the purpose of the \
            invoke or None if one is not provided. This is used to \
            create the name of the routine that replaces the \
            invoke. Defaults to None.
        :type description: str or NoneType

        :raises GenerationError: if the arguments argument is not a \
            list.

        :returns: an instance of the calling class.
        :rtype: :py:class:`psyclone.psyir.nodes.AlgorithmInvokeCall` \
            or a subclass thereof.

        '''
        if not isinstance(arguments, list):
            raise GenerationError(
                "AlgorithmInvokeCall create arguments argument should be a "
                "list but found '{0}'.".format(type(arguments).__name__))

        call = cls(routine, index, description=description)
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
        an additional description argument.

        :param bool colour: whether or not to include colour control \
            codes. Optional argument that defaults to True.

        :returns: description of this PSyIR node.
        :rtype: str

        '''
        return "{0}[description=\"{1}\"]".format(self.coloured_name(colour),
                                                 self._description)

    def _def_routine_root_name(self):
        '''Internal method that returns the proposed language-level routine
        name given the index of this invoke.

        :returns: the proposed processed routine name for this invoke.
        :rtype: str

        '''
        if self._description:
            routine_root_name = self._description.lower().strip()
            if routine_root_name[0] == '"' and routine_root_name[-1] == '"' \
               or \
               routine_root_name[0] == "'" and routine_root_name[-1] == "'":
                # fparser2 (issue #295) currently includes quotes as
                # part of a string, so strip them out.
                routine_root_name = routine_root_name[1:-1].strip()

            routine_root_name = routine_root_name.replace(" ", "_")
        else:
            routine_root_name = "invoke_{0}".format(self._index)
            if len(self.children) == 1:
                # Add the name of the kernel if there is only one call
                routine_root_name += "_" + self.children[0].name
        return routine_root_name

    def create_psylayer_symbol_root_names(self):
        '''If the PSy-layer routine and container root names have not been
        created, then create them. The invoke root name is based on
        the position of this node (compared to other nodes of the same
        type) in the PSyIR tree. Note, we do not create and store
        symbols, as the container name needs to be consistent between
        different invoke calls and we have no way of knowing whether
        one has already been created without the symbol being stored
        in the symbol table, and we don't want to add anything related
        to the lower level PSyIR to the symbol table before lowering.

        '''
        self.psylayer_routine_root_name = self._def_routine_root_name()
        # Use the name of the closest ancestor routine of this node as
        # the basis for the new container name
        node = self.ancestor(Routine, include_self=True)
        self.psylayer_container_root_name = "psy_{0}".format(node.name)

    def lower_to_language_level(self):
        '''Transform this node and its children into an appropriate Call
        node.

        '''
        self.create_psylayer_symbol_root_names()

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

        container_tag = self.psylayer_container_root_name
        try:
            container_symbol = symbol_table.lookup_with_tag(container_tag)
        except KeyError:
            container_symbol = symbol_table.new_symbol(
                root_name=container_tag, tag=container_tag,
                symbol_type=ContainerSymbol)

        routine_tag = self.psylayer_routine_root_name
        interface = ImportInterface(container_symbol)
        routine_symbol = symbol_table.new_symbol(
            root_name=routine_tag, tag=routine_tag, symbol_type=RoutineSymbol,
            interface=interface)

        call = Call.create(routine_symbol, arguments)
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

        if not isinstance(symbol, DataTypeSymbol):
            raise TypeError(
                "KernelFunctor symbol argument should be a DataTypeSymbol but "
                "found '{0}'.".format(type(symbol).__name__))

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
                "KernelFunctor create() symbol argument should be a "
                "DataTypeSymbol but found '{0}'.".format(
                    type(symbol).__name__))
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
