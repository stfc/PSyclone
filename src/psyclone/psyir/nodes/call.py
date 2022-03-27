# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2022, Science and Technology Facilities Council.
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
# Authors R. W. Ford and S. Siso STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the Call node implementation.'''

from __future__ import absolute_import
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.symbols import RoutineSymbol
from psyclone.errors import GenerationError


class Call(Statement, DataNode):
    ''' Node representing a Call. This can be found as a standalone statement
    or an expression.

    TODO #1437: The combined Statement and Expression implementation is simple
    but it has some shortcoming that may need to be addressed.

    :param routine: the routine that this call calls.
    :type routine: py:class:`psyclone.psyir.symbols.RoutineSymbol`
    :param parent: parent of this node in the PSyIR.
    :type parent: sub-class of :py:class:`psyclone.psyir.nodes.Node`

    :raises TypeError: if the routine argument is not a RoutineSymbol.

    '''
    # Textual description of the node.
    _children_valid_format = "[DataNode]*"
    _text_name = "Call"
    _colour = "cyan"

    def __init__(self, routine, parent=None):
        super(Call, self).__init__(parent=parent)

        if not isinstance(routine, RoutineSymbol):
            raise TypeError(
                f"Call routine argument should be a RoutineSymbol but found "
                f"'{type(routine).__name__}'.")

        self._routine = routine
        self._named_args = []

    @classmethod
    def create(cls, routine, arguments):
        '''Create an instance of class cls given valid instances of a routine
        symbol, and a list of child nodes (or name and node tuple) for
        its arguments.

        :param routine: the routine that class cls calls.
        :type routine: py:class:`psyclone.psyir.symbols.RoutineSymbol`
        :param arguments: the arguments to this routine, and/or \
            2-tuples containing an argument name and the \
            argument. Arguments are added as child nodes.
        :type arguments: List[ \
            Union[:py:class:``psyclone.psyir.nodes.DataNode``, \
                  Tuple[str, :py:class:``psyclone.psyir.nodes.DataNode``]]]

        :returns: an instance of cls.
        :rtype: :py:class:`psyclone.psyir.nodes.Call` or a subclass thereof.

        :raises GenerationError: if the routine argument is not a \
            RoutineSymbol.
        :raises GenerationError: if the arguments argument is not a \
            list.
        :raises GenerationError: if the contents of the arguments \
            argument are not the expected type.

        '''
        if not isinstance(routine, RoutineSymbol):
            raise GenerationError(
                f"Call create routine argument should be a RoutineSymbol but "
                f"found '{type(routine).__name__}'.")
        if not isinstance(arguments, list):
            raise GenerationError(
                f"Call create arguments argument should be a list but found "
                f"'{type(arguments).__name__}'.")

        call = cls(routine)
        for arg in arguments:
            name = None
            if isinstance(arg, tuple):
                if not len(arg) == 2:
                    raise GenerationError(
                        f"If a child of the children argument in create "
                        f"method of Call class is a tuple, it's "
                        f"length should be 2, but found {len(arg)}.")
                if not isinstance(arg[0], str):
                    raise GenerationError(
                        f"If a child of the children argument in create "
                        f"method of Call class is a tuple, its first "
                        f"argument should be a str, but found "
                        f"{type(arg[0]).__name__}.")
                name, arg = arg
            call.append_named_arg(name, arg)
        return call

    def append_named_arg(self, name, arg):
        '''Append a named argument to this call.

           :param name: the argument name.
           :type name: str or NoneType
           :param arg: the argument expression.
           :type arg: :py:class:`psyclone.psyir.nodes.DataNode`

           :raises TypeError: if the name argument is the wrong type.
           :raises ValueError: if the name argument is already used \
               for an existing argument.

        '''
        if name is not None:
            if not isinstance(name, str):
                raise TypeError(
                    f"The 'name' argument in 'append_named_arg' in the "
                    f"'Call' node should be a string or None, but found "
                    f"{type(name).__name__}.")
            for check_name in self.named_args:
                if check_name and check_name.lower() == name.lower():
                    raise ValueError(
                        f"The value of the name argument ({name}) in "
                        f"'append_named_arg' in the 'Call' node is "
                        f"already used for a named argument.")
        self._named_args.append((id(arg), name))
        self.children.append(arg)

    def insert_named_arg(self, name, arg, index):
        '''Insert a named argument to the call.

           :param name: the argument name.
           :type name: str or NoneType
           :param arg: the argument expression.
           :type arg: :py:class:`psyclone.psyir.nodes.DataNode`
           :param int index: where in the argument list to insert the \
               named argument.

           :raises TypeError: if the name argument is the wrong type.
           :raises ValueError: if the name argument is already used \
               for an existing argument.
           :raises TypeError: if the index argument is the wrong type.

        '''
        if name is not None:
            if not isinstance(name, str):
                raise TypeError(
                    f"The 'name' argument in 'insert_named_arg' in the "
                    f"'Call' node should be a string or None, but found "
                    f"{type(name).__name__}.")
            for check_name in self.named_args:
                if check_name and check_name.lower() == name.lower():
                    raise ValueError(
                        f"The value of the name argument ({name}) in "
                        f"'insert_named_arg' in the 'Call' node is "
                        f"already used for a named argument.")
        if not isinstance(index, int):
            raise TypeError(
                f"The 'index' argument in 'insert_named_arg' in the "
                f"'Call' node should be an int but found "
                f"{type(index).__name__}.")
        self._named_args.insert(index, (id(arg), name))
        self.children.insert(index, arg)

    def replace_named_arg(self, existing_name, arg):
        '''Replace one named argument with another for a Call node.

           :param str existing_name: the argument name.
           :param arg: the argument expression.
           :type arg: :py:class:`psyclone.psyir.nodes.DataNode`

           :raises TypeError: if the name argument is the wrong type.
           :raises ValueError: if the name argument is already used \
               for an existing argument.
           :raises TypeError: if the index argument is the wrong type.

        '''
        if not isinstance(existing_name, str):
            raise TypeError(
                f"The 'name' argument in 'replace_named_arg' in the "
                f"'Call' node should be a string or None, but found "
                f"{type(existing_name).__name__}.")
        index = 0
        # pylint: disable=undefined-loop-variable
        for named_arg in self._named_args:
            if named_arg[1].lower() == existing_name:
                break
            index += 1
        else:
            raise ValueError(
                f"The value of the existing_name argument ({existing_name}) "
                f"in 'insert_named_arg' in the 'Call' node is not found "
                f"in the existing arguments.")
        self.children[index] = arg
        self._named_args[index] = (id(arg), named_arg[1])

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

    @property
    def routine(self):
        '''
        :returns: the routine symbol that this call calls.
        :rtype: py:class:`psyclone.psyir.symbols.RoutineSymbol`
        '''
        return self._routine

    @property
    def named_args(self):
        '''
        :returns: a list containing the names of named arguments. If the \
            entry is None then the argument is a positional argument.
        :rtype: List[Union[str, NoneType]]
        '''
        self.reconcile()
        return [entry[1] for entry in self._named_args]

    def reconcile(self):
        '''update the _named_args values in case child arguments have been
        removed, added, re-ordered, or modified.

        '''
        new_named_args = []
        for child in self.children:
            for arg in self._named_args:
                if id(child) == arg[0]:
                    new_named_args.append(arg)
                    break
            else:
                new_named_args.append((id(child), None))
        self._named_args = new_named_args

    def node_str(self, colour=True):
        '''
        Construct a text representation of this node, optionally containing
        colour control codes.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this PSyIR node.
        :rtype: str

        '''
        return f"{self.coloured_name(colour)}[name='{self.routine.name}']"

    def __str__(self):
        return self.node_str(False)

    def copy(self):
        '''Return a copy of this node. This is a bespoke implementation for
        a Call node that ensures that any internal id's are
        consistent before and after copying.

        :returns: a copy of this node and its children.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # ensure _named_args is consistent with actual arguments
        # before copying.
        self.reconcile()
        # copy
        new_copy = super(Call, self).copy()
        # Fix invalid id's in _named_args after copying.
        new_list = []
        for idx, child in enumerate(new_copy.children):
            my_tuple = (id(child), new_copy._named_args[idx][1])
            new_list.append(my_tuple)
        new_copy._named_args = new_list

        return new_copy
