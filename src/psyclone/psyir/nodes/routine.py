# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified by: R. W. Ford, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the Routine node implementation.'''

import six

from psyclone.psyir.nodes.schedule import Schedule
from psyclone.psyir.symbols import DataSymbol, RoutineSymbol, DeferredType
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.symbols.symboltable import SymbolTable
from psyclone.psyir.nodes.commentable_mixin import CommentableMixin


class Routine(Schedule, CommentableMixin):
    '''
    A sub-class of a Schedule that represents a subroutine, function or
    program unit.

    :param str name: the name of this routine.
    :param bool is_program: whether this Routine represents the entry point \
                            into a program (e.g. Fortran Program or C main()).
    :param parent: the parent node of this Routine node in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node` or NoneType

    :raises TypeError: if any of the supplied arguments are of the wrong type.

    '''
    # Textual description of the node.
    _children_valid_format = "[Statement]*"
    _text_name = "Routine"

    def __init__(self, name, is_program=False, parent=None):
        super(Routine, self).__init__(parent=parent)

        # Name is set-up by the name setter property
        self._name = None
        self.name = name
        self._return_symbol = None

        if not isinstance(is_program, bool):
            raise TypeError("Routine argument 'is_program' must be a bool but "
                            "got '{0}'".format(type(is_program).__name__))
        self._is_program = is_program

    @classmethod
    def create(cls, name, symbol_table, children, is_program=False,
               return_symbol=None):
        '''Create an instance of the supplied class given a name, a symbol
        table and a list of child nodes. This is implemented as a classmethod
        so that it is able to act as a Factory for subclasses - e.g. it
        will create a KernelSchedule if called from KernelSchedule.create().

        :param str name: the name of the Routine (or subclass).
        :param symbol_table: the symbol table associated with this Routine.
        :type symbol_table: :py:class:`psyclone.psyGen.SymbolTable`
        :param children: a list of PSyIR nodes contained in the Routine.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`
        :param bool is_program: whether this Routine represents the entry \
            point into a program (i.e. Fortran Program or C main()).
        :param return_symbol: the Symbol that holds the return value of this \
            routine (if any). Must be present in the supplied symbol_table.
        :type return_symbol: :py:class:`psyclone.psyir.symbols.DataType` or \
            NoneType

        :returns: an instance of `cls`.
        :rtype: :py:class:`psyclone.psyGen.Routine` or subclass

        :raises TypeError: if the arguments to the create method \
            are not of the expected type.

        '''
        if not isinstance(name, six.string_types):
            raise TypeError(
                "name argument in create method of Routine class "
                "should be a string but found '{0}'."
                "".format(type(name).__name__))
        if not isinstance(symbol_table, SymbolTable):
            raise TypeError(
                "symbol_table argument in create method of Routine "
                "class should be a SymbolTable but found '{0}'."
                "".format(type(symbol_table).__name__))
        if not isinstance(children, list):
            raise TypeError(
                "children argument in create method of Routine class "
                "should be a list but found '{0}'."
                "".format(type(children).__name__))
        for child in children:
            if not isinstance(child, Node):
                raise TypeError(
                    "child of children argument in create method of "
                    "Routine class should be a PSyIR Node but "
                    "found '{0}'.".format(type(child).__name__))

        kern = cls(name)
        # pylint: disable=protected-access
        kern._is_program = is_program
        kern._symbol_table = symbol_table
        symbol_table._node = kern
        kern.children = children
        if return_symbol:
            kern.return_symbol = return_symbol
        return kern

    def node_str(self, colour=True):
        ''' Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return self.coloured_name(colour) + "[name:'" + self.name + "']"

    @property
    def dag_name(self):
        '''
        :returns: the name of this node in the dag.
        :rtype: str
        '''
        return "_".join(["routine", self.name, str(self.START_POSITION)])

    @property
    def name(self):
        '''
        :returns: the name of this Routine.
        :rtype: str
        '''
        return self._name

    @name.setter
    def name(self, new_name):
        '''
        Sets a new name for the Routine.

        TODO #1200 this node should only hold a reference to the corresponding
        RoutineSymbol and get its name from there.

        :param str new_name: new name for the Routine.

        :raises TypeError: if new_name is not a string.

        '''
        if not isinstance(new_name, six.string_types):
            raise TypeError("Routine name must be a str but got "
                            "'{0}'".format(type(new_name).__name__))
        if not self._name:
            self._name = new_name
            # TODO #1200 naming the routine should not create a symbol and
            # assign it a type!
            self.symbol_table.add(
                RoutineSymbol(new_name, DeferredType()),
                tag='own_routine_symbol')
        elif self._name != new_name:
            old_symbol = self.symbol_table.lookup(self._name)
            self.symbol_table.remove(old_symbol)
            self._name = new_name
            # TODO #1200 naming the routine should not create a symbol and
            # assign it a type!
            self.symbol_table.add(
                RoutineSymbol(new_name, old_symbol.datatype),
                tag='own_routine_symbol')

    def __str__(self):
        result = self.node_str(False) + ":\n"
        for entity in self._children:
            result += str(entity) + "\n"
        result += "End " + self.coloured_name(False)
        return result

    @property
    def is_program(self):
        '''
        :returns: whether this Routine represents the entry point into a \
                  program (e.g. is a Fortran Program or a C main()).
        :rtype: bool
        '''
        return self._is_program

    @property
    def return_symbol(self):
        '''
        :returns: the symbol which will hold the return value of this Routine \
                  or None if the Routine is not a function.
        :rtype: :py:class:`psyclone.psyir.symbols.DataSymbol` or NoneType
        '''
        return self._return_symbol

    @return_symbol.setter
    def return_symbol(self, value):
        '''
        Setter for the return-symbol of this Routine node.

        :param value: the symbol holding the value that the routine returns.
        :type value: :py:class:`psyclone.psyir.symbols.DataSymbol`

        :raises TypeError: if the supplied value is not a DataSymbol.
        :raises KeyError: if the supplied symbol is not a local entry in the \
                          symbol table of this Routine.
        '''
        if not isinstance(value, DataSymbol):
            raise TypeError("Routine return-symbol should be a DataSymbol "
                            "but found '{0}'".format(
                                type(value).__name__))
        if value not in self.symbol_table.local_datasymbols:
            raise KeyError(
                "For a symbol to be a return-symbol, it must be present in "
                "the symbol table of the Routine but '{0}' is not.".format(
                    value.name))
        self._return_symbol = value


# For automatic documentation generation
__all__ = ["Routine"]
