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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified by: R. W. Ford, STFC Daresbury Lab
# Modified by: S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the Routine node implementation.'''

from psyclone.psyir.nodes.schedule import Schedule
from psyclone.psyir.symbols import DataSymbol, RoutineSymbol, NoType
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
    :param kwargs: additional keyword arguments provided to the super class.
    :type kwargs: unwrapped dict.

    :raises TypeError: if any of the supplied arguments are of the wrong type.

    '''
    # Textual description of the node.
    _children_valid_format = "[Statement]*"
    _text_name = "Routine"

    def __init__(self, name, is_program=False, **kwargs):
        super().__init__(**kwargs)

        self._return_symbol = None
        self._name = None
        # Name is set-up by the name setter property
        self.name = name

        if not isinstance(is_program, bool):
            raise TypeError(f"Routine argument 'is_program' must be a bool "
                            f"but got '{type(is_program).__name__}'")
        self._is_program = is_program

    def __eq__(self, other):
        '''
        Checks whether two nodes are equal. Two Routine nodes are equal
        if they haev the same name, same return symbol, same is_program and
        the inherited __eq__ is True.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.name == other.name
        is_eq = is_eq and self.is_program == other.is_program
        is_eq = is_eq and self.return_symbol == other.return_symbol

        return is_eq

    @classmethod
    def create(cls, name, symbol_table, children, is_program=False,
               return_symbol_name=None):
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
        :param str return_symbol_name: name of the symbol that holds the \
            return value of this routine (if any). Must be present in the \
            supplied symbol table.

        :returns: an instance of `cls`.
        :rtype: :py:class:`psyclone.psyGen.Routine` or subclass

        :raises TypeError: if the arguments to the create method \
            are not of the expected type.

        '''
        if not isinstance(name, str):
            raise TypeError(
                f"name argument in create method of Routine class "
                f"should be a string but found '{type(name).__name__}'.")
        if not isinstance(symbol_table, SymbolTable):
            raise TypeError(
                f"symbol_table argument in create method of Routine class "
                f"should be a SymbolTable but found "
                f"'{type(symbol_table).__name__}'.")
        if not isinstance(children, list):
            raise TypeError(
                f"children argument in create method of Routine class "
                f"should be a list but found '{type(children).__name__}'.")
        for child in children:
            if not isinstance(child, Node):
                raise TypeError(
                    f"child of children argument in create method of "
                    f"Routine class should be a PSyIR Node but "
                    f"found '{type(child).__name__}'.")

        routine = cls(name, is_program=is_program, symbol_table=symbol_table)
        routine.children = children
        if return_symbol_name:
            routine.return_symbol = routine.symbol_table.lookup(
                                       return_symbol_name, scope_limit=routine)
        return routine

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
        :raises KeyError: if there already is a different named symbol with \
            the 'own_routine_tag' in the symbol_table.

        '''
        if not isinstance(new_name, str):
            raise TypeError(f"Routine name must be a str but got "
                            f"'{type(new_name).__name__}'")
        # TODO #1200 The name is duplicated in the _name attribute and the
        # symbol.name that there is in the local symbol table. This setter
        # updates both but note that a better solution is needed because
        # renaming the symbol_table symbol alone would make it inconsistent.
        if not self._name:
            # If the 'own_routine_symbol' tag already exist check that is
            # consistent with the given routine name.
            if 'own_routine_symbol' in self.symbol_table.tags_dict:
                existing_symbol = self.symbol_table.lookup_with_tag(
                        'own_routine_symbol', scope_limit=self)
                if existing_symbol.name.lower() == new_name.lower():
                    self._name = new_name
                    return  # The preexisting symbol already matches
                # Otherwise raise an exception
                raise KeyError(
                    f"Can't assign {new_name} as the routine name because "
                    f"its symbol table contains a symbol ({existing_symbol}) "
                    f"already tagged as 'own_routine_symbol'.")

            self._name = new_name
            # Since the constructor can not mark methods as functions directly
            # the symbol will always start being NoType and must be updated
            # if a return_value type is provided.
            self.symbol_table.add(RoutineSymbol(new_name, NoType()),
                                  tag='own_routine_symbol')
        elif self._name != new_name:
            symbol = self.symbol_table.lookup(self._name)
            self._name = new_name
            self.symbol_table.rename_symbol(symbol, new_name)

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
            raise TypeError(f"Routine return-symbol should be a DataSymbol "
                            f"but found '{type(value).__name__}'.")
        if value not in self.symbol_table.local_datasymbols:
            raise KeyError(
                f"For a symbol to be a return-symbol, it must be present in "
                f"the symbol table of the Routine but '{value.name}' is not.")
        self._return_symbol = value
        # The routine symbol must be updated accordingly, this is because the
        # function datatype is provided by the type of the return symbol which
        # may be given after the Routine is created.
        self.symbol_table.lookup(self._name).datatype = value.datatype

    def _refine_copy(self, other):
        ''' Refine the object attributes when a shallow copy is not the most
        appropriate operation during a call to the copy() method.

        :param other: object we are copying from.
        :type other: :py:class:`psyclone.psyir.node.Routine`

        '''
        super()._refine_copy(other)
        if other.return_symbol is not None:
            self.return_symbol = self.symbol_table.lookup(
                    other.return_symbol.name)


# For automatic documentation generation
__all__ = ["Routine"]
