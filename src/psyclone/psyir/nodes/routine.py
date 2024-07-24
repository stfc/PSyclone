# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Modified by: J. Henrichs, Bureau of Meteorology
# Modified by: A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the Routine node implementation.'''

from psyclone.errors import GenerationError
from psyclone.psyir.nodes.commentable_mixin import CommentableMixin
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.nodes.schedule import Schedule
from psyclone.psyir.symbols import DataSymbol, RoutineSymbol
from psyclone.psyir.symbols.symbol_table import SymbolTable


class Routine(Schedule, CommentableMixin):
    '''
    A sub-class of a Schedule that represents a subroutine, function or
    program unit.

    :param symbol: the Symbol used to represent this Routine in its Container.
                   If it is not supplied a RoutineSymbol will be created with
                   default arguments instead.
    :type symbol: :py:class:`psyclone.psyir.symbols.RoutineSymbol`
    :param Optional[bool] is_program: whether this Routine represents the
                                      entry point into a program (e.g.
                                      Fortran Program or C main()). Default is
                                      False.
    :param kwargs: additional keyword arguments provided to the super class.
    :type kwargs: unwrapped dict.

    :raises TypeError: if any of the supplied arguments are of the wrong type.

    '''
    # Textual description of the node.
    _children_valid_format = "[Statement]*"
    _text_name = "Routine"

    def __init__(self, symbol, is_program=False,
                 symbol_tag=None, **kwargs):
        # These attributes need to be set before anything, as the _symbol
        # is required for setting the _parent, which is overriden by Routine
        if not isinstance(symbol, RoutineSymbol):
            raise TypeError(f"Routine argument 'symbol' must be present and "
                            f"must be a RoutineSymbol but got "
                            f"'{type(symbol).__name__}'")
        self._symbol = symbol
        self._symbol_tag = symbol_tag
        self._symbol_in_table = False
        self._parent_node = None
        super().__init__(**kwargs)

        self._return_symbol = None
        if not isinstance(is_program, bool):
            raise TypeError(f"Routine argument 'is_program' must be a bool "
                            f"but got '{type(is_program).__name__}'")
        self._is_program = is_program

    def __eq__(self, other):
        '''
        Checks whether two nodes are equal. Two Routine nodes are equal
        if they have the same name, same return symbol, same properties and
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
    def create(cls, name, symbol_table=None, children=None, is_program=False,
               symbol=None, return_symbol_name=None, **kwargs):
        # pylint: disable=too-many-arguments
        '''Create an instance of the supplied class given a name, a symbol
        table and a list of child nodes. This is implemented as a classmethod
        so that it is able to act as a Factory for subclasses - e.g. it
        will create a KernelSchedule if called from KernelSchedule.create().

        :param str name: the name of the Routine (or subclass).
        :param symbol_table: the symbol table associated with this Routine.
        :type symbol_table: Optional[:py:class:`psyclone.psyGen.SymbolTable`]
        :param children: a list of PSyIR nodes contained in the Routine.
        :type children: Optional[list of :py:class:`psyclone.psyir.nodes.Node`]
        :param Optional[bool] is_program: whether this Routine represents the
                                          entry point into a program (e.g.
                                          Fortran Program or C main()). Default
                                          is False.
        :param symbol: the Symbol used to represent this Routine in its
                       Container. If it is not supplied a RoutineSymbol
                       will be created with default arguments instead.
        :type symbol: Optional[
                      :py:class:`psyclone.psyir.symbols.RoutineSymbol`]
        :param str return_symbol_name: name of the symbol that holds the \
            return value of this routine (if any). Must be present in the \
            supplied symbol table.

        :returns: an instance of `cls`.
        :rtype: :py:class:`psyclone.psyir.nodes.Routine` or subclass

        :raises TypeError: if the arguments to the create method \
            are not of the expected type.

        '''
        if not isinstance(name, str):
            raise TypeError(
                f"name argument in create method of Routine class "
                f"should be a string but found '{type(name).__name__}'.")
        if symbol_table and not isinstance(symbol_table, SymbolTable):
            raise TypeError(
                f"symbol_table argument in create method of Routine class "
                f"should be a SymbolTable but found "
                f"'{type(symbol_table).__name__}'.")
        if not children:
            children = []
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
        if symbol is not None and symbol.name != name:
            raise ValueError(
                f"name argument and symbol argument in create method "
                f"of Routine class should be the same, but found "
                f"{name} and {symbol.name}.")
        if symbol is None:
            symbol = RoutineSymbol(name)
        routine = cls(symbol, is_program=is_program, symbol_table=symbol_table,
                      **kwargs)
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
    def _parent(self):
        return self._parent_node

    @_parent.setter
    def _parent(self, parent):
        if self._parent_node is not None:
            try:
                self._parent_node.symbol_table.remove(self._symbol)
                self._symbol_in_table = False
            except ValueError:
                pass
        self._parent_node = parent
        if self._symbol and parent is not None:
            # If we weren't able to remove this symbol from a previous symbol
            # table then we need to create a new symbol for this Routine.
            if self._symbol_in_table:
                # Check if the symbol is already present in the parent's symbol
                # table. If it is and if its the symbol already associated with
                # this routine.
                try:
                    sym = parent.symbol_table.lookup(self.name)
                except KeyError:
                    sym = None
                if sym is self._symbol:
                    self._symbol_in_table = True
                else:
                    symbol_copy = self._symbol.copy()
                    self._symbol = symbol_copy
                    parent.symbol_table.add(self._symbol, self._symbol_tag)
                    self._symbol_in_table = True
            else:
                parent.symbol_table.add(self._symbol, self._symbol_tag)
                self._symbol_in_table = True

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
        return self._symbol.name

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
        if self.name != new_name:
            symbol = self._symbol
            container_sym_tab = symbol.find_symbol_table(self)
            if container_sym_tab is not None:
                container_sym_tab.rename_symbol(symbol, new_name)
            else:
                # Symbol isn't in a symbol table so we can modify its name.
                symbol._name = new_name

    def __str__(self):
        result = self.node_str(False) + ":\n"
        for entity in self._children:
            result += str(entity) + "\n"
        result += "End " + self.coloured_name(False)
        return result

    @property
    def symbol(self):
        '''
        :returns: the RoutineSymbol corresponding to this Routine.
        :rtype: :py:class:`psyclone.psyir.symbols.RoutineSymbol` or NoneType
        '''
        return self._symbol

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
        if (value not in self.symbol_table.datasymbols):
            raise KeyError(
                f"For a symbol to be a return-symbol, it must be present in "
                f"the symbol table of the Routine "
                f"but '{value.name}' is not.")
        self._return_symbol = value

    def _refine_copy(self, other):
        ''' Refine the object attributes when a shallow copy is not the most
        appropriate operation during a call to the copy() method.

        :param other: object we are copying from.
        :type other: :py:class:`psyclone.psyir.node.Routine`

        '''
        # Removing the parent_node knowledge from the default copy method
        # before calling the super, else in Node we set self._parent = None
        # and attempt to remove this symbol from a symbol table that doesn't
        # contain it.
        self._parent_node = None
        self._symbol = other._symbol.copy()
        super()._refine_copy(other)
        if other.return_symbol is not None:
            self.return_symbol = self.symbol_table.lookup(
                    other.return_symbol.name)

    def replace_with(self, node, keep_name_in_context=True):
        '''Removes self and its descendents from the PSyIR tree to which it
        is connected and replaces it with the supplied node (and its
        descendents).

        The node must be a Routine (or subclass) and has the same Symbol as
        self.

        keep_name_in_context is ignored for this replace_with implementation,
        however we keep the argument to match with the base implementation.

        :param node: the node that will replace self in the PSyIR tree.
        :type node: :py:class:`psyclone.psyir.nodes.Routine`
        :param bool keep_name_in_context: ignored.

        :raises TypeError: if the argument node is not a Routine.
        :raises GenerationError: if this node does not have a parent.
        :raises GenerationError: if the argument 'node' has a parent
        :raises GenerationError: if self and node do not have the same Symbol.
        '''
        if not isinstance(node, Routine):
            raise TypeError(
                f"The argument node in method replace_with in the Routine "
                f"class should be a Routine but found '{type(node).__name__}'."
            )
        if not self.parent:
            raise GenerationError(
                "This node should have a parent if its replace_with method "
                "is called.")
        if node.parent is not None:
            raise GenerationError(
                f"The parent of argument node in method replace_with in the "
                f"Routine class should be None but found "
                f"'{type(node.parent).__name__}'.")
        if self._symbol != node._symbol:
            raise GenerationError(
                "The symbol of argument node in method replace_with in the "
                "Routine class should be the same as the Routine being "
                "replaced.")
        # We want to not use the childrenList update information.
        node._symbol_in_table = self._symbol_in_table
        # Just calling node._parent_node.children.__setitem__ attempt
        # to update the _parent of the replacements. Due to the nature
        # of the replace_with and symbol functionality on Routines, this
        # will not work. Instead we use the list.__setitem__ method to
        # update this directly.
        super().replace_with(node, keep_name_in_context=keep_name_in_context)


# For automatic documentation generation
__all__ = ["Routine"]
