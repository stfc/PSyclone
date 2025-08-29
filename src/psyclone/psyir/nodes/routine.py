# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2025, Science and Technology Facilities Council.
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

from fparser.two import Fortran2003
from fparser.two.utils import walk

from psyclone.errors import GenerationError
from psyclone.psyir.nodes.codeblock import CodeBlock
from psyclone.psyir.commentable_mixin import CommentableMixin
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.nodes.schedule import Schedule
from psyclone.psyir.nodes.scoping_node import ScopingNode
from psyclone.psyir.symbols import (
    DataSymbol, DefaultModuleInterface,
    RoutineSymbol, SymbolError, UnresolvedInterface)
from psyclone.psyir.symbols.symbol_table import SymbolTable


class Routine(Schedule, CommentableMixin):
    '''
    A sub-class of a Schedule that represents a subroutine, function or
    program unit.

    :param symbol: the Symbol used to represent this Routine in its Container.
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

    def __init__(self, symbol, is_program=False, **kwargs):
        if not isinstance(symbol, RoutineSymbol):
            raise TypeError(f"Routine argument 'symbol' must be present and "
                            f"must be a RoutineSymbol but got "
                            f"'{type(symbol).__name__}'")
        # These attributes need to be set before anything, as the _symbol
        # is required for setting the parent links.
        self._parent = None
        self._symbol = symbol
        super().__init__(**kwargs)

        self._return_symbol = None
        if not isinstance(is_program, bool):
            raise TypeError(f"Routine argument 'is_program' must be a bool "
                            f"but got '{type(is_program).__name__}'")
        self._is_program = is_program

        # Add the symbol into the routine itself, unless the symbol table
        # already contains a symbol with that name (e.g. a Function's return
        # symbol).
        if self._symbol.name not in self._symbol_table:
            self.symbol_table.add(self._symbol)

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
               return_symbol_name=None):
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
        :param str return_symbol_name: name of the symbol that holds the
            return value of this routine (if any). Must be present in the
            supplied symbol table.

        :returns: an instance of `cls`.
        :rtype: :py:class:`psyclone.psyir.nodes.Routine` or subclass

        :raises TypeError: if the arguments to the create method
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

        symbol = RoutineSymbol(name)
        routine = cls(symbol, is_program=is_program, symbol_table=symbol_table)
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

    def check_outer_scope_accesses(self, call,
                                   kern_or_call: str,
                                   permit_unresolved: bool = True,
                                   ignore_non_data_accesses: bool = False):
        '''
        Check for unresolved symbols or for any declared in the outer scope
        Container of the target routine.

        :param call: the node representing the call to the routine that is to
            be inlined.
        :type call: Union[CodedKern, Call]
        :param kern_or_call: text appropriate to whether we have a PSyKAl
            Kernel or a generic routine.
        :param permit_unresolved: whether or not the presence of unresolved
            symbols will result in an error being raised.
        :param ignore_non_data_accesses: ignore unresolved symbols if they
            do not represent data accesses (e.g. provide type information).

        :raises SymbolError: if there is an access to an unresolved
            symbol and `permit_unresolved` is False.
        :raises SymbolError: if there is an access to a symbol that is
            declared in the parent scope of this routine.

        '''
        # TODO #2424 - this suffers from the limitation that
        # VariablesAccessMap does not work with nested scopes. (e.g. 2
        # different symbols with the same name but declared in different,
        # nested scopes will be assumed to be the same symbol).
        vam = self.reference_accesses()
        table = self.symbol_table
        name = self.name
        for sig in vam.all_signatures:
            symbol = table.lookup(sig.var_name, otherwise=None)
            if not symbol:
                raise SymbolError(
                    f"{kern_or_call} '{name}' contains accesses to "
                    f"'{sig.var_name}' but the origin of this signature is "
                    f"unknown.")
            if symbol.is_unresolved:
                routine_wildcards = table.wildcard_imports()
                # We can't be certain of the origin of this unresolved symbol.
                if permit_unresolved:
                    continue
                if (ignore_non_data_accesses and
                        not vam[sig].has_data_access()):
                    continue
                raise SymbolError(
                    f"{kern_or_call} '{name}' contains accesses to "
                    f"'{symbol.name}' which is unresolved. It is probably "
                    f"brought into scope from one of "
                    f"{[sym.name for sym in routine_wildcards]}. It may be"
                    f" resolved by adding these to RESOLVE_IMPORTS in the "
                    f"transformation script.")
            if not symbol.is_import and symbol.name not in table:
                sym_at_call_site = call.scope.symbol_table.lookup(
                    sig.var_name, otherwise=None)
                if sym_at_call_site is not symbol:
                    raise SymbolError(
                        f"{kern_or_call} '{name}' contains accesses to "
                        f"'{symbol.name}' which is declared in the callee "
                        f"module scope.")

        # We can't handle a clash between (apparently) different symbols that
        # share a name but are imported from different containers.
        routine_arg_list = self.symbol_table.argument_list[:]
        callsite_scopes = []
        cursor = call
        while cursor.ancestor(ScopingNode):
            callsite_scopes.append(cursor.ancestor(ScopingNode))
            cursor = cursor.ancestor(ScopingNode)
        for scope in self.walk(ScopingNode):
            scope_table = scope.symbol_table
            for callsite_scope in callsite_scopes:
                table = callsite_scope.symbol_table
                try:
                    table.check_for_clashes(
                        scope_table,
                        symbols_to_skip=routine_arg_list)
                except SymbolError as err:
                    raise SymbolError(
                        f"One or more symbols from routine '{name}' "
                        f"cannot be added to the table at the call site. "
                        f"Error was: {err}") from err

    def update_parent_symbol_table(self, new_parent):
        ''' Update the Routine's new parent's symbol tables with the
        corresponding RoutineSymbol.

        :param new_parent: The new parent of this node.
        :type new_parent: :py:class:`psyclone.psyir.nodes.ScopingNode`

        :raises GenerationError: if a symbol with the same name already exists
                                 in the scope.
        :raises GenerationError: if a Routine with the same name already
                                 exists in the scope.
        :raises GenerationError: if a Codeblock representing a routine with
                                 the same name already exists in the scope.
        '''
        if self._parent is not None:
            try:
                # TODO 2702: Need to check that this Routine's symbol is in
                # the current _parent symbol table, as otherwise this breaks
                # during copying.
                if (self._parent.symbol_table.lookup(self.name)
                        is self._symbol):
                    self._parent.symbol_table.remove(self._symbol)
            except ValueError:
                # It can't be removed so we make it Unresolved. This will be
                # undone when a Routine is re-attached.
                self._symbol.interface = UnresolvedInterface()
            except KeyError:
                pass
        elif new_parent is not None:
            # If the current parent is None and the new parent is not None
            # then we remove the RoutineSymbol from the Routine's symbol table
            # if it is present.
            try:
                if self.symbol_table.lookup(self.name) is self._symbol:
                    self.symbol_table.remove(self._symbol)
            except KeyError:
                pass
        if new_parent is not None:
            # If we weren't able to remove this symbol from a previous symbol
            # table then we may need to create a new symbol for this Routine.

            # The symbol may also be in scope, if it is then we check
            # whether the scope already has a Routine or CodeBlock
            # with this name and error if so.
            try:
                sym = new_parent.symbol_table.lookup(self.name,
                                                     scope_limit=new_parent)
                # If the found symbol is resolved and is not the symbol used
                # to initialise this Routine then we raise an error, as we
                # won't be able to add it to the parent.
                if sym is not self._symbol and not sym.is_unresolved:
                    raise GenerationError(
                        f"Can't add routine '{self.name}' into a scope "
                        f"that already contains a resolved symbol with "
                        f"the same name.")

                # Check that the scope doens't contain a Routine or
                # CodeBlock representing a Routine with this name.
                routines = new_parent.walk(Routine)
                for routine in routines:
                    # Ignore itself.
                    if routine is self:
                        continue
                    if routine.name == self.name:
                        raise GenerationError(
                                f"Can't add routine '{self.name}' into a "
                                f"scope that already contains a Routine "
                                f"with that name.")
                codeblocks = new_parent.walk(CodeBlock)
                for codeblock in codeblocks:
                    routines = walk(codeblock.get_ast_nodes,
                                    (Fortran2003.Subroutine_Subprogram,
                                     Fortran2003.Function_Subprogram))
                    for routine in routines:
                        name = str(routine.children[0].children[1])
                        if name == self.name:
                            raise GenerationError(
                                    f"Can't add routine '{self.name}' into"
                                    f" a scope that already contains a "
                                    f"CodeBlock representing a routine "
                                    f"with that name.")
            except KeyError:
                sym = self._symbol
            # If lookup found this Routine's symbol then we are performing
            # replace_with, which is handled here.
            if sym is self._symbol:
                try:
                    new_parent.symbol_table.lookup(self._symbol.name,
                                                   scope_limit=new_parent)
                except KeyError:
                    new_parent.symbol_table.add(self._symbol)
                # As we now have the RoutineSymbol back in a Container, we
                # can give it the right interface.
                self._symbol.interface = DefaultModuleInterface()

        elif self.symbol_table:
            # Otherwise if new_parent is None, then we place the symbol
            # into this Routine's symbol table if possible. Not all Routine
            # subclasses have a symbol table, hence the elif statement.
            try:
                self.symbol_table.lookup(self._symbol.name)
            except KeyError:
                self.symbol_table.add(self._symbol)

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
        :raises KeyError: if the symbol is not in the expected symbol table.

        '''
        if not isinstance(new_name, str):
            raise TypeError(f"Routine name must be a str but got "
                            f"'{type(new_name).__name__}'")
        if self.name != new_name:
            symbol = self._symbol
            if self.parent is not None:
                self.parent.symbol_table.rename_symbol(symbol, new_name)
            else:
                # Check if the symbol in our own symbol table is the symbol
                # During a copy it is possible for a Routine to not yet
                # have a SymbolTable so allow for that.
                if self.symbol_table:
                    try:
                        sym = self.symbol_table.lookup(symbol.name)
                        if sym is self._symbol:
                            self.symbol_table.rename_symbol(symbol, new_name)
                    except KeyError:
                        # Symbol isn't in a symbol table so we can modify its
                        # name freely
                        symbol._name = new_name
                else:
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

    @symbol.setter
    def symbol(self, symbol):
        '''
        :param symbol: the RoutineSymbol corresponding to this Routine.
        :type symbol: :py:class:`psyclone.psyir.symbols.RoutineSymbol`
                      or NoneType

        :raises TypeError: if the provided symbol is neither a RoutineSymbol
                           or None
        '''
        if symbol and not isinstance(symbol, RoutineSymbol):
            raise TypeError(f"Routine symbol must be a RoutineSymbol but got "
                            f"'{type(symbol).__name__}'")
        self._symbol = symbol

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
        if value not in self.symbol_table.datasymbols:
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
        self._symbol = other._symbol.copy()
        super()._refine_copy(other)
        if other.return_symbol is not None:
            self.return_symbol = self.symbol_table.lookup(
                    other.return_symbol.name)

    def replace_with(self, node, keep_name_in_context=True):
        '''Removes self and its descendants from the PSyIR tree to which it
        is connected and replaces it with the supplied node (and its
        descendants).

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
        super().replace_with(node, keep_name_in_context=keep_name_in_context)


# For automatic documentation generation
__all__ = ["Routine"]
