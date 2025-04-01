# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the Container node implementation.'''

from psyclone.psyir.nodes.scoping_node import ScopingNode
from psyclone.psyir.nodes.routine import Routine
from psyclone.psyir.nodes.codeblock import CodeBlock
from psyclone.psyir.symbols import (GenericInterfaceSymbol, RoutineSymbol,
                                    Symbol, SymbolTable)
from psyclone.errors import GenerationError
from psyclone.psyir.commentable_mixin import CommentableMixin


class Container(ScopingNode, CommentableMixin):
    '''Node representing a set of Routine and/or Container nodes, as well
    as a name and a SymbolTable. This construct can be used to scope
    symbols of variables, Routine names and Container names. In
    Fortran a container would naturally represent a module or a
    submodule.

    :param str name: the name of the container.
    :param parent: optional parent node of this Container in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param symbol_table: initialise the node with a given symbol table.
    :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable` or \
            NoneType

    '''
    # Textual description of the node.
    _children_valid_format = "[Container | Routine | CodeBlock]*"
    _text_name = "Container"
    _colour = "green"

    def __init__(self, name, **kwargs):
        super().__init__(**kwargs)
        self._name = name

    def __eq__(self, other):
        '''Checks the equality of this Container with other. Containers are
        equal if they are the same type, and have the same name.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.name == other.name
        return is_eq

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        # pylint: disable=unused-argument
        return isinstance(child, (Container, Routine, CodeBlock))

    @classmethod
    def create(cls, name, symbol_table, children):
        '''Create a Container instance given a name, a symbol table and a
        list of child nodes.

        :param str name: the name of the Container.
        :param symbol_table: the symbol table associated with this \
            Container.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param children: a list of PSyIR nodes contained in the \
            Container. These must be Containers or Routines.
        :type children: list of :py:class:`psyclone.psyir.nodes.Container` \
            or :py:class:`psyclone.psyir.nodes.Routine`

        :returns: an instance of `cls`.
        :rtype: :py:class:`psyclone.psyir.nodes.Container` or subclass
            thereof

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        if not isinstance(name, str):
            raise GenerationError(
                f"name argument in create method of Container class "
                f"should be a string but found '{type(name).__name__}'.")
        if not isinstance(symbol_table, SymbolTable):
            raise GenerationError(
                f"symbol_table argument in create method of Container class "
                f"should be a SymbolTable but found "
                f"'{type(symbol_table).__name__}'.")
        if not isinstance(children, list):
            raise GenerationError(
                f"children argument in create method of Container class "
                f"should be a list but found '{type(children).__name__}'.")

        container = cls(name, symbol_table=symbol_table)
        container.children = children
        return container

    @property
    def name(self):
        '''
        :returns: name of the container.
        :rtype: str

        '''
        return self._name

    @name.setter
    def name(self, new_name):
        '''Sets a new name for the container.

        :param str new_name: new name for the container.

        '''
        self._name = new_name

    def node_str(self, colour=True):
        '''
        Returns the name of this node with appropriate control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return self.coloured_name(colour) + f"[{self.name}]"

    def __str__(self):
        return f"Container[{self.name}]\n"

    def find_routine_psyir(self, name, allow_private=False,
                           check_wildcard_imports=False):
        '''
        Searches the Container for a definition of the named routine with
        appropriate visibility.

        NOTE: if the named routine corresponds to a generic interface then this
        method will return None. You will need to use `resolve_routine` first
        to find the names of the routines that the interface resolves to.

        If it is not found and the routine is named in an import statement
        then the search is continued in the named Container. Failing that,
        all wildcard imports are checked if `check_wildcard_imports` is True.

        :param str name: the name of the Routine for which to search.
        :param bool allow_private: whether the Routine is permitted to have
            a visibility of PRIVATE.
        :param bool check_wildcard_imports: whether or not to proceed to check
            for the named routine in any wildcard imports. Default is False as
            this can be expensive.

        :returns: the PSyIR of the named Routine if found, otherwise None.
        :rtype: :py:class:`psyclone.psyir.nodes.Routine` | NoneType

        '''
        rname = name.lower()
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes.routine import Routine
        from psyclone.psyir.symbols.symbol import Symbol, SymbolError
        # pylint: enable=import-outside-toplevel

        # Is the Routine defined within this Container?
        for node in self.children:
            if isinstance(node, Routine) and node.name.lower() == rname:
                # Check this routine is public
                routine_sym = self.symbol_table.lookup(node.name)
                if (allow_private or
                        routine_sym.visibility == Symbol.Visibility.PUBLIC):
                    return node
                # The Container does contain a Routine with the right name and
                # that means it cannot be imported from any other Container
                # as that would result in a clash. However, its visibility is
                # such that it can't be the one we're looking for.
                return None

        # It's not defined in this Container so look in the import that names
        # the routine if there is one.
        table = self.symbol_table
        try:
            routine_sym = table.lookup(rname)
        except KeyError:
            # Routine does not exist in the SymbolTable.
            routine_sym = None

        if routine_sym:
            if isinstance(routine_sym, GenericInterfaceSymbol):
                # The routine is actually an interface to one or more routines.
                # This should be handled outside this routine (e.g. by using
                # `resolve_routine` first).
                return None

            if not (allow_private or
                    routine_sym.visibility == Symbol.Visibility.PUBLIC):
                # The Symbol is imported into this Container but is not
                # visible outside it.
                return None

        if routine_sym and routine_sym.is_import:
            child_cntr_sym = routine_sym.interface.container_symbol
            # Find the definition of the container.
            container = None
            try:
                container = child_cntr_sym.find_container_psyir(
                    local_node=self)
            except (SymbolError, FileNotFoundError):
                pass
            if not container:
                # TODO #11 log that we didn't find the Container from which
                # the routine is imported.
                return None
            return container.find_routine_psyir(rname)

        # We may not have found a RoutineSymbol yet or we have but don't know
        # where it comes from. Look in any wildcard imports.
        if not check_wildcard_imports:
            return None

        if not routine_sym:
            # There's no Symbol in the table.
            if (not allow_private and self.symbol_table.default_visibility !=
                    Symbol.Visibility.PUBLIC):
                # No imported Symbol can satisfy the visibility requirements.
                return None

        for child_cntr_sym in table.containersymbols:
            if child_cntr_sym.wildcard_import:
                # Find the definition of the container.
                try:
                    container = child_cntr_sym.find_container_psyir(
                        local_node=self)
                except (SymbolError, FileNotFoundError):
                    continue
                # Recurse down to this container (but go no deeper).
                result = container.find_routine_psyir(
                    rname, check_wildcard_imports=False)
                if result:
                    return result
        # The required Routine was not found in the Container.
        return None

    def resolve_routine(self, name):
        '''This function returns a list of function names that might be
        actually called when the routine `name` is called. In most cases
        this is exactly `name`, but in case of a generic subroutine the
        name might change. For now (since we cannot compare routine
        signatures yet), we return the list of all possible functions that
        might be called.

        If the symbol with the specified name is a generic Symbol and is
        imported then it is specialised (in place) to become a RoutineSymbol.

        :param str name: the name of the routine to resolve

        :returns: the names of those routines that may actually be invoked
            when the routine `name` is called or an empty list if there is no
            routine with that name in this container.
        :rtype: list[str | None]

        :raises TypeError: if the Symbol with the supplied name is not a
            RoutineSymbol, GenericInterfaceSymbol or imported Symbol.
        '''
        try:
            rsym = self.symbol_table.lookup(name)
        except KeyError:
            return []
        if isinstance(rsym, GenericInterfaceSymbol):
            return [rt.symbol.name.lower() for rt in rsym.routines]
        if isinstance(rsym, RoutineSymbol):
            return [name]
        if type(rsym) is Symbol and rsym.is_import:
            rsym.specialise(RoutineSymbol)
            return [name]

        raise TypeError(
            f"Expected '{name}' to correspond to a RoutineSymbol, a "
            f"GenericInterfaceSymbol or an imported Symbol but found: {rsym}")


# For AutoAPI documentation generation
__all__ = ['Container']
