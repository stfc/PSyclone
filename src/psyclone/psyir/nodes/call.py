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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the Call node implementation.'''

from collections.abc import Iterable

from psyclone.configuration import Config
from psyclone.core import AccessType
from psyclone.errors import GenerationError
from psyclone.psyir.nodes.container import Container
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.nodes.routine import Routine
from psyclone.psyir.symbols import (
    RoutineSymbol,
    Symbol,
    SymbolError,
    UnsupportedFortranType,
    DataSymbol,
    SymbolTable,
    ContainerSymbol,
)
from typing import List, Union
from psyclone.errors import PSycloneError
from psyclone.psyir.symbols.datatypes import ArrayType


class CallMatchingArgumentsNotFoundError(PSycloneError):
    '''Exception to signal that matching arguments have not been found
    for this routine
    '''
    def __init__(self, value):
        PSycloneError.__init__(self, value)
        self.value = "CallMatchingArgumentsNotFound: " + str(value)


class Call(Statement, DataNode):
    ''' Node representing a Call. This can be found as a standalone statement
    or an expression.

    TODO #1437: The combined Statement and Expression implementation is simple
    but it has some shortcomings that may need to be addressed.

    :param kwargs: additional keyword arguments provided to the PSyIR node.
    :type kwargs: unwrapped dict.

    '''
    # Textual description of the node.
    _children_valid_format = "Reference, [DataNode]*"
    _text_name = "Call"
    _colour = "cyan"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)

        # The internal _argument_names list can be inconsistent with
        # the order of the children. Use the property/methods
        # internally and/or the _reconcile() method to make consistent
        # when required.
        self._argument_names = []

    def __eq__(self, other):
        '''
        Checks whether two nodes are equal. Two Call nodes are equal
        if their routine members are equal.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.argument_names == other.argument_names

        return is_eq

    @classmethod
    def create(cls, routine, arguments=()):
        '''Create an instance of class cls given valid instances of a routine
        symbol, and a list of child nodes (or name and node tuple) for
        its arguments.

        :param routine: the routine that class cls calls.
        :type routine: py:class:`psyclone.psyir.symbols.RoutineSymbol` |
            py:class:`psyclone.psyir.nodes.Reference`
        :param arguments: optional list of arguments for this call, these
            can be PSyIR nodes or tuples of string,Node for named arguments.
        :type arguments: Optional[Iterable[\
            Union[:py:class:`psyclone.psyir.nodes.DataNode`,\
                  Tuple[str, :py:class:`psyclone.psyir.nodes.DataNode`]]]]

        :returns: an instance of cls.
        :rtype: :py:class:`psyclone.psyir.nodes.Call` or a subclass thereof.

        :raises TypeError: if the routine argument is not a RoutineSymbol.
        :raises GenerationError: if the arguments argument is not an Iterable.

        '''
        if not isinstance(routine, (Reference, RoutineSymbol)):
            raise TypeError(
                f"The Call routine argument should be a Reference to a "
                f"RoutineSymbol or a RoutineSymbol, but "
                f"found '{type(routine).__name__}'.")

        if not isinstance(arguments, Iterable):
            raise GenerationError(
                f"Call.create 'arguments' argument should be an Iterable but "
                f"found '{type(arguments).__name__}'.")

        call = cls()
        if isinstance(routine, Reference):
            call.addchild(routine)
        else:
            call.addchild(Reference(routine))
        if arguments:
            cls._add_args(call, arguments)
        return call

    @staticmethod
    def _add_args(call, arguments):
        '''Internal utility method to add arguments to a call node. These are
        added as child nodes.

        :param call: the supplied call node.
        :type call: :py:class:`psyclone.psyir.nodes.Call`
        :param arguments: list of arguments for this call, these
            can be PSyIR nodes or tuples of string,Node for named arguments.
        :type arguments: Iterable[
            Union[:py:class:`psyclone.psyir.nodes.DataNode`,
                  Tuple[str, :py:class:`psyclone.psyir.nodes.DataNode`]]]

        :raises GenerationError: if the contents of the arguments
            argument are not in the expected form or of the expected
            type.

        '''
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

    def append_named_arg(self, name, arg):
        '''Append a named argument to this call.

           :param name: the argument name.
           :type name: Optional[str]
           :param arg: the argument expression.
           :type arg: :py:class:`psyclone.psyir.nodes.DataNode`

           :raises ValueError: if the name argument is already used \
               for an existing argument.

        '''
        if name is not None:
            # Avoid circular import.
            # pylint: disable=import-outside-toplevel
            from psyclone.psyir.frontend.fortran import FortranReader
            FortranReader.validate_name(name)
            for check_name in self.argument_names:
                if check_name and check_name.lower() == name.lower():
                    raise ValueError(
                        f"The value of the name argument ({name}) in "
                        f"'append_named_arg' in the 'Call' node is "
                        f"already used for a named argument.")
        self._argument_names.append((id(arg), name))
        self.children.append(arg)

    def insert_named_arg(self, name, arg, index):
        '''Insert a named argument to the call.

           :param name: the argument name.
           :type name: Optional[str]
           :param arg: the argument expression.
           :type arg: :py:class:`psyclone.psyir.nodes.DataNode`
           :param int index: where in the argument list to insert the \
               named argument.

           :raises ValueError: if the name argument is already used \
               for an existing argument.
           :raises TypeError: if the index argument is the wrong type.

        '''
        if name is not None:
            # Avoid circular import.
            # pylint: disable=import-outside-toplevel
            from psyclone.psyir.frontend.fortran import FortranReader
            FortranReader.validate_name(name)
            for check_name in self.argument_names:
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
        self._argument_names.insert(index, (id(arg), name))
        # The n'th argument is placed at the n'th+1 children position
        # because the 1st child is the routine reference
        self.children.insert(index + 1, arg)

    def replace_named_arg(self, existing_name, arg):
        '''Replace one named argument node with another node keeping the
        same name.

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
                f"'Call' node should be a string, but found "
                f"{type(existing_name).__name__}.")
        index = 0
        for _, name in self._argument_names:
            if name is not None and name.lower() == existing_name:
                break
            index += 1
        else:
            raise ValueError(
                f"The value of the existing_name argument ({existing_name}) "
                "in 'replace_named_arg' in the 'Call' node was not found "
                "in the existing arguments."
            )
        # The n'th argument is placed at the n'th+1 children position
        # because the 1st child is the routine reference
        self.children[index + 1] = arg
        self._argument_names[index] = (id(arg), existing_name)

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        if position == 0:
            return isinstance(child, Reference)
        return isinstance(child, DataNode)

    def reference_accesses(self, var_accesses):
        '''
        Updates the supplied var_accesses object with information on the
        arguments passed to this call.

        TODO #446 - all arguments that are passed by reference are currently
        marked as having READWRITE access (unless we know that the routine is
        PURE). We could do better than this if we have the PSyIR of the called
        Routine.

        :param var_accesses: VariablesAccessInfo instance that stores the
            information about variable accesses.
        :type var_accesses: :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        if self.is_pure:
            # If the called routine is pure then any arguments are only
            # read.
            default_access = AccessType.READ
        else:
            # We conservatively default to READWRITE otherwise (TODO #446).
            default_access = AccessType.READWRITE

        # TODO #2271: This may skip references in inner expressions of
        # structure calls, but to implement properly we new a new kind of
        # AccessType that represents being called (USED but not READ, maybe
        # the same that we need for INQUIRY type attributes?)
        for arg in self.arguments:
            if isinstance(arg, Reference):
                # This argument is pass-by-reference.
                sig, indices_list = arg.get_signature_and_indices()
                var_accesses.add_access(sig, default_access, arg)
                # Any symbols referenced in any index expressions are READ.
                for indices in indices_list:
                    for idx in indices:
                        idx.reference_accesses(var_accesses)
            else:
                # This argument is not a Reference so continue to walk down the
                # tree. (e.g. it could be/contain a Call to
                # an impure routine in which case any arguments to that Call
                # will have READWRITE access.)
                arg.reference_accesses(var_accesses)
        # Make sure that the next statement will be on the next location
        var_accesses.next_location()

    @property
    def routine(self):
        '''
        :returns: the routine reference that this call calls.
        :rtype: Optional[py:class:`psyclone.psyir.nodes.Reference`]
        '''
        if len(self._children) >= 1:
            return self.children[0]
        return None

    @property
    def arguments(self) -> List[DataNode]:
        '''
        :returns: the children of this node that represent its arguments.
        :rtype: list[py:class:`psyclone.psyir.nodes.DataNode`]
        '''
        if len(self._children) >= 2:
            return self.children[1:]
        return []

    @property
    def is_elemental(self):
        '''
        :returns: whether the routine being called is elemental (provided with
            an input array it will apply the operation individually to each of
            the array elements and return an array with the results). If this
            information is not known then it returns None.
        :rtype: NoneType | bool
        '''
        if self.routine and self.routine.symbol:
            return self.routine.symbol.is_elemental
        return None

    @property
    def is_pure(self):
        '''
        :returns: whether the routine being called is pure (guaranteed to \
            return the same result when provided with the same argument \
            values).  If this information is not known then it returns None.
        :rtype: NoneType | bool
        '''
        if self.routine and self.routine.symbol:
            return self.routine.symbol.is_pure
        return None

    def is_available_on_device(self):
        '''
        :returns: whether this call is available on an accelerated device.
        :rtype: bool

        '''
        return False

    @property
    def argument_names(self):
        '''
        :returns: a list with the name of each argument. If the entry is \
            None then the argument is a positional argument.
        :rtype: List[Optional[str]]
        '''
        self._reconcile()
        return [entry[1] for entry in self._argument_names]

    def _reconcile(self):
        '''Update the _argument_names values in case child arguments have been
        removed, added, or re-ordered.

        '''
        new_argument_names = []
        for child in self.arguments:
            for arg in self._argument_names:
                if id(child) == arg[0]:
                    new_argument_names.append(arg)
                    break
            else:
                new_argument_names.append((id(child), None))
        self._argument_names = new_argument_names

    def node_str(self, colour=True):
        '''
        Construct a text representation of this node, optionally containing
        colour control codes.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this PSyIR node.
        :rtype: str

        '''
        return (f"{self.coloured_name(colour)}"
                f"[name='{self.routine.debug_string()}']")

    def __str__(self):
        return self.node_str(False)

    def copy(self):
        '''Return a copy of this node. This is a bespoke implementation for
        a Call node that ensures that any internal id's are
        consistent before and after copying.

        :returns: a copy of this node and its children.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # ensure _argument_names is consistent with actual arguments
        # before copying.
        self._reconcile()
        # copy
        new_copy = super().copy()
        # Fix invalid id's in _argument_names after copying.
        # pylint: disable=protected-access
        new_list = []
        for idx, child in enumerate(new_copy.arguments):
            my_tuple = (id(child), new_copy._argument_names[idx][1])
            new_list.append(my_tuple)
        new_copy._argument_names = new_list

        return new_copy

    def _get_container_symbols_rec(
        self,
        container_symbols_list: List[str],
        ignore_missing_modules: bool = False,
        _stack_container_name_list: List[str] = [],
        _depth: int = 0,
    ):
        """Return a list of all container symbols that can be found
        recursively

        :param container_symbols: List of starting set of container symbols
        :type container_symbols: List[ContainerSymbol]
        :param _stack_container_list: Stack with already visited Containers
        to avoid circular searches, defaults to []
        :type _stack_container_list: List[Container], optional
        :param _depth: Depth of recursive search
        :type _depth: int
        """
        #
        # TODO:
        # - This function seems to be extremely slow:
        #   It takes considerable time to build this list over and over
        #   for each lookup.
        # - This function can also be written in a non-resursive way
        #
        # An alternative would be to cache it, but then the cache
        # needs to be invalidated once some symbols are, e.g., deleted.
        #
        ret_container_symbol_list = container_symbols_list[:]

        # Cache the container names from symbols
        container_names = [cs.name.lower() for cs in container_symbols_list]

        from psyclone.parse import ModuleManager

        module_manager = ModuleManager.get()

        for container_name in container_names:
            try:
                module_info = module_manager.get_module_info(
                    container_name.lower()
                )
                if module_info is None:
                    continue

            except (ModuleNotFoundError, FileNotFoundError) as err:
                if ignore_missing_modules:
                    continue

                raise err

            container: Container = module_info.get_psyir_container_node()

            # Avoid circular connections (which shouldn't
            # be allowed, but who knows...)
            if container.name.lower() in _stack_container_name_list:
                continue

            new_container_symbols = self._get_container_symbols_rec(
                container_symbols_list=container.symbol_table.containersymbols,
                ignore_missing_modules=ignore_missing_modules,
                _stack_container_name_list=_stack_container_name_list
                + [container.name.lower()],
                _depth=_depth + 1,
            )

            # Add symbol if it's not yet in the list of symbols
            for container_symbol in new_container_symbols:
                if container_symbol not in ret_container_symbol_list:
                    ret_container_symbol_list.append(container_symbol)

        return ret_container_symbol_list

    def get_callees(self, ignore_missing_modules: bool = False):
        """
        Searches for the implementation(s) of all potential target routines
        for this Call without any arguments check.

        :param ignore_missing_modules: If a module wasn't found, return 'None'
            instead of throwing an exception 'ModuleNotFound'.
        :type ignore_missing_modules: bool

        :returns: the Routine(s) that this call targets.
        :rtype: list[:py:class:`psyclone.psyir.nodes.Routine`]

        :raises NotImplementedError: if the routine is not local and not found
            in any containers in scope at the call site.

        """
        def _location_txt(node):
            '''
            Utility to generate meaningful location text.

            :param node: a PSyIR node.
            :type node: :py:class:`psyclone.psyir.nodes.Node`

            :returns: description of location of node.
            :rtype: str
            '''
            if isinstance(node, Container):
                return f"Container '{node.name}'"
            out_lines = node.debug_string().split("\n")
            idx = -1
            while not out_lines[idx]:
                idx -= 1
            last_line = out_lines[idx]
            return f"code:\n'{out_lines[0]}\n...\n{last_line}'"

        rsym = self.routine.symbol
        if rsym.is_unresolved:

            # Check for any "raw" Routines, i.e. ones that are not
            # in a Container.  Such Routines would exist in the PSyIR
            # as a child of a FileContainer (if the PSyIR contains a
            # FileContainer). Note, if the PSyIR does contain a
            # FileContainer, it will be the root node of the PSyIR.
            for routine in self.root.children:
                if (isinstance(routine, Routine) and
                        routine.name.lower() == rsym.name.lower()):
                    return [routine]

            # Now check for any wildcard imports and see if they can
            # be used to resolve the symbol.
            wildcard_names = []
            containers_not_found = []
            current_table: SymbolTable = self.scope.symbol_table
            while current_table:
                # TODO: Obtaining all container symbols in this way
                # breaks some tests.
                # It would be better using the ModuleManager to resolve
                # (and cache) all containers to look up for this.
                #
                # current_containersymbols = self._get_container_symbols_rec(
                #     current_table.containersymbols,
                #     ignore_missing_modules=ignore_missing_modules,
                # )
                # for container_symbol in current_containersymbols:
                for container_symbol in current_table.containersymbols:
                    container_symbol: ContainerSymbol
                    if container_symbol.wildcard_import:
                        wildcard_names.append(container_symbol.name)

                        try:
                            container: Container = (
                                container_symbol.find_container_psyir(
                                    local_node=self,
                                    ignore_missing_modules=(
                                        ignore_missing_modules
                                    ),
                                )
                            )
                        except SymbolError:
                            container = None
                        if not container:
                            # Failed to find/process this Container.
                            containers_not_found.append(container_symbol.name)
                            continue
                        routines = []
                        for name in container.resolve_routine(rsym.name):
                            # Allow private imports if an 'interface'
                            # was used. Here, we assume the name of the routine
                            # is different to the call.
                            allow_private = name != rsym.name
                            psyir = container.find_routine_psyir(
                                name, allow_private=allow_private
                            )

                            if psyir:
                                routines.append(psyir)

                        if routines:
                            return routines
                current_table = current_table.parent_symbol_table()

            if not wildcard_names:
                wc_text = "there are no wildcard imports"
            else:
                if containers_not_found:
                    wc_text = (
                        f"attempted to resolve the wildcard imports from"
                        f" {wildcard_names}. However, failed to find the "
                        f"source for {containers_not_found}. The module search"
                        f" path is set to {Config.get().include_paths}")
                else:
                    wc_text = (f"wildcard imports from {wildcard_names}")
            raise NotImplementedError(
                f"Failed to find the source code of the unresolved routine "
                f"'{rsym.name}' - looked at any routines in the same source "
                f"file and {wc_text}. Searching for external routines "
                f"that are only resolved at link time is not supported.")

        root_node = self.ancestor(Container)
        if not root_node:
            root_node = self.root
        container = root_node
        can_be_private = True

        if rsym.is_import:
            cursor = rsym
            # A Routine imported from another Container must be public in that
            # Container.
            can_be_private = False
            while cursor.is_import:
                csym = cursor.interface.container_symbol
                try:
                    container = csym.find_container_psyir(local_node=self)
                except SymbolError:
                    raise NotImplementedError(
                        f"RoutineSymbol '{rsym.name}' is imported from "
                        f"Container '{csym.name}' but the source defining "
                        f"that container could not be found. The module search"
                        f" path is set to {Config.get().include_paths}")
                imported_sym = container.symbol_table.lookup(cursor.name)
                if imported_sym.visibility != Symbol.Visibility.PUBLIC:
                    # The required Symbol must be shadowed with a PRIVATE
                    # Symbol in this Container. This means that the one we
                    # actually want is brought into scope via a wildcard
                    # import.
                    # TODO #924 - Use ModuleManager to search?
                    raise NotImplementedError(
                        f"RoutineSymbol '{rsym.name}' is imported from "
                        f"Container '{csym.name}' but that Container defines "
                        f"a private Symbol of the same name. Searching for the"
                        f" Container that defines a public Routine with that "
                        f"name is not yet supported - TODO #924")
                if not isinstance(imported_sym, RoutineSymbol):
                    # We now know that this is a RoutineSymbol so specialise it
                    # in place.
                    imported_sym.specialise(RoutineSymbol)
                cursor = imported_sym
            rsym = cursor
            root_node = container

        if isinstance(rsym.datatype, UnsupportedFortranType):
            # TODO #924 - an UnsupportedFortranType here typically indicates
            # that the target is actually an interface.
            raise NotImplementedError(
                f"RoutineSymbol '{rsym.name}' exists in "
                f"{_location_txt(root_node)} but is of "
                f"UnsupportedFortranType:\n{rsym.datatype.declaration}\n"
                f"Cannot get the PSyIR of such a routine.")

        if isinstance(container, Container):
            routines = []
            for name in container.resolve_routine(rsym.name):
                psyir = container.find_routine_psyir(
                    name, allow_private=can_be_private)
                if psyir:
                    routines.append(psyir)
            if routines:
                return routines

        raise SymbolError(
            f"Failed to find a Routine named '{rsym.name}' in "
            f"{_location_txt(root_node)}. This is normally because the routine"
            f" is within a CodeBlock.")

    def _check_inline_types(
        self,
        call_arg: DataSymbol,
        routine_arg: DataSymbol,
        check_array_type: bool = True,
    ):
        """This function performs tests to see whether the
        inlining can cope with it.

        :param call_arg: The argument of a call
        :type call_arg: DataSymbol
        :param routine_arg: The argument of a routine
        :type routine_arg: DataSymbol
        :param check_array_type: Perform strong checks on array types,
            defaults to `True`
        :type check_array_type: bool, optional

        :raises TransformationError: Raised if transformation can't be done

        :return: 'True' if checks are successful
        :rtype: bool
        """
        from psyclone.psyir.transformations.transformation_error import (
            TransformationError,
        )
        from psyclone.errors import LazyString
        from psyclone.psyir.nodes import Literal, Range
        from psyclone.psyir.symbols import (
            UnresolvedType,
            UnsupportedType,
            INTEGER_TYPE,
        )

        _ONE = Literal("1", INTEGER_TYPE)

        # If the formal argument is an array with non-default bounds then
        # we also need to know the bounds of that array at the call site.
        if not isinstance(routine_arg.datatype, ArrayType):
            # Formal argument is not an array so we don't need to do any
            # further checks.
            return True

        if not isinstance(call_arg, (Reference, Literal)):
            # TODO #1799 this really needs the `datatype` method to be
            # extended to support all nodes. For now we have to abort
            # if we encounter an argument that is not a scalar (according
            # to the corresponding formal argument) but is not a
            # Reference or a Literal as we don't know whether the result
            # of any general expression is or is not an array.
            # pylint: disable=cell-var-from-loop
            raise TransformationError(
                LazyString(
                    lambda: (
                        f"The call '{self.debug_string()}' "
                        "cannot be inlined because actual argument "
                        f"'{call_arg.debug_string()}' corresponds to a "
                        "formal argument with array type but is not a "
                        "Reference or a Literal."
                    )
                )
            )

        # We have an array argument. We are only able to check that the
        # argument is not re-shaped in the called routine if we have full
        # type information on the actual argument.
        # TODO #924. It would be useful if the `datatype` property was
        # a method that took an optional 'resolve' argument to indicate
        # that it should attempt to resolve any UnresolvedTypes.
        if check_array_type:
            if isinstance(
                call_arg.datatype, (UnresolvedType, UnsupportedType)
            ) or (
                isinstance(call_arg.datatype, ArrayType)
                and isinstance(
                    call_arg.datatype.intrinsic,
                    (UnresolvedType, UnsupportedType),
                )
            ):
                raise TransformationError(
                    f"Routine '{self.routine.name}' cannot be "
                    "inlined because the type of the actual argument "
                    f"'{call_arg.symbol.name}' corresponding to an array"
                    f" formal argument ('{routine_arg.name}') is unknown."
                )

            formal_rank = 0
            actual_rank = 0
            if isinstance(routine_arg.datatype, ArrayType):
                formal_rank = len(routine_arg.datatype.shape)
            if isinstance(call_arg.datatype, ArrayType):
                actual_rank = len(call_arg.datatype.shape)
            if formal_rank != actual_rank:
                # It's OK to use the loop variable in the lambda definition
                # because if we get to this point then we're going to quit
                # the loop.
                # pylint: disable=cell-var-from-loop
                raise TransformationError(
                    LazyString(
                        lambda: (
                            "Cannot inline routine"
                            f" '{self.routine.name}' because it"
                            " reshapes an argument: actual argument"
                            f" '{call_arg.debug_string()}' has rank"
                            f" {actual_rank} but the corresponding formal"
                            f" argument, '{routine_arg.name}', has rank"
                            f" {formal_rank}"
                        )
                    )
                )
            if actual_rank:
                ranges = call_arg.walk(Range)
                for rge in ranges:
                    ancestor_ref = rge.ancestor(Reference)
                    if ancestor_ref is not call_arg:
                        # Have a range in an indirect access.
                        # pylint: disable=cell-var-from-loop
                        raise TransformationError(
                            LazyString(
                                lambda: (
                                    "Cannot inline routine"
                                    f" '{self.routine.name}' because"
                                    " argument"
                                    f" '{call_arg.debug_string()}' has"
                                    " an array range in an indirect"
                                    " access #(TODO 924)."
                                )
                            )
                        )
                    if rge.step != _ONE:
                        # TODO #1646. We could resolve this problem by
                        # making a new array and copying the necessary
                        # values into it.
                        # pylint: disable=cell-var-from-loop
                        raise TransformationError(
                            LazyString(
                                lambda: (
                                    "Cannot inline routine"
                                    f" '{self.routine.name}' because"
                                    " one of its arguments is an array"
                                    " slice with a non-unit stride:"
                                    f" '{call_arg.debug_string()}' (TODO"
                                    " #1646)"
                                )
                            )
                        )

    def _check_argument_type_matches(
        self,
        call_arg: DataSymbol,
        routine_arg: DataSymbol,
        check_strict_array_datatype: bool = True,
    ) -> bool:
        """Return information whether argument types are matching.
        This also supports 'optional' arguments by using
        partial types.

        :param call_arg: Argument from the call
        :type call_arg: DataSymbol
        :param routine_arg: Argument from the routine
        :type routine_arg: DataSymbol
        :param check_strict_array_datatype: Check strictly for matching
            array types. If `False`, only checks for ArrayType itself are done.
        :type check_strict_array_datatype: bool
        :returns: True if arguments match, False otherwise
        :rtype: bool
        :raises CallMatchingArgumentsNotFound: Raised if no matching arguments
            were found.
        """

        self._check_inline_types(call_arg, routine_arg)

        type_matches = False
        if not check_strict_array_datatype:
            # No strict array checks have to be performed, just accept it
            if isinstance(call_arg.datatype, ArrayType) and isinstance(
                routine_arg.datatype, ArrayType
            ):
                type_matches = True

        if not type_matches:
            if isinstance(routine_arg.datatype, UnsupportedFortranType):
                # This could be an 'optional' argument.
                # This has at least a partial data type
                if call_arg.datatype != routine_arg.datatype.partial_datatype:
                    raise CallMatchingArgumentsNotFoundError(
                        "Argument partial type mismatch of call "
                        f"argument '{call_arg}' and routine argument "
                        f"'{routine_arg}'"
                    )
            else:
                if call_arg.datatype != routine_arg.datatype:
                    raise CallMatchingArgumentsNotFoundError(
                        "Argument type mismatch of call argument "
                        f"'{call_arg}' with type '{call_arg.datatype} "
                        "and routine argument "
                        f"'{routine_arg}' with type '{routine_arg.datatype}."
                    )

        return True

    def _get_argument_routine_match(
        self,
        routine: Routine,
        check_strict_array_datatype: bool = True,
    ) -> Union[None, List[int]]:
        """Return a list of integers giving for each argument of the call
        the index of the argument in argument_list (typically of a routine)

        :param check_strict_array_datatype: Strict datatype check for
            array types
        :type check_strict_array_datatype: bool

        :param check_matching_arguments: If no match is possible,
            return the first routine in the list of potential candidates.
        :type check_matching_arguments: bool

        :return: None if no match was found, otherwise list of integers
            referring to matching arguments.
        :rtype: None|List[int]
        :raises CallMatchingArgumentsNotFound: If there was some problem in
            finding matching arguments.
        """

        # Create a copy of the list of actual arguments to the routine.
        # Once an argument has been successfully matched, set it to 'None'
        routine_argument_list: List[DataSymbol] = (
            routine.symbol_table.argument_list[:]
        )

        if len(self.arguments) > len(routine.symbol_table.argument_list):
            call_str = self.debug_string().replace("\n", "")
            raise CallMatchingArgumentsNotFoundError(
                f"More arguments in call ('{call_str}')"
                f" than callee (routine '{routine.name}')"
            )

        # Iterate over all arguments to the call
        ret_arg_idx_list = []
        for call_arg_idx, call_arg in enumerate(self.arguments):
            call_arg_idx: int
            call_arg: DataSymbol

            # If the associated name is None, it's a positional argument
            # => Just return the index if the types match
            if self.argument_names[call_arg_idx] is None:
                routine_arg = routine_argument_list[call_arg_idx]
                routine_arg: DataSymbol

                self._check_argument_type_matches(
                    call_arg, routine_arg, check_strict_array_datatype
                )

                ret_arg_idx_list.append(call_arg_idx)
                routine_argument_list[call_arg_idx] = None
                continue

            #
            # Next, we handle all named arguments
            #
            arg_name = self.argument_names[call_arg_idx]
            routine_arg_idx = None

            for routine_arg_idx, routine_arg in enumerate(
                routine_argument_list
            ):
                routine_arg: DataSymbol

                # Check if argument was already processed
                if routine_arg is None:
                    continue

                if arg_name == routine_arg.name:
                    self._check_argument_type_matches(
                        call_arg,
                        routine_arg,
                        check_strict_array_datatype=(
                            check_strict_array_datatype
                        ),
                    )
                    ret_arg_idx_list.append(routine_arg_idx)
                    break

            else:
                # It doesn't match => Raise exception
                raise CallMatchingArgumentsNotFoundError(
                    f"Named argument '{arg_name}' not found"
                )

            routine_argument_list[routine_arg_idx] = None

        #
        # Finally, we check if all left-over arguments are optional arguments
        #
        for routine_arg in routine_argument_list:
            routine_arg: DataSymbol

            if routine_arg is None:
                continue

            # TODO #759: Optional keyword is not yet supported in psyir.
            # Hence, we use a simple string match.
            if ", OPTIONAL" in str(routine_arg.datatype):
                continue

            raise CallMatchingArgumentsNotFoundError(
                f"Argument '{routine_arg.name}' in subroutine"
                f" '{routine.name}' not handled"
            )

        return ret_arg_idx_list

    def get_callee(
        self,
        check_matching_arguments: bool = True,
        check_strict_array_datatype: bool = True,
        ignore_missing_modules: bool = False,
        ignore_unresolved_symbol: bool = False,
    ):
        '''
        Searches for the implementation(s) of the target routine for this Call
        including argument checks.

        :param check_matching_arguments: Also check argument types to match.
            If set to `False` and in case it doesn't find matching arguments,
            the very first implementation of the matching routine will be
            returned (even if the argument type check failed). The argument
            types and number of arguments might therefore mismatch!
        :type ret_arg_match_list: bool

        :returns: A tuple of two elements. The first element is the routine
            that this call targets. The second one a list of arguments
            providing the information on matching argument indices.
        :rtype: Set[psyclone.psyir.nodes.Routine, List[int]]

        :raises NotImplementedError: if the routine is not local and not found
            in any containers in scope at the call site.
        '''

        routine_list = self.get_callees(
            ignore_missing_modules=ignore_missing_modules
        )

        if len(routine_list) == 0:
            raise NotImplementedError(
                f"No routine or interface found for name '{self.routine.name}'"
            )

        err_info_list = []

        # Search for the routine matching the right arguments
        for routine_node in routine_list:
            routine_node: Routine

            try:
                arg_match_list = self._get_argument_routine_match(
                    routine_node,
                    check_strict_array_datatype=check_strict_array_datatype,
                )
            except CallMatchingArgumentsNotFoundError as err:
                err_info_list.append(err.value)
                continue

            return (routine_node, arg_match_list)

        # If we didn't find any routine, return some routine if no matching
        # arguments have been found.
        # This is handy for the transition phase until optional argument
        # matching is supported.
        if not check_matching_arguments:
            # Also return a list of dummy argument indices
            return (routine_list[0], [i for i in range(len(self.arguments))])

        error_msg = "\n".join(err_info_list)

        raise CallMatchingArgumentsNotFoundError(
            "Found routines, but no routine with matching arguments found "
            f"for '{self.routine.name}':\n"
            + error_msg
        )
