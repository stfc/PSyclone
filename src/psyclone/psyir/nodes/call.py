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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the Call node implementation.'''

# Support for *postponed* type annotations.
from __future__ import annotations

from collections.abc import Iterable
from typing import List, Tuple, Union

from psyclone.configuration import Config
from psyclone.core import AccessType, VariablesAccessMap
from psyclone.errors import GenerationError, PSycloneError
from psyclone.psyir.nodes.codeblock import CodeBlock
from psyclone.psyir.nodes.container import Container
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.nodes.routine import Routine
from psyclone.psyir.symbols import (
    DataSymbol,
    DataType,
    DataTypeSymbol,
    GenericInterfaceSymbol,
    RoutineSymbol,
    Symbol,
    SymbolError,
    UnsupportedFortranType,
)
from psyclone.psyir.symbols.datatypes import ArrayType


class CallMatchingArgumentsNotFound(PSycloneError):
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

    def replace_named_arg(self, existing_name: str, arg: DataNode):
        '''Replace one named argument node with another node keeping the
        same name.

        :param existing_name: the argument name.
        :param arg: the argument expression.

        :raises TypeError: if the name argument is the wrong type.
        :raises ValueError: if the name argument is already used
            for an existing argument.

        '''
        if not isinstance(existing_name, str):
            raise TypeError(
                f"The 'name' argument in 'replace_named_arg' in the "
                f"'Call' node should be a string, but found "
                f"{type(existing_name).__name__}.")
        index = 0
        lname = existing_name.lower()
        for _, name in self._argument_names:
            if name is not None and name.lower() == lname:
                break
            index += 1
        else:
            raise ValueError(
                f"The value of the existing_name argument ({existing_name}) "
                f"in 'replace_named_arg' in the 'Call' node was not found "
                f"in the existing arguments."
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

    def reference_accesses(self) -> VariablesAccessMap:
        '''
        TODO #446 - all arguments that are passed by reference are currently
        marked as having READWRITE access (unless we know that the routine is
        PURE). We could do better than this if we have the PSyIR of the called
        Routine.

        :returns: a map of all the symbol accessed inside this node, the
            keys are Signatures (unique identifiers to a symbol and its
            structure acccessors) and the values are AccessSequence
            (a sequence of AccessTypes).

        '''
        var_accesses = VariablesAccessMap()

        # The RoutineSymbol has a CALL access.
        sig, indices_list = self.routine.get_signature_and_indices()
        var_accesses.add_access(sig, AccessType.CALL, self.routine)

        # Attempt to find the target of the Call so that the argument intents
        # can be examined.
        try:
            routine = self.get_callee()[0]
            args = routine.symbol_table.argument_list
            routine_intents = [arg.interface.access for arg in args]
        except Exception:
            routine_intents = None

        # Continue processing references in any index expressions.
        for indices in indices_list:
            for idx in indices:
                var_accesses.update(idx.reference_accesses())

        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.symbols import ArgumentInterface
        for idx, arg in enumerate(self.arguments):
            if isinstance(arg, Reference):
                # This argument is pass-by-reference.
                sig, indices_list = arg.get_signature_and_indices()
                if routine_intents:
                    if routine_intents[idx] == ArgumentInterface.Access.WRITE:
                        access_type = AccessType.WRITE
                    elif routine_intents[idx] == ArgumentInterface.Access.READ:
                        access_type = AccessType.READ
                    else:
                        access_type = AccessType.READWRITE
                else:
                    # We haven't resolved the target of the Call so arguments
                    # default to having the READWRITE (worst-case) access.
                    access_type = AccessType.READWRITE
                var_accesses.add_access(sig, access_type, arg)

                # Continue processing references in any index expressions.
                for indices in indices_list:
                    for idx in indices:
                        var_accesses.update(idx.reference_accesses())
            else:
                # This argument is not a Reference so continue to walk down the
                # tree. (e.g. it could be/contain a Call to
                # an impure routine in which case any arguments to that Call
                # will have READWRITE access.)
                var_accesses.update(arg.reference_accesses())
        return var_accesses

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
    def arguments(self) -> Tuple[DataNode]:
        '''
        :returns: the children of this node that represent its arguments.
        :rtype: list[py:class:`psyclone.psyir.nodes.DataNode`]
        '''
        if len(self._children) >= 2:
            return tuple(self.children[1:])
        return ()

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
        :returns: whether the routine being called is pure (guaranteed to
            return the same result when provided with the same argument
            values).  If this information is not known then it returns None.
        :rtype: NoneType | bool
        '''
        if (self.routine and self.routine.symbol and
                isinstance(self.routine.symbol, RoutineSymbol)):
            return self.routine.symbol.is_pure
        return None

    def is_available_on_device(self, device_string: str = "") -> bool:
        '''
        :param device_string: optional string to identify the offloading
            device (or its compiler-platform family).
        :returns: whether this call is available on an accelerated device.

        '''
        # pylint: disable=unused-argument
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
        :rtype: :py:class:`psyclone.psyir.node.Call`

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

    def get_callees(self) -> List[Routine]:
        '''
        Searches for the implementation(s) of all potential target routines
        for this Call. It does *not* attempt to resolve static polymorphism
        by checking the argument types.

        :returns: the Routine(s) that this call targets.

        :raises NotImplementedError: if the routine is not found or a
            limitation prevents definite determination of the target routine.

        '''
        rsym = self.routine.symbol
        if rsym.is_unresolved:
            # Search for the Routine in the current file. This search is
            # stopped if we encouter a wildcard import that could be
            # responsible for shadowing the routine name with an external
            # implementation.
            table = rsym.find_symbol_table(self)
            cursor = table.node
            have_codeblock = False
            while cursor:
                # We want to look in both Containers and FileContainers.
                if isinstance(cursor, Container):
                    routines = []
                    for name in cursor.resolve_routine(rsym.name):
                        # Since we're looking in the local Container, the
                        # target is permitted to be private.
                        psyir = cursor.find_routine_psyir(name,
                                                          allow_private=True)
                        if psyir:
                            routines.append(psyir)
                    if routines:
                        return routines
                if not have_codeblock:
                    have_codeblock = any(isinstance(child, CodeBlock) for
                                         child in cursor.children)
                wildcard_names = [csym.name for csym in
                                  cursor.symbol_table.wildcard_imports(
                                      scope_limit=cursor)]
                if wildcard_names:
                    # We haven't yet found an implementation of the Routine
                    # but we have found a wildcard import and that could be
                    # bringing it into scope so we stop searching (the
                    # alternative is to resolve every wildcard import we
                    # encounter and that is very costly).
                    msg = (f"Failed to find the source code of the "
                           f"unresolved routine '{rsym.name}'. It may be "
                           f"being brought into scope from one of "
                           f"{wildcard_names}")
                    if have_codeblock:
                        msg += (" or it may be within a CodeBlock. If it "
                                "isn't, you ")
                    else:
                        msg += ". You "
                    msg += ("may wish to add the appropriate module name "
                            "to the `RESOLVE_IMPORTS` variable in the "
                            "transformation script.")
                    raise NotImplementedError(msg)
                parent = cursor.parent
                cursor = parent.scope if parent else None

            # We haven't found a Routine and nor have we encountered any
            # wildcard imports.
            msg = (f"Failed to find the source code of the unresolved routine "
                   f"'{rsym.name}'. There are no wildcard imports that could "
                   f"be bringing it into scope")
            if have_codeblock:
                msg += (" but it might be within a CodeBlock. If it isn't "
                        "then it ")
            else:
                msg += ". It "
            msg += ("must be an external routine that is only resolved at "
                    "link time and searching for such routines is not "
                    "supported.")
            raise NotImplementedError(msg)

        root_node = self.ancestor(Container)
        if not root_node:
            root_node = self.root
        container = root_node
        can_be_private = True

        if rsym.is_import:
            # Chase down the Container from which the symbol is imported.
            cursor = rsym
            # A Routine imported from another Container must be public in that
            # Container.
            can_be_private = False
            while cursor.is_import:
                csym = cursor.interface.container_symbol
                if cursor.interface.orig_name:
                    target_name = cursor.interface.orig_name
                else:
                    target_name = cursor.name
                try:
                    container = csym.find_container_psyir(local_node=self)
                except SymbolError:
                    raise NotImplementedError(
                        f"RoutineSymbol '{rsym.name}' is imported from "
                        f"Container '{csym.name}' but the source defining "
                        f"that container could not be found. The module search"
                        f" path is set to {Config.get().include_paths}")
                if not container:
                    raise NotImplementedError(
                        f"RoutineSymbol '{rsym.name}' is imported from "
                        f"Container '{csym.name}' but the PSyIR for that "
                        f"container could not be generated.")
                imported_sym = container.symbol_table.lookup(target_name)
                if imported_sym.visibility != Symbol.Visibility.PUBLIC:
                    # The required Symbol must be shadowed with a PRIVATE
                    # Symbol in this Container. This means that the one we
                    # actually want is brought into scope via a wildcard
                    # import.
                    # TODO #924 - Use ModuleManager to search?
                    raise NotImplementedError(
                        f"RoutineSymbol '{target_name}' is imported from "
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

        # At this point, we should have found the PSyIR tree containing the
        # routine - we just need to locate it. It may be in a Container or
        # it may be in the parent FileContainer.
        cursor = container
        while cursor and isinstance(cursor, Container):
            routines = []
            all_names = cursor.resolve_routine(rsym.name)
            if isinstance(rsym, GenericInterfaceSymbol):
                # Although the interface must be public, the routines to which
                # it points may themselves be private.
                can_be_private = True
            for name in all_names:
                psyir = cursor.find_routine_psyir(
                    name, allow_private=can_be_private)
                if psyir:
                    routines.append(psyir)
            if all_names and len(routines) == len(all_names):
                # We've resolved everything.
                return routines
            cursor = cursor.parent

        if isinstance(root_node, Container):
            location_txt = f"Container '{root_node.name}'"
        else:
            out_lines = root_node.debug_string().split("\n")
            idx = -1
            while not out_lines[idx]:
                idx -= 1
            last_line = out_lines[idx]
            location_txt = f"code:\n'{out_lines[0]}\n...\n{last_line}'"

        raise SymbolError(
            f"Failed to find a Routine named '{rsym.name}' in "
            f"{location_txt}. This is normally because the routine"
            f" is within a CodeBlock.")

    def _check_argument_type_matches(
                self,
                call_arg: DataNode,
                routine_arg: DataSymbol
            ) -> None:
        """Checks whether the supplied call and routine arguments are
        compatible. This also supports 'optional' arguments by using
        partial types.

        :param call_arg: One argument of the call
        :param routine_arg: One argument of the routine

        :raises CallMatchingArgumentsNotFound: if the supplied arguments
            do not match.

        """
        def type_symbols_match(type1: Union[DataTypeSymbol, DataType],
                               type2: Union[DataTypeSymbol, DataType]) -> bool:
            '''
            :returns: True if the two types correspond to DataTypeSymbols with
                      the same name (case insensitive), False otherwise.
            '''
            return (isinstance(type1, DataTypeSymbol) and
                    isinstance(type2, DataTypeSymbol) and
                    (type1.name.lower() == type2.name.lower()))

        actual_type = call_arg.datatype
        dummy_type = routine_arg.datatype
        if isinstance(actual_type, ArrayType) and isinstance(dummy_type,
                                                             ArrayType):
            # Arguments must have the same shape.
            if len(actual_type.shape) != len(dummy_type.shape):
                call_arg_str = call_arg.debug_string().strip()
                routine_arg_str = routine_arg.name
                raise CallMatchingArgumentsNotFound(
                    f"Rank mismatch of call argument '{call_arg_str}' "
                    f"(rank {len(actual_type.shape)}) and routine argument "
                    f"'{routine_arg_str}' (rank {len(dummy_type.shape)})")
            # Arguments must have the same intrinsic type.
            if actual_type.intrinsic != dummy_type.intrinsic:
                if type_symbols_match(actual_type.intrinsic,
                                      dummy_type.intrinsic):
                    return
                call_arg_str = call_arg.debug_string().strip()
                routine_arg_str = routine_arg.name
                raise CallMatchingArgumentsNotFound(
                    f"Array argument type mismatch of call argument "
                    f"'{call_arg_str}' ({actual_type.intrinsic}) and routine "
                    f"argument '{routine_arg_str}' ({dummy_type.intrinsic})")
            return

        if isinstance(dummy_type, UnsupportedFortranType):
            # This could be an 'optional' argument. If so, it will have at
            # least a partial datatype which we can check.
            if actual_type != dummy_type.partial_datatype:
                call_arg_str = call_arg.debug_string().strip()
                routine_arg_str = routine_arg.name
                raise CallMatchingArgumentsNotFound(
                    f"Argument partial type mismatch of call argument "
                    f"'{call_arg_str}' ({actual_type}) and routine "
                    f"argument '{routine_arg_str}' ("
                    f"{dummy_type.partial_datatype})"
                )
        else:
            if actual_type != dummy_type:
                if type_symbols_match(actual_type, dummy_type):
                    return
                call_arg_str = call_arg.debug_string().strip()
                routine_arg_str = routine_arg.name
                raise CallMatchingArgumentsNotFound(
                    f"Argument type mismatch of call argument '{call_arg_str}'"
                    f" ({actual_type}) and routine argument "
                    f"'{routine_arg_str}' ({dummy_type})"
                )

    def get_argument_map(self, routine: Routine) -> List[int]:
        '''Return a list of indices mapping from each argument of this
        call to the corresponding entry in the argument list of the
        supplied routine.

        :param routine: the target of this Call.

        :return: list of integers referring to matching arguments of the
                 supplied routine.

        :raises CallMatchingArgumentsNotFound: If there was some problem in
            finding matching arguments.

        '''
        # Create a copy of the list of actual arguments to the routine.
        # Once an argument has been successfully matched, set it to 'None'
        routine_argument_list: List[DataSymbol] = (
            routine.symbol_table.argument_list[:]
        )

        if len(self.arguments) > len(routine.symbol_table.argument_list):
            call_str = self.debug_string().strip()
            raise CallMatchingArgumentsNotFound(
                f"More arguments in call ('{call_str}')"
                f" than callee (routine '{routine.name}')"
            )

        ret_arg_idx_list = []
        # Iterate over all arguments to the call
        for call_arg_idx, call_arg in enumerate(self.arguments):
            call_arg_idx: int
            call_arg: DataNode

            # If the associated name is None, it's a positional argument
            # => Just return the index if the types match
            if self.argument_names[call_arg_idx] is None:
                routine_arg = routine_argument_list[call_arg_idx]
                routine_arg: DataSymbol

                self._check_argument_type_matches(call_arg, routine_arg)

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

                if arg_name.lower() == routine_arg.name.lower():
                    self._check_argument_type_matches(
                        call_arg,
                        routine_arg,
                    )
                    ret_arg_idx_list.append(routine_arg_idx)
                    break

            else:
                # It doesn't match => Raise exception
                raise CallMatchingArgumentsNotFound(
                    f"Named argument '{arg_name}' not found for routine"
                    f" '{routine.name}' in call '{self.debug_string()}'"
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
            if ", OPTIONAL" not in str(routine_arg.datatype):
                call_name = self.debug_string().replace("\n", "")
                raise CallMatchingArgumentsNotFound(
                    f"Argument '{routine_arg.name}' in subroutine "
                    f"'{routine.name}' does not match any in the call "
                    f"'{call_name}' and is not OPTIONAL."
                )

        return ret_arg_idx_list

    def get_callee(
            self,
            use_first_callee_and_no_arg_check: bool = False
    ) -> Tuple[Routine, List[int]]:
        '''
        Searches for the implementation(s) of the target routine for this Call
        including argument checks.

        .. warning::
            If `use_first_callee_and_no_arg_check` is set to True, the very
            first implementation of a Routine with a matching name will be
            returned. In this case, the arguments of the Call and the Routine
            might not match.

        :param use_first_callee_and_no_arg_check: whether or not (the default)
            to just find the first potential callee without checking its
            arguments.

        :returns: A tuple of two elements. The first element is the routine
            that this call targets. The second one a list of arguments
            providing the information on matching argument indices.

        :raises NotImplementedError: if the routine is not local and not found
            in any containers in scope at the call site.

        '''
        routine_list = self.get_callees()

        if use_first_callee_and_no_arg_check:
            arg_match_list = list(i for i in range(
                len(routine_list[0].symbol_table.argument_list)))
            return (routine_list[0], arg_match_list)

        err_info_list = []

        # Search for the routine matching the right arguments
        for routine in routine_list:
            routine: Routine

            try:
                arg_match_list = self.get_argument_map(routine)

            except CallMatchingArgumentsNotFound as err:
                err_info_list.append(err.value)
                continue

            return (routine, arg_match_list)

        error_msg = "\n".join(err_info_list)

        call_str = self.debug_string().replace("\n", "")
        raise CallMatchingArgumentsNotFound(
            f"No matching routine found for '{call_str}':"
            "\n" + error_msg
        )
