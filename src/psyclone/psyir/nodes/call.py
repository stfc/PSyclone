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

from collections.abc import Iterable

from psyclone.core import AccessType
from psyclone.errors import GenerationError
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.symbols import RoutineSymbol
from typing import List


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
        for _, name in self._argument_names:
            if name is not None and name.lower() == existing_name:
                break
            index += 1
        else:
            raise ValueError(
                f"The value of the existing_name argument ({existing_name}) "
                f"in 'replace_named_arg' in the 'Call' node was not found "
                f"in the existing arguments.")
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

        # The RoutineSymbol has a CALL access.
        sig, indices_list = self.routine.get_signature_and_indices()
        var_accesses.add_access(sig, AccessType.CALL, self.routine)
        # Continue processing references in any index expressions.
        for indices in indices_list:
            for idx in indices:
                idx.reference_accesses(var_accesses)

        for arg in self.arguments:
            if isinstance(arg, Reference):
                # This argument is pass-by-reference.
                sig, indices_list = arg.get_signature_and_indices()
                var_accesses.add_access(sig, default_access, arg)
                # Continue processing references in any index expressions.
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
        :returns: whether the routine being called is pure (guaranteed to
            return the same result when provided with the same argument
            values).  If this information is not known then it returns None.
        :rtype: NoneType | bool
        '''
        if (self.routine and self.routine.symbol and
                isinstance(self.routine.symbol, RoutineSymbol)):
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

    def get_callees(self, ignore_missing_modules: bool = False):
        '''
        Searches for the implementation(s) of all potential target routines
        for this Call without any arguments check.

        Deprecation warning: This only exists for backwards compatibility
        reason. It's recommende to directly use `CallRoutineMatcher`.

        :param ignore_missing_modules: If a module wasn't found, return 'None'
            instead of throwing an exception 'ModuleNotFound'.
        :type ignore_missing_modules: bool

        :returns: the Routine(s) that this call targets.
        :rtype: list[:py:class:`psyclone.psyir.nodes.Routine`]

        :raises NotImplementedError: if the routine is not local and not found
            in any containers in scope at the call site.

        '''
        from psyclone.psyir.tools import (
            CallRoutineMatcher
        )

        call_routine_matcher: CallRoutineMatcher = CallRoutineMatcher(self)
        call_routine_matcher.set_option(
            ignore_missing_modules=ignore_missing_modules,
        )

        return call_routine_matcher.get_callee_candidates()

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

        If `check_matching_arguments` is set to `False`, the very first
        implementation of the matching routine will be returned in case no
        match was found. Then, the arguments of the call and routine
        might not match each other.

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

        from psyclone.psyir.tools.call_routine_matcher import (
            CallRoutineMatcher
        )

        call_routine_matcher: CallRoutineMatcher = CallRoutineMatcher(self)
        call_routine_matcher.set_option(
                check_matching_arguments=check_matching_arguments,
                check_strict_array_datatype=(
                    check_strict_array_datatype),
                ignore_missing_modules=ignore_missing_modules,
                ignore_unresolved_types=ignore_unresolved_symbol,
            )

        return call_routine_matcher.get_callee()
