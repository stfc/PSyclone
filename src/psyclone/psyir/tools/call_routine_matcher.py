# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024, Science and Technology Facilities Council and
#   University Grenoble Alpes
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
# This file is based on gathering various components related to
# calls and routines from across psyclone. Hence, there's no clear author.
# Initial author of this file: M. Schreiber, University Grenoble Alpes
# Further authors: R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

from typing import List, Union, Set
from psyclone.psyir.symbols.datatypes import ArrayType, UnresolvedType
from psyclone.errors import PSycloneError
from psyclone.psyir.nodes import Call, Routine
from psyclone.psyir.nodes.container import Container
from psyclone.psyir.symbols import (
    RoutineSymbol,
    Symbol,
    SymbolError,
    UnsupportedFortranType,
    DataSymbol,
    SymbolTable,
    ContainerSymbol,
)
from psyclone.configuration import Config


class CallMatchingArgumentsNotFoundError(PSycloneError):
    """Exception to signal that matching arguments have not been found
    for this routine

    """

    def __init__(self, value):
        PSycloneError.__init__(self, value)
        self.value = "CallMatchingArgumentsNotFound: " + str(value)


class CallRoutineMatcher:
    """Helper routines to help matching 'Call' and 'Routines'.
    This includes, e.g.,
    - searching for matching 'Routines',
    - argument matching
    """

    def __init__(self, call_node: Call = None, routine_node: Routine = None):

        # Psyir node of Call
        self._call_node: Call = call_node

        # Psyir node of Routine
        self._routine_node: Call = routine_node

        # List of indices relating each argument of call to one argument
        # of routine. This is required to support optional arguments.
        self._arg_match_list: List[int] = None

        # Also check argument types to match.
        # If set to `False` and in case it doesn't find matching arguments,
        # the very first implementation of the matching routine will be
        # returned (even if the argument type check failed). The argument
        # types and number of arguments might therefore mismatch!
        self._option_check_matching_arguments: bool = True

        # Use strict array datatype checks for matching
        self._option_check_strict_array_datatype: bool = True

        # If 'True', missing modules don't raise an Exception
        self._option_ignore_missing_modules: bool = False

        # If 'True', unresolved types don't raise an Exception
        self._option_ignore_unresolved_types: bool = False

    def set_call_node(self, call_node: Call):
        self._call_node = call_node

    def set_routine_node(self, routine_node: Routine):
        self._routine_node = routine_node

    def set_option(self,
                   check_matching_arguments: bool = None,
                   check_strict_array_datatype: bool = None,
                   ignore_missing_modules: bool = None,
                   ignore_unresolved_types: bool = None,
                   ):

        if check_matching_arguments is not None:
            self._option_check_matching_arguments = check_matching_arguments

        if check_strict_array_datatype is not None:
            self._option_check_strict_array_datatype = (
                check_strict_array_datatype)

        if ignore_missing_modules is not None:
            self._option_ignore_missing_modules = ignore_missing_modules

        if ignore_unresolved_types is not None:
            self._option_ignore_unresolved_types = ignore_unresolved_types

    def _check_argument_type_matches(
        self,
        call_arg: DataSymbol,
        routine_arg: DataSymbol
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

        if self._option_check_strict_array_datatype:
            # No strict array checks have to be performed, just accept it
            if isinstance(call_arg.datatype, ArrayType) and isinstance(
                routine_arg.datatype, ArrayType
            ):
                return True

        if self._option_ignore_unresolved_types:
            if isinstance(call_arg.datatype, UnresolvedType):
                return True

        if isinstance(routine_arg.datatype, UnsupportedFortranType):
            # This could be an 'optional' argument.
            # This has at least a partial data type
            if call_arg.datatype != routine_arg.datatype.partial_datatype:
                raise CallMatchingArgumentsNotFoundError(
                    "Argument partial type mismatch of call "
                    f"argument '{call_arg}' and routine argument "
                    f"'{routine_arg}'"
                )

            return True

        if (isinstance(routine_arg.datatype, ArrayType) and
                isinstance(call_arg.datatype, ArrayType)):
            
            # If these are two arrays, only make sure that the types
            # match.
            if (call_arg.datatype.datatype == routine_arg.datatype.datatype):
                return True

        if call_arg.datatype != routine_arg.datatype:
            raise CallMatchingArgumentsNotFoundError(
                "Argument type mismatch of call argument "
                f"'{call_arg}' with type '{call_arg.datatype}' "
                "and routine argument "
                f"'{routine_arg}' with type '{routine_arg.datatype}'."
            )

        return True

    def get_argument_routine_match_list(
        self,
    ) -> Union[None, List[int]]:
        '''Return a list of integers giving for each argument of the call
        the index of the argument in argument_list (typically of a routine)

        :return: None if no match was found, otherwise list of integers
            referring to matching arguments.
        :rtype: None|List[int]
        :raises CallMatchingArgumentsNotFound: If there was some problem in
            finding matching arguments.
        '''

        # Create a copy of the list of actual arguments to the routine.
        # Once an argument has been successfully matched, set it to 'None'
        routine_argument_list: List[DataSymbol] = (
            self._routine_node.symbol_table.argument_list[:]
        )

        if len(self._call_node.arguments) > len(
                self._routine_node.symbol_table.argument_list):
            call_str = self._call_node.debug_string().replace("\n", "")
            raise CallMatchingArgumentsNotFoundError(
                f"More arguments in call ('{call_str}')"
                f" than callee (routine '{self._routine_node.name}')"
            )

        # Iterate over all arguments to the call
        ret_arg_idx_list = []
        for call_arg_idx, call_arg in enumerate(self._call_node.arguments):
            call_arg_idx: int
            call_arg: DataSymbol

            # If the associated name is None, it's a positional argument
            # => Just return the index if the types match
            if self._call_node.argument_names[call_arg_idx] is None:
                routine_arg = routine_argument_list[call_arg_idx]
                routine_arg: DataSymbol

                self._check_argument_type_matches(
                    call_arg, routine_arg
                )

                ret_arg_idx_list.append(call_arg_idx)
                routine_argument_list[call_arg_idx] = None
                continue

            #
            # Next, we handle all named arguments
            #
            arg_name = self._call_node.argument_names[call_arg_idx]
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
                        routine_arg
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
                f" '{self._routine_node.name}' not handled"
            )

        return ret_arg_idx_list

    def get_callee_candidates(self) -> List[Routine]:
        '''
        Searches for the implementation(s) of all potential target routines
        for this Call without any arguments check.

        :returns: the Routine(s) that this call targets.

        :raises NotImplementedError: if the routine is not local and not found
            in any containers in scope at the call site.

        '''

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

        rsym = self._call_node.routine.symbol
        if rsym.is_unresolved:

            # Check for any "raw" Routines, i.e. ones that are not
            # in a Container.  Such Routines would exist in the PSyIR
            # as a child of a FileContainer (if the PSyIR contains a
            # FileContainer). Note, if the PSyIR does contain a
            # FileContainer, it will be the root node of the PSyIR.
            for routine in self._call_node.root.children:
                if (
                    isinstance(routine, Routine)
                    and routine.name.lower() == rsym.name.lower()
                ):
                    return [routine]

            # Now check for any wildcard imports and see if they can
            # be used to resolve the symbol.
            wildcard_names = []
            containers_not_found = []
            current_table: SymbolTable = self._call_node.scope.symbol_table
            while current_table:
                # TODO: Obtaining all container symbols in this way
                # breaks some tests.
                # It would be better using the ModuleManager to resolve
                # (and cache) all containers to look up for this.
                #
                # current_containersymbols =
                #   self._call_node._get_container_symbols_rec(
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
                                    local_node=self._call_node,
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
                        "attempted to resolve the wildcard imports from"
                        f" {wildcard_names}. However, failed to find the "
                        f"source for {containers_not_found}. The module search"
                        f" path is set to {Config.get().include_paths}"
                    )
                else:
                    wc_text = f"wildcard imports from {wildcard_names}"
            raise NotImplementedError(
                "Failed to find the source code of the unresolved routine "
                f"'{rsym.name}' - looked at any routines in the same source "
                f"file and {wc_text}. Searching for external routines "
                "that are only resolved at link time is not supported."
            )

        root_node = self._call_node.ancestor(Container)
        if not root_node:
            root_node = self._call_node.root
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
                    container = csym.find_container_psyir(
                        local_node=self._call_node)
                except SymbolError:
                    raise NotImplementedError(
                        f"RoutineSymbol '{rsym.name}' is imported from "
                        f"Container '{csym.name}' but the source defining "
                        "that container could not be found. The module search"
                        f" path is set to {Config.get().include_paths}"
                    )
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
                        "a private Symbol of the same name. Searching for the"
                        " Container that defines a public Routine with that "
                        "name is not yet supported - TODO #924"
                    )
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
                "Cannot get the PSyIR of such a routine."
            )

        if isinstance(container, Container):
            routines = []
            for name in container.resolve_routine(rsym.name):
                psyir = container.find_routine_psyir(
                    name, allow_private=can_be_private
                )
                if psyir:
                    routines.append(psyir)
            if routines:
                return routines

        raise SymbolError(
            f"Failed to find a Routine named '{rsym.name}' in "
            f"{_location_txt(root_node)}. This is normally because the routine"
            " is within a CodeBlock."
        )

    def get_callee(self) -> Set[Union[Routine, List[int]]]:
        '''
        Searches for the implementation(s) of the target routine for this Call
        including argument checks.

        :returns: A tuple of two elements. The first element is the routine
            that this call targets. The second one a list of arguments
            providing the information on matching argument indices.

        :raises CallMatchingArgumentsNotFoundError: if the routine is not local
            and not found in any containers in scope at the call site or if
            the arguments don't match.
        '''

        routine_list = self.get_callee_candidates()
        assert len(routine_list) != 0

        err_info_list = []

        # Search for the routine matching the right arguments
        for routine_node in routine_list:
            routine_node: Routine
            self._routine_node = routine_node

            try:
                self._arg_match_list = self.get_argument_routine_match_list()
            except CallMatchingArgumentsNotFoundError as err:
                err_info_list.append(err.value)
                continue

            return (self._routine_node, self._arg_match_list)

        # If we didn't find any routine, return some routine if no matching
        # arguments have been found.
        # This is handy for the transition phase until optional argument
        # matching is supported.
        if not self._option_check_matching_arguments:
            # Also return a list of dummy argument indices
            self._routine_node = routine_list[0]
            self._arg_match_list = list(range(len(self._call_node.arguments)))
            return (self._routine_node, self._arg_match_list)

        error_msg = "\n".join(err_info_list)

        raise CallMatchingArgumentsNotFoundError(
            "Found routines, but no routine with matching arguments found "
            f"for '{self._call_node.routine.name}':\n"
            + error_msg
        )
