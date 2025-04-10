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
# Modified I. Kavcic, A. Coughtrie, L. Turner and O. Brunt, Met Office
# Modified J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk and N. Nobre, STFC Daresbury Lab

''' This module contians the LFRicRunTimeChecks class which handles
declarations and code generation for run-time checks. The methods
check fields' function spaces and read-only fields against kernel
function-space metadata on initialisation. The class inherits from
the LFRicCollection class.'''

# Imports
from psyclone.configuration import Config
from psyclone.core import AccessType
from psyclone.domain.lfric import LFRicCollection, LFRicConstants
from psyclone.psyir.symbols import (
    CHARACTER_TYPE, ContainerSymbol, RoutineSymbol, ImportInterface,
    DataSymbol, UnresolvedType, INTEGER_TYPE)
from psyclone.psyir.nodes import (
    Call, StructureReference, BinaryOperation, Reference, Literal, IfBlock,
    ArrayOfStructuresReference)


class LFRicRunTimeChecks(LFRicCollection):
    '''Handle declarations and code generation for run-time checks. This
    is not used in the stub generator.

    '''

    def invoke_declarations(self):
        '''Insert declarations of all data and functions required by the
        run-time checks code into the PSy layer.

        '''
        super().invoke_declarations()
        if Config.get().api_conf("lfric").run_time_checks:
            # Only add if run-time checks are requested
            const = LFRicConstants()
            csym = self.symtab.find_or_create(
                const.UTILITIES_MOD_MAP["logging"]["module"],
                symbol_type=ContainerSymbol
            )
            self.symtab.find_or_create(
                "log_event", symbol_type=RoutineSymbol,
                interface=ImportInterface(csym)
            )
            self.symtab.find_or_create(
                "LOG_LEVEL_ERROR", symbol_type=DataSymbol,
                datatype=UnresolvedType(),
                interface=ImportInterface(csym)
            )

    def _check_field_fs(self, cursor: int) -> int:
        '''
        Internal method that adds run-time checks to make sure that the
        field's function space is consistent with the appropriate
        kernel metadata function spaces.

        :param int cursor: position where to add the next initialisation
            statements.
        :returns: Updated cursor value.
        :rtype: int

        '''
        # When issue #30 is addressed (with issue #79 helping further)
        # we may know some or all field function spaces statically. If
        # so, we should remove these from the fields to check at run
        # time (as they will have already been checked at code
        # generation time).

        const = LFRicConstants()
        symtab = self._invoke.schedule.symbol_table
        existing_checks = []
        first = True
        for kern_call in self._invoke.schedule.kernels():
            for arg in kern_call.arguments.args:
                if not arg.is_field:
                    # This check is limited to fields
                    continue
                fs_name = arg.function_space.orig_name
                field_name = arg.name_indexed
                if fs_name in const.VALID_ANY_SPACE_NAMES:
                    # We don't need to check validity of a field's
                    # function space if the metadata specifies
                    # any_space as this means that all spaces are
                    # valid.
                    continue
                if (fs_name, field_name) in existing_checks:
                    # This particular combination has already been
                    # checked.
                    continue
                existing_checks.append((fs_name, field_name))

                if fs_name in const.VALID_ANY_DISCONTINUOUS_SPACE_NAMES:
                    # We need to check against all discontinuous
                    # function spaces
                    function_space_names = const.DISCONTINUOUS_FUNCTION_SPACES
                elif fs_name == "any_w2":
                    # We need to check against all any_w2 function
                    # spaces
                    function_space_names = const.ANY_W2_FUNCTION_SPACES
                else:
                    # We need to check against a specific function space
                    function_space_names = [fs_name]

                field_symbol = symtab.lookup(arg.name)

                if_condition = None
                for name in function_space_names:
                    if arg._vector_size > 1:
                        call = Call.create(ArrayOfStructuresReference.create(
                            field_symbol, [Literal('1', INTEGER_TYPE)],
                            ["which_function_space"]))
                    else:
                        call = Call.create(StructureReference.create(
                            field_symbol, ["which_function_space"]))
                    mod_symbol = symtab.find_or_create(
                        "fs_continuity_mod", symbol_type=ContainerSymbol)
                    symbol = symtab.find_or_create(
                        name.upper(),
                        interface=ImportInterface(mod_symbol))
                    cmp = BinaryOperation.create(
                        BinaryOperation.Operator.NE,
                        call, Reference(symbol)
                    )
                    if if_condition is None:
                        if_condition = cmp
                    else:
                        if_condition = BinaryOperation.create(
                            BinaryOperation.Operator.AND, if_condition, cmp
                        )

                if_body = Call.create(
                    symtab.lookup("log_event"),
                    [Literal(f"In alg '{self._invoke.invokes.psy.orig_name}' "
                             f"invoke '{self._invoke.name}', the field "
                             f"'{arg.name}' is passed to kernel "
                             f"'{kern_call.name}' but its function space is "
                             f"not compatible with the function space "
                             f"specified in the kernel metadata '{fs_name}'.",
                             CHARACTER_TYPE),
                     Reference(symtab.lookup("LOG_LEVEL_ERROR"))])

                ifblock = IfBlock.create(if_condition, [if_body])
                self._invoke.schedule.addchild(ifblock, cursor)
                cursor += 1
                if first:
                    ifblock.preceding_comment = (
                        "Check field function space and kernel metadata "
                        "function spaces are compatible")
                    first = False
        return cursor

    def _check_field_ro(self, cursor: int) -> int:
        '''
        Internal method that adds runtime checks to make sure that if the
        field is on a read-only function space then the associated
        kernel metadata does not specify that the field is modified.

        As we make use of the LFRic infrastructure halo exchange
        function, there is no need to check whether the halo of a
        read-only field is clean (which it should always be) as the
        LFric halo-exchange will raise an exception if it is called
        with a read-only field.

        Whilst the LFRic infrastructure halo exchange would also
        indirectly pick up a readonly field being modified, it would
        not be picked up where the error occured. Therefore adding
        checks here is still useful.

        :param cursor: position where to add the next initialisation
            statements.
        :returns: Updated cursor value.

        '''
        symtab = self._invoke.schedule.symbol_table
        # When issue #30 is addressed (with issue #79 helping further)
        # we may know some or all field function spaces statically. If
        # so, we should remove these from the fields to check at run
        # time (as they will have already been checked at code
        # generation time).

        # Create a list of modified fields
        modified_fields = []
        for call in self._invoke.schedule.kernels():
            for arg in call.arguments.args:
                if (arg.text and arg.is_field and
                        arg.access != AccessType.READ and
                        not [entry for entry in modified_fields if
                             entry[0].name == arg.name]):
                    modified_fields.append((arg, call))
        first = True
        for field, call in modified_fields:
            if_condition = field.generate_method_call("is_readonly")
            if_body = Call.create(
                symtab.lookup("log_event"),
                [Literal(f"In alg '{self._invoke.invokes.psy.orig_name}' "
                         f"invoke '{self._invoke.name}', field '{field.name}' "
                         f"is on a read-only function space but is modified "
                         f"by kernel '{call.name}'.", CHARACTER_TYPE),
                 Reference(symtab.lookup("LOG_LEVEL_ERROR"))])

            ifblock = IfBlock.create(if_condition, [if_body])
            self._invoke.schedule.addchild(ifblock, cursor)
            cursor += 1
            if first:
                ifblock.preceding_comment = (
                    "Check that read-only fields are not modified")
                first = False
        return cursor

    def initialise(self, cursor: int) -> int:
        '''Add runtime checks to make sure that the arguments being passed
        from the algorithm layer are consistent with the metadata
        specified in the associated kernels. Currently checks are
        limited to ensuring that field function spaces are consistent
        with the associated kernel function-space metadata.

        :param cursor: position where to add the next initialisation
            statements.
        :returns: Updated cursor value.

        '''
        if not Config.get().api_conf("lfric").run_time_checks:
            # Run-time checks are not requested.
            return cursor

        init_cursor = cursor

        # Check that field function spaces are compatible with the
        # function spaces specified in the kernel metadata.
        cursor = self._check_field_fs(cursor)

        # Check that fields on read-only function spaces are not
        # passed into a kernel where the kernel metadata specifies
        # that the field will be modified.
        cursor = self._check_field_ro(cursor)

        self._invoke.schedule[init_cursor].preceding_comment = (
            "Perform run-time checks\n"
            + self._invoke.schedule[init_cursor].preceding_comment
        )

        # These checks should be expanded. Issue #768 suggests
        # extending function space checks to operators.
        return cursor


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for.
__all__ = ['LFRicRunTimeChecks']
