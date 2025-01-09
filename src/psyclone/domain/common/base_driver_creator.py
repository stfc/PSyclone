# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024-2025, Science and Technology Facilities Council.
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
# Authors: J. Henrichs, Bureau of Meteorology

'''This module provides a base class for all domain-specific kernel extraction
implementations.
'''

from psyclone.psyir.nodes import Call, Literal, Reference
from psyclone.psyir.symbols import (CHARACTER_TYPE, ContainerSymbol,
                                    ImportInterface, INTEGER_TYPE, NoType,
                                    RoutineSymbol)


class BaseDriverCreator:
    '''This class provides the functionality common to all driver creations.
    A more extended version will be created for TODO #2049, for now it only
    provides two identical methods for LFRicExtractDriverCreator and
    ExtractDriverCreator (which might need to be renamed to GOcean....)

    '''
    # TODO #2049: Turn this into a proper base class.

    # -------------------------------------------------------------------------
    @staticmethod
    def add_call(program, name, args):
        '''This function creates a call to the subroutine of the given name,
        providing the arguments. The call will be added to the program and
        the corresponding RoutineSymbol to its symbol table (if not already
        present).

        :param program: the PSyIR Routine to which any code must
            be added. It also contains the symbol table to be used.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`
        :param str name: name of the subroutine to call.
        :param args: list of all arguments for the call.
        :type args: list[:py:class:`psyclone.psyir.nodes.Node`]

        :raises TypeError: if there is a symbol with the
            specified name defined that is not a RoutineSymbol.
        '''
        if name in program.symbol_table:
            routine_symbol = program.symbol_table.lookup(name)
            if not isinstance(routine_symbol, RoutineSymbol):
                raise TypeError(
                    f"Error creating call to '{name}' - existing symbol is "
                    f"of type '{type(routine_symbol).__name__}', not a "
                    f"'RoutineSymbol'.")
        else:
            routine_symbol = RoutineSymbol(name)
            program.symbol_table.add(routine_symbol)
        call = Call.create(routine_symbol, args)
        program.addchild(call)

    # -------------------------------------------------------------------------
    @staticmethod
    def add_result_tests(program, output_symbols):
        '''Adds tests to check that all output variables have the expected
        value.

        :param program: the program to which the tests should be added.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`
        :param output_symbols: a list containing all output variables of
            the executed code. Each entry in the list is a 2-tuple,
            containing first the symbol that was computed when executing
            the kernels, and then the symbol containing the expected
            values that have been read in from a file.
        :type output_symbols: list[tuple[
            :py:class:`psyclone.psyir.symbols.Symbol`,
            :py:class:`psyclone.psyir.symbols.Symbol`]]
        '''

        module = ContainerSymbol("compare_variables_mod")
        program.symbol_table.add(module)
        for compare_func in ["compare", "compare_init", "compare_summary"]:
            compare_sym = RoutineSymbol(compare_func, NoType(),
                                        interface=ImportInterface(module))
            program.symbol_table.add(compare_sym)

        BaseDriverCreator.add_call(program, "compare_init",
                                   [Literal(f"{len(output_symbols)}",
                                            INTEGER_TYPE)])

        # TODO #2083: check if this can be combined with psyad result
        # comparison.
        for (sym_computed, sym_read) in output_symbols:
            lit_name = Literal(sym_computed.name, CHARACTER_TYPE)
            BaseDriverCreator.add_call(program, "compare",
                                       [lit_name, Reference(sym_computed),
                                        Reference(sym_read)])

        BaseDriverCreator.add_call(program, "compare_summary", [])
