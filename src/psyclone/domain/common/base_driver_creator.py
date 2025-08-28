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
# Modified: S. Siso, STFC Daresbury Lab

'''This module provides a base class for all domain-specific kernel extraction
implementations.
'''

from typing import List, Optional, Tuple

from psyclone.core import Signature
from psyclone.parse import ModuleManager
from psyclone.psyir.nodes import (
    Call, Literal, Reference, ExtractNode, Routine, Node)
from psyclone.psyir.symbols import (
    CHARACTER_TYPE, ContainerSymbol, ImportInterface, INTEGER_TYPE, NoType,
    RoutineSymbol, DataSymbol, UnsupportedFortranType, Symbol,
    AutomaticInterface, UnresolvedType, SymbolTable)
from psyclone.psyir.tools import ReadWriteInfo


class BaseDriverCreator:
    '''This class provides the functionality common to all driver creations.
    A more extended version will be created for TODO #2049, for now it only
    provides two identical methods for LFRicExtractDriverCreator and
    ExtractDriverCreator (which might need to be renamed to GOcean....)

    '''
    # TODO #2049: Turn this into a proper base class.

    # -------------------------------------------------------------------------
    @staticmethod
    def add_call(program: Routine, name: str, args: List[Node]):
        '''This function creates a call to the subroutine of the given name,
        providing the arguments. The call will be added to the program and
        the corresponding RoutineSymbol to its symbol table (if not already
        present).

        :param program: the PSyIR Routine to which any code must be added.
        :param name: name of the subroutine to call.
        :param args: list of all arguments for the call.

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

    @staticmethod
    def add_read_call(program: Routine, name_lit: Literal, sym: DataSymbol,
                      read_var: str):
        '''This function creates a call to the subroutine that read fields
        from the data file.

        :param program: the PSyIR Routine to which any code must be added.
        :param name_lit: the name of the field in the data file.
        :param sym: the symbol to store the read data.
        :param str read_var: the method name to read the data.
        '''
        # TODO #2898: the test for array can be removed if
        # `is_allocatable` is supported for non-arrays.
        if sym.is_array and not sym.datatype.is_allocatable:
            # In case of a non-allocatable array (e.g. a constant
            # size array from a module), call the ReadVariable
            # function that does not require an allocatable field
            BaseDriverCreator.add_call(program, read_var+"NonAlloc",
                                       [name_lit, Reference(sym)])
        else:
            # In case of an allocatable array, call the ReadVariable
            # function that will also allocate this array.
            BaseDriverCreator.add_call(program, read_var,
                                       [name_lit, Reference(sym)])

    # -------------------------------------------------------------------------
    @staticmethod
    def add_result_tests(program: Routine,
                         output_symbols: List[Tuple[Symbol, Symbol]]) -> None:
        '''Adds tests to check that all output variables have the expected
        value.

        :param program: the program to which the tests should be added.
        :param output_symbols: a list containing all output variables of
            the executed code. Each entry in the list is a 2-tuple,
            containing first the symbol that was computed when executing
            the kernels, and then the symbol containing the expected
            values that have been read in from a file.
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

    @staticmethod
    def _create_output_var_code(
        name: str, program: Routine, read_var: str,
        postfix: str, module_name: Optional[str] = None
    ) -> Tuple[Symbol, Symbol]:
        '''
        This function creates all code required for an output variable:
        1. It declares (and initialises if necessary) the post variable
        2. It reads the '_post' field which stores the expected value of
        variables at the end of the driver.

        :param name: the name of original variable (i.e.
            without _post), which will be looked up as a tag in the symbol
            table. If index is provided, it is incorporated in the tag using
            f"{name}_{index}_data".
        :param program: the PSyIR Routine to which any code must
            be added. It also contains the symbol table to be used.
        :param read_var: the readvar method to be used including the
            name of the PSyData object (e.g. 'psy_data%ReadVar')
        :param postfix: the postfix to use for the expected output
            values, which are read from the file.
        :param module_name: if the variable is part of an external module,
            this contains the module name from which it is imported.
            Otherwise, this must either not be specified or an empty string.

        :returns: a 2-tuple containing the output Symbol after the kernel,
             and the expected output read from the file.

        '''
        # Obtain the symbol of interest
        symbol_table = program.symbol_table
        if module_name:
            # If a module_name is specified, this indicates that this variable
            # is imported from an external module. The name of the module will
            # be appended to the tag used in the extracted kernel file, e.g.
            # `dummy_var2@dummy_mod`.
            sym = symbol_table.lookup_with_tag(f"{name}@{module_name}")
        else:
            sym = symbol_table.lookup(name)

        # For each variable, we declare a new variable that stores the expected
        # value with the same datatype and with the given postfix
        post_name = sym.name + postfix
        post_sym = symbol_table.new_symbol(post_name,
                                           symbol_type=DataSymbol,
                                           datatype=sym.datatype.copy())
        if isinstance(post_sym.datatype, UnsupportedFortranType):
            # Manually update symbol name from Unsupported declarations
            # pylint: disable=protected-access
            post_sym.datatype = post_sym.datatype.copy()
            post_sym.datatype._declaration = \
                post_sym.datatype._declaration.replace(sym.name, post_name)

        # Add a psydata read call with the proper name_tag
        if module_name:
            post_tag = f"{name}{postfix}@{module_name}"
        else:
            post_tag = f"{name}{postfix}"
        name_lit = Literal(post_tag, CHARACTER_TYPE)
        BaseDriverCreator.add_read_call(program, name_lit, post_sym, read_var)

        return (sym, post_sym)

    def _create_read_in_code(
            self,
            program: Routine,
            psy_data: DataSymbol,
            original_symtab: SymbolTable,
            read_write_info: ReadWriteInfo,
            postfix: str,
            removable_vars: List[Tuple[str, Signature]],
        ) -> List[Tuple[Symbol, Symbol]]:
        '''This function creates the code that reads in the data file
        produced during extraction. For each:

        - input variable, it will declare the symbol and add code that reads in
          the variable using the PSyData library.
        - output variable, it will create code to read in the expected value,
          and at the end compare the driver value with the expected one.

        :param program: the PSyIR Routine to which any code must be added.
        :param psy_data: the PSyData symbol to be used.
        :param original_symtab: this is needed because read_write_info has
            signatures instead of symbols, and the signature still have to
            be looked up to retrive the symbol and then the type.
        :param read_write_info: information about all input and output
            parameters.
        :param postfix: a postfix that is added to a variable name to
            create the corresponding variable that stores the output
            value from the kernel data file.

        :returns: all output variables. Each entry is a 2-tuple containing the
            symbol of the output variable, and the symbol that contains the
            originally values read from the data file.

        '''
        symbol_table = program.scope.symbol_table
        read_var = f"{psy_data.name}%ReadVariable"

        # First handle the input local variables that are read (local variables
        # do not have a module_name and are guaranteed to be in the symtab when
        # doing lookups, external variables are handled below). Note that at
        # the moment we consider all read and/or written as input variables.
        read_stmts = []
        for module_name, signature in read_write_info.all_used_vars_list:
            if not module_name:
                orig_sym = original_symtab.lookup(signature[0])
                sym = orig_sym.copy()
                sym.interface = AutomaticInterface()
                symbol_table.add(sym)
                # Only add the variable if it is not to be ignored
                if (module_name, signature) not in removable_vars:
                    name_lit = Literal(str(signature), CHARACTER_TYPE)
                    read_stmts.append((name_lit, sym))

        # Now do the input external variables. This are done after the locals
        # so that they match the literal tags of the extracting psy-layer
        ExtractNode.bring_external_symbols(read_write_info, symbol_table)
        mod_man = ModuleManager.get()
        for module_name, signature in read_write_info.all_used_vars_list:
            # Only add if a variable is not supposed to be ignored
            if module_name and (module_name, signature) not in removable_vars:
                mod_info = mod_man.get_module_info(module_name)
                orig_sym = mod_info.get_symbol(signature[0])
                tag = f"{signature[0]}@{module_name}"
                sym = symbol_table.lookup_with_tag(tag)
                name_lit = Literal(tag, CHARACTER_TYPE)
                read_stmts.append((name_lit, sym))

        for name_lit, sym in read_stmts:
            self.add_read_call(program, name_lit, sym, read_var)

        # Finally handle the output variables (these are the ones compared
        # to a stored _post variable)
        output_symbols = []
        for module_name, signature in read_write_info.write_list:
            if (module_name, signature) in removable_vars:
                continue
            # Find the right symbol for the variable. Note that all variables
            # in the input and output list have been detected as being used
            # when the variable accesses were analysed. Therefore, these
            # variables will already have been declared in the symbol table.
            if module_name:
                orig_sym = mod_man.get_module_info(module_name).get_symbol(
                    signature[0])
            else:
                orig_sym = symbol_table.lookup(signature[0])
            sym_tuple = self._create_output_var_code(
                str(signature), program, read_var, postfix,
                module_name=module_name)
            output_symbols.append(sym_tuple)

        return output_symbols

    @staticmethod
    def _make_valid_unit_name(name: str) -> str:
        '''Valid program or routine names are restricted to 63 characters,
        and no special characters like '-' (which is used when adding
        invoke and region numbers).

        :param name: a proposed unit name.

        :returns: a valid program or routine name with special characters
            removed and restricted to a length of 63 characters.

        '''
        return name.replace("-", "")[:63]

    @staticmethod
    def import_modules(program: Routine) -> None:
        '''This function adds all the import statements required for the
        actual kernel calls. It finds all calls in the PSyIR tree and
        checks for calls with a ImportInterface. Any such call will
        get a ContainerSymbol added for the module, and a RoutineSymbol
        with an import interface pointing to this module.

        :param program: the PSyIR Routine to which any code must be added.

        '''
        symtab = program.scope.symbol_table
        for call in program.walk(Call):
            routine = call.routine.symbol
            if not isinstance(routine.interface, ImportInterface):
                continue
            if routine.name in symtab:
                # Symbol has already been added - ignore
                continue
            # We need to create a new symbol for the module and the routine
            # called (the PSyIR backend will then create a suitable import
            # statement).
            if routine.interface.container_symbol.name in symtab:
                module = symtab.lookup(routine.interface.container_symbol.name)
            else:
                module = ContainerSymbol(
                            routine.interface.container_symbol.name)
                symtab.add(module)
            new_routine_sym = RoutineSymbol(routine.name, UnresolvedType(),
                                            interface=ImportInterface(module))
            symtab.add(new_routine_sym)
