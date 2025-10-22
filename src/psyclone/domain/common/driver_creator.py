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

from abc import abstractmethod
from typing import Optional

from psyclone.line_length import FortLineLength
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.backend.language_writer import LanguageWriter
from psyclone.parse import ModuleManager
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import (
    Call, ExtractNode, FileContainer, Literal, Node, Reference, Routine,
    StructureReference)
from psyclone.psyir.symbols import (
    CHARACTER_TYPE, ContainerSymbol, DataTypeSymbol, ImportInterface,
    INTEGER_TYPE, NoType, RoutineSymbol, DataSymbol, UnsupportedFortranType,
    Symbol, AutomaticInterface, UnresolvedType, SymbolTable)
from psyclone.psyir.tools import ReadWriteInfo


class DriverCreator:
    '''This class provides the functionality common to all driver creations.
    The driver is created as follows:

    1. A file container is created and the (empty) driver program is added.
    2. All symbols from the `read_kernel_data_mod` are added to the symbol
       table, and a `psy_data` symbol is created that handles the reading
       of data from the kernel data file.
    3. The command line handler is added to the driver. It is responsible
       for handling command line parameters for the driver specifying the
       kernel data file to read (and if nothing is specified to use the
       default, which is the name used in the extraction without MPI
       rank attached).
    4. A copy of the kernel that is being instrumented is created, so that
       it can be modified without affecting the actual code creation if
       PSyclone.
    5. The method `verify_and_cleanup_psyir` is called with the copied kernel
       as parameter. The base implementation checks that no structure accesses
       are in the code (which are not supported - the structures used by the
       domain-specific code have already been replaced since the extracted
       region has been lowered). This can be overwritten by domain-specific
       implementation. For example, in LFRic calls to `set_dirty` and
       `set_clean` are removed (since they are irrelevant) first.
    6. The read-in code is added, using the read_write information which
       is used when extracting the data. This ensures that the extraction
       and driver are matching.
    7. The copy of the kernel is added.
    8. All modules required by the kernel are imported.
    9. A call to `handle_precision_symbols` is made, allowing domain-specific
       derived classes to add precision related symbols as required
       (see `lfric_extract_driver_creator.py`), or to replace precision
       symbols (see `extract_driver_creator.py`).
    10. Add the code that will compare the results, i.e. comparing all output
        variables computed in the kernel with the post values stored in the
        kernel data file.

    :param region_name: Suggested region name.
    '''

    # -------------------------------------------------------------------------
    def __init__(self, region_name: Optional[tuple[str, str]] = None):
        self._region_name = region_name

    # -------------------------------------------------------------------------
    @staticmethod
    def add_call(program: Routine, name: str, args: list[Node]):
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
            DriverCreator.add_call(program, read_var+"NonAlloc",
                                   [name_lit, Reference(sym)])
        else:
            # In case of an allocatable array, call the ReadVariable
            # function that will also allocate this array.
            DriverCreator.add_call(program, read_var,
                                   [name_lit, Reference(sym)])

    # -------------------------------------------------------------------------
    @staticmethod
    def add_result_tests(program: Routine,
                         output_symbols: list[tuple[Symbol, Symbol]]):
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

        DriverCreator.add_call(program, "compare_init",
                               [Literal(f"{len(output_symbols)}",
                                        INTEGER_TYPE)])

        # TODO #2083: check if this can be combined with psyad result
        # comparison.
        for (sym_computed, sym_read) in output_symbols:
            lit_name = Literal(sym_computed.name, CHARACTER_TYPE)
            DriverCreator.add_call(program, "compare",
                                   [lit_name, Reference(sym_computed),
                                    Reference(sym_read)])

        DriverCreator.add_call(program, "compare_summary", [])

    @staticmethod
    def _create_output_var_code(
        name: str,
        program: Routine,
        read_var: str,
        postfix: str,
        module_name: Optional[str] = None
    ) -> tuple[Symbol, Symbol]:
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
        DriverCreator.add_read_call(program, name_lit, post_sym, read_var)

        return (sym, post_sym)

    def _create_read_in_code(
            self, program: Routine, psy_data: DataSymbol,
            original_symtab: SymbolTable, read_write_info: ReadWriteInfo,
            postfix: str) -> list[tuple[Symbol, Symbol]]:
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
                name_lit = Literal(str(signature), CHARACTER_TYPE)
                read_stmts.append((name_lit, sym))

        # Now do the input external variables. These are done after the locals
        # so that they match the literal tags of the extracting psy-layer
        ExtractNode.bring_external_symbols(read_write_info, symbol_table)
        mod_man = ModuleManager.get()
        for module_name, signature in read_write_info.all_used_vars_list:
            if module_name:
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
    def import_modules(program: Routine):
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

    # -------------------------------------------------------------------------
    def verify_and_cleanup_psyir(self, extract_region: Node) -> None:
        """This method is called to verify that no unsupported language
        features are used. The base implementation checks for
        StructureReferences (except for ones used in a call, which some
        DLSs use, e.g. set_dirty in LFRic)

        :param extract_region: the node with the extracted region.

        :raises ValueError: if a StructureReference outside of a call
            is found.
        """
        # DSLs use StructureReferences in calls (e.g in LFRic
        # set_dirty etc) must be handled (e.g. removed) by a derived
        # class first
        for sref in extract_region.walk(StructureReference):
            raise ValueError(f"The DriverCreator does not support "
                             f"StructureReferences, any such references "
                             f"in the extraction region should have been "
                             f"flattened by the ExtractNode, but found: "
                             f"'{sref.debug_string()}'")

    # -------------------------------------------------------------------------
    @abstractmethod
    def handle_precision_symbols(self, symbol_table: SymbolTable) -> None:
        """This method is called to allow DSL-specific changes of type
        information. E.g. it might replace DSL-specific integer types
        with generic declarations, or add additional import statements
        to make the required types available.

        :param symbol_table: the SymbolTable of the driver.
        """

    # -------------------------------------------------------------------------
    def _add_command_line_handler(self, program, psy_data_var, module_name,
                                  region_name):
        '''
        This function adds code to handle the command line. For now an
        alternative filename (to the default one that is hard-coded by
        the created driver) can be specified, which allows the driver to
        be used with different files, e.g. several dumps from one run, and/or
        a separate file from each process. It will also add the code to
        open the input file using the read_kernel_data routine from the
        extraction library.

        :param program: The driver PSyIR.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`
        :param psy_data_var: the symbol of the PSyDataExtraction type.
        :type psy_data_var: :py:class:`psyclone.psyir.symbols.Symbol`
        :param str module_name: the name of the module, used to create the
            implicit default kernel dump file name.
        :param str region_name: the name of the region, used to create the
            implicit default kernel dump file name.

        '''
        # pylint: disable=too-many-locals
        program_symbol_table = program.symbol_table

        # PSyIR does not support allocatable strings, so create the two
        # variables we need in a loop.
        # TODO #2137: The UnsupportedFortranType could be reused for all
        #             variables once this is fixed.
        for str_name in ["psydata_filename", "psydata_arg"]:
            str_unique_name = \
                program_symbol_table.next_available_name(str_name)
            str_type = UnsupportedFortranType(
                f"character(:), allocatable :: {str_unique_name}")
            sym = DataTypeSymbol(str_unique_name, str_type)
            program_symbol_table.add(sym)
            if str_name == "psydata_filename":
                psydata_filename = str_unique_name
            else:
                psydata_arg = str_unique_name

        psydata_len = \
            program_symbol_table.find_or_create("psydata_len",
                                                symbol_type=DataSymbol,
                                                datatype=INTEGER_TYPE).name
        psydata_i = \
            program_symbol_table.find_or_create("psydata_i",
                                                symbol_type=DataSymbol,
                                                datatype=INTEGER_TYPE).name
        # We can only parse one statement at a time, so start with the
        # command line handling:
        code = f"""
        do {psydata_i}=1,command_argument_count()
           call get_command_argument({psydata_i}, length={psydata_len})
           allocate(character({psydata_len})::{psydata_arg})
           call get_command_argument({psydata_i}, {psydata_arg}, &
                                     length={psydata_len})
           if ({psydata_arg} == "--update") then
              ! For later to allow marking fields as being updated
           else
              allocate(character({psydata_len})::{psydata_filename})
              {psydata_filename} = {psydata_arg}
           endif
           deallocate({psydata_arg})
        enddo
        """
        command_line = \
            FortranReader().psyir_from_statement(code, program_symbol_table)
        program.children.insert(0, command_line)

        # Now add the handling of the filename parameter
        code = f"""
        if (allocated({psydata_filename})) then
           call {psy_data_var.name}%OpenReadFileName({psydata_filename})
        else
           call {psy_data_var.name}%OpenReadModuleRegion('{module_name}', &
                                                         '{region_name}')
        endif
        """
        filename_test = \
            FortranReader().psyir_from_statement(code, program_symbol_table)
        program.children.insert(1, filename_test)

    # -------------------------------------------------------------------------
    @staticmethod
    def collect_all_required_modules(
            file_container: FileContainer) -> dict[str, set[str]]:
        '''Collects recursively all modules used in the file container.
        It returns a dictionary, with the keys being all the (directly or
        indirectly) used modules.

        :param file_container: the FileContainer for which to collect all
            used modules.

        :returns: a dictionary, with the required module names as key, and
            as value a set of all modules required by the key module.

        '''
        all_mods: set[str] = set()
        for container in file_container.children:
            sym_tab = container.symbol_table
            # Add all imported modules (i.e. all container symbols)
            all_mods.update(symbol.name for symbol in sym_tab.symbols
                            if isinstance(symbol, ContainerSymbol))

        mod_manager = ModuleManager.get()
        return mod_manager.get_all_dependencies_recursively(
            list(all_mods))

    # -------------------------------------------------------------------------
    def create(self,
               nodes: list[Node],
               read_write_info: ReadWriteInfo,
               prefix: str,
               postfix: str,
               region_name: tuple[str, str]) -> FileContainer:
        # pylint: disable=too-many-arguments
        '''This function uses the PSyIR to create a stand-alone driver
        that reads in a previously created file with kernel input and
        output information, and calls the kernels specified in the 'nodes'
        PSyIR tree with the parameters from the file. The `nodes` are
        consecutive nodes from the PSyIR tree.
        It returns the file container which contains the driver.

        :param nodes: a list of nodes.
        :param read_write_info: information about all input and output
            parameters.
        :param prefix: the prefix to use for each PSyData symbol,
            e.g. 'extract' as prefix will create symbols ``extract_psydata``.
        :param postfix: a postfix that is appended to an output variable
            to create the corresponding variable that stores the output
            value from the kernel data file. The caller must guarantee that
            no name clashes are created when adding the postfix to a variable
            and that the postfix is consistent between extract code and
            driver code (see 'ExtractTrans.determine_postfix()').
        :param region_name: an optional name to
            use for this PSyData area, provided as a 2-tuple containing a
            location name followed by a local name. The pair of strings
            should uniquely identify a region.

        :returns: the program PSyIR for a stand-alone driver.

        '''
        # pylint: disable=too-many-locals

        module_name, local_name = region_name
        unit_name = self._make_valid_unit_name(f"{module_name}_{local_name}")

        # First create the file container, which will only store the program:
        file_container = FileContainer(unit_name)

        # Create the program and add it to the file container:
        program = Routine.create(unit_name, is_program=True)
        program_symbol_table = program.symbol_table
        file_container.addchild(program)

        # Add the extraction library symbols
        psy_data_mod = ContainerSymbol("read_kernel_data_mod")
        program_symbol_table.add(psy_data_mod)
        psy_data_type = DataTypeSymbol("ReadKernelDataType", UnresolvedType(),
                                       interface=ImportInterface(psy_data_mod))
        program_symbol_table.add(psy_data_type)
        if prefix:
            prefix = prefix + "_"
        root_name = prefix + "psy_data"
        psy_data = program_symbol_table.new_symbol(root_name=root_name,
                                                   symbol_type=DataSymbol,
                                                   datatype=psy_data_type)

        # Add cmd line hander, read in, and result comparison for the code
        self._add_command_line_handler(program, psy_data, region_name[0],
                                       region_name[1])

        original_symbol_table = nodes[0].ancestor(Routine).symbol_table

        # Add the modified extracted region into the driver program
        extract_region = nodes[0].copy()
        self.verify_and_cleanup_psyir(extract_region)
        output_symbols = self._create_read_in_code(program, psy_data,
                                                   original_symbol_table,
                                                   read_write_info, postfix)

        # Copy the nodes that are part of the extraction
        program.children.extend(extract_region.pop_all_children())

        # Find all imported modules and add them to the symbol table
        self.import_modules(program)
        self.handle_precision_symbols(program.scope.symbol_table)

        self.add_result_tests(program, output_symbols)

        # Replace pointers with allocatables
        program_symbol_table = program.symbol_table
        for symbol in program_symbol_table.datasymbols:
            if isinstance(symbol.datatype, UnsupportedFortranType):
                symbol.datatype = symbol.datatype.copy()
                newt = symbol.datatype.declaration
                newt = newt.replace('pointer', 'allocatable')
                newt = newt.replace('=> null()', '')
                symbol.datatype._declaration = newt

        return file_container

    # -------------------------------------------------------------------------
    def get_driver_as_string(self,
                             nodes: list[Node],
                             read_write_info: ReadWriteInfo,
                             prefix: str,
                             postfix: str,
                             region_name: tuple[str, str],
                             writer: LanguageWriter = FortranWriter()) -> str:
        # pylint: disable=too-many-arguments, too-many-locals
        '''This function uses the `create()` function to get the PSyIR of a
        stand-alone driver, and then uses the provided language writer
        to create a string representation in the selected language
        (defaults to Fortran).
        All required modules will be inlined in the correct order, i.e. each
        module will only depend on modules inlined earlier, which will allow
        compilation of the driver. No other dependencies (except system
        dependencies like NetCDF) are required for compilation.

        :param nodes: a list of nodes.
        :param read_write_info: information about all input and output
            parameters.
        :param prefix: the prefix to use for each PSyData symbol,
            e.g. 'extract' as prefix will create symbols `extract_psydata`.
        :param postfix: a postfix that is appended to an output variable
            to create the corresponding variable that stores the output
            value from the kernel data file. The caller must guarantee that
            no name clashes are created when adding the postfix to a variable
            and that the postfix is consistent between extract code and
            driver code (see 'ExtractTrans.determine_postfix()').
        :param region_name: an optional name to
            use for this PSyData area, provided as a 2-tuple containing a
            location name followed by a local name. The pair of strings
            should uniquely identify a region.
        :param language_writer: a backend visitor to convert PSyIR
            representation to the selected language. It defaults to
            the FortranWriter.

        :returns: the driver in the selected language.

        '''
        file_container = self.create(nodes, read_write_info, prefix,
                                     postfix, region_name)

        # Inline all required modules into the driver source file so that
        # it is stand-alone.
        module_dependencies = self.collect_all_required_modules(file_container)
        # Sort the modules by dependencies, i.e. start with modules
        # that have no dependency. This is required for compilation, the
        # compiler must have found any dependent modules before it can
        # compile a module.
        mod_manager = ModuleManager.get()
        sorted_modules = mod_manager.sort_modules(module_dependencies)

        out = []

        for module in sorted_modules:
            # Note that all modules in `sorted_modules` are known to be in
            # the module manager, so we can always get the module info here.
            mod_info = mod_manager.get_module_info(module)
            out.append(mod_info.get_source_code())

        out.append(writer(file_container))

        return "\n".join(out)

    # -------------------------------------------------------------------------
    def write_driver(self,
                     nodes: list[Node],
                     read_write_info: ReadWriteInfo,
                     prefix: str,
                     postfix: str,
                     region_name: tuple[str, str],
                     writer: LanguageWriter = FortranWriter()) -> None:
        # pylint: disable=too-many-arguments
        '''This function uses the `get_driver_as_string()` function to get a
        a stand-alone driver, and then writes this source code to a file. The
        file name is derived from the region name:
        "driver-"+module_name+"_"+region_name+".F90"

        :param nodes: a list of nodes containing the body of the driver
            routine.
        :param read_write_info: information about all input and output
            parameters.
        :param prefix: the prefix to use for each PSyData symbol,
            e.g. 'extract' as prefix will create symbols `extract_psydata`.
        :param postfix: a postfix that is appended to an output variable
            to create the corresponding variable that stores the output
            value from the kernel data file. The caller must guarantee that
            no name clashes are created when adding the postfix to a variable
            and that the postfix is consistent between extract code and
            driver code (see 'ExtractTrans.determine_postfix()').
        :param region_name: an optional name to
            use for this PSyData area, provided as a 2-tuple containing a
            location name followed by a local name. The pair of strings
            should uniquely identify a region.
        :param writer: a backend visitor to convert PSyIR
            representation to the selected language. It defaults to
            the FortranWriter.

        '''
        if self._region_name is not None:
            region_name = self._region_name
        code = self.get_driver_as_string(nodes, read_write_info, prefix,
                                         postfix, region_name, writer=writer)
        fll = FortLineLength()
        code = fll.process(code)
        module_name, local_name = region_name
        with open(f"driver-{module_name}-{local_name}.F90", "w",
                  encoding='utf-8') as out:
            out.write(code)
