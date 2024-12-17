# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024, Science and Technology Facilities Council.
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

from psyclone.core import Signature
from psyclone.errors import InternalError
from psyclone.line_length import FortLineLength
from psyclone.parse import ModuleManager
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import Call, Literal, Reference, StructureReference
from psyclone.psyir.symbols import (CHARACTER_TYPE, ContainerSymbol,
                                    DataSymbol,
                                    ImportInterface, INTEGER_TYPE, NoType,
                                    RoutineSymbol,
                                    UnresolvedType, UnsupportedFortranType)


class BaseDriverCreator:
    '''This class provides the functionality common to all driver creations.
    A more extended version will be created for TODO #2049, for now it only
    provides two identical methods for LFRicExtractDriverCreator and
    ExtractDriverCreator (which might need to be renamed to GOcean....)

    '''
    # TODO #2049: Turn this into a proper base class.

    # -------------------------------------------------------------------------
    @staticmethod
    def _flatten_signature(signature):
        '''Creates a 'flattened' string for a signature by using ``_`` to
        separate the parts of a signature. For example, in Fortran
        a reference to ``a%b`` would be flattened to be ``a_b``.

        :param signature: the signature to be flattened.
        :type signature: :py:class:`psyclone.core.Signature`

        :returns: a flattened string (all '%' replaced with '_'.)
        :rtype: str

        '''
        return str(signature).replace("%", "_")

    # -------------------------------------------------------------------------
    @staticmethod
    def _make_valid_unit_name(name):
        '''Valid program or routine names are restricted to 63 characters,
        and no special characters like '-' (which is used when adding
        invoke and region numbers).

        :param str name: a proposed unit name.

        :returns: a valid program or routine  name with special characters
            removed and restricted to a length of 63 characters.
        :rtype: str

        '''
        return name.replace("-", "")[:63]

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
        value. It takes a list of tuples. Each tuple contains:

        1. the symbol containing the result when the kernel is called in the
            driver.
        2. the symbol containing the original results, i.e. the values read
            from the extracted file.
        3. The signature of the original access. This is used in case of
            derived types to get the members used.

        :param program: the program to which the tests should be added.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`
        :param output_symbols: a list containing all output variables of
            the executed code. Each entry in the list is a 2-tuple,
            containing first the symbol that was computed when executing
            the kernels, and then the symbol containing the expected
            values that have been read in from a file.
        :type output_symbols: list[tuple[
            :py:class:`psyclone.psyir.symbols.Symbol`,
            :py:class:`psyclone.psyir.symbols.Symbol`,
            :py:class:`psyclone.core.signature.Signature`]]
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
        for (sym_computed, sym_read, signature) in output_symbols:
            # First create the reference (including potential member access)
            # to the newly computed value, and the value read from the file:
            ref_computed = signature.create_reference(sym_computed)
            ref_read = Reference(sym_read)
            # Create the tag that was used in extraction:
            name_in_kernel_file = Signature(sym_computed.name, signature[1:])
            lit_name = Literal(str(name_in_kernel_file), CHARACTER_TYPE)
            BaseDriverCreator.add_call(program, "compare",
                                       [lit_name, ref_computed, ref_read])

        BaseDriverCreator.add_call(program, "compare_summary", [])

    # -------------------------------------------------------------------------
    @staticmethod
    def import_modules(symbol_table, sched):
        '''This function adds all the import statements required for the
        actual kernel calls. It finds all calls in the schedule and
        checks for calls with an ImportInterface. Any such call will
        add a ContainerSymbol for the module and a RoutineSymbol (pointing
        to the container) to the symbol table.

        :param symbol_table: the symbol table to which the symbols are added.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param sched: the schedule to analyse for module imports.
        :type sched: :py:class:`psyclone.psyir.nodes.Schedule`

        '''
        for call in sched.walk(Call):
            routine = call.routine.symbol
            if not isinstance(routine.interface, ImportInterface):
                # No import required, can be ignored.
                continue
            if routine.name in symbol_table:
                # Symbol has already been added - ignore
                continue
            # We need to create a new symbol for the module and the routine
            # called (the PSyIR backend will then create a suitable import
            # statement).
            module = ContainerSymbol(routine.interface.container_symbol.name)
            symbol_table.add(module)
            new_routine_sym = RoutineSymbol(routine.name, UnresolvedType(),
                                            interface=ImportInterface(module))
            symbol_table.add(new_routine_sym)

    # -------------------------------------------------------------------------
    def is_supported_derived_type(self, symbol):
        '''This method checks if a unknown derived type is being used, which
        might not be supported in all domains. This default implementation
        will allow all derived types through.

        :param symbol: the symbol whose datatype is to be checked.
        :type symbol: py:class:`psyclone.psyir.symbols.Symbol`

        :returns: whether the datatype is supported or not
        :rtype bool:

        '''
        # pylint: disable=unused-argument
        return True

    # -------------------------------------------------------------------------
    def add_all_kernel_symbols(self, sched, symbol_table, read_write_info,
                               writer=FortranWriter()):

        '''This function adds all symbols used in ``sched`` to the symbol
        table. It uses LFRic-specific knowledge to declare fields and flatten
        their name.

        :param sched: the schedule that will be called by this driver program.
        :type sched: :py:class:`psyclone.psyir.nodes.Schedule`
        :param symbol_table: the symbol table to which to add all found
            symbols.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param read_write_info: information about all input and output
            parameters.
        :type read_write_info: :py:class:`psyclone.psyir.tools.ReadWriteInfo`

        '''
        # pylint: disable=too-many-branches, too-many-locals
        all_references = sched.walk(Reference)

        # First we add all non-structure names to the symbol table. This way
        # the flattened name can be ensured not to clash with a variable name
        # used in the program.
        for reference in all_references:
            # Skip routine references
            if (isinstance(reference.parent, Call) and
                    reference.parent.routine is reference):
                continue
            # For now ignore structure names, which require flattening (which
            # could introduce duplicated symbols, so they need to be processed
            # after all existing symbols have been added.
            if isinstance(reference, StructureReference):
                continue

            old_symbol = reference.symbol
            if old_symbol.name in symbol_table:
                # The symbol has already been declared. We then still
                # replace the old symbol with the new symbol to have all
                # symbols consistent (otherwise if we would for whatever
                # reason modify a symbol in the driver's symbol table, only
                # some references would use the new values, the others
                # would be the symbol from the original kernel for which
                # the driver is being created).
                reference.symbol = symbol_table.lookup(old_symbol.name)
                continue

            # Now we have a reference with a symbol that is in the old symbol
            # table (i.e. not in the one of the driver). Create a new symbol
            # (with the same name) in the driver's symbol table), and use
            # it in the reference.
            datatype = old_symbol.datatype
            if isinstance(datatype, UnsupportedFortranType):
                # Currently fields are of UnsupportedFortranType because they
                # are pointers in the PSy layer. Here we just want the base
                # type (i.e. not a pointer).
                datatype = old_symbol.datatype.partial_datatype

            new_symbol = symbol_table.new_symbol(root_name=reference.name,
                                                 tag=reference.name,
                                                 symbol_type=DataSymbol,
                                                 datatype=datatype.copy())
            new_symbol.replace_symbols_using(symbol_table)
            reference.symbol = new_symbol

        # Now handle all derived types. The name of a derived type is
        # 'flattened', i.e. all '%' are replaced with '_', and this is then
        # declared as a non-structured type. We also need to make sure that a
        # flattened name does not clash with a variable declared by the user.
        # We use the structured name (with '%') as tag to handle this.
        for reference in all_references:
            # Skip routine references
            if (isinstance(reference.parent, Call) and
                    reference.parent.routine is reference):
                continue

            # Skip references that are not any kind of structure
            if not isinstance(reference, StructureReference):
                continue

            old_symbol = reference.symbol
            # TODO: Do we actually need this test? Since it's a kernel call,
            # any derived type will only use a Fortran base class, which
            # the kernel extraction and driver creation should be able to
            # handle.
            if not self.is_supported_derived_type(old_symbol):
                fortran_string = writer(reference)
                raise InternalError(
                    f"Error when constructing driver for '{sched.name}': "
                    f"Unknown derived type '{old_symbol.datatype.name}' "
                    f"in reference '{fortran_string}'.")
            # We have a structure reference to a field, flatten it, and
            # replace the StructureReference with a new Reference to this
            # flattened name (e.g. `fld%data` becomes `fld_data`)

            self._flatten_reference(reference, symbol_table)

        # Now add all non-local symbols, which need to be
        # imported from the appropriate module. Note that
        # this will not create the `_post ` version of the
        # variables.
        # -----------------------------------------------
        mod_man = ModuleManager.get()
        for module_name, signature in read_write_info.set_of_all_used_vars:
            if not module_name:
                # Ignore local symbols, which will have been added above
                continue
            container = symbol_table.find_or_create(
                module_name, symbol_type=ContainerSymbol)

            # Now look up the original symbol. While the variable could
            # be declared Unresolved here (i.e. just imported), we need the
            # type information for the output variables (VAR_post), which
            # are created later and which will query the original symbol for
            # its type. And since they are not imported, they need to be
            # explicitly declared.
            mod_info = mod_man.get_module_info(module_name)
            try:
                sym_tab = mod_info.get_psyir().symbol_table
                container_symbol = sym_tab.lookup(signature[0])
            except (AttributeError, KeyError):
                # TODO #2120: This typically indicates a problem with parsing
                # a module: the psyir does not have the full tree structure.
                # AttributeErrors happen when we can't get a PSyIR at all,
                # KeyError when a symbol is missing (likely due to a CodeBlock)
                continue

            # It is possible that external symbol name (signature[0]) already
            # exist in the symbol table (the same name is used in the local
            # subroutine and in a module). In this case, the imported symbol
            # must be renamed:
            if signature[0] in symbol_table:
                interface = ImportInterface(container, orig_name=signature[0])
            else:
                interface = ImportInterface(container)

            symbol_table.find_or_create_tag(
                tag=f"{signature[0]}@{module_name}", root_name=signature[0],
                symbol_type=DataSymbol, interface=interface,
                datatype=container_symbol.datatype)

    # -------------------------------------------------------------------------
    @staticmethod
    def collect_all_required_modules(file_container):
        '''Collects recursively all modules used in the file container.
        It returns a dictionary, with the keys being all the (directly or
        indirectly) used modules.

        :param file_container: the FileContainer for which to collect all
            used modules.
        :type file_container:
            :py:class:`psyclone.psyir.psyir.nodes.FileContainer`

        :returns: a dictionary, with the required module names as key, and
            as value a set of all modules required by the key module.
        :rtype: Dict[str, Set[str]]

        '''
        all_mods = set()
        for container in file_container.children:
            sym_tab = container.symbol_table
            # Add all imported modules (i.e. all container symbols)
            all_mods.update(symbol.name for symbol in sym_tab.symbols
                            if isinstance(symbol, ContainerSymbol))

        mod_manager = ModuleManager.get()
        return mod_manager.get_all_dependencies_recursively(all_mods)

    # -------------------------------------------------------------------------
    def get_driver_as_string(self, nodes, read_write_info, prefix, postfix,
                             region_name, writer=FortranWriter()):
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
        :type nodes: list[:py:class:`psyclone.psyir.nodes.Node`]
        :param read_write_info: information about all input and output
            parameters.
        :type read_write_info: :py:class:`psyclone.psyir.tools.ReadWriteInfo`
        :param str prefix: the prefix to use for each PSyData symbol,
            e.g. 'extract' as prefix will create symbols `extract_psydata`.
        :param str postfix: a postfix that is appended to an output variable
            to create the corresponding variable that stores the output
            value from the kernel data file. The caller must guarantee that
            no name clashes are created when adding the postfix to a variable
            and that the postfix is consistent between extract code and
            driver code (see 'ExtractTrans.determine_postfix()').
        :param Tuple[str,str] region_name: an optional name to
            use for this PSyData area, provided as a 2-tuple containing a
            location name followed by a local name. The pair of strings
            should uniquely identify a region.
        :param language_writer: a backend visitor to convert PSyIR
            representation to the selected language. It defaults to
            the FortranWriter.
        :type language_writer:
            :py:class:`psyclone.psyir.backend.language_writer.LanguageWriter`

        :returns: the driver in the selected language.
        :rtype: str

        '''
        file_container = self.create(nodes, read_write_info, prefix,
                                     postfix, region_name)

        module_dependencies = self.collect_all_required_modules(file_container)
        # Sort the modules by dependencies, i.e. start with modules
        # that have no dependency. This is required for compilation, the
        # compiler must have found any dependent modules before it can
        # compile a module.
        mod_manager = ModuleManager.get()
        sorted_modules = mod_manager.sort_modules(module_dependencies)

        # Inline all required modules into the driver source file so that
        # it is stand-alone.
        out = []

        for module in sorted_modules:
            # Note that all modules in `sorted_modules` are known to be in
            # the module manager, so we can always get the module info here.
            mod_info = mod_manager.get_module_info(module)
            out.append(mod_info.get_source_code())

        out.append(writer(file_container))

        return "\n".join(out)

    # -------------------------------------------------------------------------
    def write_driver(self, nodes, read_write_info, prefix, postfix,
                     region_name, writer=FortranWriter()):
        # pylint: disable=too-many-arguments
        '''This function uses the ``get_driver_as_string()`` function to get a
        a stand-alone driver, and then writes this source code to a file. The
        file name is derived from the region name:
        "driver-"+module_name+"_"+region_name+".F90"

        :param nodes: a list of nodes containing the body of the driver
            routine.
        :type nodes: List[:py:class:`psyclone.psyir.nodes.Node`]
        :param read_write_info: information about all input and output
            parameters.
        :type read_write_info: :py:class:`psyclone.psyir.tools.ReadWriteInfo`
        :param str prefix: the prefix to use for each PSyData symbol,
            e.g. 'extract' as prefix will create symbols `extract_psydata`.
        :param str postfix: a postfix that is appended to an output variable
            to create the corresponding variable that stores the output
            value from the kernel data file. The caller must guarantee that
            no name clashes are created when adding the postfix to a variable
            and that the postfix is consistent between extract code and
            driver code (see 'ExtractTrans.determine_postfix()').
        :param Tuple[str,str] region_name: an optional name to
            use for this PSyData area, provided as a 2-tuple containing a
            location name followed by a local name. The pair of strings
            should uniquely identify a region.
        :param writer: a backend visitor to convert PSyIR
            representation to the selected language. It defaults to
            the FortranWriter.
        :type writer:
            :py:class:`psyclone.psyir.backend.language_writer.LanguageWriter`

        '''
        code = self.get_driver_as_string(nodes, read_write_info, prefix,
                                         postfix, region_name, writer=writer)
        fll = FortLineLength()
        code = fll.process(code)
        module_name, local_name = region_name
        with open(f"driver-{module_name}-{local_name}.F90", "w",
                  encoding='utf-8') as out:
            out.write(code)
