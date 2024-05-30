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

from psyclone.line_length import FortLineLength
from psyclone.parse import ModuleManager
from psyclone.psyir.backend.fortran import FortranWriter
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
        the supplied program and the corresponding RoutineSymbol to its
        symbol table (if not already present).

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
