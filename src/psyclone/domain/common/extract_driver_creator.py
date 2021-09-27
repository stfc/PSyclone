# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Author: J. Henrichs, Bureau of Meteorology

'''This module provides functionality for the PSyclone kernel extraction
functionality. It contains the class that creates a driver that
reads in extracted data, calls the kernel, and then compares the result with
the output data contained in the input file.
'''

from __future__ import absolute_import

import six

from psyclone.configuration import Config
from psyclone.errors import InternalError
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import (Assignment, Call, FileContainer,
                                  Literal, Reference, Routine,
                                  StructureReference)
from psyclone.psyir.symbols import (ArrayType, CHARACTER_TYPE,
                                    ContainerSymbol, DataSymbol,
                                    DataTypeSymbol, DeferredType,
                                    ImportInterface, INTEGER_TYPE,
                                    REAL8_TYPE, RoutineSymbol, ScalarType)
from psyclone.psyir.transformations import ExtractTrans

# TODO 1392: once we support LFRic, make this into a proper base class
# and put the domain-specific implementations into the domain/* directories.


class ExtractDriverCreator:
    '''This class provides the functionality to create a driver that
    reads in extracted data produced by using the PSyData kernel-extraction
    functionality.

    :param integer_type: default scalar integer type to be used for integer \
        variables. Defaults to INTEGER_TYPE.
    :type integer_type: :py:class:`psyclone.psyir.symbols.ScalarType`
    :param real_type: default scalar real type to be used for real \
        variables. Defaults to REAL8_TYPE.
    :type real_type: :py:class:`psyclone.psyir.symbols.ScalarType`

    '''
    def __init__(self, integer_type=INTEGER_TYPE, real_type=REAL8_TYPE):
        # Set the integer and real types to use.
        # For convenience, also add the names used in the gocean config file:
        self._default_types = {ScalarType.Intrinsic.INTEGER: integer_type,
                               "integer": integer_type,
                               ScalarType.Intrinsic.REAL: real_type,
                               "real": real_type}

    # -------------------------------------------------------------------------
    def create_flattened_symbol(self, flattened_name, reference, symbol_table,
                                writer=FortranWriter()):
        '''Takes a reference to a structure and creates a new Symbol of the
        type that the reference resolves to, e.g. fld%data... will be mapped
        to `real, dimension(:,:)`, and `fld%data$whole%xstart` to `integer`.

        :param str flattened_name: the new 'flattened' name to be used for \
            the newly created symbol.
        :param reference: the reference to a structure.
        :type reference: :py:class:`psyclone.psyir.nodes.StructureReference`
        :param symbol_table: the symbol table to use to make sure a unique \
            name is created (based on `flattened_name`).
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param writer: a backend visitor to convert the reference into a \
            string, which is then used to find the corresponding \
            gocean-grid-properties.
        :type writer: \
            :py:class:`psyclone.psyir.backend.language_writer.LanguageWriter`

        :raises InternalError: if the structure access is not to a \
            GOCean grid property.
        :raises InternalError: if there is no default type defined for \
            the type of the GOCean grid property (defaults are defined in \
            the constructor of this class).
        :raises InternalError: if the gocean grid property type is \
            neither 'array' nor 'scalar'.

        :returns: the new symbol created.
        :rtype: :py:class:`psyclone.psyir.symbol.DataSymbol`

        '''
        fortran_expression = writer(reference)
        api_config = Config.get().api_conf("gocean1.0")
        grid_properties = api_config.grid_properties

        for prop_name in grid_properties:
            gocean_property = grid_properties[prop_name]
            deref_name = gocean_property.fortran.format(reference.name)
            if fortran_expression == deref_name:
                break
        else:
            raise InternalError(
                "Could not find type for reference '{0}' in the "
                "config file '{1}'."
                .format(fortran_expression, Config.get().filename))
        try:
            base_type = self._default_types[gocean_property.intrinsic_type]
        except KeyError as err:
            raise six.raise_from(
                InternalError("Type '{0}' of the property reference "
                              "'{1}' as defined in the config file "
                              "'{2}' is not supported in the GOcean API."
                              .format(gocean_property.intrinsic_type,
                                      fortran_expression,
                                      Config.get().filename)),
                err)
        # Handle name clashes (e.g. if the user used a variable that is
        # the same as a flattened grid property)
        flattened_name = symbol_table.next_available_name(flattened_name)
        if gocean_property.type == "scalar":
            new_symbol = DataSymbol(flattened_name, base_type)

        elif gocean_property.type == "array":
            # At this stage all gocean arrays are 2d (even integer ones)
            # so no need to add any further tests here.
            array = ArrayType(base_type, [ArrayType.Extent.DEFERRED,
                                          ArrayType.Extent.DEFERRED])
            new_symbol = DataSymbol(flattened_name, array)
        else:
            raise InternalError("The expression '{0}' maps to an unknown "
                                "GOcean property type '{1}' in the config "
                                "file '{2}'."
                                .format(fortran_expression,
                                        gocean_property.type,
                                        Config.get().filename))

        return new_symbol

    # -------------------------------------------------------------------------
    @staticmethod
    def flatten_string(fortran_string):
        '''Replaces all `%` with `_` in the string, creating a 'flattened'
        name.

        :param str fortran_string: the Fortran string containing '%'.

        :returns: a flattened string (all '%' replaced with '_'.)
        :rtype: str

        '''
        return fortran_string.replace("%", "_")

    # -------------------------------------------------------------------------
    def flatten_reference(self, old_reference, symbol_table,
                          writer=FortranWriter()):
        '''Replaces `old_reference` which is a structure type with a new
        simple Reference and a flattened name (replacing all % with _).

        :param old_reference: a reference to a structure member.
        :type old_reference: \
            :py:class:`psyclone.psyir.nodes.StructureReference`
        :param symbol_table: the symbol table to which to add the newly \
            defined flattened symbol.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param writer: a Fortran writer used when flattening a \
            `StructureReference`.
        :type writer: :py:class:`psyclone.psyir.backend.fortran.FortranWriter`

        '''
        # A field access (`fld%data`) will get the `%data` removed, since then
        # this avoids a potential name clash (`fld` is guaranteed to
        # be unique, since it's a variable already, but `fld_data` could clash
        # with a user variable if the user uses `fld` and `fld_data`).
        # Furthermore, the netcdf file declares the variable without `%data`,
        # so removing `%data` here also simplifies code creation later on.
        signature, _ = old_reference.get_signature_and_indices()
        fortran_string = writer(old_reference)
        if signature[-1] == "data":
            # Remove %data
            fortran_string = fortran_string[:-5]
        try:
            symbol = symbol_table.lookup_with_tag(fortran_string)
        except KeyError:
            flattened_name = self.flatten_string(fortran_string)
            # Symbol not in table, create a new symbol
            symbol = self.create_flattened_symbol(flattened_name,
                                                  old_reference, symbol_table,
                                                  writer)
            symbol_table.add(symbol, tag=fortran_string)
        # We need to create a new, flattened Reference and replace the
        # StructureReference with it:
        old_reference.replace_with(Reference(symbol))

    # -------------------------------------------------------------------------
    def add_all_kernel_symbols(self, sched, symbol_table,
                               writer=FortranWriter()):
        '''This function adds all symbols used in `sched` to the symbol table.
        It uses GOcean-specific knowledge to declare fields and flatten their
        name.

        :param sched: the schedule that will be called by this driver program.
        :type sched: :py:class:`psyclone.psyir.nodes.Schedule`
        :param symbol_table: the symbol table to which to add all found \
            symbols.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param writer: a Fortran writer used when flattening a \
            `StructureReference`.
        :type writer: :py:class:`psyclone.psyir.backend.fortan.FortranWriter`

        :raises InternalError: if a non-derived type has an unknown \
            intrinsic type.
        :raises InternalError: if an unknown derived type is \
            encountered. At this stage only the dl_esm_inf `field` type \
            is supported.

        '''
        all_references = sched.walk(Reference)
        # First we add all non-structure names to the symbol table. This way
        # the flattened name can be ensured not to clash with a variable name
        # used in the program.
        for reference in all_references:
            # For now ignore structure names, which require flattening
            if isinstance(reference, StructureReference):
                continue
            old_symbol = reference.symbol
            if old_symbol.name in symbol_table:
                # The symbol has already been declared. We then still
                # replace the old symbol with the new symbol to have all
                # symbols consistent:
                reference.symbol = symbol_table.lookup(old_symbol.name)
                continue

            # We found a new symbol, so we create a new symbol in the new
            # symbol table here. GOcean does not support any arbitrary array
            # as kernel argument (only fields and grid properties), so we
            # only need to declare scalars here.
            try:
                new_type = self._default_types[old_symbol.datatype.intrinsic]
            except KeyError as err:
                fortran_string = writer(reference)
                valid = list(self._default_types.keys())
                # Sort to make sure we get a reproducible order for testing
                valid.sort()
                six.raise_from(InternalError(
                    "Error when constructing driver for '{0}': "
                    "Unknown intrinsic data type '{1}' in reference '{2}'. "
                    "Valid types are '{3}'."
                    .format(sched.name, old_symbol.datatype.intrinsic,
                            fortran_string, valid)),
                    err)
            new_symbol = symbol_table.new_symbol(root_name=reference.name,
                                                 tag=reference.name,
                                                 symbol_type=DataSymbol,
                                                 datatype=new_type)
            reference.symbol = new_symbol

        # Now handle all derived type. In GOcean the only supported derived
        # type is "r2d_field". This type might be used to access the field
        # data, loop boundaries or other properties. We use the
        # grid_properties information from the config file to identify which
        # property is used. The name of a derived type is 'flattened', i.e.
        # all '%' are replaced with '_', and this is then declared as a
        # non-structured type. We also need to make sure that a flattened
        # name does not clash with a variable declared by the user. We use
        # the structured name (with '%') as tag to handle this.
        for reference in all_references:
            if not isinstance(reference, StructureReference):
                continue
            old_symbol = reference.symbol
            if old_symbol.datatype.name != "r2d_field":
                fortran_string = writer(reference)
                raise InternalError(
                    "Error when constructing driver for '{0}': "
                    "Unknown derived type '{1}' in reference '{2}'."
                    .format(sched.name, old_symbol.datatype.name,
                            fortran_string))
            # We have a structure reference to a field, flatten it, and
            # replace the StructureReference with a new Reference to this
            # flattened name (e.g. `fld%data` becomes `fld_data`)
            self.flatten_reference(reference, symbol_table, writer=writer)

    # -------------------------------------------------------------------------
    @staticmethod
    def add_call(program, name, args):
        '''This function creates a call to the subroutine of the given name,
        providing the arguments. The call will be added to the program and
        to the symbol table.

        :param program: the PSyIR Routine to which any code must \
            be added. It also contains the symbol table to be used.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`
        :param str name: name of the subroutine to call.
        :param args: list of all arguments for the call.
        :type args: list of :py:class:`psyclone.psyir.nodes.Node`

        :raises TypeError: if there is a symbol with the \
            specified name defined that is not a RoutineSymbol.
        '''
        if name in program.symbol_table:
            routine_symbol = program.symbol_table.lookup(name)
            if not isinstance(routine_symbol, RoutineSymbol):
                raise TypeError(
                    "Error when adding call: Routine '{0}' is "
                    "a symbol of type '{1}', not a 'RoutineSymbol'."
                    .format(name, type(routine_symbol).__name__))
        else:
            routine_symbol = RoutineSymbol(name)
            program.symbol_table.add(routine_symbol)
        call = Call.create(routine_symbol, args)
        program.addchild(call)

    # -------------------------------------------------------------------------
    @staticmethod
    def create_read_in_code(program, psy_data, input_list, output_list,
                            postfix):
        '''This function creates the code that reads in the NetCDF file
        produced during extraction. For each:

        - variable that is read-only, it will declare the symbol and add code
          that reads in the variable using the PSyData library.
        - variable that is read and written, it will create code to read in the
          variable that is read, and create a new variable with the same name
          and "_post" added which is read in to store the values from the
          NetCDF file after the instrumented region was executed. In the end,
          the variable that was read and written should have the same value
          as the corresponding "_post" variable.
        - variable that is written only, it will create a variable with "_post"
          as postfix that reads in the output data from the NetCDF file. It
          then also declares a variable without postfix (which will be the
          parameter to the function), allocates it based on the shape of
          the corresponding "_post" variable, and initialises it with 0.

        :param program: the PSyIR Routine to which any code must \
            be added. It also contains the symbol table to be used.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`
        :param psy_data: the PSyData symbol to be used.
        :type psy_data: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param input_list: list of all signatures that are input variables \
            to the instrumented region.
        :type input_list: list of :py:class:`psyclone.core.Signature`
        :param output_list: list of all signatures that are output \
            variables of the instrumented region.
        :type output_list: list of :py:class:`psyclone.core.Signature`
        :param str postfix: a postfix that is added to a variable to \
            create the corresponding variable that stores the output \
            value from the kernel data file.

        :returns: a list with all output parameters, i.e. variables that \
            need to be verified after executing the kernel. Each entry is \
            a 2-tuple containing the symbol of the computed variable, and \
            the symbol of the variable that contains the value read from \
            the file.
        :rtype: list of 2-tuples of \
            :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        # pylint: disable=too-many-locals
        all_sigs = list(set(input_list).union(set(output_list)))
        all_sigs.sort()
        symbol_table = program.scope.symbol_table
        read_var = "{0}%ReadVariable".format(psy_data.name)

        # Collect all output symbols to later create the tests for
        # correctness. This list stores 2-tuples: first one the
        # variable that stores the output from the kernel, the second
        # one the variable that stores the output values read from the
        # file. The content of these two variables should be identical
        # at the end.
        output_symbols = []

        for signature in all_sigs:
            # Find the right symbol for the variable. Note that all variables
            # in the input and output list have been detected as being used
            # when the variable accesses were analysed. Therefore, these
            # variables have References, and will already have been declared
            # in the symbol table (in add_all_kernel_symbols).
            sig_str = str(signature)
            sym = symbol_table.lookup_with_tag(sig_str)
            is_input = signature in input_list
            is_output = signature in output_list

            # First handle variables that are read:
            # -------------------------------------
            if is_input:
                name_lit = Literal(sig_str, CHARACTER_TYPE)
                ExtractDriverCreator.add_call(program, read_var,
                                              [name_lit, Reference(sym)])
                if not is_output:
                    # input only variable, nothing else to do.
                    continue

            # The variable is written (and maybe read as well)
            # ------------------------------------------------
            # Declare a 'post' variable of the same type and
            # read in its value.
            post_name = sig_str+postfix
            post_sym = symbol_table.new_symbol(post_name,
                                               symbol_type=DataSymbol,
                                               datatype=sym.datatype)
            ExtractDriverCreator.add_call(program, read_var,
                                          [Literal(post_name, CHARACTER_TYPE),
                                           Reference(post_sym)])

            # Now if a variable is written to, but not read, the variable
            # is not allocated. So we need to allocate it and set it to 0.
            if not is_input:
                if isinstance(post_sym.datatype, ArrayType):
                    # TODO #1366 Once allocate is supported in PSyIR
                    # this parsing of a file can be replaced. Also,
                    # if the mold parameter is supported, we can
                    # use the second allocate statement to create code
                    # that's independent of the number of dimensions.
                    code = '''
                        subroutine tmp()
                          integer, allocatable, dimension(:,:) :: b
                          allocate({0}(size({1},1), size({1},2)))
                          !allocate({0}, mold={1})
                        end subroutine tmp'''.format(sig_str, post_name)
                    fortran_reader = FortranReader()
                    container = fortran_reader.psyir_from_source(code)\
                        .children[0]
                    alloc = container.children[0].detach()
                    program.addchild(alloc)
                set_zero = Assignment.create(Reference(sym),
                                             Literal("0", INTEGER_TYPE))
                program.addchild(set_zero)
            output_symbols.append((sym, post_sym))
        return output_symbols

    # -------------------------------------------------------------------------
    @staticmethod
    def import_modules(program, sched):
        '''This function adds all the import statements required for the
        actual kernel calls. It finds all calls in the PSyIR tree and
        checks for calls with a ImportInterface. Any such call will
        get a ContainerSymbol added for the module, and a RoutineSymbol
        with an import interface pointing to this module.

        :param program: the PSyIR Routine to which any code must \
            be added. It also contains the symbol table to be used.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`
        :param sched: the schedule that will be called by the driver \
            program created.
        :type sched: :py:class:`psyclone.psyir.nodes.Schedule`

        '''
        symbol_table = program.scope.symbol_table
        for call in sched.walk(Call):
            routine = call.routine
            if not isinstance(routine.interface, ImportInterface):
                continue
            if routine.name in symbol_table:
                # Symbol has already been added - ignore
                continue
            # We need to create a new symbol for the module and the routine
            # called (the PSyIR backend will then create a suitable import
            # statement).
            module = ContainerSymbol(routine.interface.container_symbol.name)
            symbol_table.add(module)
            new_routine_sym = RoutineSymbol(routine.name, DeferredType(),
                                            interface=ImportInterface(module))
            symbol_table.add(new_routine_sym)

    # -------------------------------------------------------------------------
    @staticmethod
    def add_result_tests(program, output_symbols):
        '''Adds tests to check that all output variables have the expected
        value.

        :param program: the program to which the tests should be added.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`
        :param output_symbols: a list containing all output variables of \
            the executed code. Each entry in the list is a 2-tuple, \
            containing first the symbol that was computed when executing \
            the kernels, and then the symbol containing the expected \
            values that have been read in from a file.
        :type output_symbols: list of 2-tuples of \
            :py:class:`psyclone.psyir.symbols.Symbol`
        '''

        for (sym_computed, sym_read) in output_symbols:
            if isinstance(sym_computed.datatype, ArrayType):
                cond = "all({0} - {1} == 0.0)".format(sym_computed.name,
                                                      sym_read.name)
            else:
                cond = "{0} == {1}".format(sym_computed.name,
                                           sym_read.name)
            # The PSyIR has no support for output functions, so we parse
            # Fortran code to create a code block which stores the output
            # statements.
            code = '''
                subroutine tmp()
                  integer :: {0}, {1}
                  if ({2}) then
                     print *,"{0} correct"
                  else
                     print *,"{0} incorrect. Values are:"
                     print *,{0}
                     print *,"{0} values should be:"
                     print *,{1}
                  endif
                end subroutine tmp'''.format(sym_computed.name,
                                             sym_read.name, cond)

            fortran_reader = FortranReader()
            container = fortran_reader.psyir_from_source(code)
            if_block = container.children[0].children[0]
            program.addchild(if_block.detach())

    # -------------------------------------------------------------------------
    def create(self, nodes, input_list, output_list,
               prefix, postfix, region_name):
        # pylint: disable=too-many-arguments
        '''This function uses the PSyIR to create a stand-alone driver
        that reads in a previously created file with kernel input and
        output information, and calls the kernels specified in the 'nodes'
        PSyIR tree with the parameters from the file. It returns the
        file container which contains the driver.

        :param nodes: a list of nodes.
        :type nodes: list of :py:obj:`psyclone.psyir.nodes.Node`
        :param input_list: list of variables that are input parameters.
        :type input_list: list of :py:class:`psyclone.core.Signature`
        :param output_list: list of variables that are output parameters.
        :type output_list: list or :py:class:`psyclone.core.Signature`
        :param str prefix: the prefix to use for each PSyData symbol, \
            e.g. 'extract' as prefix will create symbols `extract_psydata`.
        :param str postfix: a postfix that is appended to an output variable \
            to create the corresponding variable that stores the output \
            value from the kernel data file. The caller must guarantee that \
            no name clashes are created when adding the postfix to a variable \
            and that the postfix is consistent between extract code and \
            driver code (see 'ExtractTrans.determine_postfix()').
        :param (str,str) region_name: an optional name to \
            use for this PSyData area, provided as a 2-tuple containing a \
            location name followed by a local name. The pair of strings \
            should uniquely identify a region.

        :returns: the program PSyIR for a stand-alone driver.
        :rtype: :py:class:`psyclone.psyir.psyir.nodes.FileContainer`

        '''
        # pylint: disable=too-many-locals

        # Since this is a 'public' method of an entirely separate class,
        # we check that the list of nodes is what it expects. This is done
        # by invoking the validate function of the basic extract function.
        extract_trans = ExtractTrans()
        # We need to provide the prefix to the validation function:
        extract_trans.validate(nodes, options={"prefix": prefix})

        module_name, local_name = region_name
        unit_name = "{0}_{1}".format(module_name, local_name)

        # First create the file container, which will only store the program:
        file_container = FileContainer(unit_name)

        # Create the program and add it to the file container:
        program = Routine(unit_name, is_program=True)
        program_symbol_table = program.symbol_table
        file_container.addchild(program)

        if prefix:
            prefix = prefix + "_"

        psy_data_mod = ContainerSymbol(prefix+"psy_data_mod")
        program_symbol_table.add(psy_data_mod)
        psy_data_type = DataTypeSymbol(prefix+"PsyDataType", DeferredType(),
                                       interface=ImportInterface(psy_data_mod))
        program_symbol_table.add(psy_data_type)

        writer = FortranWriter()
        # The validation of the extract transform guarantees that all nodes
        # in the node list have the same parent.
        schedule_copy = nodes[0].parent.copy()
        schedule_copy.lower_to_language_level()
        self.import_modules(program, schedule_copy)
        self.add_all_kernel_symbols(schedule_copy, program_symbol_table,
                                    writer)

        root_name = prefix + "psy_data"
        psy_data = program_symbol_table.new_symbol(root_name=root_name,
                                                   symbol_type=DataSymbol,
                                                   datatype=psy_data_type)

        module_str = Literal(module_name, CHARACTER_TYPE)
        region_str = Literal(local_name, CHARACTER_TYPE)
        self.add_call(program, "{0}%OpenRead".format(psy_data.name),
                      [module_str, region_str])

        output_symbols = self.create_read_in_code(program, psy_data,
                                                  input_list, output_list,
                                                  postfix)
        # Copy over all of the executable part of the extracted region
        all_children = schedule_copy.pop_all_children()
        for child in all_children:
            program.addchild(child)

        self.add_result_tests(program, output_symbols)

        return file_container

    # -------------------------------------------------------------------------
    def get_driver_as_string(self, nodes, input_list, output_list,
                             prefix, postfix, region_name,
                             writer=FortranWriter()):
        # pylint: disable=too-many-arguments
        '''This function uses 'create()` function to get a PSyIR of a
        stand-alone driver, and then uses the provided language writer
        to create a string representation in the selected language
        (defaults to Fortran).

        :param nodes: a list of nodes.
        :type nodes: list of :py:obj:`psyclone.psyir.nodes.Node`
        :param input_list: list of variables that are input parameters.
        :type input_list: list of :py:class:`psyclone.core.Signature`
        :param output_list: list of variables that are output parameters.
        :type output_list: list or :py:class:`psyclone.core.Signature`
        :param str prefix: the prefix to use for each PSyData symbol, \
            e.g. 'extract' as prefix will create symbols `extract_psydata`.
        :param str postfix: a postfix that is appended to an output variable \
            to create the corresponding variable that stores the output \
            value from the kernel data file. The caller must guarantee that \
            no name clashes are created when adding the postfix to a variable \
            and that the postfix is consistent between extract code and \
            driver code (see 'ExtractTrans.determine_postfix()').
        :param (str,str) region_name: an optional name to \
            use for this PSyData area, provided as a 2-tuple containing a \
            location name followed by a local name. The pair of strings \
            should uniquely identify a region.
        :param language_writer: a backend visitor to convert PSyIR \
            representation to the selected language. It defaults to \
            the FortranWriter.
        :type language_writer: \
            :py:class:`psyclone.psyir.backend.language_writer.LanguageWriter`

        :returns: the driver in the selected language.
        :rtype: str

        '''
        file_container = self.create(nodes, input_list, output_list,
                                     prefix, postfix, region_name)
        return writer(file_container)

    # -------------------------------------------------------------------------
    def write_driver(self, nodes, input_list, output_list,
                     prefix, postfix, region_name, writer=FortranWriter()):
        # pylint: disable=too-many-arguments
        '''This function uses the 'get_driver_as_string()` function to get a
        a stand-alone driver, and then writes this source code to a file. The
        file name is derived from the region name:
        "driver-"+module_name+"_"+region_name+".f90"

        :param nodes: a list of nodes.
        :type nodes: list of :py:obj:`psyclone.psyir.nodes.Node`
        :param input_list: list of variables that are input parameters.
        :type input_list: list of :py:class:`psyclone.core.Signature`
        :param output_list: list of variables that are output parameters.
        :type output_list: list or :py:class:`psyclone.core.Signature`
        :param str prefix: the prefix to use for each PSyData symbol, \
            e.g. 'extract' as prefix will create symbols `extract_psydata`.
        :param str postfix: a postfix that is appended to an output variable \
            to create the corresponding variable that stores the output \
            value from the kernel data file. The caller must guarantee that \
            no name clashes are created when adding the postfix to a variable \
            and that the postfix is consistent between extract code and \
            driver code (see 'ExtractTrans.determine_postfix()').
        :param (str,str) region_name: an optional name to \
            use for this PSyData area, provided as a 2-tuple containing a \
            location name followed by a local name. The pair of strings \
            should uniquely identify a region.
        :param language_writer: a backend visitor to convert PSyIR \
            representation to the selected language. It defaults to \
            the FortranWriter.
        :type language_writer: \
            :py:class:`psyclone.psyir.backend.language_writer.LanguageWriter`

        '''
        code = self.get_driver_as_string(nodes, input_list, output_list,
                                         prefix, postfix, region_name,
                                         writer=writer)
        module_name, local_name = region_name
        with open("driver-{0}-{1}.f90".
                  format(module_name, local_name), "w") as out:
            out.write(code)
