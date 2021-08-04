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

'''This module contains the object that helps creating a driver that reads
in extracted data, calls the kernel, and then compares the result with
the output data contained in the input file.
'''

import six

from psyclone.configuration import Config
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.transformations import TransformationError


from psyclone.psyir.nodes import Assignment, Routine, FileContainer
from psyclone.psyir.symbols import DataSymbol, ContainerSymbol, \
    GlobalInterface, DataTypeSymbol, DeferredType
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import Call, Literal, Reference, \
    StructureReference
from psyclone.psyir.symbols import ArrayType, CHARACTER_TYPE, \
    REAL8_TYPE, INTEGER_TYPE, ScalarType, RoutineSymbol

from psyclone.psyGen import InvokeSchedule


class ExtractDriverCreator:
    '''This class provides the functionality to create a driver that
    reads in extracted data.

    :param integer_type: default scalar integer type to be used for integer \
        variables. Defaults to INTEGER_TYPE.
    :type integer_type: :py:class:`psyclone.psyir.symbol.ScalarType`
    :param real_type: default scalar real type to be used for real \
        variables. Defaults to REAL8_TYPE.
    :type real_type: :py:class:`psyclone.psyir.symbol.ScalarType`

    '''
    def __init__(self, prefix,
                 integer_type=INTEGER_TYPE,
                 real_type=REAL8_TYPE):
        self._prefix = prefix
        # Set the integer and real types to use. If required, the constructor
        # could take a parameter to change these.
        # For convenience, also add the names used in the gocean config file:
        self._default_types = {ScalarType.Intrinsic.INTEGER: integer_type,
                               "integer": integer_type,
                               ScalarType.Intrinsic.REAL: real_type,
                               "real": real_type}

    # -------------------------------------------------------------------------
    def get_type(self, new_name, reference, symbol_table,
                 writer=FortranWriter()):
        '''Takes a reference to a structure and determines the Fortran type.
        E.g. fld%data will be mapped to `real, dimension(:,:)`, and
        `fld%data$whole%xstart` to `integer`.
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
            raise TransformationError(
                "Could not find type for reference '{0}'."
                .format(fortran_expression))
        try:
            base_type = self._default_types[gocean_property.intrinsic_type]
        except KeyError as err:
            raise six.raise_from(
                TransformationError("Unknown type '{0}' in GOcean API."
                                    .format(gocean_property.intrinsic_type)),
                err)
        # Handle name clashes (e.g. if the user used a variable that is
        # the same as a flattened grid property)
        new_name = symbol_table.next_available_name(new_name)
        if gocean_property.type == "scalar":
            new_symbol = DataSymbol(new_name, base_type)

        elif gocean_property.type == "array":
            # At this stage all gocean arrays are 2d (even integer ones)
            # so no need to add any further tests here.
            array = ArrayType(base_type, [ArrayType.Extent.DEFERRED,
                                          ArrayType.Extent.DEFERRED])
            new_symbol = DataSymbol(new_name, array)

        return new_symbol

    # -------------------------------------------------------------------------
    @staticmethod
    def flatten_string(fortran_string):
        '''Replaces all `%` with `_` in the string, creating a 'flattened'
        name.

        :param str fortran_string: the Fortran string containing '%'.
        '''
        return fortran_string.replace("%", "_")

    # -------------------------------------------------------------------------
    def flatten_reference(self, old_reference, symbol_table,
                          writer=FortranWriter()):
        '''Replaces `old_reference` which is a structure type with a new
        simple Reference and a flattened name (replacing all % with _).

        :param old_reference: a reference to a structured member.
        :type old_reference: \
            :py:class:`psyclone.psyir.nodes.StructureReference`
        :param symbol_table: the symbol table to which to add the newly \
            defind flattened symbol.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param writer: a Fortran writer used when flattening a \
            `StructureReference`.
        :type writer: :py:`psyclone.psyir.backend.fortan.FortranWriter`

        '''

        # A field access (`fld%data`) will get the `%data` removed, since then
        # there is less of a chance of a name class (`fld` is guaranteed to
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
            flattened_name = \
                ExtractDriverCreator.flatten_string(fortran_string)
            # Symbol already in table
            symbol = self.get_type(flattened_name, old_reference,
                                   symbol_table, writer)
            symbol_table.add(symbol, tag=fortran_string)

        # We need to create a new, flattened Reference and replace the
        # StructureReference with it:
        new_ref = Reference(symbol)
        position = old_reference.position
        old_reference.parent.children[position] = new_ref

    # -------------------------------------------------------------------------
    def add_all_kernel_symbols(self, nodes, symbol_table,
                               writer=FortranWriter()):
        '''This function adds all symbols used in `nodes` to the symbol table.
        It uses GOcean-specific knowledge to declare fields and flatten their
        name.

        :param nodes: the schedule that will be called by this driver program.
        :type nodes: :py:class:`psyclone.psyir.nodes.Schedule`
        :param symbol_table: the symbol table to which to add all found \
            symbols.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param writer: a Fortran writer used when flattening a \
            `StructureReference`.
        :type writer: :py:`psyclone.psyir.backend.fortan.FortranWriter`

        '''
        all_references = nodes.walk(Reference)
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
            # symbol table here. GOcean does not support any array types
            # as parameter, so we only need to declare scalars here.
            try:
                new_type = self._default_types[old_symbol.datatype.intrinsic]
            except KeyError as err:
                six.raise_from(TransformationError(
                    "Unknown intrinsic data type '{0}'."
                    .format(old_symbol.datatype.intrinsic)), err)
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
                raise TransformationError("Unknown derived type '{0}'."
                                          .format(old_symbol.datatype.name))
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

        :param program: the program PSyIR Routine to which any code must \
            be added. It also contains the symbol table to be used.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`
        :param str name: name of the subroutine to call.
        :param args: list of all arguments for the call.
        :type args: list of :py:class:`psyclone.psyir.nodes.Node`

        '''
        if name in program.symbol_table:
            routine_symbol = program.symbol_table.lookup(name)
        else:
            routine_symbol = RoutineSymbol(name)
            program.symbol_table.add(routine_symbol)
        call = Call.create(routine_symbol, args)
        program.addchild(call)

    # -------------------------------------------------------------------------
    @staticmethod
    def create_read_in_code(program, psy_data, input_list, output_list):
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
        - variable that is written only, it will create a cariable with "_post"
            as postfix that reads in the output data from the NetCDF file. It
            then also declares a variable without postfix (which will be the
            parameter to the function), allocates it based on the shape of
            the corresponding "_post" variable, and initialises it with 0.

        :param program: the program PSyIR Routine to which any code must \
            be added. It also contains the symbol table to be used.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`
        :param psy_data: the PSyData symbol to be used.
        :type psy_data: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param input_list: list of all signatures that are input variables \
            to the instrumended region.
        :type input_list: list of :py:class:`psyclone.core.Signature`
        :param output_list: list of all signatures that are output \
            variables to the instrumended region.
        :type output_list: list of :py:class:`psyclone.core.Signature`

        '''
        # pylint: disable=too-many-locals
        all_sigs = list(set(input_list).union(set(output_list)))
        all_sigs.sort()
        symbol_table = program.scope.symbol_table
        read_var = "{0}%ReadVariable".format(psy_data.name)

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
            post_name = sig_str+"_post"
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
                    # this parsing of a file can be replaced.
                    code = '''
                        module test
                        contains
                        subroutine tmp()
                          integer, allocatable, dimension(:,:) :: b
                          allocate({0}(size({1},1), size({1},2)))
                          !allocate({0}, mold={1})
                        end subroutine tmp
                        end module test'''.format(sig_str, post_name)
                    fortran_reader = FortranReader()
                    container = fortran_reader.psyir_from_source(code)\
                        .children[0]
                    alloc = container.children[0].children[0]
                    alloc.parent.children.remove(alloc)
                    program.addchild(alloc)
                set_zero = Assignment.create(Reference(sym),
                                             Literal("0", INTEGER_TYPE))
                program.addchild(set_zero)

    # -------------------------------------------------------------------------
    @staticmethod
    def import_modules(program, nodes):
        '''This function adds all the import statements required for the
        actual kernel calls. It find all calls in the PSyIR trees and
        checks for calls with a GlobalInterface. Any such call will
        get a ContainerSymbol added for the module, and a RoutineSymbol
        with a global interface pointing to this module.

        :param program: the program PSyIR Routine to which any code must \
            be added. It also contains the symbol table to be used.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`
        :param nodes: the schedule that will be called by the driver \
            program created.
        :type nodes: :py:class:`psyclone.psyir.nodes.Schedule`

        '''
        symbol_table = program.scope.symbol_table
        for call in nodes.walk(Call):
            routine = call.routine
            if not isinstance(routine.interface, GlobalInterface):
                continue
            if routine.name in symbol_table:
                # Symbol has already been added - ignore
                continue
            # We need to create a new symbol for the module and the routine
            # called (which will  trigger to add the import statement later):
            module = ContainerSymbol(routine.interface.container_symbol.name)
            symbol_table.add(module)
            new_routine_sym = RoutineSymbol(routine.name, DeferredType(),
                                            interface=GlobalInterface(module))
            symbol_table.add(new_routine_sym)

    # -------------------------------------------------------------------------
    def create(self, nodes, input_list, output_list, options):
        '''This function uses the PSyIR to create a stand-alone driver
        that reads in a previously created file with kernel input and
        output information, and calls the kernel with the parameters from
        the file.

        :param input_list: list of variables that are input parameters.
        :type input_list: list of :py:class:`psyclone.core.Signature`
        :param output_list: list of variables that are output parameters.
        :type output_list: list or :py:class:`psyclone.core.Signature`

        '''
        # pylint: disable=too-many-locals
        if options.get("region_name", False):
            module_name, region_name = options["region_name"]
        else:
            if isinstance(nodes, list):
                invoke = nodes[0].ancestor(InvokeSchedule).invoke
            else:
                invoke = nodes.ancestor(InvokeSchedule).invoke
            module_name = invoke.invokes.psy.name
            region_name = invoke.name

        # module_name, region_name = self.region_identifier
        unit_name = "{0}_{1}".format(module_name, region_name)

        # First create the file container, which will only store the program:
        file_container = FileContainer(unit_name)

        # Create the program and add it to the file container:
        program = Routine(unit_name, is_program=True)
        program_symbol_table = program.symbol_table
        file_container.addchild(program)

        # Import the PSyDataType:
        if self._prefix:
            prefix = self._prefix+"_"
        else:
            prefix = ""
        psy_data_mod = ContainerSymbol(prefix+"psy_data_mod")
        program_symbol_table.add(psy_data_mod)
        psy_data_type = DataTypeSymbol(prefix+"PsyDataType", DeferredType(),
                                       interface=GlobalInterface(psy_data_mod))
        program_symbol_table.add(psy_data_type)

        writer = FortranWriter()
        schedule_copy = nodes[0].parent.copy()
        schedule_copy.lower_to_language_level()
        ExtractDriverCreator.import_modules(program, schedule_copy)
        self.add_all_kernel_symbols(schedule_copy, program_symbol_table,
                                    writer)

        root_name = prefix + "psy_data"
        psy_data = program_symbol_table.new_symbol(root_name=root_name,
                                                   symbol_type=DataSymbol,
                                                   datatype=psy_data_type)

        module_str = Literal(module_name, CHARACTER_TYPE)
        region_str = Literal(region_name, CHARACTER_TYPE)
        ExtractDriverCreator.add_call(program,
                                      "{0}%OpenRead".format(psy_data.name),
                                      [module_str, region_str])

        ExtractDriverCreator.create_read_in_code(program, psy_data,
                                                 input_list, output_list)

        all_children = schedule_copy.pop_all_children()
        for child in all_children:
            program.addchild(child)

        code = writer(file_container)

        with open("driver-{0}-{1}.f90".
                  format(module_name, region_name), "w") as out:
            out.write(code)
