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
from psyclone.domain.gocean.nodes import GOceanExtractNode
from psyclone.gocean1p0 import GOLoop
from psyclone.psyir.transformations import ExtractTrans, TransformationError

from psyclone.psyir.nodes import Routine, FileContainer
from psyclone.psyir.symbols import DataSymbol, ContainerSymbol, \
    GlobalInterface, DataTypeSymbol, DeferredType
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import Reference, \
    StructureReference
from psyclone.psyir.symbols import ArrayType, \
    REAL8_TYPE, REAL_SINGLE_TYPE, INTEGER_SINGLE_TYPE, REAL_TYPE, \
    INTEGER_TYPE, LocalInterface, ScalarType


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
    def __init__(self, integer_type=INTEGER_TYPE,
                 real_type=REAL8_TYPE):
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
            print(self._default_types)
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
    def flatten_reference(self, old_reference, symbol_table,
                          writer=FortranWriter()):
        '''Replaces `old_reference` which is a structure type with a new
        simple variable name (replacing all % with _).
        '''

        fortran_string = writer(old_reference)
        try:
            symbol = symbol_table.lookup_with_tag(fortran_string)
        except KeyError:
            new_name = fortran_string.replace("%", "_")
            print("New symbol", fortran_string)
            # Symbol already in table
            symbol = self.get_type(new_name, old_reference,
                                   symbol_table, writer)
            print("ST", symbol_table)
            symbol_table.add(symbol, tag=fortran_string)

        old_reference.symbol = symbol

    # -------------------------------------------------------------------------
    def add_all_kernel_symbols(self, nodes, symbol_table,
                               writer=FortranWriter()):
        '''This function adds all symbols used in `nodes` to the symbol table.
        It uses GOcean-specific knowledge to declare fields and flatten their
        name.

        '''
        # First we add all non-structure names to the symbol table. This way
        # the flattened name can be ensured not to clash with a variable name
        # used in the program.
        for reference in nodes.walk(Reference):
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
        for reference in nodes.walk(Reference):
            if not isinstance(reference, StructureReference):
                continue
            old_symbol = reference.symbol
            if old_symbol.datatype.name != "r2d_field":
                raise TransformationError("Unknown derived type '{0}'."
                                          .format(old_symbol.datatype.name))
            # We have a structure reference to a field, flatten it:
            self.flatten_reference(reference, symbol_table, writer=writer)

    # -------------------------------------------------------------------------
    def create(self, nodes, input_list, output_list, options):
        '''This function uses the PSyIR to create a stand-alone driver
        that reads in a previously created file with kernel input and
        output information, and calls the kernel with the parameters from
        the file.

        :param input_list: list of variables that are input parameters.
        :type input_list: list of str
        :param output_list: list of variables that are output parameters.
        :type output_list: list or str
        '''

        # module_name, region_name = self.region_identifier
        module_name, region_name = "module", "region"
        unit_name = "{0}_{1}".format(module_name, region_name)

        # First create the file container, which will only store the program:
        file_container = FileContainer(unit_name)

        # Create the program and add it to the file container:
        program = Routine(unit_name, is_program=True)
        program_symbol_table = program.symbol_table
        file_container.addchild(program)

        # Import the PSyDataType:
        psy_data_mod = ContainerSymbol("psy_data_mod")
        program_symbol_table.add(psy_data_mod)
        psy_data_type = DataTypeSymbol("PsyDataType", DeferredType(),
                                       interface=GlobalInterface(psy_data_mod))
        program_symbol_table.add(psy_data_type)

        # Declare a variable of the above PSyDataType:
        prefix = options.get("prefix", "")
        if prefix:
            # Add an _ if there is a prefix
            prefix += "_"

        root_name = prefix + "psy_data"

        psy_data = program_symbol_table.new_symbol(root_name=root_name,
                                                   symbol_type=DataSymbol,
                                                   datatype=psy_data_type)

        writer = FortranWriter()
        schedule_copy = nodes[0].parent.copy()
        schedule_copy.lower_to_language_level()
        self.add_all_kernel_symbols(schedule_copy, program_symbol_table,
                                    writer)
        all_children = schedule_copy.pop_all_children()
        for child in all_children:
            program.addchild(child)

        code = writer(file_container)
        print("CODE", code)

        # with open("driver-{0}-{1}.f90".
        #           format(module_name, region_name), "w") as out:
        #     out.write(code)
        # file_container = FileContainer.create(
        #     "dummy", SymbolTable(), [container, program])
        # writer = FortranWriter()
        # result = writer(file_container)
        # print("--------------", module_name, region_name)
        # print(result)
        # print("--------------", module_name, region_name)
