# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
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
#          N. Nobre, STFC Daresbury Lab

'''This module provides functionality for the PSyclone kernel extraction
functionality. It contains the class that creates a driver that
reads in extracted data, calls the kernel, and then compares the result with
the output data contained in the input file.
'''


from psyclone.domain.common import BaseDriverCreator
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import Call, FileContainer, Literal, Routine
from psyclone.psyir.symbols import (CHARACTER_TYPE,
                                    ContainerSymbol, DataSymbol,
                                    DataTypeSymbol, UnresolvedType,
                                    ImportInterface, INTEGER_TYPE,
                                    REAL8_TYPE, RoutineSymbol, ScalarType)
from psyclone.psyGen import InvokeSchedule

# TODO 1382: once we support LFRic, make this into a proper base class
# and put the domain-specific implementations into the domain/* directories.


class ExtractDriverCreator(BaseDriverCreator):
    '''This class provides the functionality to create a driver that
    reads in extracted data produced by using the PSyData kernel-extraction
    functionality.

    :param integer_type: default scalar integer type to be used for integer
        variables. Defaults to INTEGER_TYPE.
    :type integer_type: :py:class:`psyclone.psyir.symbols.ScalarType`
    :param real_type: default scalar real type to be used for real
        variables. Defaults to REAL8_TYPE.
    :type real_type: :py:class:`psyclone.psyir.symbols.ScalarType`

    '''
    def __init__(self, integer_type=INTEGER_TYPE, real_type=REAL8_TYPE,
                 region_name=None):
        super().__init__()
        self._region_name = region_name
        # Set the integer and real types to use.
        # For convenience, also add the names used in the gocean config file:
        self._default_types = {"integer": integer_type,
                               "real": real_type}

    # -------------------------------------------------------------------------
    @staticmethod
    def import_modules(program, sched):
        '''This function adds all the import statements required for the
        actual kernel calls. It finds all calls in the PSyIR tree and
        checks for calls with a ImportInterface. Any such call will
        get a ContainerSymbol added for the module, and a RoutineSymbol
        with an import interface pointing to this module.

        :param program: the PSyIR Routine to which any code must
            be added. It also contains the symbol table to be used.
        :type program: :py:class:`psyclone.psyir.nodes.Routine`
        :param sched: the schedule that will be called by the driver
            program created.
        :type sched: :py:class:`psyclone.psyir.nodes.Schedule`

        '''
        symbol_table = program.scope.symbol_table
        for call in sched.walk(Call):
            routine = call.routine.symbol
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
            new_routine_sym = RoutineSymbol(routine.name, UnresolvedType(),
                                            interface=ImportInterface(module))
            symbol_table.add(new_routine_sym)

    # -------------------------------------------------------------------------
    def create(self, nodes, read_write_info, prefix, postfix, region_name):
        # pylint: disable=too-many-arguments
        '''This function uses the PSyIR to create a stand-alone driver
        that reads in a previously created file with kernel input and
        output information, and calls the kernels specified in the 'nodes'
        PSyIR tree with the parameters from the file. It returns the
        file container which contains the driver.

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
        :param (str,str) region_name: an optional name to
            use for this PSyData area, provided as a 2-tuple containing a
            location name followed by a local name. The pair of strings
            should uniquely identify a region.

        :returns: the program PSyIR for a stand-alone driver.
        :rtype: :py:class:`psyclone.psyir.psyir.nodes.FileContainer`

        '''
        # pylint: disable=too-many-locals

        # Since this is a 'public' method of an entirely separate class,
        # we check that the list of nodes is what it expects. This is done
        # by invoking the validate function of the basic extract function.
        module_name, local_name = region_name
        unit_name = self._make_valid_unit_name(f"{module_name}_{local_name}")

        # First create the file container, which will only store the program:
        file_container = FileContainer(unit_name)

        # Create the program and add it to the file container:
        program = Routine.create(unit_name, is_program=True)
        program_symbol_table = program.symbol_table
        file_container.addchild(program)

        if prefix:
            prefix = prefix + "_"

        psy_data_mod = ContainerSymbol("read_kernel_data_mod")
        program_symbol_table.add(psy_data_mod)
        psy_data_type = DataTypeSymbol("ReadKernelDataType", UnresolvedType(),
                                       interface=ImportInterface(psy_data_mod))
        program_symbol_table.add(psy_data_type)

        # Create a routine with a copy of the region of interest
        schedule_copy = Routine.create("name")
        schedule_copy.children.extend([n.copy() for n in nodes[0].children])
        og_symtab = nodes[0].ancestor(InvokeSchedule).symbol_table

        self.import_modules(program, schedule_copy)

        root_name = prefix + "psy_data"
        psy_data = program_symbol_table.new_symbol(root_name=root_name,
                                                   symbol_type=DataSymbol,
                                                   datatype=psy_data_type)

        module_str = Literal(module_name, CHARACTER_TYPE)
        region_str = Literal(local_name, CHARACTER_TYPE)
        self.add_call(program, f"{psy_data.name}%OpenReadModuleRegion",
                      [module_str, region_str])

        output_symbols = self._create_read_in_code(program, psy_data,
                                                   og_symtab,
                                                   read_write_info, postfix)

        # Move the nodes making up the extracted region into the Schedule
        # of the driver program
        all_children = schedule_copy.pop_all_children()
        for child in all_children:
            program.addchild(child)

        self.replace_precisions(program)

        self.add_result_tests(program, output_symbols)

        return file_container

    def replace_precisions(self, program):
        ''' Replaces the precisions with the values given in the _default_types
        in order to avoid imported precision symbols.
        '''
        for symbol in program.symbol_table.symbols:
            if isinstance(symbol, DataSymbol):
                dt = symbol.datatype
                if isinstance(dt, ScalarType):
                    if dt.intrinsic == ScalarType.Intrinsic.INTEGER:
                        symbol.datatype = self._default_types["integer"]
                    if dt.intrinsic == ScalarType.Intrinsic.REAL:
                        symbol.datatype = self._default_types["real"]

    # -------------------------------------------------------------------------
    def get_driver_as_string(self, nodes, read_write_info,
                             prefix, postfix, region_name,
                             writer=FortranWriter()):
        # pylint: disable=too-many-arguments
        '''This function uses `create()` function to get the PSyIR of a
        stand-alone driver, and then uses the provided language writer
        to create a string representation in the selected language
        (defaults to Fortran).

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
        :param (str,str) region_name: an optional name to
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
        file_container = self.create(nodes, read_write_info,
                                     prefix, postfix, region_name)
        return writer(file_container)

    # -------------------------------------------------------------------------
    def write_driver(self, nodes, read_write_info, prefix, postfix,
                     region_name, writer=FortranWriter()):
        # pylint: disable=too-many-arguments
        '''This function uses the `get_driver_as_string()` function to get a
        a stand-alone driver, and then writes this source code to a file. The
        file name is derived from the region name:
        "driver-"+module_name+"_"+region_name+".f90"

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
        :param (str,str) region_name: an optional name to
            use for this PSyData area, provided as a 2-tuple containing a
            location name followed by a local name. The pair of strings
            should uniquely identify a region.
        :param language_writer: a backend visitor to convert PSyIR
            representation to the selected language. It defaults to
            the FortranWriter.
        :type language_writer:
            :py:class:`psyclone.psyir.backend.language_writer.LanguageWriter`

        '''
        if self._region_name is not None:
            region_name = self._region_name
        code = self.get_driver_as_string(nodes, read_write_info, prefix,
                                         postfix, region_name, writer=writer)
        module_name, local_name = region_name
        with open(f"driver-{module_name}-{local_name}.f90", "w",
                  encoding='utf-8') as out:
            out.write(code)
