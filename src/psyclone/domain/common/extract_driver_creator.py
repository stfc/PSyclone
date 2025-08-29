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
#          N. Nobre and S. Siso, STFC Daresbury Lab

'''This module provides functionality for the PSyclone kernel extraction
functionality. It contains the class that creates a driver that
reads in extracted data, calls the kernel, and then compares the result with
the output data contained in the input file.
'''

from typing import List, Optional, Tuple

from psyclone.core import Signature
from psyclone.domain.common import BaseDriverCreator
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.backend.language_writer import LanguageWriter
from psyclone.psyir.nodes import FileContainer, Literal, Node, Routine
from psyclone.psyir.symbols import (CHARACTER_TYPE,
                                    ContainerSymbol, DataSymbol,
                                    DataTypeSymbol, UnresolvedType,
                                    ImportInterface, INTEGER_TYPE,
                                    REAL8_TYPE, ScalarType)
from psyclone.psyir.tools import ReadWriteInfo

# TODO 1382: once we support LFRic, make this into a proper base class
# and put the domain-specific implementations into the domain/* directories.


class ExtractDriverCreator(BaseDriverCreator):
    '''This class provides the functionality to create a driver that
    reads in extracted data produced by using the PSyData kernel-extraction
    functionality.

    :param integer_type: default scalar integer type to be used for integer
        variables. Defaults to INTEGER_TYPE.
    :param real_type: default scalar real type to be used for real
        variables. Defaults to REAL8_TYPE.
    :param region_name: Suggested region name.

    '''
    def __init__(self, integer_type: ScalarType = INTEGER_TYPE,
                 real_type: ScalarType = REAL8_TYPE,
                 region_name: Optional[Tuple[str, str]] = None):
        super().__init__()
        self._region_name = region_name
        # Set the integer and real types to use.
        # For convenience, also add the names used in the gocean config file:
        self._default_types = {"integer": integer_type,
                               "real": real_type}

    # -------------------------------------------------------------------------
    def create(self,
               nodes,
               read_write_info,
               prefix,
               postfix,
               region_name,
               removable_vars: List[Tuple[str, Signature]],
               ):
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
        :param removable_vars: a list of tuples containing signatures and
            the container name of variables that were not written to the
            kernel data file (and as such should not be read in, though
            they still need to be declared).

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
        og_symtab = nodes[0].ancestor(Routine).symbol_table
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

        module_str = Literal(module_name, CHARACTER_TYPE)
        region_str = Literal(local_name, CHARACTER_TYPE)
        self.add_call(program, f"{psy_data.name}%OpenReadModuleRegion",
                      [module_str, region_str])

        output_symbols = self._create_read_in_code(program, psy_data,
                                                   og_symtab,
                                                   read_write_info, postfix,
                                                   removable_vars)

        # Copy the nodes that are part of the extraction
        extract_region = nodes[0].copy()
        program.children.extend(extract_region.pop_all_children())

        # Find all imported modules and add them to the symbol table
        self.import_modules(program)

        self.replace_precisions(program)

        self.add_result_tests(program, output_symbols)

        return file_container

    def replace_precisions(self, program: Routine):
        ''' Replaces the precisions with the values given in the _default_types
        in order to avoid imported precision symbols.

        :param program: the PSyIR Routine in which to replace the symbols.
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
                             removable_vars: List[Tuple[str, Signature]],
                             writer=FortranWriter()) -> str:
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
        :param removable_vars: a list of tuples containing signatures and
            the container name of variables that were not written to the
            kernel data file (and as such should not be read in, though
            they still need to be declared).
        :param language_writer: a backend visitor to convert PSyIR
            representation to the selected language. It defaults to
            the FortranWriter.
        :type language_writer:
            :py:class:`psyclone.psyir.backend.language_writer.LanguageWriter`

        :returns: the driver in the selected language.

        '''
        file_container = self.create(nodes, read_write_info,
                                     prefix, postfix, region_name,
                                     removable_vars)
        return writer(file_container)

    # -------------------------------------------------------------------------
    def write_driver(self,
                     nodes: List[Node],
                     read_write_info: ReadWriteInfo,
                     prefix: str,
                     postfix: str,
                     region_name: Tuple[str, str],
                     removable_vars: List[Tuple[str, Signature]],
                     writer: LanguageWriter = FortranWriter()) -> None:
        # pylint: disable=too-many-arguments
        '''This function uses the `get_driver_as_string()` function to get a
        a stand-alone driver, and then writes this source code to a file. The
        file name is derived from the region name:
        "driver-"+module_name+"_"+region_name+".f90"

        :param nodes: a list of nodes.
        :param read_write_info: information about all input and output
            parameters.
        :type read_write_info: :py:class:`psyclone.psyir.tools.ReadWriteInfo`
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
        :param removable_vars: a list of tuples containing signatures and
            the container name of variables that were not written to the
            kernel data file (and as such should not be read in, though
            they still need to be declared).
        :param language_writer: a backend visitor to convert PSyIR
            representation to the selected language. It defaults to
            the FortranWriter.

        '''
        if self._region_name is not None:
            region_name = self._region_name
        code = self.get_driver_as_string(nodes, read_write_info, prefix,
                                         postfix, region_name,
                                         removable_vars, writer=writer)
        module_name, local_name = region_name
        with open(f"driver-{module_name}-{local_name}.f90", "w",
                  encoding='utf-8') as out:
            out.write(code)
