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

from psyclone.domain.common import BaseDriverCreator
from psyclone.psyir.nodes import FileContainer, Node, Routine
from psyclone.psyir.symbols import (DataSymbol,
                                    INTEGER_TYPE,
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
                 region_name: Optional[Tuple[str, str]] = None) -> None:
        super().__init__(region_name)
        # Set the integer and real types to use.
        # For convenience, also add the names used in the gocean config file:
        self._default_types = {"integer": integer_type,
                               "real": real_type}

    # -------------------------------------------------------------------------
    def create(self,
               nodes: List[Node],
               read_write_info: ReadWriteInfo,
               prefix: str,
               postfix: str,
               region_name: Tuple[str, str]) -> FileContainer:
        # pylint: disable=too-many-arguments
        '''This function uses the PSyIR to create a stand-alone driver
        that reads in a previously created file with kernel input and
        output information, and calls the kernels specified in the 'nodes'
        PSyIR tree with the parameters from the file. It returns the
        file container which contains the driver.

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

        :returns: the program PSyIR for a stand-alone driver.

        '''
        # pylint: disable=too-many-locals

        file_container, psy_data = self.create_driver_template(region_name,
                                                               prefix)

        program = file_container.walk(Routine)[0]
        og_symtab = nodes[0].ancestor(Routine).symbol_table

        output_symbols = self._create_read_in_code(program, psy_data,
                                                   og_symtab,
                                                   read_write_info, postfix)

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
