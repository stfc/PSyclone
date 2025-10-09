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

from typing import Optional

from psyclone.domain.common import DriverCreator
from psyclone.psyir.symbols import (DataSymbol, INTEGER_TYPE, REAL8_TYPE,
                                    ScalarType, SymbolTable)


class GOceanDriverCreator(DriverCreator):
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
                 region_name: Optional[tuple[str, str]] = None) -> None:
        super().__init__(region_name)
        # Set the integer and real types to use.
        # For convenience, also add the names used in the gocean config file:
        self._default_types = {"integer": integer_type,
                               "real": real_type}

    # -------------------------------------------------------------------------
    def handle_precision_symbols(self, symbol_table: SymbolTable) -> None:
        ''' Replaces the precisions with the values given in the _default_types
        in order to avoid imported precision symbols.

        :param program: the PSyIR Routine in which to replace the symbols.
        '''
        for symbol in symbol_table.symbols:
            if isinstance(symbol, DataSymbol):
                dt = symbol.datatype
                if isinstance(dt, ScalarType):
                    if dt.intrinsic == ScalarType.Intrinsic.INTEGER:
                        symbol.datatype = self._default_types["integer"]
                    if dt.intrinsic == ScalarType.Intrinsic.REAL:
                        symbol.datatype = self._default_types["real"]
