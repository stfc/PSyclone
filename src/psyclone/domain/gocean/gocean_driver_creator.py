# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module provides functionality for the PSyclone kernel extraction
functionality. It contains the class that creates a driver that
reads in extracted data, calls the kernel, and then compares the result with
the output data contained in the input file.
'''

from typing import Optional

from psyclone.domain.common import DriverCreator
from psyclone.psyir.symbols import DataSymbol, ScalarType, SymbolTable


class GOceanDriverCreator(DriverCreator):
    '''This class provides the functionality to create a driver that
    reads in extracted data produced by using the PSyData kernel-extraction
    functionality.

    :param integer_type: default scalar integer type to be used for integer
        variables. Defaults to ScalarType.integer_type().
    :param real_type: default scalar real type to be used for real
        variables. Defaults to ScalarType.real8_type().
    :param region_name: Suggested region name.

    '''
    def __init__(self, integer_type: ScalarType = ScalarType.integer_type(),
                 real_type: ScalarType = ScalarType.real8_type(),
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
