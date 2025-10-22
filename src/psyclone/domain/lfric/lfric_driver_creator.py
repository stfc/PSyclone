# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2025, Science and Technology Facilities Council.
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
# Modified: I. Kavcic, O. Brunt and L. Turner, Met Office
# Modified: S. Siso, STFC Daresbury Lab

'''This module provides functionality for the PSyclone kernel extraction
functionality for LFRic. It contains the class that creates a driver that
reads in extracted data, calls the kernel, and then compares the result with
the output data contained in the input file.
'''

from typing import Optional

from psyclone.configuration import Config
from psyclone.domain.common import DriverCreator
from psyclone.domain.lfric import LFRicConstants
from psyclone.psyir.nodes import Call, Node, StructureReference
from psyclone.psyir.symbols import (ContainerSymbol, DataSymbol,
                                    ImportInterface, INTEGER_TYPE,
                                    SymbolTable)


class LFRicDriverCreator(DriverCreator):
    '''This class provides the functionality to create a driver that
    reads in extracted data produced by using the PSyData kernel-extraction
    functionality.


    :param region_name: the suggested region_name.
    '''
    def __init__(self, region_name: Optional[tuple[str, str]] = None):
        super().__init__(region_name)
        # TODO #2069: check if this list can be taken from LFRicConstants
        # TODO #2018: once r_field is defined in the LFRic infrastructure,
        #             it should be added to this list.
        self._all_field_types = ["integer_field_type", "field_type",
                                 "r_bl_field", "r_solver_field_type",
                                 "r_tran_field_type"]

    # -------------------------------------------------------------------------
    def handle_precision_symbols(self, symbol_table: SymbolTable) -> None:
        '''This function adds an import of the various precision
        symbols used by LFRic from the constants_mod module.

        :param symbol_table: the symbol table to which the precision symbols
            must be added.

        '''
        const = LFRicConstants()
        mod_name = const.UTILITIES_MOD_MAP["constants"]["module"]
        constant_mod = ContainerSymbol(mod_name)
        symbol_table.add(constant_mod)

        # r_quad is defined in constants_mod, but not exported. And r_phys
        # does not exist at all in LFRic, but is still in LFRic's psyclone.cfg
        # file. TODO #2018 and
        # https://code.metoffice.gov.uk/trac/lfric/ticket/4674
        api_config = Config.get().api_conf("lfric")
        all_precisions = [name for name in api_config.precision_map
                          if name not in ["r_quad", "r_phys"]]
        for prec_name in all_precisions:
            symbol_table.new_symbol(prec_name,
                                    tag=f"{prec_name}@{mod_name}",
                                    symbol_type=DataSymbol,
                                    datatype=INTEGER_TYPE,
                                    interface=ImportInterface(constant_mod))

    # -------------------------------------------------------------------------
    def verify_and_cleanup_psyir(self, extract_region: Node) -> None:
        """This implementation removes MPI related calls in LFRic (`set_dirty`
        and `set_clean`. Note that any LFRic-specific StructureReferences
        should have been replaced as part of the lowering process.

        :param extract_region: the node with the extracted region.

        :raises ValueError: if structure references are found (raised in the
            base class)
        """

        # Here check for LFRic-specific set_dirty/set_clean calls, which
        # can just be removed:
        dm_methods = ("set_dirty", "set_clean")
        for sref in extract_region.walk(StructureReference):
            if (isinstance(sref.parent, Call) and
                    sref.member.name in dm_methods):
                # Some methods regarding distributed-memory can be deleted as
                # we know the driver is executed with a single rank.
                sref.parent.detach()

        # This will flag any StructureReference (including other calls)
        # still remaining.
        super().verify_and_cleanup_psyir(extract_region)
