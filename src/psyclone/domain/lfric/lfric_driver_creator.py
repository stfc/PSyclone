# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

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
                                    ImportInterface, ScalarType,
                                    SymbolTable)


class LFRicDriverCreator(DriverCreator):
    '''This class provides the functionality to create a driver that
    reads in extracted data produced by using the PSyData kernel-extraction
    functionality.


    :param region_name: the suggested region_name.
    '''
    def __init__(self, region_name: Optional[tuple[str, str]] = None) -> None:
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
        symbols used by LFRic from the constants_mod module. It also adds
        imports of the real32 and real64 intrinsic kinds.

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
        names_to_skip = ["r_quad", "r_phys"] + list(const.INTRINSIC_KINDS)
        api_config = Config.get().api_conf("lfric")
        all_precisions = [name for name in api_config.precision_map
                          if name not in names_to_skip]
        for prec_name in all_precisions:
            symbol_table.new_symbol(prec_name,
                                    tag=f"{prec_name}@{mod_name}",
                                    symbol_type=DataSymbol,
                                    datatype=ScalarType.integer_type(),
                                    interface=ImportInterface(constant_mod))
        # Intrinsic kind symbols are imported into constants_mod but are
        # private to it so have to be handled separately.
        iso_mod = ContainerSymbol(const.FORTRAN_ISO_MOD_NAME,
                                  is_intrinsic=True)
        symbol_table.add(iso_mod)
        for prec_name in const.INTRINSIC_KINDS:
            symbol_table.new_symbol(prec_name,
                                    tag=f"{prec_name}@{iso_mod.name}",
                                    symbol_type=DataSymbol,
                                    datatype=ScalarType.integer_type(),
                                    interface=ImportInterface(iso_mod))

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
