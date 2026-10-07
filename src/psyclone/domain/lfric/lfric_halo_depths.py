# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2024-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
This module contains the LFRicHaloDepths class which manages the
declarations for halo-depth arguments required by special kernels that iterate
into the halo cells.

'''

from psyclone.configuration import Config
from psyclone.domain.lfric.lfric_collection import LFRicCollection
from psyclone.psyir.nodes import Literal, Reference
from psyclone.psyir.symbols import ArgumentInterface, DataSymbol


class LFRicHaloDepths(LFRicCollection):
    '''
    Manages the declarations for all halo-depth arguments (as needed by
    kernels that have operates_on == HALO_CELL_COLUMNS or
    OWNED_AND_HALO_CELL_COLUMNS) required by an Invoke.

    :param node: the LFRic Invoke for which to manage halo-depth arguments.
    :type node: py:class:`psyclone.domain.lfric.lfric_invoke.LFRicInvoke`

    :raises NotImplementedError: if the halo-depth passed to a Kernel from the
        Algorithm layer is not a literal or a scalar reference.

    '''
    def __init__(self, node):
        super().__init__(node)
        self._halo_depth_vars = set()
        if not Config.get().distributed_memory:
            # No distributed memory so there are no halo regions.
            return
        depth_names = set()
        for kern in self.kernel_calls:
            if not kern.halo_depth:
                continue
            if not isinstance(kern.halo_depth, Literal):
                # pylint: disable-next=unidiomatic-typecheck
                if not type(kern.halo_depth) is Reference:
                    raise NotImplementedError(
                        f"A kernel halo-depth argument must currently be a "
                        f"scalar reference or literal but Kernel '{kern.name}'"
                        f" is passed a depth given by "
                        f"'{kern.halo_depth.debug_string()}'")
                name = kern.halo_depth.symbol.name
                if name not in depth_names:
                    # An invoke could call the same kernel multiple times with
                    # different halo depths.
                    depth_names.add(name)
                    self._halo_depth_vars.add(kern.halo_depth.symbol)

    def invoke_declarations(self):
        '''
        Creates the declarations for the depths to which any 'halo'
        kernels iterate into the halos.

        '''
        super().invoke_declarations()
        # Add the Invoke subroutine argument declarations for the
        # different halo depths. They are declared as intent "in".
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.lfric import LFRicTypes
        if self._halo_depth_vars:
            var_names = [sym.name for sym in self._halo_depth_vars]
            var_names.sort()
            for name in var_names:
                sym = self.symtab.find_or_create(
                    name, symbol_type=DataSymbol,
                    datatype=LFRicTypes("LFRicIntegerScalarDataType")())
                sym.interface = ArgumentInterface(
                                        ArgumentInterface.Access.READ)
                self.symtab.append_argument(sym)


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for.
__all__ = ['LFRicHaloDepths']
