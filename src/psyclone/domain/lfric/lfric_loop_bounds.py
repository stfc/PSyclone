# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module provides the LFRicLoopBounds Class that handles all variables
    required for specifying loop limits within an LFRic PSy-layer routine.'''

from psyclone.domain.lfric import LFRicCollection, LFRicLoop, LFRicTypes
from psyclone.psyGen import Kern
from psyclone.psyir.nodes import Assignment, Literal, Reference
from psyclone.psyir.symbols import DataSymbol, Symbol


class LFRicLoopBounds(LFRicCollection):
    '''
    Handles all variables required for specifying loop limits within
    an LFRic PSy-layer routine.
    '''

    def initialise(self, _) -> int:
        '''
        Updates the PSyIR so that all of the variables holding the lower
        and upper bounds of all loops in an Invoke are initialised.

        :returns: Updated cursor value.

        '''
        assigns = self._invoke.schedule.walk(Assignment)
        assign_map: dict[Symbol, Assignment] = {}
        for assign in assigns:
            assign_map[assign.lhs.symbol] = assign

        # Loop bounds go just before the first kernel and after any existing
        # assignments (cheap way of ensuring dependencies satisfied).
        kernels = self._invoke.schedule.walk(Kern)
        if kernels:
            indices = kernels[0].path_from(self._invoke.schedule)
            cursor = indices[0]
        elif assigns:
            # Allow for the fact that an assignment might be within a loop.
            indices = assigns[-1].path_from(self._invoke.schedule)
            cursor = indices[0] + 1
        else:
            cursor = 0

        loops = self._invoke.schedule.walk(LFRicLoop)
        first = True
        for idx, loop in enumerate(loops):

            if loop.loop_type == "null":
                # Generic or 'null' loops don't need any variables to be set
                continue

            if (isinstance(loop.start_expr, Reference) and
                    loop.start_expr.symbol in assign_map):
                continue

            # Set the lower bound
            bound_psyir = loop.lower_bound_psyir()
            if isinstance(bound_psyir, Literal):
                loop.start_expr = bound_psyir
            else:
                root_name = f"loop{idx}_start"
                lbound = self.symtab.new_symbol(
                    root_name, symbol_type=DataSymbol,
                    datatype=LFRicTypes("LFRicIntegerScalarDataType")())
                assignment = Assignment.create(
                        lhs=Reference(lbound),
                        rhs=bound_psyir)
                loop.start_expr = Reference(lbound)
                self._invoke.schedule.addchild(assignment, cursor)

                cursor += 1
                if first:
                    assignment.preceding_comment = (
                        "Set-up all of the loop bounds")
                    first = False

            # Set the upper bound
            if loop.loop_type not in ("cells_in_colour", "tiles_in_colour",
                                      "cells_in_tile"):
                bnd_psyir = loop.upper_bound_psyir()
                if (isinstance(bnd_psyir, Reference) and
                        bnd_psyir.symbol.is_scalar):
                    loop.stop_expr = bnd_psyir
                else:
                    root_name = f"loop{idx}_stop"
                    ubound = self.symtab.new_symbol(
                        root_name, symbol_type=DataSymbol,
                        datatype=LFRicTypes("LFRicIntegerScalarDataType")())
                    self._invoke.schedule.addchild(
                        Assignment.create(
                            lhs=Reference(ubound),
                            rhs=loop.upper_bound_psyir()
                        ), cursor)
                    cursor += 1
                    loop.stop_expr = Reference(ubound)
            else:
                # If it needs a colour look-up, it has to be in-place
                loop.stop_expr = loop.upper_bound_psyir()

        return cursor


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for.
__all__ = ['LFRicLoopBounds']
