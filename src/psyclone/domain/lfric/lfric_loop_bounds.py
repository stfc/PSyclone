# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module provides the LFRicLoopBounds Class that handles all variables
    required for specifying loop limits within an LFRic PSy-layer routine.'''

from psyclone.domain.lfric import LFRicCollection, LFRicLoop, LFRicTypes
from psyclone.psyir.nodes import Assignment, Reference
from psyclone.psyir.symbols import DataSymbol


class LFRicLoopBounds(LFRicCollection):
    '''
    Handles all variables required for specifying loop limits within
    an LFRic PSy-layer routine.
    '''

    def initialise(self, cursor: int) -> int:
        '''
        Updates the PSyIR so that all of the variables holding the lower
        and upper bounds of all loops in an Invoke are initialised.

        :param cursor: position where to add the next initialisation
            statements.
        :returns: Updated cursor value.

        '''
        loops = filter(lambda x: isinstance(x, LFRicLoop),
                       self._invoke.schedule.loops())

        first = True
        for idx, loop in enumerate(loops):

            if loop.loop_type == "null":
                # Generic or 'null' loops don't need any variables to be set
                continue

            # Set the lower bound
            root_name = f"loop{idx}_start"
            lbound = self.symtab.new_symbol(
                root_name, symbol_type=DataSymbol,
                datatype=LFRicTypes("LFRicIntegerScalarDataType")())
            assignment = Assignment.create(
                    lhs=Reference(lbound),
                    rhs=loop.lower_bound_psyir())
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
                # If it needs a color look-up, it has to be in-place
                loop.stop_expr = loop.upper_bound_psyir()

        return cursor


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for.
__all__ = ['LFRicLoopBounds']
