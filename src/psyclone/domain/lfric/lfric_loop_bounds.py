# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, A. Coughtrie, L. Turner and O. Brunt, Met Office
# Modified J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk and N. Nobre, STFC Daresbury Lab

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
            loop.children[0] = Reference(lbound)
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
                loop.children[1] = Reference(ubound)
            else:
                # If it needs a color look-up, it has to be in-place
                loop.children[1] = loop.upper_bound_psyir()

        return cursor


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for.
__all__ = ['LFRicLoopBounds']
