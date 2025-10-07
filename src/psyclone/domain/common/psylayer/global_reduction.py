# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the GlobalReduction node implementation.'''

from __future__ import annotations
import copy
from enum import Enum
from typing import Any

from psyclone.core import AccessType, VariablesAccessMap
from psyclone.psyGen import KernelArgument
from psyclone.psyir.nodes import Statement


class ReductionOp(Enum):
    '''
    '''
    MIN = 1
    MAX = 2
    MINVAL = 3
    MAXVAL = 4
    SUM = 5


class GlobalReduction(Statement):
    '''
    Generic global reduction operation.

    :param reduction:

    :raises TypeError:

    '''
    # Textual description of the node.
    _children_valid_format = "<LeafNode>"
    _text_name = "GlobalReduction"
    _colour = "cyan"

    def __init__(self,
                 reduction: ReductionOp,
                 operand: Any,
                 **kwargs):
        if not isinstance(reduction, ReductionOp):
            raise TypeError("huh")
        self._operation = reduction
        # Ideally, 'operand' would be a child of this node but it's typically
        # a KernelArgument, not a PSyIR Node.
        # TODO Without this `copy`, the tests for the old-style DA fail.
        self._operand = copy.copy(operand)
        if isinstance(operand, KernelArgument):
            # Add old-style dependency information
            # Here "readwrite" denotes how the class GlobalSum
            # accesses/updates a scalar
            self._operand.access = AccessType.READWRITE
            self._operand.call = self
        super().__init__(kwargs)

    @property
    def operand(self):
        '''
        :returns: the operand of this global reduction.
        :rtype: Any
        '''
        return self._operand

    @property
    def dag_name(self):
        '''
        :returns: the name to use in the DAG for this node.
        :rtype: str
        '''
        return f"globalreduction({self._operand.name})_{self.position}"

    @property
    def args(self):
        ''' Return the list of arguments associated with this node. Override
        the base method and simply return our argument.'''
        return [self._operand]

    def node_str(self, colour: bool = True) -> str:
        '''
        Returns a text description of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.

        '''
        return (f"{self.coloured_name(colour)}[{self._operation.name}, "
                f"operand='{self._operand.name}']")

    def ARPDBG_reference_accesses(self):
        '''
        '''
        var_accesses = VariablesAccessMap()
        var_accesses.update(self._operand.reference_accesses())
        return var_accesses
