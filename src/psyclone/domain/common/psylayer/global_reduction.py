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
from typing import Any

from psyclone.configuration import Config
from psyclone.core import AccessType
from psyclone.errors import GenerationError, InternalError
from psyclone.psyGen import KernelArgument
from psyclone.psyir.nodes import Statement


# TODO make this virtual
class GlobalReduction(Statement):
    '''
    Generic global reduction operation.

    :param operand: the operand of the reduction operation.

    '''
    # Textual description of the node.
    _children_valid_format = "<LeafNode>"
    _text_name = "GlobalReduction"
    _colour = "cyan"

    def __init__(self,
                 operand: Any,
                 **kwargs):
        # Check that distributed memory is enabled
        if not Config.get().distributed_memory:
            raise GenerationError(
                "It makes no sense to create a GlobalReduction object "
                "when distributed memory is not enabled (dm=False).")

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
            # Check that the global reduction argument is indeed a scalar
            if not operand.is_scalar:
                raise InternalError(
                    f"GlobalReduction.init(): A global reduction argument "
                    f"should be a scalar but found argument of type "
                    f"'{operand.argument_type}'.")
            # Check scalar intrinsic types that this class supports (only
            # "real" for now)
            if operand.intrinsic_type != "real":
                raise GenerationError(
                    f"GlobalReduction currently only supports real scalars, "
                    f"but argument '{operand.name}' in Kernel "
                    f"'{operand.call.name}' has '{operand.intrinsic_type}' "
                    f"intrinsic type.")
        super().__init__(kwargs)

    def initialise_reduction_variable(self, parent):
        '''
        '''

    @property
    def operand(self) -> Any:
        '''
        :returns: the operand of this global reduction.
        '''
        return self._operand

    @property
    def dag_name(self) -> str:
        '''
        :returns: the name to use in the DAG for this node.
        '''
        return (f"globalreduction({self._operand.name})"
                f"_{self.position}")

    @property
    def args(self) -> list[Any]:
        '''
        :returns: the arguments associated with this node. Override
                  the base method and simply return the operand.
        '''
        return [self._operand]

    def node_str(self, colour: bool = True) -> str:
        '''
        Returns a text description of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.

        '''
        # TODO move this to sub-classes (e.g. GlobalSum)
        return (f"{self.coloured_name(colour)}["
                f"operand='{self._operand.name}']")
