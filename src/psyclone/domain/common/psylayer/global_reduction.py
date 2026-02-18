# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2026, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
# Modified by I. Kavcic and L. Turner, Met Office
# Modified by C.M. Maynard, Met Office / University of Reading
# Modified by J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module provides the GlobalReduction base class. '''

import copy

from psyclone.configuration import Config
from psyclone.core import AccessType
from psyclone.errors import GenerationError, InternalError
from psyclone.psyGen import KernelArgument
from psyclone.psyir.nodes import Statement
from psyclone.psyir.nodes.node import Node


class GlobalReduction(Statement):
    '''
    Represents a global-reduction in the PSyIR.

    :raises GenerationError: if distributed memory is not enabled.
    :raises InternalError: if the supplied argument doesn't represent a scalar.

    '''
    #: Textual description of the node.
    _children_valid_format = "<LeafNode>"
    #: Name of the node.
    _text_name = "GlobalReduction"
    #: The colour to use when creating a view of this node.
    _colour = "cyan"

    def __init__(self, scalar: KernelArgument, parent: Node = None):
        super().__init__(children=[], parent=parent)
        # Check that distributed memory is enabled
        if not Config.get().distributed_memory:
            raise GenerationError(
                f"It makes no sense to create a {self._text_name} object "
                f"when distributed memory is not enabled (dm=False).")

        # Check that the global sum argument is indeed a scalar
        if not scalar.is_scalar:
            raise InternalError(
                f"{self._text_name}.init(): A global reduction argument should"
                f" be a scalar but found argument of type "
                f"'{scalar.argument_type}'.")

        self._scalar = copy.copy(scalar)
        if scalar:
            # Update scalar values appropriately
            # Here "readwrite" denotes how the class GlobalSum
            # accesses/updates a scalar
            self._scalar.access = AccessType.READWRITE
            self._scalar.call = self

    def node_str(self, colour: bool = True) -> str:
        '''
        Returns a text description of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        '''
        return f"{self.coloured_name(colour)}[scalar='{self._scalar.name}']"

    @property
    def scalar(self) -> KernelArgument:
        ''':returns: the scalar field that this global reduction acts on '''
        return self._scalar

    @property
    def dag_name(self) -> str:
        '''
        :returns: the name to use in the DAG for this node.
        '''
        return f"{self._text_name}({self._scalar.name})_{self.position}"

    @property
    def args(self) -> list[KernelArgument]:
        ''':returns: the list of arguments associated with this node. Override
                     the base method and simply return our argument.'''
        return [self._scalar]
