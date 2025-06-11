# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the implementation of the abstract
    ArrayOfStructuresMixin. '''

from typing import Tuple
import abc

from psyclone.core import Signature
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.nodes.member import Member
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.nodes.ranges import Range
from psyclone.psyir.nodes.structure_accessor_mixin import (
    StructureAccessorMixin)
from psyclone.errors import InternalError


class ArrayOfStructuresMixin(ArrayMixin,  StructureAccessorMixin,
                             metaclass=abc.ABCMeta):
    '''
    Abstract class that combines the ArrayMixin and the StructureAccessorMixin.
    As such, it has a member (as child 0) and indices (starting from child 1)

    '''
    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: sub-class of :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        if position == 0:
            # The first child must be a Member
            return isinstance(child, Member)
        # All subsequent children must be array-index expressions
        return isinstance(child, (DataNode, Range))

    def index_of(self, node):
        '''
        If the given node is one of the index expressions of the array, it
        returns the zero-indexed dimension of the array that it belongs to.
        Note that this is different to `node.position` because
        ArraysOfStructures have a Member child, and it is different from
        `array.indices.index(node)` because that would use the equality
        operator, but sibling indices may be equal and provide unexpected
        results.

        :param node: the node to get the index of.
        :type node: :py:class:`psyclone.psyir.nodes.Node`

        :returns: the index of the given node in the array.
        :rtype: int

        :raises ValueError: if node is not an index of the array.

        '''
        if node.parent is self:
            return node.position - 1  # -1 to account for the member child
        raise ValueError(f"'{node}' is not a child of '{self}'")

    @property
    def indices(self) -> Tuple[Node]:
        '''
        Supports semantic-navigation by returning the list of nodes
        representing the index expressions for this array reference.

        :returns: the PSyIR nodes representing the array-index expressions.
        :rtype: list of :py:class:`psyclone.psyir.nodes.Node`

        :raises InternalError: if this node does not have at least two \
                               children.

        '''
        if len(self._children) < 2:
            raise InternalError(
                f"{type(self).__name__} malformed or incomplete: must "
                f"have one or more children representing array-index "
                f"expressions but found none.")
        for idx, child in enumerate(self._children[1:], start=1):
            if not self._validate_child(idx, child):
                raise InternalError(
                    f"{type(self).__name__} malformed or incomplete: child "
                    f"{idx} must represent an array-index expression but "
                    f"found '{type(child).__name__}' instead of "
                    f"psyir.nodes.DataNode or Range")
        return tuple(self._children[1:])

    def get_signature_and_indices(self):
        ''':returns: the Signature of this array of structure reference, \
            and a list of lists of the indices used for each component.
        :rtype: tuple(:py:class:`psyclone.core.Signature`, list of \
            lists of indices)

        '''
        sub_sig, indices = self.children[0].get_signature_and_indices()
        sig = Signature(self.name)
        return (Signature(sig, sub_sig), [list(self.indices)]+indices)


# For AutoAPI documentation generation
__all__ = ['ArrayOfStructuresMixin']
