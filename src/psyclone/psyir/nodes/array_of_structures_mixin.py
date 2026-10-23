# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
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

    def component_indices(self) -> tuple[tuple[Node]]:
        '''
        :returns: a tuple of tuples of index expressions; one for every
            component in the accessor. For example, for a scalar it
            returns `(())`, for `a%b` it returns ((),()) - two components
            with 0 indices in each, and for `a(i)%b(j,k+1)` it
            returns `((i,),(j,k+1))`.
        '''
        return (self.indices, *self.member.component_indices())

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
