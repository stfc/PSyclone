# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the implementation of the ArrayOfStructuresMember
    node.'''

from psyclone.psyir.nodes.structure_member import StructureMember
from psyclone.psyir.nodes.array_of_structures_mixin import \
    ArrayOfStructuresMixin


class ArrayOfStructuresMember(ArrayOfStructuresMixin, StructureMember):
    '''
    Node representing a membership expression of a parent structure where the
    expression resolves to the component of one or more elements of an array
    of structures.
    As such, its first child must be a member of that structure. Subsequent
    children give the array-index expressions.

    '''
    # Textual description of the node. The first child must be a Member
    # describing an access to a member of this structure. Subsequent children
    # give the array-index expressions.
    _children_valid_format = "Member, [DataNode | Range]+"
    _text_name = "ArrayOfStructuresMember"

    # pylint: disable=arguments-differ
    @staticmethod
    def create(member_name, indices, inner_member):
        '''
        Create an access to a member of one or more elements of an array of
        structures that is itself a member of a structure.

        e.g. if we had the Fortran `grid%subdomains(1)%xstart` then
        `subdomains` must be an array of structure (derived) type. We would
        construct an ArrayOfStructuresMember for this access by calling:

        >>> from psyclone.psyir.nodes import (
        ...     ArrayOfStructuresMember, Member, Literal)
        >>> from psyclone.psyir.symbols import ScalarType
        >>> aosmem = ArrayOfStructuresMember.create(
        ...     "subdomains", [Literal("1", ScalarType.integer_type())],
        ...     Member("xstart"))

        :param str member_name: the name of the array member of the structure \
            that is being accessed.
        :param indices: the array-index expressions.
        :type indices: list of :py:class:`psyclone.psyir.nodes.DataNode`
        :param inner_member: the member of the `member_name` structure that \
            is being accessed.
        :type inner_member: :py:class:`psyclone.psyir.nodes.Member`

        :returns: a new ArrayOfStructuresMember object.
        :rtype: :py:class:`psyclone.psyir.nodes.ArrayOfStructuresMember`

        '''
        obj = ArrayOfStructuresMember(member_name)
        # Add the inner_member node as the first child
        obj.addchild(inner_member)
        # Add the array-index expressions as subsequent children
        for child in indices:
            obj.addchild(child)
        return obj


# For AutoAPI automatic documentation generation
__all__ = ['ArrayOfStructuresMember']
