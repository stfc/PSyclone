# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the implementation of the ArrayMember node.'''

from psyclone.psyir.nodes.member import Member
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.errors import GenerationError


class ArrayMember(ArrayMixin, Member):
    '''
    Node representing an access to the element(s) of an array that is a
    member of a structure. Must have one or more children which give the
    array-index expressions for the array access.

    '''
    # Textual description of the node.
    _children_valid_format = "[DataNode | Range]+"
    _text_name = "ArrayMember"

    @staticmethod
    def create(member_name, indices):
        '''
        Construct an ArrayMember instance describing an array access to a
        member of a structure.

        e.g. for the Fortran `grid%subdomains(1,2)`, `subdomains` must be an
        array and we are accessing element (1,2) of it. We would therefore
        create the ArrayMember for this access by calling:

        >>> from psyclone.psyir.nodes import ArrayMember, Literal
        >>> from psyclone.psyir.symbols import ScalarType
        >>> amem = ArrayMember.create(
        ...     "subdomains",
        ...     [Literal("1", ScalarType.integer_type()),
        ...     Literal("2", ScalarType.integer_type())])

        :param str member_name: the name of the member of the structure that \
            is being accessed.
        :param indices: the array-index expressions.
        :type indices: list of :py:class:`psyclone.psyir.nodes.DataNode` or
            :py:class:`psyclone.psyir.nodes.Range`

        :raises GenerationError: if the supplied `indices` argument is not \
                                 a list.
        '''
        if not isinstance(indices, list):
            raise GenerationError(
                f"indices argument in create method of ArrayMember class "
                f"should be a list but found '{type(indices).__name__}'.")

        obj = ArrayMember(member_name)
        # Add any array-index expressions as children
        for child in indices:
            obj.addchild(child)
        return obj


# For AutoAPI documentation generation
__all__ = ['ArrayMember']
