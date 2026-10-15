
# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2024-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the implementation of the StructureAccessor Mixin. '''

import abc
from psyclone.psyir.nodes.member import Member
from psyclone.psyir.nodes.node import Node
from psyclone.errors import InternalError


class StructureAccessorMixin(metaclass=abc.ABCMeta):
    '''
    Abstract class used to add functionality common to Nodes that represent
    Structure accesses. These all have a "member" child at position 0.

    '''
    @property
    def member(self):
        '''
        :returns: the PSyIR child representing the accessor component.
        :rtype: :py:class:`psyclone.psyir.nodes.Member`

        :raises InternalError: if the first child of this node is not an
                               instance of Member.
        '''
        if not self.children or not isinstance(self.children[0], Member):
            raise InternalError(
                f"{type(self).__name__} malformed or incomplete. It must have "
                f"a first child that must be a (sub-class of) Member, but "
                f"found: {self.children}")
        return self.children[0]

    def component_indices(self) -> tuple[tuple[Node]]:
        '''
        :returns: a tuple of tuples of index expressions; one for every
            component in the accessor. For example, for a scalar it
            returns `(())`, for `a%b` it returns ((),()) - two components
            with 0 indices in each, and for `a(i)%b(j,k+1)` it
            returns `((i,),(j,k+1))`.
        '''
        return ((), *self.member.component_indices())
