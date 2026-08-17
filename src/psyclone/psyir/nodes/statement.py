# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the Statement abstract node implementation.'''

import abc

from psyclone.psyir.nodes.node import Node
from psyclone.psyir.commentable_mixin import CommentableMixin


class Statement(Node, CommentableMixin, metaclass=abc.ABCMeta):
    '''
    Abstract node representing a general PSyIR Statement.
    '''

    @abc.abstractmethod
    def next_accesses(self) -> list[Node]:
        '''
        Abstract method for finding the next_accesses of a statement.
        Subclasses should override this according to their own structure to
        return future accesses to any References contained in the statement.

        :returns: an empty list.
        '''
        return []

    @abc.abstractmethod
    def previous_accesses(self) -> list[Node]:
        '''
        Abstract method for finding the previous_accesses of a statement.
        Subclasses should override this according to their own structure to
        return previous accesses to any References contained in the statement.

        :returns: an empty list.
        '''
        return []

    def _merge_accesses(
        self, current_accesses: list[Node], new_accesses: list[Node]
    ) -> None:
        '''
        Helper function to merge access lists together for Statement
        subclass next/previous_accesses functions.
        Take all the accesses from new_accesses and adds them to the
        current_accesses list if they're not already present and are
        not contained in this nodes subtree.

        :param current_accesses: The list of currently computed dependent
            accesses for this node.
        :param new_accesses: The list of accesses to merge into
            current_accesses.
        '''
        for access in new_accesses:
            in_subtree = access.is_descendant_of(self)
            # If the access is not in the subtree of this node, and
            # is not already present in the current_accesses array
            # then add it to the array
            if not in_subtree and all(
                    [acc is not access for acc in current_accesses]):
                current_accesses.append(access)


# For automatic API documentation generation
__all__ = ["Statement"]
