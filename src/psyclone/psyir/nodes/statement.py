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
        '''
        return []


# For automatic API documentation generation
__all__ = ["Statement"]
