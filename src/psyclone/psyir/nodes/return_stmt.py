# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the Return node implementation.'''

from psyclone.psyir.nodes.node import Node
from psyclone.psyir.nodes.statement import Statement


class Return(Statement):
    '''
    Node representing a Return statement (subroutine break without return
    value).

    '''
    # Textual description of the node.
    _children_valid_format = "<LeafNode>"
    _text_name = "Return"
    _colour = "yellow"

    def next_accesses(self) -> list[Node]:
        '''
        Abstract method for finding the next_accesses of a statement.
        Subclasses should override this according to their own structure to
        return future accesses to any References contained in the statement.

        :returns: an empty list.
        '''
        # FIXME Implement
        return []

    def previous_accesses(self) -> list[Node]:
        '''
        Abstract method for finding the previous_accesses of a statement.
        Subclasses should override this according to their own structure to
        return previous accesses to any References contained in the statement.
        '''
        # FIXME Implement
        return []
