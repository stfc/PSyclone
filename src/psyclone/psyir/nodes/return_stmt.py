# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the Return node implementation.'''

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
