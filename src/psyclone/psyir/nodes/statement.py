# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the Statement abstract node implementation.'''

from psyclone.psyir.nodes.node import Node
from psyclone.psyir.commentable_mixin import CommentableMixin


class Statement(Node, CommentableMixin):
    '''
    Abstract node representing a general PSyIR Statement.
    '''


# For automatic API documentation generation
__all__ = ["Statement"]
