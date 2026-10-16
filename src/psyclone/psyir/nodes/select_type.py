# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the SelectType node implementation.'''

from psyclone.core import VariablesAccessMap
from psyclone.errors import InternalError, GenerationError
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.schedule import Schedule
from psyclone.psyir.nodes.statement import Statement


class SelectType(Statement):
    '''
    Node representing an SelectType block within the PSyIR. It has two mandatory
    children: the first one represents the if-condition and the second one
    the if-body; and an optional third child representing the else-body.

    '''
