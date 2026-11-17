# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the Clause abstract node implementation. '''

import abc
from typing import Any

from psyclone.psyir.nodes.node import Node


class Clause(Node, metaclass=abc.ABCMeta):
    '''
    Base abstract class for all clauses.
    '''
    _children_valid_format = None
    _colour = "green"
    # The base string for this clause, e.g. nowait or private
    _clause_string = None

    @property
    def clause_string(self) -> str:
        '''
        :returns: the base clause string for this Clause.
        '''
        return self._clause_string


class OperatorClause(Clause, metaclass=abc.ABCMeta):
    '''
    Base abstract class for all clauses that have an operator.
    '''

    _operator = None

    @property
    def operator(self) -> Any:
        '''
        Returns the operand for this Clause.
        '''
        return self._operator
