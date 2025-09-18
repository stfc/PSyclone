# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Authors: A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the implementation of the abstract AtomicMixin. '''

from abc import ABCMeta
from enum import Enum

from psyclone.psyir.nodes.assignment import Assignment
from psyclone.psyir.nodes.intrinsic_call import IntrinsicCall
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.nodes.operation import BinaryOperation
from psyclone.psyir.symbols.datatypes import ScalarType


class AtomicDirectiveType(Enum):
    '''Enumeration of the available atomic operation types supported by
    OpenMP and OpenACC.'''
    UPDATE = 1
    READ = 2
    WRITE = 3
    CAPTURE = 4


class AtomicDirectiveMixin(metaclass=ABCMeta):
    '''A mixin class for directives implementing atomic behaviour.'''

    # Tuple of operators valid for atomic behaviour.
    _VALID_OPERATORS = (BinaryOperation.Operator.ADD,
                        BinaryOperation.Operator.SUB,
                        BinaryOperation.Operator.MUL,
                        BinaryOperation.Operator.DIV,
                        BinaryOperation.Operator.AND,
                        BinaryOperation.Operator.OR,
                        BinaryOperation.Operator.EQV,
                        BinaryOperation.Operator.NEQV)

    # Tuple of intrinsics valid for atomic behaviour
    _VALID_INTRINSICS = (IntrinsicCall.Intrinsic.MAX,
                         IntrinsicCall.Intrinsic.MIN,
                         IntrinsicCall.Intrinsic.IAND,
                         IntrinsicCall.Intrinsic.IOR,
                         IntrinsicCall.Intrinsic.IEOR)

    @classmethod
    def is_valid_atomic_statement(cls, stmt: Node) -> bool:
        ''' Check if a given statement is a valid atomic expression.

        :param stmt: a node to be validated.

        :returns: whether a given statement is compliant with the classes
                  atomic expressions.

        '''
        # TODO #2398 If the statement is true can we return the
        # correct AtomicDirectiveType?
        # TODO #2398 If we don't want to return the correct
        # AtomicDirectiveType (i.e. to use this for validation only) then
        # we should check the statement is valid given the provided (optional)
        # AtomicDirectiveType.

        if not isinstance(stmt, Assignment):
            return False

        # Not all rules are checked, just that:
        # - operands are of a scalar intrinsic type
        if not isinstance(stmt.lhs.datatype, ScalarType):
            return False

        # - the top-level operator is one of: +, *, -, /, AND, OR, EQV, NEQV
        if isinstance(stmt.rhs, BinaryOperation):
            if stmt.rhs.operator not in cls._VALID_OPERATORS:
                return False
        # - or intrinsics: MAX, MIN, IAND, IOR, or IEOR
        if isinstance(stmt.rhs, IntrinsicCall):
            if stmt.rhs.intrinsic not in cls._VALID_INTRINSICS:
                return False

        return True


# For automatic API documentation generation
__all__ = ["AtomicDirectiveType", "AtomicDirectiveMixin"]
