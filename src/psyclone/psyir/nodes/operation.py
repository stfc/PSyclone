# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2023, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the implementation of the Operation class and its
sub-classes.'''

from abc import ABCMeta
from enum import Enum

from psyclone.errors import GenerationError
from psyclone.psyir.nodes.datanode import DataNode


class Operation(DataNode, metaclass=ABCMeta):
    '''
    Abstract base class for PSyIR nodes representing operators.

    :param operator: the operator used in the operation.
    :type operator: :py:class:`psyclone.psyir.nodes.UnaryOperation.Operator` \
        or :py:class:`psyclone.psyir.nodes.BinaryOperation.Operator` or \
        :py:class:`psyclone.psyir.nodes.NaryOperation.Operator`
    :param parent: the parent node of this Operation in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    :raises TypeError: if the supplied operator is not an instance of \
                       self.Operator.

    '''
    # Must be overridden in sub-class to hold an Enumeration of the Operators
    # that it can represent.
    Operator = object
    # Colour of the node in a view tree.
    _colour = "blue"

    def __init__(self, operator, parent=None):
        super().__init__(parent=parent)

        if not isinstance(operator, self.Operator):
            raise TypeError(
                f"{type(self).__name__} operator argument must be of type "
                f"{type(self).__name__}.Operator but found "
                f"{type(operator).__name__}.")
        self._operator = operator

    def __eq__(self, other):
        '''Checks whether two Operations are equal. Operations are equal
        if they are the same type, have the same operator and if the inherited
        equality is True.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.operator == other.operator

        return is_eq

    @property
    def operator(self):
        '''
        Return the operator.

        :returns: Enumerated type capturing the operator.
        :rtype: :py:class:`psyclone.psyir.nodes.UnaryOperation.Operator` or \
                :py:class:`psyclone.psyir.nodes.BinaryOperation.Operator` or \
                :py:class:`psyclone.psyir.nodes.NaryOperation.Operator`

        '''
        return self._operator

    def __str__(self):
        result = f"{self.node_str(False)}\n"
        for entity in self._children:
            result += f"{str(entity)}\n"

        # Delete last line break
        if result[-1] == "\n":
            result = result[:-1]
        return result

    def node_str(self, colour=True):
        '''
        Construct a text representation of this node, optionally with control
        codes for coloured display in a suitable terminal.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this PSyIR node.
        :rtype: str
        '''
        return self.coloured_name(colour) + \
            "[operator:'" + self._operator.name + "']"


class UnaryOperation(Operation):
    '''
    Node representing a UnaryOperation expression. As such it has one operand
    as child 0, and an attribute with the operator type.
    '''
    # Textual description of the node.
    _children_valid_format = "DataNode"

    Operator = Enum('Operator', [
        # Arithmetic Operators
        'MINUS', 'PLUS',
        # Logical Operators
        'NOT',
        ])

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return position == 0 and isinstance(child, DataNode)

    @staticmethod
    def create(operator, operand):
        '''Create a UnaryOperation instance given an operator and operand.

        :param operator: the specified operator.
        :type operator: \
            :py:class:`psyclone.psyir.nodes.UnaryOperation.Operator`
        :param operand: the PSyIR node that oper operates on, or a tuple \
            containing the name of the argument and the PSyIR node.
        :type operand: Union[:py:class:`psyclone.psyir.nodes.Node` | \
            Tuple[str, :py:class:``psyclone.psyir.nodes.Node``]]

        :returns: a UnaryOperation instance.
        :rtype: :py:class:`psyclone.psyir.nodes.UnaryOperation`

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        if not isinstance(operator, Enum) or \
           operator not in UnaryOperation.Operator:
            raise GenerationError(
                f"operator argument in create method of UnaryOperation class "
                f"should be a PSyIR UnaryOperation Operator but found "
                f"'{type(operator).__name__}'.")

        unary_op = UnaryOperation(operator)
        unary_op.addchild(operand)
        return unary_op


class BinaryOperation(Operation):
    '''
    Node representing a BinaryOperation expression. As such it has two operands
    as children 0 and 1, and an attribute with the operator type.

    '''
    Operator = Enum('Operator', [
        # Arithmetic Operators. ('REM' is remainder AKA 'MOD' in Fortran.)
        'ADD', 'SUB', 'MUL', 'DIV', 'REM', 'POW',
        # Relational Operators
        'EQ', 'NE', 'GT', 'LT', 'GE', 'LE',
        # Logical Operators
        'AND', 'OR', 'EQV', 'NEQV',
        ])
    # Textual description of the node.
    _children_valid_format = "DataNode, DataNode"

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return position in (0, 1) and isinstance(child, DataNode)

    @staticmethod
    def create(operator, lhs, rhs):
        '''Create a BinaryOperator instance given an operator and lhs and rhs
        child instances with optional names.

        :param operator: the operator used in the operation.
        :type operator: \
            :py:class:`psyclone.psyir.nodes.BinaryOperation.Operator`
        :param lhs: the PSyIR node containing the left hand side of \
            the assignment, or a tuple containing the name of the \
            argument and the PSyIR node.
        :type lhs: Union[:py:class:`psyclone.psyir.nodes.Node`, \
            Tuple[str, :py:class:`psyclone.psyir.nodes.Node`]]
        :param rhs: the PSyIR node containing the right hand side of \
            the assignment, or a tuple containing the name of the \
            argument and the PSyIR node.
        :type rhs: Union[:py:class:`psyclone.psyir.nodes.Node`, \
            Tuple[str, :py:class:`psyclone.psyir.nodes.Node`]]

        :returns: a BinaryOperator instance.
        :rtype: :py:class:`psyclone.psyir.nodes.BinaryOperation`

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        if not isinstance(operator, Enum) or \
           operator not in BinaryOperation.Operator:
            raise GenerationError(
                f"operator argument in create method of BinaryOperation class "
                f"should be a PSyIR BinaryOperation Operator but found "
                f"'{type(operator).__name__}'.")

        binary_op = BinaryOperation(operator)
        binary_op.addchild(lhs)
        binary_op.addchild(rhs)
        return binary_op


# For automatic API documentation generation
__all__ = ["Operation", "UnaryOperation", "BinaryOperation"]
