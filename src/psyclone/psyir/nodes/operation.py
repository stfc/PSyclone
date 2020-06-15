# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2020, Science and Technology Facilities Council.
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

import abc
from enum import Enum
import six
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.errors import GenerationError


@six.add_metaclass(abc.ABCMeta)
class Operation(DataNode):
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
    Operator = None
    # Textual description of the node.
    _text_name = "Operation"
    _colour_key = "Operation"

    def __init__(self, operator, parent=None):
        super(Operation, self).__init__(parent=parent)

        if not isinstance(operator, self.Operator):
            raise TypeError(
                "{0} operator argument must be of type "
                "{0}.Operator but found {1}.".format(type(self).__name__,
                                                     type(operator).__name__))
        self._operator = operator

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

    def __str__(self):
        result = self.node_str(False) + "\n"
        for entity in self._children:
            result += str(entity) + "\n"

        # Delete last line break
        if result[-1] == "\n":
            result = result[:-1]
        return result


class UnaryOperation(Operation):
    '''
    Node representing a UnaryOperation expression. As such it has one operand
    as child 0, and an attribute with the operator type.
    '''
    # Textual description of the node.
    _children_valid_format = "DataNode"
    _text_name = "UnaryOperation"

    Operator = Enum('Operator', [
        # Arithmetic Operators
        'MINUS', 'PLUS', 'SQRT', 'EXP', 'LOG', 'LOG10',
        # Logical Operators
        'NOT',
        # Trigonometric Operators
        'COS', 'SIN', 'TAN', 'ACOS', 'ASIN', 'ATAN',
        # Other Maths Operators
        'ABS', 'CEIL',
        # Casting Operators
        'REAL', 'INT'
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
    def create(oper, child):
        '''Create a UnaryOperation instance given oper and child instances.

        :param oper: the specified operator.
        :type oper: :py:class:`psyclone.psyir.nodes.UnaryOperation.Operator`
        :param child: the PSyIR node that oper operates on.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :returns: a UnaryOperation instance.
        :rtype: :py:class:`psyclone.psyir.nodes.UnaryOperation`

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        if not isinstance(oper, UnaryOperation.Operator):
            raise GenerationError(
                "oper argument in create method of UnaryOperation class "
                "should be a PSyIR UnaryOperation Operator but found '{0}'."
                "".format(type(oper).__name__))

        unary_op = UnaryOperation(oper)
        unary_op.children = [child]
        child.parent = unary_op
        return unary_op


class BinaryOperation(Operation):
    '''
    Node representing a BinaryOperation expression. As such it has two operands
    as children 0 and 1, and an attribute with the operator type.
    '''
    Operator = Enum('Operator', [
        # Arithmetic Operators. ('REM' is remainder AKA 'MOD' in Fortran.)
        'ADD', 'SUB', 'MUL', 'DIV', 'REM', 'POW', 'SUM',
        # Relational Operators
        'EQ', 'NE', 'GT', 'LT', 'GE', 'LE',
        # Logical Operators
        'AND', 'OR',
        # Other Maths Operators
        'SIGN', 'MIN', 'MAX',
        # Query Operators
        'SIZE', 'LBOUND', 'UBOUND',
        # Matrix and Vector Operators
        'MATMUL'
        ])
    '''Arithmetic operators:

    .. function:: POW(arg0, arg1) -> type(arg0)

       :returns: `arg0` raised to the power of `arg1`.

    Query operators:

    .. function:: SIZE(array, index) -> int

       :returns: the size of the `index` dimension of `array`.

    .. function:: LBOUND(array, index) -> int

       :returns: the value of the lower bound of the `index` dimension of \
                 `array`.

    .. function:: UBOUND(array, index) -> int

       :returns: the value of the upper bound of the `index` dimension of \
                 `array`.

    Matrix and Vector Operators:

    .. function:: MATMUL(array1, array2) -> array

       :returns: the result of performing a matrix multiply with a \
                 matrix (`array1`) and a matrix or a vector
                 (`array2`).

    .. note:: `array1` must be a 2D array. `array2` may be a 2D array
        or a 1D array (vector). The size of the second dimension of
        `array1` must be the same as the first dimension of
        `array1`. If `array2` is 2D then the resultant array will be
        2D with the size of its first dimension being the same as the
        first dimension of `array1` and the size of its second
        dimension being the same as second dimension of `array2`. If
        `array2` is a vector then the resultant array is a vector with
        the its size being the size of the first dimension of
        `array1`.

    .. note:: The type of data in `array1` and `array2` must be the
        same and the resultant data will also have the same
        type. Currently only REAL data is supported.

    '''
    # Textual description of the node.
    _children_valid_format = "DataNode, DataNode"
    _text_name = "BinaryOperation"

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

    def math_equal(self, other):
        ''':param other: the node to compare self with.
        :type other: py:class:`psyclone.psyir.nodes.Node`
        :returns: True if the self has the same results as other.
        :rtype: bool
        '''
        if not super(BinaryOperation, self).math_equal(other):
            # Support some commutative law, unfortunately we now need
            # to repeat some tests already done in super(), since we
            # don't know why the above test failed
            # TODO #533 for documenting restrictions
            # pylint: disable=unidiomatic-typecheck
            if type(self) != type(other):
                return False
            if self.operator != other.operator:
                return False
            if self.operator not in [self.Operator.ADD, self.Operator.MUL,
                                     self.Operator.AND, self.Operator.OR,
                                     self.Operator.EQ]:
                return False
            return self._children[0].math_equal(other.children[1]) and \
                self._children[1].math_equal(other.children[0])
        return self.operator == other.operator

    @staticmethod
    def create(oper, lhs, rhs):
        '''Create a BinaryOperator instance given an operator and lhs and rhs
        child instances.

        :param operator: the operator used in the operation.
        :type operator: \
            :py:class:`psyclone.psyir.nodes.BinaryOperation.Operator`
        :param lhs: the PSyIR node containing the left hand side of \
            the assignment.
        :type lhs: :py:class:`psyclone.psyir.nodes.Node`
        :param rhs: the PSyIR node containing the right hand side of \
            the assignment.
        :type rhs: :py:class:`psyclone.psyir.nodes.Node`

        :returns: a BinaryOperator instance.
        :rtype: :py:class:`psyclone.psyir.nodes.BinaryOperation`

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        if not isinstance(oper, BinaryOperation.Operator):
            raise GenerationError(
                "oper argument in create method of BinaryOperation class "
                "should be a PSyIR BinaryOperation Operator but found '{0}'."
                "".format(type(oper).__name__))

        binary_op = BinaryOperation(oper)
        binary_op.children = [lhs, rhs]
        lhs.parent = binary_op
        rhs.parent = binary_op
        return binary_op


class NaryOperation(Operation):
    '''
    Node representing a n-ary operation expression. The n operands are the
    stored as the 0 - n-1th children of this node and the type of the operator
    is held in an attribute.
    '''
    # Textual description of the node.
    _children_valid_format = "[DataNode]+"
    _text_name = "NaryOperation"

    Operator = Enum('Operator', [
        # Arithmetic Operators
        'MAX', 'MIN', 'SUM'
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
        # pylint: disable=unused-argument
        return isinstance(child, DataNode)

    @staticmethod
    def create(oper, children):
        '''Create an NaryOperator instance given an operator and a list of
        Node instances.

        :param operator: the operator used in the operation.
        :type operator: :py:class:`psyclone.psyir.nodes.NaryOperation.Operator`
        :param children: a list of PSyIR nodes that the operator \
            operates on.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`

        :returns: an NaryOperator instance.
        :rtype: :py:class:`psyclone.psyir.nodes.NaryOperation`

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        if not isinstance(oper, NaryOperation.Operator):
            raise GenerationError(
                "oper argument in create method of NaryOperation class "
                "should be a PSyIR NaryOperation Operator but found '{0}'."
                "".format(type(oper).__name__))
        if not isinstance(children, list):
            raise GenerationError(
                "children argument in create method of NaryOperation class "
                "should be a list but found '{0}'."
                "".format(type(children).__name__))

        nary_op = NaryOperation(oper)
        nary_op.children = children
        for child in children:
            child.parent = nary_op
        return nary_op
