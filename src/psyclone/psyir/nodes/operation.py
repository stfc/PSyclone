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

''' This module contains the operation node implementation'''

import abc
from enum import Enum
import six
from psyclone.psyir.nodes import Node

@six.add_metaclass(abc.ABCMeta)
class Operation(Node):
    '''
    Abstract base class for PSyIR nodes representing operators.

    :param operator: the operator used in the operation.
    :type operator: :py:class:`psyclone.psyGen.UnaryOperation.Operator` or \
                    :py:class:`psyclone.psyGen.BinaryOperation.Operator` or \
                    :py:class:`psyclone.psyGen.NaryOperation.Operator`
    :param parent: the parent node of this Operation in the PSyIR.
    :type parent: :py:class:`psyclone.psyGen.Node`

    :raises TypeError: if the supplied operator is not an instance of \
                       self.Operator.

    '''
    # Must be overridden in sub-class to hold an Enumeration of the Operators
    # that it can represent.
    Operator = None

    def __init__(self, operator, parent=None):
        super(Operation, self).__init__(parent=parent)

        if not isinstance(operator, self.Operator):
            raise TypeError(
                "{0} operator argument must be of type "
                "{0}.Operator but found {1}.".format(type(self).__name__,
                                                     type(operator).__name__))
        self._operator = operator
        self._text_name = "Operation"
        self._colour_key = "Operation"

    @property
    def operator(self):
        '''
        Return the operator.

        :returns: Enumerated type capturing the operator.
        :rtype: :py:class:`psyclone.psyGen.UnaryOperation.Operator` or \
                :py:class:`psyclone.psyGen.BinaryOperation.Operator` or \
                :py:class:`psyclone.psyGen.NaryOperation.Operator`

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

    :param operator: Enumerated type capturing the unary operator.
    :type operator: :py:class:`psyclone.psyGen.UnaryOperation.Operator`
    :param parent: the parent node of this UnaryOperation in the PSyIR.
    :type parent: :py:class:`psyclone.psyGen.Node`

    '''
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

    def __init__(self, operation, parent=None):
        super(UnaryOperation, self).__init__(operation, parent)
        self._text_name = "UnaryOperation"

    @staticmethod
    def create(oper, child):
        '''Create a UnaryOperation instance given oper and child instances.

        :param oper: the specified operator.
        :type oper: :py:class:`psyclone.psyGen.UnaryOperation.Operator`
        :param child: the PSyIR node that oper operates on.
        :type child: :py:class:`psyclone.psyGen.Node`

        :returns: a UnaryOperation instance.
        :rtype: :py:class:`psyclone.psyGen.UnaryOperation`

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        from psyclone.psyGen import GenerationError
        if not isinstance(oper, UnaryOperation.Operator):
            raise GenerationError(
                "oper argument in create method of UnaryOperation class "
                "should be a PSyIR UnaryOperation Operator but found '{0}'."
                "".format(type(oper).__name__))
        if not isinstance(child, Node):
            raise GenerationError(
                "child argument in create method of UnaryOperation class "
                "should be a PSyIR Node but found '{0}'."
                "".format(type(child).__name__))

        unary_op = UnaryOperation(oper)
        child.parent = unary_op
        unary_op.children = [child]
        return unary_op


class BinaryOperation(Operation):
    '''
    Node representing a BinaryOperation expression. As such it has two operands
    as children 0 and 1, and an attribute with the operator type.

    :param operator: the operator used in the operation.
    :type operator: :py:class:`psyclone.psyGen.BinaryOperation.Operator`
    :param parent: the parent node of this Operation in the PSyIR.
    :type parent: :py:class:`psyclone.psyGen.Node`

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
        'SIZE'
        ])

    def __init__(self, operator, parent=None):
        super(BinaryOperation, self).__init__(operator, parent)
        self._text_name = "BinaryOperation"

    def math_equal(self, other):
        ''':param other: the node to compare self with.
        :type other: py:class:`psyclone.psyGen.Node`
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
        :type operator: :py:class:`psyclone.psyGen.BinaryOperation.Operator`
        :param lhs: the PSyIR node containing the left hand side of \
            the assignment.
        :type lhs: :py:class:`psyclone.psyGen.Node`
        :param rhs: the PSyIR node containing the right hand side of \
            the assignment.
        :type rhs: :py:class:`psyclone.psyGen.Node`

        :returns: a BinaryOperator instance.
        :rtype: :py:class:`psyclone.psyGen.BinaryOperator`

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        from psyclone.psyGen import GenerationError
        if not isinstance(oper, BinaryOperation.Operator):
            raise GenerationError(
                "oper argument in create method of BinaryOperation class "
                "should be a PSyIR BinaryOperation Operator but found '{0}'."
                "".format(type(oper).__name__))
        for name, instance in [("lhs", lhs), ("rhs", rhs)]:
            if not isinstance(instance, Node):
                raise GenerationError(
                    "{0} argument in create method of BinaryOperation class "
                    "should be a PSyIR Node but found '{1}'."
                    "".format(name, type(instance).__name__))

        binary_op = BinaryOperation(oper)
        lhs.parent = binary_op
        rhs.parent = binary_op
        binary_op.children = [lhs, rhs]
        return binary_op


class NaryOperation(Operation):
    '''
    Node representing a n-ary operation expression. The n operands are the
    stored as the 0 - n-1th children of this node and the type of the operator
    is held in an attribute.


    :param operator: the operator used in the operation.
    :type operator: :py:class:`psyclone.psyGen.NaryOperation.Operator`
    :param parent: the parent node of this Operation in the PSyIR.
    :type parent: :py:class:`psyclone.psyGen.Node`

    '''
    Operator = Enum('Operator', [
        # Arithmetic Operators
        'MAX', 'MIN', 'SUM'
        ])

    def __init__(self, operator, parent=None):
        super(NaryOperation, self).__init__(operator, parent)
        self._text_name = "NaryOperation"

    @staticmethod
    def create(oper, children):
        '''Create an NaryOperator instance given an operator and a list of
        Node instances.

        :param operator: the operator used in the operation.
        :type operator: :py:class:`psyclone.psyGen.NaryOperation.Operator`
        :param children: a list of PSyIR nodes that the operator \
            operates on.
        :type children: list of :py:class:`psyclone.psyGen.Node`

        :returns: an NaryOperator instance.
        :rtype: :py:class:`psyclone.psyGen.NaryOperator`

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        from psyclone.psyGen import GenerationError
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
        for child in children:
            if not isinstance(child, Node):
                raise GenerationError(
                    "child of children argument in create method of "
                    "NaryOperation class should be a PSyIR Node but "
                    "found '{0}'.".format(type(child).__name__))

        nary_op = NaryOperation(oper)
        for child in children:
            child.parent = nary_op
        nary_op.children = children
        return nary_op


