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
from psyclone.psyir.nodes import DataNode, Node
from psyclone.psyir.symbols import DataType, DataSymbol
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

    :param operator: Enumerated type capturing the unary operator.
    :type operator: :py:class:`psyclone.psyir.nodes.UnaryOperation.Operator`
    :param parent: the parent node of this UnaryOperation in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

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
    # Specify the datatypes that the intrinsics can operate on.
    valid_real = [
        Operator.MINUS, Operator.PLUS, Operator.SQRT, Operator.EXP,
        Operator.LOG, Operator.LOG10, Operator.COS, Operator.SIN,
        Operator.TAN, Operator.ACOS, Operator.ASIN, Operator.ATAN,
        Operator.ABS, Operator.CEIL, Operator.INT]
    valid_integer = [
        Operator.MINUS, Operator.PLUS, Operator.ABS, Operator.REAL]
    valid_logical = [Operator.NOT]

    def __init__(self, operator, parent=None):
        UnaryOperation._check_operator(operator)
        super(UnaryOperation, self).__init__(operator, parent)
        self._text_name = "UnaryOperation"

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
        # The oper argument is checked by UnaryOperation.__init__()
        unary_op = UnaryOperation(oper)
        
        # Check the child argument
        unary_op._check_child(child)
        
        child.parent = unary_op
        unary_op.children = [child]
        return unary_op

    @staticmethod
    def _check_operator(operator):
        '''Check that the supplied operation is valid.

        :raises GenerationError: if the oper argument is not a valid \
            UnaryOperation operator.

        '''
        if not isinstance(operator, UnaryOperation.Operator):
            raise GenerationError(
                "the operator in the UnaryOperation class should be a "
                "PSyIR UnaryOperation Operator, but found '{0}'."
                "".format(type(operator).__name__))

    def _check_child(self, child):
        '''Check that the supplied Node is valid.

        :raises GenerationError: if the child argument is an invalid \
            type of Node for this operator.

        '''
        # Check the node type is valid.
        if not isinstance(child, DataNode):
            raise GenerationError(
                "the child argument in the UnaryOperation class should be "
                "a PSyIR DataNode, but found '{0}'."
                "".format(type(child).__name__))

        # Only scalar values are supported in all unary operators.
        # if child.dimension != 0:
        if child.datasymbol.shape:
            raise GenerationError(
                "Operator '{0}' only supports scalars but argument is an "
                "array with {1} dimension(s)."
                "".format(self.operator.name, len(child.datasymbol.shape)))

        # Check that the datatype of this argument is valid for this
        # object's operator.
        child_datatype = child.datasymbol.datatype
        if child_datatype == DataType.REAL and \
           self.operator in UnaryOperation.valid_real:
            return None
        elif child_datatype == DataType.INTEGER and \
             self.operator in UnaryOperation.valid_integer:
            return None
        elif child_datatype == DataType.BOOLEAN and \
             self.operator in UnaryOperation.valid_logical:
            return None
        raise GenerationError(
            "Unsupported datatype '{0}' for operator '{1}'."
            "".format(str(child_datatype), self.operator.name))

    @property
    def datasymbol(self):
        '''
        :returns: the properties of the return value of this \
            UnaryOperator as a DataSymbol.
        :rtype: :py:class:`psyclone.psyir.symbols.DataSymbol`

        '''
        if self.operator == UnaryOperation.Operator.REAL:
            # This operator casts the data to REAL.
            datatype = DataType.REAL
        elif self.operator == UnaryOperation.Operator.INT:
            # This operator casts the data to INT.
            datatype = DataType.INTEGER
        elif self.operator == UnaryOperation.Operator.CEIL:
            # This operator takes a REAL and returns an INT.
            datatype = DataType.INTEGER
        else:
            # All other operators return data with the same datatype
            datatype = self.children[0].datasymbol.datatype
        # All unary operators return scalar values.
        return DataSymbol("none", datatype)


class BinaryOperation(Operation):
    '''
    Node representing a BinaryOperation expression. As such it has two operands
    as children 0 and 1, and an attribute with the operator type.

    :param operator: the operator used in the operation.
    :type operator: :py:class:`psyclone.psyir.nodes.BinaryOperation.Operator`
    :param parent: the parent node of this Operation in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

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
        type. Currently only `DataType.REAL` is supported.

    '''

    def __init__(self, operator, parent=None):
        BinaryOperation._check_operator(operator)
        super(BinaryOperation, self).__init__(operator, parent)
        self._text_name = "BinaryOperation"

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
        # The oper argument is checked by BinaryOperation.__init__()
        binary_op = BinaryOperation(oper)

        # Check children elements
        binary_op._check_children(lhs, rhs)

        lhs.parent = binary_op
        rhs.parent = binary_op
        binary_op.children = [lhs, rhs]
        return binary_op

    @staticmethod
    def _check_operator(operator):
        '''Check that the supplied operator is valid.

        :raises GenerationError: if the oper argument is not a valid \
            BinaryOperation operator.

        '''
        if not isinstance(operator, BinaryOperation.Operator):
            raise GenerationError(
                "the operator in the BinaryOperation class should be a "
                "PSyIR BinaryOperation Operator, but found '{0}'."
                "".format(type(operator).__name__))

    def _check_children(self, lhs, rhs):
        '''Check that the supplied Nodes are valid.

        :raises GenerationError: if either or both of the child \
            arguments are invalid types of Node for this operator.

        '''
        # Check the node types are valid.
        for name, instance in [("lhs", lhs), ("rhs", rhs)]:
            if not isinstance(instance, Node):
                raise GenerationError(
                    "{0} argument in create method of BinaryOperation class "
                    "should be a PSyIR Node but found '{1}'."
                    "".format(name, type(instance).__name__))

        # Check scalar/array operators
        if self.operator == BinaryOperation.Operator.MATMUL:
            # The first argument must be a 2D array and the second
            # argument must be a 1D or 2D array.
            lhs_dimension = 0
            if lhs.datasymbol.shape:
                lhs_dimension = len(lhs.datasymbol.shape)
            if lhs_dimension != 2:
                raise GenerationError(
                    "Operator 'MATMUL' requires the LHS argument to be a 2D "
                    "array, but found {0} dimension(s)."
                    "".format(lhs_dimension))
            rhs_dimension = 0
            if rhs.datasymbol.shape:
                rhs_dimension = len(rhs.datasymbol.shape)            
            if not rhs_dimension in [1, 2]:
                raise GenerationError(
                    "Operator 'MATMUL' requires the RHS argument to be a 1D "
                    "or 2D array, but found {0} dimension(s)."
                    "".format(rhs_dimension))
            # The size of the 2nd dimension of the first array must be
            # the same size as the first dimension of the second
            # array (see issue #692).
        else:
            # Both arguments should be scalars for all Operators
            # except MATMUL.
            for name, child in [("lhs", lhs), ("rhs", rhs)]:
                dimension = 0
                if child.datasymbol.shape:
                    dimension = len(child.datasymbol.shape)
                if dimension > 0:
                    raise GenerationError(
                        "Operator '{0}' only supports scalars but {1} "
                        "argument is an array with {2} dimension(s)."
                        "".format(self.operator.name, child.name,
                                  child.dimension))

        # Check datatypes
        if self.operator == BinaryOperation.Operator.MATMUL:
            # MATMUL only supports REAL data.
            if lhs.datasymbol.datatype != DataType.REAL or \
               rhs.datasymbol.datatype != DataType.REAL:
                raise GenerationError(
                    "Operator MATMUL expects both arguments to be of type "
                    "REAL but found [{0}, {1}]."
                    "".format(lhs.datasymbol.datatype, rhs.datasymbol.datatype))
        else:
            # Operators other than MATMUL support REAL or INTEGER data.
            if (lhs.datasymbol.datatype == DataType.REAL and
               rhs.datasymbol.datatype == DataType.REAL) or \
               (lhs.datasymbol.datatype == DataType.INTEGER and
               rhs.datasymbol.datatype == DataType.INTEGER):
                return None
            raise GenerationError(
                "Operator {0} expects both arguments to either be of type "
                "REAL or INTEGER but found '{0}' and '{1}'."
                "".format(lhs.datasymbol.datatype, rhs.datasymbol.datatype))

    @property
    def datasymbol(self):
        '''
        :returns: the properties of the return value of this \
            BinaryOperator as a DataSymbol.
        :rtype: :py:class:`psyclone.psyir.symbols.DataSymbol`

        '''
        # All operators except equivalence operators return the same
        # type as their first argument.
        datatype = self.children[0].datasymbol.datatype
        if self.operator in ['EQ', 'NE', 'GT', 'LT', 'GE', 'LE']:
            # Equivalence operators all return boolean data
            datatype = BinaryOperator.BOOLEAN

        if self.operator == BinaryOperation.Operator.MATMUL:
            if len(self.children[1].datasymbol.shape) == 1:
                # This is a matrix-vector multiply. The resultant
                # array will be 1D with size the same as the 1st
                # dimension of the 1st argument.
                # r(n) = a(n,l)*b(l)
                return DataSymbol("none", datatype,
                                  shape=[self.children[0].datasymbol.shape[0]])
            else:
                # This is a matrix-matrix multiply. The resultant
                # array will be 2D with the size of its 1st dimension
                # being the same as the 1st dimension of the 1st
                # argument. and the 2nd dimension being the same size
                # as the 2nd dimension of the 2nd argument.
                # r(n,m) = a(n,l)*b(l,m)
                return DataSymbol("none", datatype,
                                  shape=[self.children[0].datasymbol.shape[0],
                                         self.children[1].datasymbol.shape[1]])
        else:
            # All other binary operators return a scalar.
            return DataSymbol("none", datatype)


class NaryOperation(Operation):
    '''
    Node representing a n-ary operation expression. The n operands are the
    stored as the 0 - n-1th children of this node and the type of the operator
    is held in an attribute.


    :param operator: the operator used in the operation.
    :type operator: :py:class:`psyclone.psyir.nodes.NaryOperation.Operator`
    :param parent: the parent node of this Operation in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    '''
    Operator = Enum('Operator', [
        # Arithmetic Operators
        'MAX', 'MIN', 'SUM'
        ])

    def __init__(self, operator, parent=None):
        NaryOperation._check_operator(operator)
        super(NaryOperation, self).__init__(operator, parent)
        self._text_name = "NaryOperation"

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

        # The oper argument is checked by NaryOperation.__init__()
        nary_op = NaryOperation(oper)

        # Check the children argument
        nary_op._check_children(children)

        for child in children:
            child.parent = nary_op
        nary_op.children = children
        return nary_op

    @staticmethod
    def _check_operator(operator):
        '''Check that the supplied operation is valid.

        :raises GenerationError: if the oper argument is not a valid \
            NaryOperation operator.

        '''
        if not isinstance(operator, NaryOperation.Operator):
            raise GenerationError(
                "the operator in the NaryOperation class should be a "
                "PSyIR NaryOperation Operator, but found '{0}'."
                "".format(type(operator).__name__))

    def _check_children(self, children):
        '''Check that the supplied Nodes are valid.

        :raises GenerationError: if any of the child arguments are \
            invalid types of Node for this operator.

        '''
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
        print("**** ADD TESTS TO NaryOperation class in operation.py ***")
        exit(1)

    @property
    def datasymbol(self):
        '''
        :returns: the properties of the return value of this \
            NaryOperator as a DataSymbol.
        :rtype: :py:class:`psyclone.psyir.symbols.DataSymbol`

        '''
        # All nary operators return scalar values with datatypes the
        # same as their arguments.
        return DataSymbol("none", self.children[0].datasymbol.datatype)
