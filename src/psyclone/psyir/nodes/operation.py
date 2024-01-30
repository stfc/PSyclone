# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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

from psyclone.errors import GenerationError, InternalError
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.symbols.datatypes import (
    ArrayType, BOOLEAN_TYPE, UnresolvedType, ScalarType,
    UnsupportedFortranType, UnsupportedType)


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

    # The numeric operators.
    _numeric_ops = (Operator.MINUS, Operator.PLUS)

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

    @property
    def datatype(self):
        '''
        :returns: the datatype of the result of this UnaryOperation.
        :rtype: :py:class:`psyclone.psyir.symbols.DataType`

        '''
        return self.children[0].datatype


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
    # The numeric operators.
    _numeric_ops = (Operator.ADD, Operator.SUB, Operator.MUL, Operator.DIV,
                    Operator.REM, Operator.POW)
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
        :type operator:
            :py:class:`psyclone.psyir.nodes.BinaryOperation.Operator`
        :param lhs: the PSyIR node containing the left hand side of
            the assignment, or a tuple containing the name of the
            argument and the PSyIR node.
        :type lhs: Union[:py:class:`psyclone.psyir.nodes.Node`,
            Tuple[str, :py:class:`psyclone.psyir.nodes.Node`]]
        :param rhs: the PSyIR node containing the right hand side of
            the assignment, or a tuple containing the name of the
            argument and the PSyIR node.
        :type rhs: Union[:py:class:`psyclone.psyir.nodes.Node`,
            Tuple[str, :py:class:`psyclone.psyir.nodes.Node`]]

        :returns: a BinaryOperator instance.
        :rtype: :py:class:`psyclone.psyir.nodes.BinaryOperation`

        :raises GenerationError: if the arguments to the create method
            are not of the expected type.

        '''
        if (not isinstance(operator, Enum) or
                operator not in BinaryOperation.Operator):
            raise GenerationError(
                f"operator argument in create method of BinaryOperation class "
                f"should be a PSyIR BinaryOperation Operator but found "
                f"'{type(operator).__name__}'.")
        binary_op = BinaryOperation(operator)
        binary_op.addchild(lhs)
        binary_op.addchild(rhs)
        return binary_op

    def _get_result_precision(self, precisions):
        '''
        Compares the two precisions to determine the precision of the result
        of the operation.

        If the two precisions are the same, then that value is returned.
        Otherwise, Section 7.1.9.3 of the Fortran2008 standard says that in
        this case, the precision of the result is the greater of the two.
        If the precision cannot be determined then
        `ScalarType.Precision.UNDEFINED` is returned.

        :param precisions: the precision of the two operands.
        :type precisions: list[int |
            :py:class:`psyclone.psyir.symbols.ScalarType.Precision |
            :py:class:`psyclone.psyir.nodes.Reference`]

        :returns: the precision of the result of the operation.
        :rtype: int | :py:class:`psyclone.psyir.symbols.ScalarType.Precision

        :raises InternalError: if an unsupported Precision value is encountered
            (this is to defend against any future extension of
            ScalarType.Precision).

        '''
        if precisions[0] == precisions[1]:
            return precisions[0]

        # Operands have different precisions.
        if all(isinstance(prec, int) for prec in precisions):
            # Both precisions are integer.
            return max(precisions)

        if all(isinstance(prec, ScalarType.Precision) for
               prec in precisions):
            # Both precisions are of ScalarType.Precision type.
            if ScalarType.Precision.UNDEFINED in precisions:
                return ScalarType.Precision.UNDEFINED
            if ScalarType.Precision.DOUBLE in precisions:
                return ScalarType.Precision.DOUBLE
            raise InternalError(
                f"Operation._get_result_precision: got unsupported Precision "
                f"value(s) '{precisions[0]}' and '{precisions[1]}' for "
                f"operands '{self.children[0].debug_string()}' and "
                f"'{self.children[1].debug_string()}'")

        # We can't reason about the precision of the result.
        return ScalarType.Precision.UNDEFINED

    def _get_result_scalar_type(self, argtypes):
        '''
        Examines the two operand types to determine the base type of the
        operation using the rules in Section 7.2 of the Fortran2008 standard.
        If the type cannot be determined then an instance of `UnresolvedType`
        is returned.

        :param argtypes: the types of the two operands.
        :type argtypes: list[:py:class:`psyclone.psyir.symbols.DataType`,
                             :py:class:`psyclone.psyir.symbols.DataType`]

        :returns: the base type of the result of the operation.
        :rtype: :py:class:`psyclone.psyir.symbols.DataType`

        :raises TypeError: if an unexpected intrinsic type is found for
                           either of the operands to a numeric operation.

        '''
        if self.operator not in self._numeric_ops:
            # Must be a relational or logical operator. Intrinsic type of
            # result will be boolean.
            return BOOLEAN_TYPE

        # We have a numerical operation.
        if any(isinstance(atype.intrinsic, UnresolvedType)
               for atype in argtypes):
            # datatype of a numerical operation on a UnresolvedType is a
            # UnresolvedType.
            return UnresolvedType()

        base_type = None

        # If either of the operands has REAL intrinsic type then the result
        # must also be REAL.
        if argtypes[0].intrinsic == argtypes[1].intrinsic:
            # Operands are of the same intrinsic type.
            precision = self._get_result_precision([argtypes[0].precision,
                                                    argtypes[1].precision])
            base_type = ScalarType(argtypes[0].intrinsic, precision)
        elif argtypes[0].intrinsic == ScalarType.Intrinsic.REAL:
            base_type = argtypes[0]
        elif argtypes[1].intrinsic == ScalarType.Intrinsic.REAL:
            base_type = argtypes[1]

        # Check that the type of the result is consistent with a numerical
        # operation.
        if not base_type or base_type.intrinsic not in (
                ScalarType.Intrinsic.INTEGER,
                ScalarType.Intrinsic.REAL):
            for atype in argtypes:
                if atype.intrinsic not in (ScalarType.Intrinsic.INTEGER,
                                           ScalarType.Intrinsic.REAL):
                    raise TypeError(
                        f"Invalid argument of type '{atype.intrinsic}' to "
                        f"numerical operation '{self.operator}' in "
                        f"'{self.debug_string()}'. Currently only "
                        f"ScalarType.Intrinsic.REAL/INTEGER are "
                        f"supported (TODO #1590)")
        return base_type

    @property
    def datatype(self):
        '''
        Determines the datatype of this operation. If it cannot be determined
        for any reason then an instance of UnresolvedType is returned.

        :returns: the datatype of the result of this BinaryOperation.
        :rtype: :py:class:`psyclone.psyir.symbols.DataType`

        :raises InternalError: if the operands are both arrays but are of
                               different shapes.
        '''
        # Get the types of the operands.
        argtypes = []
        for child in self.children:
            # If the operand is itself an operation this will recurse.
            dtype = child.datatype
            if isinstance(dtype, UnsupportedFortranType):
                if dtype.partial_datatype:
                    # We are still OK provided we have partial type information
                    # since that means the intrinsic type can be handled in the
                    # PSyIR.
                    dtype = dtype.partial_datatype
                else:
                    return UnresolvedType()
            if isinstance(dtype, UnresolvedType):
                # If either operand is of UnresolvedType then we can't do
                # any better.
                return UnresolvedType()
            if isinstance(dtype, ArrayType):
                # We know this is an array but do we know its intrinsic type?
                if isinstance(dtype.intrinsic, UnresolvedType):
                    dtype = ArrayType(UnresolvedType(), shape=dtype.shape)
                if isinstance(dtype.intrinsic, UnsupportedType):
                    if (isinstance(dtype.intrinsic, UnsupportedFortranType) and
                            dtype.intrinsic.partial_datatype):
                        dtype = ArrayType(dtype.intrinsic.partial_datatype,
                                          shape=dtype.shape)
                    else:
                        dtype = ArrayType(UnresolvedType(), shape=dtype.shape)

            argtypes.append(dtype)

        # Determine the base (scalar) type of the result.
        base_type = self._get_result_scalar_type(argtypes)
        if (isinstance(base_type, UnresolvedType) or
                all(isinstance(atype, ScalarType) for atype in argtypes)):
            # Both operands are of scalar type.
            return base_type

        if all(isinstance(atype, ArrayType) for atype in argtypes):
            # Both operands are of array type.
            if len(argtypes[0].shape) != len(argtypes[1].shape):
                raise InternalError(
                    f"Binary operation '{self.debug_string()}' has operands "
                    f"of different shape: '{self.children[0].debug_string()}' "
                    f"has rank {len(argtypes[0].shape)} and "
                    f"'{self.children[1].debug_string()}' has rank "
                    f"{len(argtypes[1].shape)}")
            # In general there is no way we can check that the extents of each
            # dimension match so we have to assume that they do.
        shape = (argtypes[0].shape if isinstance(argtypes[0], ArrayType) else
                 argtypes[1].shape)
        return ArrayType(base_type, shape=shape)


# For automatic API documentation generation
__all__ = ["Operation", "UnaryOperation", "BinaryOperation"]
