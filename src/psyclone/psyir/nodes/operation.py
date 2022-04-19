# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2022, Science and Technology Facilities Council.
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
import re
import six

from psyclone.errors import GenerationError
from psyclone.psyir.nodes.datanode import DataNode


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
    Operator = object
    _non_elemental_ops = []
    # Textual description of the node.
    _text_name = "Operation"
    _colour = "blue"

    def __init__(self, operator, parent=None):
        super(Operation, self).__init__(parent=parent)

        if not isinstance(operator, self.Operator):
            raise TypeError(
                f"{type(self).__name__} operator argument must be of type "
                f"{type(self).__name__}.Operator but found "
                f"{type(operator).__name__}.")
        self._operator = operator
        self._argument_names = []

    def append_named_arg(self, name, arg):
        '''Append a named argument to this operation.

           :param name: the argument name.
           :type name: Optional[str]
           :param arg: the argument expression.
           :type arg: :py:class:`psyclone.psyir.nodes.DataNode`

           :raises ValueError: if the name argument is already used \
               for an existing argument.

        '''
        self._validate_name(name)
        if name is not None:
            for check_name in self.argument_names:
                if check_name and check_name.lower() == name.lower():
                    raise ValueError(
                        f"The value of the name argument ({name}) in "
                        f"'append_named_arg' in the 'Operator' node is "
                        f"already used for a named argument.")
        self._argument_names.append((id(arg), name))
        self.children.append(arg)

    def insert_named_arg(self, name, arg, index):
        '''Insert a named argument to the operation.

           :param name: the argument name.
           :type name: Optional[str]
           :param arg: the argument expression.
           :type arg: :py:class:`psyclone.psyir.nodes.DataNode`
           :param int index: where in the argument list to insert the \
               named argument.

           :raises ValueError: if the name argument is already used \
               for an existing argument.
           :raises TypeError: if the index argument is the wrong type.

        '''
        self._validate_name(name)
        if name is not None:
            for check_name in self.argument_names:
                if check_name and check_name.lower() == name.lower():
                    raise ValueError(
                        f"The value of the name argument ({name}) in "
                        f"'insert_named_arg' in the 'Operator' node is "
                        f"already used for a named argument.")
        if not isinstance(index, int):
            raise TypeError(
                f"The 'index' argument in 'insert_named_arg' in the "
                f"'Operator' node should be an int but found "
                f"{type(index).__name__}.")
        self._argument_names.insert(index, (id(arg), name))
        self.children.insert(index, arg)

    def replace_named_arg(self, existing_name, arg):
        '''Replace one named argument with another (for an Operation node).

           :param str existing_name: the argument name.
           :param arg: the argument expression.
           :type arg: :py:class:`psyclone.psyir.nodes.DataNode`

           :raises TypeError: if the name argument is the wrong type.
           :raises ValueError: if the name argument is already used \
               for an existing argument.
           :raises TypeError: if the index argument is the wrong type.

        '''
        if not isinstance(existing_name, str):
            raise TypeError(
                f"The 'name' argument in 'replace_named_arg' in the "
                f"'Operator' node should be a string or None, but found "
                f"{type(existing_name).__name__}.")
        index = 0
        # pylint: disable=undefined-loop-variable
        for named_arg in self._argument_names:
            if named_arg[1].lower() == existing_name:
                break
            index += 1
        else:
            raise ValueError(
                f"The value of the existing_name argument ({existing_name}) "
                f"in 'insert_named_arg' in the 'Operator' node is not found "
                f"in the existing arguments.")
        self.children[index] = arg
        self._argument_names[index] = (id(arg), named_arg[1])

    @staticmethod
    def _validate_name(name):
        '''Utility method that checks that the supplied name has a valid
        format.

        :param name: the name to check.
        :type name: Optional[str]

        :raises TypeError: if the name is not a string or None.
        :raises ValueError: if this is not a valid name.

        '''
        if name is None:
            return
        if not isinstance(name, str):
            raise TypeError(
                f"A name should be a string or None, but found "
                f"{type(name).__name__}.")
        if not re.match(r'^[a-zA-Z]\w*$', name):
            raise ValueError(
                f"Invalid name '{name}' found.")

    def __eq__(self, other):
        '''Checks whether two Operations are equal. Operations are equal
        if they are the same type, have the same operator and if the inherited
        equality is True.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super(Operation, self).__eq__(other)
        is_eq = is_eq and self.operator == other.operator
        is_eq = is_eq and self.argument_names == other.argument_names

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

    @property
    def argument_names(self):
        '''
        :returns: a list containing the names of named arguments. If the \
            entry is None then the argument is a positional argument.
        :rtype: List[Optional[str]]
        '''
        self._reconcile()
        return [entry[1] for entry in self._argument_names]

    def _reconcile(self):
        '''update the _argument_names values in case child arguments have been
        removed, or added.

        '''
        new_argument_names = []
        for child in self.children:
            for arg in self._argument_names:
                if id(child) == arg[0]:
                    new_argument_names.append(arg)
                    break
            else:
                new_argument_names.append((id(child), None))
        self._argument_names = new_argument_names

    def is_elemental(self):
        '''
        :returns: whether this operation is elemental (provided with an input \
            array it will apply the operation individually to each of the \
            array elements and return an array with the results).
        :rtype: bool
        '''
        return self.operator not in self._non_elemental_ops

    def __str__(self):
        result = f"{self.node_str(False)}\n"
        for idx, entity in enumerate(self._children):
            if self.argument_names[idx]:
                result += f"{self.argument_names[idx]}={str(entity)}\n"
            else:
                result += f"{str(entity)}\n"

        # Delete last line break
        if result[-1] == "\n":
            result = result[:-1]
        return result

    def copy(self):
        '''Return a copy of this node. This is a bespoke implementation for
        Operation nodes that ensures that any internal id's are
        consistent before and after copying.

        :returns: a copy of this node and its children.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # ensure _argument_names is consistent with actual arguments
        # before copying.
        # pylint: disable=protected-access
        self._reconcile()
        # copy
        new_copy = super(Operation, self).copy()
        # Fix invalid id's in _argument_names after copying.
        new_list = []
        for idx, child in enumerate(new_copy.children):
            my_tuple = (id(child), new_copy._argument_names[idx][1])
            new_list.append(my_tuple)
        new_copy._argument_names = new_list

        return new_copy


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
        'MINUS', 'PLUS', 'SQRT', 'EXP', 'LOG', 'LOG10', 'SUM',
        # Logical Operators
        'NOT',
        # Trigonometric Operators
        'COS', 'SIN', 'TAN', 'ACOS', 'ASIN', 'ATAN',
        # Other Maths Operators
        'ABS', 'CEIL',
        # Casting Operators
        'REAL', 'INT', 'NINT'
        ])

    _non_elemental_ops = [Operator.SUM]

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
        name = None
        if isinstance(operand, tuple):
            if not len(operand) == 2:
                raise GenerationError(
                    f"If the argument in the create method of "
                    f"UnaryOperation class is a tuple, it's length "
                    f"should be 2, but it is {len(operand)}.")
            if not isinstance(operand[0], str):
                raise GenerationError(
                    f"If the argument in the create method of "
                    f"UnaryOperation class is a tuple, its first "
                    f"argument should be a str, but found "
                    f"{type(operand[0]).__name__}.")
            name, operand = operand

        unary_op.append_named_arg(name, operand)
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
        # Casting operators
        'REAL', 'INT', 'CAST',
        # Array Query Operators
        'SIZE', 'LBOUND', 'UBOUND',
        # Matrix and Vector Operators
        'MATMUL', 'DOT_PRODUCT'
        ])
    _non_elemental_ops = [Operator.SUM, Operator.MATMUL, Operator.SIZE,
                          Operator.LBOUND, Operator.UBOUND,
                          Operator.DOT_PRODUCT]
    '''Arithmetic operators:

    .. function:: POW(arg0, arg1) -> type(arg0)

       :returns: `arg0` raised to the power of `arg1`.

    Array query operators:

    .. function:: SIZE(array, index) -> int

       :returns: the size of the `index` dimension of `array`.

    .. function:: LBOUND(array, index) -> int

       :returns: the value of the lower bound of the `index` dimension of \
                 `array`.

    .. function:: UBOUND(array, index) -> int

       :returns: the value of the upper bound of the `index` dimension of \
                 `array`.

    Casting Operators:

    .. function:: REAL(arg0, precision)

       :returns: `arg0` converted to a floating point number of the \
                 specified precision.

    .. function:: INT(arg0, precision)

       :returns: `arg0` converted to an integer number of the specified \
                  precision.

    .. function:: CAST(arg0, mold)

       :returns: `arg0` with the same bitwise representation but interpreted \
                 with the same type as the specified `mold` argument.

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

    .. function:: DOT_PRODUCT(vector1, vector2) -> scalar

       :returns: the result of performing a dot product on two equal \
           sized vectors.

    .. note:: The type of data in `vector1` and `vector2` must be the
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
        for name, arg in [("lhs", lhs), ("rhs", rhs)]:
            if isinstance(arg, tuple):
                if not len(arg) == 2:
                    raise GenerationError(
                        f"If the {name} argument in create method of "
                        f"BinaryOperation class is a tuple, it's length "
                        f"should be 2, but it is {len(arg)}.")
                if not isinstance(arg[0], str):
                    raise GenerationError(
                        f"If the {name} argument in create method of "
                        f"BinaryOperation class is a tuple, its first "
                        f"argument should be a str, but found "
                        f"{type(arg[0]).__name__}.")

        lhs_name = None
        if isinstance(lhs, tuple):
            lhs_name, lhs = lhs
        rhs_name = None
        if isinstance(rhs, tuple):
            rhs_name, rhs = rhs

        binary_op = BinaryOperation(operator)
        binary_op.append_named_arg(lhs_name, lhs)
        binary_op.append_named_arg(rhs_name, rhs)
        return binary_op


class NaryOperation(Operation):
    '''Node representing a n-ary operation expression. The n operands are
    the stored as the 0 - n-1th children of this node and the type of
    the operator is held in an attribute.

    '''
    # Textual description of the node.
    _children_valid_format = "[DataNode]+"
    _text_name = "NaryOperation"

    Operator = Enum('Operator', [
        # Arithmetic Operators
        'MAX', 'MIN', 'SUM'
        ])
    _non_elemental_ops = [Operator.SUM]

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
    def create(operator, operands):
        '''Create an NaryOperator instance given an operator and a list of
        Node (or name and Node tuple) instances.

        :param operator: the operator used in the operation.
        :type operator: :py:class:`psyclone.psyir.nodes.NaryOperation.Operator`
        :param operands: a list containing PSyIR nodes and/or 2-tuples \
            which contain an argument name and a PSyIR node, that the \
            operator operates on.
        :type operands: List[Union[:py:class:`psyclone.psyir.nodes.Node`, \
            Tuple[str, :py:class:`psyclone.psyir.nodes.DataNode`]]]

        :returns: an NaryOperator instance.
        :rtype: :py:class:`psyclone.psyir.nodes.NaryOperation`

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        if not isinstance(operator, Enum) or \
           operator not in NaryOperation.Operator:
            raise GenerationError(
                f"operator argument in create method of NaryOperation class "
                f"should be a PSyIR NaryOperation Operator but found "
                f"'{type(operator).__name__}'.")
        if not isinstance(operands, list):
            raise GenerationError(
                f"operands argument in create method of NaryOperation class "
                f"should be a list but found '{type(operands).__name__}'.")

        nary_op = NaryOperation(operator)
        for operand in operands:
            name = None
            if isinstance(operand, tuple):
                if not len(operand) == 2:
                    raise GenerationError(
                        f"If an element of the operands argument in create "
                        f"method of NaryOperation class is a tuple, it's "
                        f"length should be 2, but found {len(operand)}.")
                if not isinstance(operand[0], str):
                    raise GenerationError(
                        f"If an element of the operands argument in create "
                        f"method of NaryOperation class is a tuple, "
                        f"its first argument should be a str, but found "
                        f"{type(operand[0]).__name__}.")
                name, operand = operand
            nary_op.append_named_arg(name, operand)
        return nary_op


# TODO #658 this can be removed once we have support for determining the
# type of a PSyIR expression.
#: Those operators that perform a reduction on an array.
REDUCTION_OPERATORS = [UnaryOperation.Operator.SUM,
                       BinaryOperation.Operator.SUM,
                       NaryOperation.Operator.SUM]

# For automatic API documentation generation
__all__ = ["Operation", "UnaryOperation", "BinaryOperation", "NaryOperation"]
