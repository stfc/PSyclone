# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2023, Science and Technology Facilities Council
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
# Author: R. W. Ford, STFC Daresbury Lab
# Modified: S. Siso, STFC Daresbury Lab

'''Module providing a transformation from a PSyIR SUM intrinsic to
PSyIR code. This could be useful if the SUM operator is not supported
by the back-end or if the performance in the inline code is better
than the intrinsic.

'''
from psyclone.psyir.nodes import (
    BinaryOperation, Assignment, Reference,
    Literal, Loop, ArrayReference, IfBlock, Range, IntrinsicCall)
from psyclone.psyir.symbols import (
    DataSymbol, INTEGER_TYPE, ScalarType, ArrayType)
from psyclone.psyGen import Transformation
from psyclone.psyir.transformations.transformation_error import \
    TransformationError


class Sum2CodeTrans(Transformation):
    '''Provides a transformation from a PSyIR SUM Operator node to
    equivalent code in a PSyIR tree. Validity checks are also
    performed.

    If SUM contains a single positional argument which is an array,
    all element on that array are summed and the result returned in
    the scalar R.

    .. code-block:: python

        R = SUM(ARRAY)

    For example, if the array is two dimensional, the equivalent code
    for real data is:

    .. code-block:: python

        R = 0.0
        DO J=LBOUND(ARRAY,2),UBOUND(ARRAY,2)
          DO I=LBOUND(ARRAY,1),UBOUND(ARRAY,1)
            R = R + ARRAY(I,J)

    If the dimension argument is provided then only that dimension is
    summed:

    .. code-block:: python

        R = SUM(ARRAY, dimension=2)

    If the array is two dimensional, the equivalent code
    for real data is:

    .. code-block:: python

        R(:) = 0.0
        DO J=LBOUND(ARRAY,2),UBOUND(ARRAY,2)
          DO I=LBOUND(ARRAY,1),UBOUND(ARRAY,1)
            R(I) = R(I) + ARRAY(I,J)

    If the mask argument is provided then the mask is used to
    determine whether the sum is applied:

    .. code-block:: python

        R = SUM(ARRAY, mask=MOD(ARRAY, 2.0)==1)

    If the array is two dimensional, the equivalent code
    for real data is:

    .. code-block:: python

        R = 0.0
        DO J=LBOUND(ARRAY,2),UBOUND(ARRAY,2)
          DO I=LBOUND(ARRAY,1),UBOUND(ARRAY,1)
            if (MOD(ARRAY(I,J), 2.0)==1):
              R = R + ARRAY(I,J)

    For example:

    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.transformations import Sum2CodeTrans
    >>> code = ("subroutine sum_test(array,n,m)\\n"
    ...         "  integer :: n, m\\n"
    ...         "  real :: array(10,10)\\n"
    ...         "  real :: result\\n"
    ...         "  result = sum(array)\\n"
    ...         "end subroutine\\n")
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> sum_node = psyir.children[0].children[0].children[1]
    >>> Sum2CodeTrans().apply(sum_node)
    >>> print(FortranWriter()(psyir))
    subroutine sum_test(array, n, m)
      integer :: n
      integer :: m
      real, dimension(10,10) :: array
      real :: result
      real :: sum_var
      integer :: i_0
      integer :: i_1
    <BLANKLINE>
      sum_var = 0.0
      do i_1 = 1, 10, 1
        do i_0 = 1, 10, 1
          sum_var = sum_var + array(i_0,i_1)
        enddo
      enddo
      result = sum_var
    <BLANKLINE>
    end subroutine sum_test
    <BLANKLINE>

    '''
    @staticmethod
    def _get_args(node):
        '''Utility method that returns the sum arguments, (array reference,
        dimension and mask).

        :param node: a Sum intrinsic.
        :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

        returns: a tuple containing the 3 sum arguments.
        rtype: Tuple[py:class:`psyclone.psyir.nodes.reference.Reference`, \
                     py:class:`psyclone.psyir.nodes.Literal` | \
                     :py:class:`psyclone.psyir.nodes.Reference`, \
                     Optional[:py:class:`psyclone.psyir.nodes.node`]]

        '''
        # Determine the arguments to sum
        args = [None, None, None]
        arg_names_map = {"array": 0, "dim": 1, "mask": 2}
        for idx, child in enumerate(node.children):
            if not node.argument_names[idx]:
                # positional arg
                args[idx] = child
            else:
                # named arg
                name = node.argument_names[idx].lower()
                args[arg_names_map[name]] = child
        array_ref = args[0]
        dimension_ref = args[1]
        mask_ref = args[2]
        return (array_ref, dimension_ref, mask_ref)

    def __str__(self):
        return "Convert the PSyIR SUM intrinsic to equivalent PSyIR code."

    def validate(self, node, options=None):
        '''Check that the input node is valid before applying the
        transformation.

        :param node: a Sum intrinsic.
        :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`
        :param options: options for the transformation.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the supplied node is not an \
            intrinsic.
        :raises TransformationError: if the supplied node is not a sum \
            intrinsic.
        :raises TransformationError: if a valid value for the \
            dimension argument can't be determined.
        :raises TransformationError: if the array argument is not an array.
        :raises TransformationError: if the shape of the array is not \
            supported.
        :raises TransformationError: if the array datatype is not \
            supported.

        '''
        if not isinstance(node, IntrinsicCall):
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied node "
                f"argument is not an intrinsic, found "
                f"'{type(node).__name__}'.")

        if node.routine.name.lower() != "sum":
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied node "
                f"argument is not a sum intrinsic, found "
                f"'{node.routine.name}'.")

        array_ref, dim_ref, _ = self._get_args(node)
        if dim_ref and not isinstance(dim_ref, (Literal, Reference)):
            raise TransformationError(
                f"Can't find the value of the dimension argument. Expected "
                f"it to be a literal or a reference but found "
                f"'{dim_ref.debug_string()}' which is a "
                f"'{type(dim_ref).__name__}'.")

        # pylint: disable=unidiomatic-typecheck
        if not (isinstance(array_ref, ArrayReference) or
                type(array_ref) == Reference):
            raise TransformationError(
                f"Sum2CodeTrans only support arrays for the first argument, "
                f"but found '{type(array_ref).__name__}'.")

        if len(array_ref.children) == 0:
            if not array_ref.symbol.is_array:
                raise TransformationError(
                    f"Expected '{array_ref.name}' to be an array.")

        for shape in array_ref.children:
            if not isinstance(shape, Range):
                raise TransformationError(
                    f"Sum2CodeTrans only supports arrays with array ranges, "
                    f"but found a fixed dimension in "
                    f"'{array_ref.debug_string()}'.")

        try:
            _ = array_ref.symbol.shape
        except TypeError as err:
            raise TransformationError(
                f"Unexpected shape for array '{array_ref.symbol.name}': "
                f"{err}") from err

        array_intrinsic = array_ref.symbol.datatype.intrinsic
        if array_intrinsic not in [ScalarType.Intrinsic.REAL,
                                   ScalarType.Intrinsic.INTEGER]:
            raise TransformationError(
                f"Only real and integer types supported for array "
                f"'{array_ref.name}', but found '{array_intrinsic.name}'.")

    # pylint: disable=too-many-locals
    # pylint: disable=too-many-branches
    # pylint: disable=too-many-statements
    def apply(self, node, options=None):
        '''Apply the SUM intrinsic conversion transformation to the specified
        node. This node must be a SUM Operation which is converted to
        equivalent inline code.

        :param node: a Sum intrinsic.
        :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`
        :param options: options for the transformation.
        :type options: Optional[Dict[str, Any]]

        '''
        self.validate(node)

        array_ref, dimension_ref, mask_ref = self._get_args(node)

        # Determine the literal value of the dimension argument
        dimension_literal = None
        if not dimension_ref:
            # there is no dimension argument
            pass
        elif isinstance(dimension_ref, Literal):
            dimension_literal = dimension_ref
        elif (isinstance(dimension_ref, Reference) and
              dimension_ref.symbol.is_constant):
            dimension_literal = dimension_ref.symbol.constant_value
        # else exception is handled by the validate method.

        # Determine the dimension and extent of the array
        ndims = None
        if len(array_ref.children) == 0:
            # Note, the potential 'if not array_ref.symbol.is_array:'
            # exception is already handled by the validate method.
            ndims = len(array_ref.symbol.shape)

            loop_bounds = []
            for idx, shape in enumerate(array_ref.symbol.shape):
                if shape in [ArrayType.Extent.DEFERRED,
                             ArrayType.Extent.ATTRIBUTE]:
                    # runtime extent using LBOUND and UBOUND required
                    lbound = BinaryOperation.create(
                        BinaryOperation.Operator.LBOUND,
                        Reference(array_ref.symbol),
                        Literal(str(idx+1), INTEGER_TYPE))
                    ubound = BinaryOperation.create(
                        BinaryOperation.Operator.UBOUND,
                        Reference(array_ref.symbol),
                        Literal(str(idx+1), INTEGER_TYPE))
                    loop_bounds.append((lbound, ubound))
                elif isinstance(shape, ArrayType.ArrayBounds):
                    # array extent is defined in the array declaration
                    loop_bounds.append(shape)
                #  Note, the validate method guarantees that an else
                #  clause is not required.
        else:
            # The validate method guarantees that this is an array
            # reference.
            loop_bounds = []
            ndims = len(array_ref.children)
            for shape in array_ref.children:
                loop_bounds.append((shape.start.copy(), shape.stop.copy()))

        # Determine the datatype of the array's values and create a
        # scalar of that type
        array_intrinsic = array_ref.symbol.datatype.intrinsic
        array_precision = array_ref.symbol.datatype.precision
        scalar_type = ScalarType(array_intrinsic, array_precision)

        symbol_table = node.scope.symbol_table
        assignment = node.ancestor(Assignment)

        datatype = scalar_type
        array_reduction = False
        if dimension_ref and ndims > 1:
            array_reduction = True
            # We are reducing from one array to another
            shape = []
            for idx, bounds in enumerate(loop_bounds):
                if int(dimension_literal.value)-1 == idx:
                    pass
                else:
                    shape.append(bounds)
            datatype = ArrayType(scalar_type, shape)

        # Create temporary sum variable (sum_var)
        symbol_sum_var = symbol_table.new_symbol(
            "sum_var", symbol_type=DataSymbol, datatype=datatype)

        # Replace operation with a temporary variable (sum_var).
        node.replace_with(Reference(symbol_sum_var))

        # sum_var=0.0 or 0
        lhs = Reference(symbol_sum_var)
        if scalar_type.intrinsic == scalar_type.Intrinsic.REAL:
            rhs = Literal("0.0", scalar_type)
        elif scalar_type.intrinsic == scalar_type.Intrinsic.INTEGER:
            rhs = Literal("0", scalar_type)
        # Note, the validate method guarantees that an else branch is
        # not required.

        new_assignment = Assignment.create(lhs, rhs)
        assignment.parent.children.insert(assignment.position, new_assignment)

        # Create the loop iterators
        loop_iterators = []
        array_iterators = []
        for idx in range(ndims):
            loop_iterator = symbol_table.new_symbol(
                f"i_{idx}", symbol_type=DataSymbol, datatype=INTEGER_TYPE)
            loop_iterators.append(loop_iterator)
            if array_reduction and idx != int(dimension_literal.value)-1:
                array_iterators.append(loop_iterator)

        if array_reduction:
            # sum_var(i,...) = sum_var(i,...) + array(i,...)
            array_indices = [Reference(iterator)
                             for iterator in array_iterators]
            lhs = ArrayReference.create(symbol_sum_var, array_indices)
            array_indices = [Reference(iterator)
                             for iterator in array_iterators]
            rhs_child1 = ArrayReference.create(symbol_sum_var, array_indices)
        else:
            # sum_var = sum_var + array(i,...)
            lhs = Reference(symbol_sum_var)
            rhs_child1 = Reference(symbol_sum_var)

        rhs_child2 = node.children[0].detach()
        rhs = BinaryOperation.create(BinaryOperation.Operator.ADD, rhs_child1,
                                     rhs_child2)
        statement = Assignment.create(lhs, rhs)
        if mask_ref:
            # A mask argument has been provided
            for ref in mask_ref.walk(Reference):
                if ref.name == array_ref.name:
                    # The array needs indexing
                    shape = [Reference(obj) for obj in loop_iterators]
                    reference = ArrayReference.create(ref.symbol, shape)
                    ref.replace_with(reference)
            statement = IfBlock.create(mask_ref.detach(), [statement])

        array_indices = []
        for idx in range(ndims):
            statement = Loop.create(
                loop_iterators[idx], loop_bounds[idx][0], loop_bounds[idx][1],
                Literal("1", INTEGER_TYPE), [statement])
            array_indices.append(Reference(loop_iterators[idx]))

        assignment.parent.children.insert(assignment.position, statement)
        rhs_child2.replace_with(
            ArrayReference.create(rhs_child2.symbol, array_indices))
