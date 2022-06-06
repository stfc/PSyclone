# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council
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

'''Module providing a transformation from a PSyIR DOT_PRODUCT operator
to PSyIR code. This could be useful if the DOT_PRODUCT operator is not
supported by the back-end or if the performance in the inline code is
better than the intrinsic.

'''

# pylint: disable=too-many-locals

from __future__ import absolute_import
from psyclone.psyir.nodes import BinaryOperation, Assignment, Reference, \
    Loop, Literal, ArrayReference, Range, Routine
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, REAL_TYPE, \
    ArrayType, ScalarType
from psyclone.psyir.transformations.transformation_error \
    import TransformationError
from psyclone.psyir.transformations.intrinsics.operator2code_trans import \
    Operator2CodeTrans


def _get_array_bound(vector1, vector2):
    '''A utility function that returns the appropriate loop bounds (lower,
    upper and step) for a vector.
    If either of the vectors are declared with known bounds (an integer or a
    symbol) then these bound values are used. If the size is unknown
    (a deferred or attribute type) then the LBOUND and UBOUND PSyIR
    nodes are used.

    The validate() method in DotProduct2CodeTrans will ensure that the
    arguments to _get_array_bound() are valid (as it is always called
    beforehand). The validate() method also ensures that array slices
    are for the first dimension of the array and that they are for the
    full size of that dimension (they are limited to ":"). This
    function makes use of these constraint, e.g. it always returns 1
    for the stride.

    Issue #717 requires similar functionality to this
    function. However, to use this function safely in other situations
    we would need to move the tests in validate into this function
    first and then potentially add this function to the ArrayMixin
    class, or a separate utils module.

    :param array: the reference that we are interested in.
    :type array: :py:class:`psyir.nodes.Reference`
    :param int index: the (array) reference index that we are \
        interested in.

    :returns: the loop bounds for this array index.
    :rtype: (Literal, Literal, Literal) or \
        (BinaryOperation, BinaryOperation, Literal)

    '''
    # Look for explicit bounds in one of the array declarations
    for vector in [vector1, vector2]:
        symbol = vector.symbol
        my_dim = symbol.shape[0]
        if isinstance(my_dim, ArrayType.ArrayBounds):
            lower_bound = my_dim.lower
            upper_bound = my_dim.upper
            step = Literal("1", INTEGER_TYPE)
            return (lower_bound, upper_bound, step)

    # No explicit array bound information could be found for either
    # array so use the LBOUND and UBOUND intrinsics.
    symbol = vector1.symbol
    my_dim = symbol.shape[0]
    lower_bound = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND, Reference(symbol),
        Literal("1", INTEGER_TYPE))
    upper_bound = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, Reference(symbol),
        Literal("1", INTEGER_TYPE))
    step = Literal("1", INTEGER_TYPE)
    return (lower_bound, upper_bound, step)


class DotProduct2CodeTrans(Operator2CodeTrans):
    '''Provides a transformation from a PSyIR DOT_PRODUCT Operator node to
    equivalent code in a PSyIR tree. Validity checks are also
    performed.

    If ``R`` is a scalar and ``A``, and ``B`` have dimension ``N``,
    The transformation replaces:

    .. code-block:: fortran

        R = ... DOT_PRODUCT(A,B) ...

    with the following code:

    .. code-block:: fortran

        TMP = 0.0
        do I=1,N
            TMP = TMP + A(i)*B(i)
        R = ... TMP ...

    For example:

    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import BinaryOperation
    >>> from psyclone.psyir.transformations import DotProduct2CodeTrans
    >>> code = ("subroutine dot_product_test(v1,v2)\\n"
    ...         "real,intent(in) :: v1(10), v2(10)\\n"
    ...         "real :: result\\n"
    ...         "result = dot_product(v1,v2)\\n"
    ...         "end subroutine\\n")
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> trans = DotProduct2CodeTrans()
    >>> trans.apply(psyir.walk(BinaryOperation)[0])
    >>> print(FortranWriter()(psyir))
    subroutine dot_product_test(v1, v2)
      real, dimension(10), intent(in) :: v1
      real, dimension(10), intent(in) :: v2
      real :: result
      integer :: i
      real :: res_dot_product
    <BLANKLINE>
      res_dot_product = 0.0
      do i = 1, 10, 1
        res_dot_product = res_dot_product + v1(i) * v2(i)
      enddo
      result = res_dot_product
    <BLANKLINE>
    end subroutine dot_product_test
    <BLANKLINE>

    '''
    def __init__(self):
        super().__init__()
        self._operator_name = "DOTPRODUCT"
        self._classes = (BinaryOperation,)
        self._operators = (BinaryOperation.Operator.DOT_PRODUCT,)

    def validate(self, node, options=None):
        '''Perform checks to ensure that it is valid to apply the
        DotProduct2CodeTran transformation to the supplied node.

        Note, this validation does not check for invalid argument
        combinations to dot_product (e.g. different precision or
        different datatypes) as that should have already been picked
        up when creating the PSyIR.

        :param node: the node that is being checked.
        :type node: :py:class:`psyclone.psyir.nodes.BinaryOperation`
        :param options: a dictionary with options for transformations.
        :type options: dict of str:str or None

        :raises TransformationError: if one of the arguments is not a \
            Reference node.
        :raises TransformationError: if an argument does not use array \
            slice notation and is not a 1d array.
        :raises TransformationError: if an argument uses array slice \
            notation but the array slice is not for the first dimension of \
            the array.
        :raises TransformationError: if an argument uses array slice \
            notation but it is not for the full range of the dimension.

        '''
        super().validate(node, options)

        # Both arguments should be references (or array references)
        for arg in node.children:
            if arg.__class__ not in [Reference, ArrayReference]:
                raise TransformationError(
                    f"The DotProduct2CodeTrans transformation only supports "
                    f"the transformation of a dotproduct intrinsic if its "
                    f"arguments are plain arrays, but found "
                    f"{self._writer(arg)} in {self._writer(node)}.")

        for arg in node.children:
            # The argument should be a 1D array if the argument does
            # not provide any array slice information (i.e. it is a
            # Reference)
            if arg.__class__ is Reference:
                symbol = arg.symbol
                # This symbol should be a 1D array
                if not(isinstance(symbol, DataSymbol) and symbol.is_array and
                       len(symbol.shape) == 1):
                    raise TransformationError(
                        f"The DotProduct2CodeTrans transformation only "
                        f"supports the transformation of a dotproduct "
                        f"intrinsic with an argument not containing an array "
                        f"slice if the argument is a 1D array, but found "
                        f"{self._writer(arg)} with {len(symbol.shape)} "
                        f"dimensions in {self._writer(node)}.")

        for arg in node.children:
            # If the argument does provide array slice information
            # then check the array slice is in the first dimension of
            # the array and that the slice is for the full range of that
            # dimension i.e. uses a ':'.
            if isinstance(arg, ArrayReference):
                if not isinstance(arg.indices[0], Range):
                    raise TransformationError(
                        f"The DotProduct2CodeTrans transformation only "
                        f"supports the transformation of a dotproduct "
                        f"intrinsic with an argument containing an array "
                        f"slice if the array slice is for the 1st dimension "
                        f"of the array, but found {self._writer(arg)} in "
                        f"{self._writer(node)}.")

                if not arg.is_full_range(0):
                    raise TransformationError(
                        f"The DotProduct2CodeTrans transformation only "
                        f"supports the transformation of a dotproduct "
                        f"intrinsic with an argument containing an array "
                        f"slice if the argument is for the 1st dimension "
                        f"of the array and is for the full range of that "
                        f"dimension, but found {self._writer(arg)} in "
                        f"{self._writer(node)}.")

        # Both arguments should be real (as other intrinsic datatypes
        # are not suported).
        for arg in node.children:
            if arg.symbol.datatype.intrinsic != ScalarType.Intrinsic.REAL:
                raise TransformationError(
                    f"The DotProduct2CodeTrans transformation only supports "
                    f"arrays of real data, but found {self._writer(arg)} of "
                    f"type {arg.symbol.datatype.intrinsic.name} in "
                    f"{self._writer(node)}.")

    def apply(self, node, options=None):
        '''Apply the DOT_PRODUCT intrinsic conversion transformation to the
        specified node. This node must be a DOT_PRODUCT
        BinaryOperation.  If the transformation is successful then an
        assignment which includes a DOT_PRODUCT BinaryOperation node
        is converted to equivalent inline code.

        :param node: a DOT_PRODUCT Binary-Operation node.
        :type node: :py:class:`psyclone.psyir.nodes.BinaryOperation`
        :param options: a dictionary with options for transformations.
        :type options: dict of str:str or None

        '''
        self.validate(node)

        assignment = node.ancestor(Assignment)
        vector1 = node.children[0]
        vector2 = node.children[1]
        symbol_table = node.ancestor(Routine).symbol_table

        # Create new i loop iterator.
        i_loop_symbol = symbol_table.new_symbol(
            "i", symbol_type=DataSymbol, datatype=INTEGER_TYPE)

        # Create temporary result variable. Use the datatype of one of
        # the arguments. We can do this as the validate method only
        # allows plain real arrays.
        vector1_datatype = vector1.symbol.datatype
        datatype = ScalarType(
            vector1_datatype.intrinsic, vector1_datatype.precision)
        symbol_res_var = symbol_table.new_symbol(
            "res_dot_product", symbol_type=DataSymbol,
            datatype=datatype)

        # Replace operation with the temporary result variable.
        result_ref = Reference(symbol_res_var)
        node.replace_with(result_ref)

        # Create "vector1(i)"
        vector1_dims = [Reference(i_loop_symbol)]
        if len(vector1.children) > 1:
            # Add any additional dimensions (in case of an array slice)
            for child in vector1.children[1:]:
                vector1_dims.append(child.copy())
        vector1_array_reference = ArrayReference.create(
            vector1.symbol, vector1_dims)
        # Create "vector2(i)"
        vector2_dims = [Reference(i_loop_symbol)]
        if len(vector2.children) > 1:
            # Add any additional dimensions (in case of an array slice)
            for child in vector2.children[1:]:
                vector2_dims.append(child.copy())
        vector2_array_reference = ArrayReference.create(
            vector2.symbol, vector2_dims)
        # Create "vector1(i) * vector2(i)"
        multiply = BinaryOperation.create(
            BinaryOperation.Operator.MUL, vector1_array_reference,
            vector2_array_reference)
        # Create "result + vector1(i) * vector2(i)"
        rhs = BinaryOperation.create(
            BinaryOperation.Operator.ADD, result_ref.copy(), multiply)
        # Create "result = result + vector1(i) * vector2(i)"
        assign = Assignment.create(result_ref.copy(), rhs)
        # Work out the loop bounds
        lower_bound, upper_bound, step = _get_array_bound(vector1, vector2)
        # Create i loop and add the above code as a child
        iloop = Loop.create(i_loop_symbol, lower_bound.copy(),
                            upper_bound.copy(), step.copy(), [assign])
        # Create "result = 0.0"
        assign = Assignment.create(result_ref.copy(),
                                   Literal("0.0", REAL_TYPE))
        # Add the initialisation and loop nodes into the PSyIR tree
        assignment.parent.children.insert(assignment.position, assign)
        assignment.parent.children.insert(assignment.position, iloop)
