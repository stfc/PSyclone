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
PSyIR code. This could be useful if the SUM intrinsic is not supported
by the back-end, the required parallelisation approach, or if the
performance in the inline code is better than the intrinsic.

'''
from psyclone.psyir.nodes import (
    BinaryOperation, Assignment, Reference, Literal, ArrayReference)
from psyclone.psyir.symbols import ScalarType
from psyclone.psyir.transformations.intrinsics.mms_base_trans import (
    MMSBaseTrans)


class Sum2CodeTrans(MMSBaseTrans):
    '''Provides a transformation from a PSyIR SUM IntrinsicCall node to
    equivalent code in a PSyIR tree. Validity checks are also
    performed.

    If SUM contains a single positional argument which is an array,
    all elements of that array are summed and the result returned in
    the scalar R.

    .. code-block:: fortran

        R = SUM(ARRAY)

    For example, if the array is two dimensional, the equivalent code
    for real data is:

    .. code-block:: fortran

        R = 0.0
        DO J=LBOUND(ARRAY,2),UBOUND(ARRAY,2)
          DO I=LBOUND(ARRAY,1),UBOUND(ARRAY,1)
            R = R + ARRAY(I,J)

    If the dimension argument is provided then only that dimension is
    summed:

    .. code-block:: fortran

        R = SUM(ARRAY, dimension=2)

    If the array is two dimensional, the equivalent code
    for real data is:

    .. code-block:: fortran

        R(:) = 0.0
        DO J=LBOUND(ARRAY,2),UBOUND(ARRAY,2)
          DO I=LBOUND(ARRAY,1),UBOUND(ARRAY,1)
            R(I) = R(I) + ARRAY(I,J)

    A restriction is that the value of dimension must be able to be
    determined by PSyclone, either being a literal or a reference to
    something with a known value.

    If the mask argument is provided then the mask is used to
    determine whether the sum is applied:

    .. code-block:: fortran

        R = SUM(ARRAY, mask=MOD(ARRAY, 2.0)==1)

    If the array is two dimensional, the equivalent code
    for real data is:

    .. code-block:: fortran

        R = 0.0
        DO J=LBOUND(ARRAY,2),UBOUND(ARRAY,2)
          DO I=LBOUND(ARRAY,1),UBOUND(ARRAY,1)
            IF (MOD(ARRAY(I,J), 2.0)==1) THEN
              R = R + ARRAY(I,J)

    The array passed to SUM may use array syntax, array notation or
    array sections (or a mixture of the two), but scalar bounds are
    not allowed:

    .. code-block:: fortran

        R = SUM(ARRAY) ! array syntax
        R = SUM(ARRAY(:,:)) ! array notation
        R = SUM(ARRAY(1:10,lo:hi)) ! array sections
        R = SUM(ARRAY(1:10,:)) ! mix of array sections and array notation
        R = SUM(ARRAY(1:10,2)) ! NOT SUPPORTED as 2 is a scalar bound

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
    _INTRINSIC_NAME = "SUM"

    def _loop_body(self, array_reduction, array_iterators, var_symbol,
                   array_ref):
        '''Provide the body of the nested loop that computes the sum of an
        array.

        :param bool array_reduction: True if the implementation should
            provide a sum over a particular array dimension and False
            if the sum is for all elements of the array.
        :param array_iterators: a list of datasymbols containing the
            loop iterators ordered from outermost loop symbol to innermost
            loop symbol.
        :type array_iterators:
            List[:py:class:`psyclone.psyir.symbols.DataSymbol`]
        :param var_symbol: the symbol used to store the final result.
        :type var_symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param array_ref: a reference to the array for which the
            sum is being determined.
        :type array_ref: :py:class:`psyclone.psyir.nodes.ArrayReference`

        :returns: PSyIR for the body of the nested loop.
        :rtype: :py:class:`psyclone.psyir.nodes.Assignment`

        '''
        if array_reduction:
            # sum_var(i,...) = sum_var(i,...) + array(i,...)
            array_indices = [Reference(iterator)
                             for iterator in array_iterators]
            lhs = ArrayReference.create(var_symbol, array_indices)
            array_indices = [Reference(iterator)
                             for iterator in array_iterators]
            rhs_child1 = ArrayReference.create(var_symbol, array_indices)
        else:
            # sum_var = sum_var + array(i,...)
            lhs = Reference(var_symbol)
            rhs_child1 = Reference(var_symbol)

        rhs_child2 = array_ref
        rhs = BinaryOperation.create(BinaryOperation.Operator.ADD, rhs_child1,
                                     rhs_child2)
        return Assignment.create(lhs, rhs)

    def _init_var(self, var_symbol):
        '''The initial value for the variable that computes the sum
        of an array.

        :param var_symbol: the symbol used to store the final result.
        :type var_symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`

        :returns: PSyIR for the value to initialise the variable that
            computes the sum.
        :rtype: :py:class:`psyclone.psyir.nodes.Literal`

        '''
        intrinsic = var_symbol.datatype.intrinsic
        precision = var_symbol.datatype.precision
        scalar_type = ScalarType(intrinsic, precision)
        if intrinsic == ScalarType.Intrinsic.REAL:
            value_str = "0.0"
        elif intrinsic == ScalarType.Intrinsic.INTEGER:
            value_str = "0"
        # Note, the validate method guarantees that an else branch is
        # not required.
        return Literal(value_str, scalar_type)
