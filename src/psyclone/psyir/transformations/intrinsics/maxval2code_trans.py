# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council
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

'''Module providing a transformation from a PSyIR MAXVAL intrinsic to
PSyIR code. This could be useful if the MAXVAL operator is not
supported by the back-end, the required parallelisation approach, or
if the performance in the inline code is better than the intrinsic.

'''
from psyclone.psyir.nodes import (
    Reference, Assignment, IfBlock, BinaryOperation, ArrayReference,
    IntrinsicCall)
from psyclone.psyir.transformations.intrinsics.mms_base_trans import (
    MMSBaseTrans)


class Maxval2CodeTrans(MMSBaseTrans):
    '''Provides a transformation from a PSyIR MAXVAL IntrinsicCall node to
    equivalent code in a PSyIR tree. Validity checks are also
    performed.

    If MAXVAL contains a single positional argument which is an array,
    the maximum value of all of the elements in the array is returned
    in the the scalar R.

    .. code-block:: fortran

        R = MAXVAL(ARRAY)

    For example, if the array is two dimensional, the equivalent code
    for real data is:

    .. code-block:: fortran

        R = TINY(R)
        DO J=LBOUND(ARRAY,2),UBOUND(ARRAY,2)
          DO I=LBOUND(ARRAY,1),UBOUND(ARRAY,1)
            IF (R < ARRAY(I,J)) THEN
              R = ARRAY(I,J)

    If the dimension argument is provided then the maximum value is
    returned along the row for each entry in that dimension:

    .. code-block:: fortran

        R = MAXVAL(ARRAY, dimension=2)

    If the array is two dimensional, the equivalent code
    for real data is:

    .. code-block:: fortran

        R(:) = TINY(R)
        DO J=LBOUND(ARRAY,2),UBOUND(ARRAY,2)
          DO I=LBOUND(ARRAY,1),UBOUND(ARRAY,1)
            IF (R(I) < ARRAY(I,J)) THEN
              R(I) = ARRAY(I,J)

    A restriction is that the value of dimension must be able to be
    determined by PSyclone, either being a literal or a reference to
    something with a known value.

    If the mask argument is provided then the mask is used to
    determine whether the maxval is applied:

    .. code-block:: fortran

        R = MAXVAL(ARRAY, mask=MOD(ARRAY, 2.0)==1)

    If the array is two dimensional, the equivalent code
    for real data is:

    .. code-block:: fortran

        R = TINY(R)
        DO J=LBOUND(ARRAY,2),UBOUND(ARRAY,2)
          DO I=LBOUND(ARRAY,1),UBOUND(ARRAY,1)
            IF (MOD(ARRAY(I,J), 2.0)==1) THEN
              IF (R < ARRAY(I,J)) THEN
                R = ARRAY(I,J)

    The array passed to MAXVAL may use array syntax, array notation or
    array sections (or a mixture of the two), but scalar bounds are
    not allowed:

    .. code-block:: fortran

        R = MAXVAL(ARRAY) ! array syntax
        R = MAXVAL(ARRAY(:,:)) ! array notation
        R = MAXVAL(ARRAY(1:10,lo:hi)) ! array sections
        R = MAXVAL(ARRAY(1:10,:)) ! mix of array sections and array notation
        R = MAXVAL(ARRAY(1:10,2)) ! NOT SUPPORTED as 2 is a scalar bound

    For example:

    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.transformations import Maxval2CodeTrans
    >>> code = ("subroutine maxval_test(array)\\n"
    ...         "  real :: array(10,10)\\n"
    ...         "  real :: result\\n"
    ...         "  result = maxval(array)\\n"
    ...         "end subroutine\\n")
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> sum_node = psyir.children[0].children[0].children[1]
    >>> Maxval2CodeTrans().apply(sum_node)
    >>> print(FortranWriter()(psyir))
    subroutine maxval_test(array)
      real, dimension(10,10) :: array
      real :: result
      real :: maxval_var
      integer :: i_0
      integer :: i_1
    <BLANKLINE>
      maxval_var = TINY(maxval_var)
      do i_1 = 1, 10, 1
        do i_0 = 1, 10, 1
          if (maxval_var < array(i_0,i_1)) then
            maxval_var = array(i_0,i_1)
          end if
        enddo
      enddo
      result = maxval_var
    <BLANKLINE>
    end subroutine maxval_test
    <BLANKLINE>

    '''
    _INTRINSIC_NAME = "MAXVAL"

    def _loop_body(self, array_reduction, array_iterators, var_symbol,
                   array_ref):
        '''Provide the body of the nested loop that computes the maximum value
        of an array.

        :param bool array_reduction: True if the implementation should
            provide a maximum over a particular array dimension and False
            if the maximum is for all elements of the array.
        :param array_iterators: a list of datasymbols containing the
            loop iterators ordered from innermost loop symbol to outermost
            loop symbol.
        :type array_iterators:
            List[:py:class:`psyclone.psyir.symbols.DataSymbol`]
        :param var_symbol: the symbol used to store the final result.
        :type var_symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param array_ref: a reference to the array for which the
            maximum is being determined.
        :type array_ref: :py:class:`psyclone.psyir.nodes.ArrayReference`

        :returns: PSyIR for the body of the nested loop.
        :rtype: :py:class:`psyclone.psyir.nodes.IfBlock`

        '''
        # maxval_var() = array(i...)
        if array_reduction:
            array_indices = [Reference(iterator)
                             for iterator in array_iterators]
            lhs = ArrayReference.create(var_symbol, array_indices)
        else:
            lhs = Reference(var_symbol)
        rhs = array_ref
        assignment = Assignment.create(lhs, rhs)

        # maxval_var() < array(i...)
        lhs = lhs.copy()
        rhs = rhs.copy()
        if_condition = BinaryOperation.create(
            BinaryOperation.Operator.LT, lhs, rhs)

        # if maxval_var() < array(i...) then
        #   maxval_var() = array(i...)
        # end if
        return IfBlock.create(if_condition, [assignment])

    def _init_var(self, var_symbol):
        '''The initial value for the variable that computes the maximum value
        of an array.

        :param var_symbol: the symbol used to store the final result.
        :type var_symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`

        :returns: PSyIR for the value to initialise the variable that
            computes the maximum value.
        :rtype: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

        '''
        return IntrinsicCall.create(
            IntrinsicCall.Intrinsic.TINY, [Reference(var_symbol)])
