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
from psyclone.psyir.transformations.intrinsics.mms_base_trans import (
    MMSBaseTrans)


class Maxval2CodeTrans(MMSBaseTrans):
    '''Provides a transformation from a PSyIR MAXVAL Operator node to
    equivalent code in a PSyIR tree. Validity checks are also
    performed.

    If MAXVAL contains a single positional argument which is an array,
    the maximum value of all of the elements in the array is returned
    in the the scalar R.

    .. code-block:: python

        R = MAXVAL(ARRAY)

    For example, if the array is two dimensional, the equivalent code
    for real data is:

    .. code-block:: python

        R = ARRAY(LBOUND(ARRAY,1),LBOUND(ARRAY,2))
        DO J=LBOUND(ARRAY,2),UBOUND(ARRAY,2)
          DO I=LBOUND(ARRAY,1),UBOUND(ARRAY,1)
            IF R < ARRAY(I,J) THEN
              R = ARRAY(I,J)

    If the dimension argument is provided then the maximum value is
    returned along the row for each entry in that dimension:

    .. code-block:: python

        R = MAXVAL(ARRAY, dimension=2)

    If the array is two dimensional, the equivalent code
    for real data is:

    .. code-block:: python

        DO I=LBOUND(ARRAY,1),UBOUND(ARRAY,1)
          R(I) = ARRAY(I,1)
        DO J=LBOUND(ARRAY,2),UBOUND(ARRAY,2)
          DO I=LBOUND(ARRAY,1),UBOUND(ARRAY,1)
            IF R(I) < ARRAY(I,J) THEN
              R(I) = ARRAY(I,J)

    If the mask argument is provided then the mask is used to
    determine whether the maxval is applied:

    .. code-block:: python

        R = MAXVAL(ARRAY, mask=MOD(ARRAY, 2.0)==1)

    If the array is two dimensional, the equivalent code
    for real data is:

    .. code-block:: python

        R = ARRAY(LBOUND(ARRAY,1),UBOUND(ARRAY,2))
        DO J=LBOUND(ARRAY,2),UBOUND(ARRAY,2)
          DO I=LBOUND(ARRAY,1),UBOUND(ARRAY,1)
            if (MOD(ARRAY(I,J), 2.0)==1):
              R = R + ARRAY(I,J)

    For example:

    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.transformations import Maxval2CodeTrans
    >>> code = ("subroutine maxval_test(array,n,m)\\n"
    ...         "  real :: array(10,10)\\n"
    ...         "  real :: result\\n"
    ...         "  result = maxval(array)\\n"
    ...         "end subroutine\\n")
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> sum_node = psyir.children[0].children[0].children[1]
    >>> Maxval2CodeTrans().apply(sum_node)
    >>> print(FortranWriter()(psyir))
    subroutine maxval_test(array, n, m)
      real, dimension(10,10) :: array
      real :: result
      real :: maxval_var
      integer :: i_0
      integer :: i_1
    <BLANKLINE>
      maxval_var = array(1,1)
      do i_1 = 1, 10, 1
        do i_0 = 1, 10, 1
          maxval_var = maxval_var + array(i_0,i_1)
        enddo
      enddo
      result = maxval_var
    <BLANKLINE>
    end subroutine maxval_test
    <BLANKLINE>

    '''
    _INTRINSIC_NAME = "MAXVAL"

    def _loop_body(self, array_reduction, array_iterators, symbol_maxval_var, array_ref):
        ''' xxx '''
        from psyclone.psyir.nodes import Reference, Assignment, IfBlock, BinaryOperation, ArrayReference
        if array_reduction:
            array_indices = [Reference(iterator)
                             for iterator in array_iterators]
            lhs = ArrayReference.create(symbol_maxval_var, array_indices)
        else:
            lhs = Reference(symbol_maxval_var)

        # if maxval_var < array(i...) then
        #   maxval_var = array(i...)
        # end if

        rhs = array_ref
        assignment = Assignment.create(lhs, rhs)
        lhs = lhs.copy()
        rhs = array_ref.copy()
        if_condition = BinaryOperation.create(BinaryOperation.Operator.LT,
                                              lhs, rhs)
        return IfBlock.create(if_condition, [assignment])

    def _init_var(self, symbol_maxval_var, _, array_ref, loop_bounds, loop_iterators, array_iterators, array_reduction, dimension_literal):
        ''' xxx '''
        from psyclone.psyir.nodes import Reference, Assignment, ArrayReference, Loop, Literal
        from psyclone.psyir.symbols import INTEGER_TYPE
        if array_reduction:
            array_indices = [Reference(iterator)
                             for iterator in array_iterators]
            lhs = ArrayReference.create(symbol_maxval_var, array_indices)
            array_indices = []
            for idx in range(len(loop_bounds)):
                if idx == int(dimension_literal.value)-1:
                    array_indices.append(loop_bounds[idx][0].copy())
                else:
                    array_indices.append(Reference(loop_iterators[idx]))
            rhs = ArrayReference.create(array_ref.symbol, array_indices)
            statement = Assignment.create(lhs, rhs)
            for idx in range(len(loop_bounds)):
                if idx != int(dimension_literal.value)-1:
                    statement = Loop.create(
                        loop_iterators[idx], loop_bounds[idx][0], loop_bounds[idx][1],
                        Literal("1", INTEGER_TYPE), [statement])
            return statement
        else:
            lhs = Reference(symbol_maxval_var)
            array_indices = []
            for idx in range(len(loop_bounds)):
                array_indices.append(loop_bounds[idx][0].copy())
            rhs = ArrayReference.create(array_ref.symbol, array_indices)
            return Assignment.create(lhs, rhs)
