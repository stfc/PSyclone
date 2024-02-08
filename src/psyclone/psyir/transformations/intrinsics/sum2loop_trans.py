# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council
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

'''Module providing a transformation from a PSyIR SUM intrinsic to an
equivalent PSyIR loop structure. This could be useful if the SUM
intrinsic is not supported by the back-end, the required
parallelisation approach, or if the performance in the inline code is
better than the intrinsic.

'''
from psyclone.psyir.nodes import BinaryOperation, Literal, IntrinsicCall
from psyclone.psyir.symbols import ScalarType
from psyclone.psyir.transformations.intrinsics.array_reduction_base_trans \
    import ArrayReductionBaseTrans


class Sum2LoopTrans(ArrayReductionBaseTrans):
    '''Provides a transformation from a PSyIR SUM IntrinsicCall node to an
    equivalent PSyIR loop structure that is suitable for running in
    parallel on CPUs and GPUs. Validity checks are also performed.

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

    The dimension argument is currently not supported and will result
    in a TransformationError exception being raised.

    .. code-block:: fortran

        R = SUM(ARRAY, dimension=2)

    The array passed to MAXVAL may use any combination of array
    syntax, array notation, array sections and scalar bounds:

    .. code-block:: fortran

        R = SUM(ARRAY) ! array syntax
        R = SUM(ARRAY(:,:)) ! array notation
        R = SUM(ARRAY(1:10,lo:hi)) ! array sections
        R = SUM(ARRAY(1:10,:)) ! mix of array section and array notation
        R = SUM(ARRAY(1:10,2)) ! mix of array section and scalar bound

    For example:

    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.transformations import Sum2LoopTrans
    >>> code = ("subroutine sum_test(array,n,m)\\n"
    ...         "  integer :: n, m\\n"
    ...         "  real :: array(10,10)\\n"
    ...         "  real :: result\\n"
    ...         "  result = sum(array)\\n"
    ...         "end subroutine\\n")
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> sum_node = psyir.children[0].children[0].children[1]
    >>> Sum2LoopTrans().apply(sum_node)
    >>> print(FortranWriter()(psyir))
    subroutine sum_test(array, n, m)
      integer :: n
      integer :: m
      real, dimension(10,10) :: array
      real :: result
      integer :: idx
      integer :: idx_1
    <BLANKLINE>
      result = 0.0
      do idx = 1, 10, 1
        do idx_1 = 1, 10, 1
          result = result + array(idx_1,idx)
        enddo
      enddo
    <BLANKLINE>
    end subroutine sum_test
    <BLANKLINE>

    '''
    _INTRINSIC_NAME = "SUM"
    _INTRINSIC_TYPE = IntrinsicCall.Intrinsic.SUM

    def _loop_body(self, lhs, rhs):
        '''Provide the body of the nested loop that computes the sum
        of the lhs and rhs.

        :param lhs: the lhs value for the sum operation.
        :type lhs: :py:class:`psyclone.psyir.nodes.Node`
        :param rhs: the rhs value for the sum operation.
        :type rhs: :py:class:`psyclone.psyir.nodes.Node`

        :returns: the sum of the lhs and rhs.
        :rtype: :py:class:`psyclone.psyir.nodes.BinaryOperation`

        '''
        # return lhs + rhs
        return BinaryOperation.create(BinaryOperation.Operator.ADD, lhs, rhs)

    def _init_var(self, reference):
        '''The initial value for the variable that computes the sum
        of an array.

        :param reference: the reference used to store the final result.
        :type reference: :py:class:`psyclone.psyir.node.Reference`

        :returns: PSyIR for the value to initialise the variable that
            computes the sum.
        :rtype: :py:class:`psyclone.psyir.nodes.Literal`

        '''
        intrinsic = reference.datatype.intrinsic
        precision = reference.datatype.precision
        scalar_type = ScalarType(intrinsic, precision)
        if intrinsic == ScalarType.Intrinsic.REAL:
            value_str = "0.0"
        elif intrinsic == ScalarType.Intrinsic.INTEGER:
            value_str = "0"
        # Note, the validate method guarantees that an else branch is
        # not required.
        return Literal(value_str, scalar_type)
