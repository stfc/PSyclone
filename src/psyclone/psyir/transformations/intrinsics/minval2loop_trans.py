# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council
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

'''Module providing a transformation from a PSyIR MINVAL intrinsic to
an equivalent PSyIR loop structure. This could be useful if the MINVAL
operator is not supported by the back-end, the required
parallelisation approach, or if the performance in the inline code is
better than the intrinsic.

'''
from psyclone.psyir.nodes import Reference, IntrinsicCall
from psyclone.psyir.transformations.intrinsics.array_reduction_base_trans \
    import ArrayReductionBaseTrans


class Minval2LoopTrans(ArrayReductionBaseTrans):
    '''Provides a transformation from a PSyIR MINVAL IntrinsicCall node to
    an equivalent PSyIR loop structure that is suitable for running in
    parallel on CPUs and GPUs. Validity checks are also performed.

    If MINVAL contains a single positional argument which is an array,
    the minimum value of all of the elements in the array is returned
    in the the scalar R.

    .. code-block:: fortran

        R = MINVAL(ARRAY)

    For example, if the array is two dimensional, the equivalent code
    for real data is:

    .. code-block:: fortran

        R = HUGE(R)
        DO J=LBOUND(ARRAY,2),UBOUND(ARRAY,2)
          DO I=LBOUND(ARRAY,1),UBOUND(ARRAY,1)
            R = MIN(R, ARRAY(I,J))

    If the mask argument is provided then the mask is used to
    determine whether the minval is applied:

    .. code-block:: fortran

        R = MINVAL(ARRAY, mask=MOD(ARRAY, 2.0)==1)

    If the array is two dimensional, the equivalent code
    for real data is:

    .. code-block:: fortran

        R = HUGE(R)
        DO J=LBOUND(ARRAY,2),UBOUND(ARRAY,2)
          DO I=LBOUND(ARRAY,1),UBOUND(ARRAY,1)
            IF (MOD(ARRAY(I,J), 2.0)==1) THEN
              R = MIN(R, ARRAY(I,J))

    The dimension argument is currently not supported and will result
    in a TransformationError exception being raised.

    .. code-block:: fortran

        R = MINVAL(ARRAY, dimension=2)

    The array passed to MINVAL may use any combination of array
    syntax, array notation, array sections and scalar bounds:

    .. code-block:: fortran

        R = MINVAL(ARRAY) ! array syntax
        R = MINVAL(ARRAY(:,:)) ! array notation
        R = MINVAL(ARRAY(1:10,lo:hi)) ! array sections
        R = MINVAL(ARRAY(1:10,:)) ! mix of array section and array notation
        R = MINVAL(ARRAY(1:10,2)) ! mix of array section and scalar bound

    For example:

    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.transformations import Minval2LoopTrans
    >>> code = ("subroutine minval_test(array)\\n"
    ...         "  real :: array(10,10)\\n"
    ...         "  real :: result\\n"
    ...         "  result = minval(array)\\n"
    ...         "end subroutine\\n")
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> sum_node = psyir.children[0].children[0].children[1]
    >>> Minval2LoopTrans().apply(sum_node)
    >>> print(FortranWriter()(psyir))
    subroutine minval_test(array)
      real, dimension(10,10) :: array
      real :: result
      integer :: idx
      integer :: idx_1
    <BLANKLINE>
      result = HUGE(result)
      do idx = 1, 10, 1
        do idx_1 = 1, 10, 1
          result = MIN(result, array(idx_1,idx))
        enddo
      enddo
    <BLANKLINE>
    end subroutine minval_test
    <BLANKLINE>

    '''
    _INTRINSIC_NAME = "MINVAL"
    _INTRINSIC_TYPE = IntrinsicCall.Intrinsic.MINVAL

    def _loop_body(self, lhs, rhs):
        '''Provide the body of the nested loop that computes the minimum value
        of the lhs and rhs.

        :param lhs: the lhs value for the min operation.
        :type lhs: :py:class:`psyclone.psyir.nodes.Node`
        :param rhs: the rhs value for the min operation.
        :type rhs: :py:class:`psyclone.psyir.nodes.Node`

        :returns: a MIN IntrinsicCall.
        :rtype: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

        '''
        # return min(lhs,rhs)
        return IntrinsicCall.create(IntrinsicCall.Intrinsic.MIN, [lhs, rhs])

    def _init_var(self, reference):
        '''The initial value for the variable that computes the minimum value
        of an array.

        :param reference: the reference used to store the final result.
        :type reference: :py:class:`psyclone.psyir.node.Reference`

        :returns: PSyIR for the value to initialise the variable that
            computes the minimum value.
        :rtype: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

        '''
        return IntrinsicCall.create(
            IntrinsicCall.Intrinsic.HUGE, [reference.copy()])
