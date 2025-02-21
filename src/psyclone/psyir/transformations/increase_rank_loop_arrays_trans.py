# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025, Science and Technology Facilities Council.
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
# Authors: S. Siso, STFC Daresbury Lab

'''This module contains the IncreaseRankLoopArrays transformation. This
transformation increases the dimensionality of the selected arrays with
the length of the loop iteration space, when it updates each array access
with the variable loop, so that each access an independent element.
Effectively it provides an alternative to array privatisation.

'''

from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import (
    Loop, Routine, CodeBlock)
from psyclone.psyir.transformations.transformation_error \
    import TransformationError


class IncreaseRankLoopArraysTrans(Transformation):
    ''' This transformation takes a loop and a list of arrays accessed inside
    the loop, and increases those arrays with an additional dimension with the
    size of the interation space.

    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Loop
    >>> from psyclone.psyir.transformations import IncreaseRankLoopArraysTrans
    >>> code = ("""
    ... program test
    ...     integer :: N=10, M=10
    ...     integer :: i, j
    ...     real, dimension(N) :: ztmp
    ...     do i = 1, M
    ...         do j = 1, N
    ...             ztmp(j) = 1
    ...         end do
    ...         do j = 1, N
    ...             ztmp(j) = ztmp(j) + 1
    ...         end do
    ...     end do
    ... end program
    ... """)
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> hoist = IncreaseRankLoopArraysTrans()
    >>> hoist.apply(psyir.walk(Loop)[0])
    >>> print(FortranWriter()(psyir))
    program test
      integer :: i
      integer :: j
      integer :: n
      real, dimension(n,n) :: a
      real :: value
    <BLANKLINE>
      value = 1.0
      do i = 1, n, 1
        do j = 1, n, 1
          a(i,j) = value
        enddo
      enddo
    <BLANKLINE>
    end program test
    <BLANKLINE>

    '''
    def __str__(self):
        return ("Increases the Rank of the supplied arrays by the iteration "
                "space of the given loop, and update all its references")

    def apply(self, node, options=None):
        '''Applies the transformation.

        :param node: target PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        '''
        self.validate(node, options)

    def validate(self, node, options=None):
        '''Checks that the supplied node is a valid target.

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the supplied node is not a Loop.

        '''
        # The node should be an assignment
        if not isinstance(node, Loop):
            raise TransformationError(
                f"The target of the {self.name} transformation should be a "
                f"Loop, but found '{type(node).__name__}'.")

        # Check that the whole routine has no CodeBlocks (we need to see each
        # access to the target arrays)
        routine = node.ancestor(Routine)
        if not routine or routine.walk(CodeBlock):
            raise TransformationError(
                "The supplied loop should be inside a routine, and the whole "
                "routine should have no CodeBlocks.")


_all__ = ["IncreaseRankLoopArraysTrans"]
