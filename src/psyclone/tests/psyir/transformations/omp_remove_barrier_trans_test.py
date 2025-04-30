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
# ----------------------------------------------------------------------------
# Author: A. B. G. Chalk, STFC Daresbury Lab

''' Tests for the OMPRemoveBarrierTrans transformation. '''

# import pytest
from psyclone.psyir.nodes import (Loop, Routine, OMPBarrierDirective)
from psyclone.psyir.transformations import (
        OMPLoopTrans, OMPRemoveBarrierTrans,
        # OMPTargetTrans,
        # TransformationError,
)
from psyclone.transformations import OMPParallelTrans


def test_basic_barrier_removal(fortran_reader, fortran_writer):
    code = """
    subroutine test
        integer, dimension(100) :: a,b
        integer :: i

        do i = 1, 100
          a(i) = i
        end do

        do i = 1, 100
          b(i) = i
        end do

        do i = 1, 100
          b(i) = b(i) + 1
        end do

        do i = 1, 100
          a(i) = a(i) + 1
        end do
    end subroutine
    """

    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    otrans = OMPLoopTrans()
    for loop in psyir.walk(Loop):
        otrans.apply(loop, nowait=True)

    paralleltrans = OMPParallelTrans()
    paralleltrans.apply(routine.children[:])

    # The initial implementation would give 3 barriers (since we add one
    # at the end for safety which is unneccessary but maximises safety).
    assert len(psyir.walk(OMPBarrierDirective)) == 3

    rtrans = OMPRemoveBarrierTrans()

    # Attempt to remove barriers - at the moment we remove the barrier at the
    # end of the scope - maybe we shouldn't? Though its probably fine.
    rtrans.apply(routine)
    assert len(psyir.walk(OMPBarrierDirective)) == 1
    correct = """  !$omp do schedule(auto)
  do i = 1, 100, 1
    a(i) = i
  enddo
  !$omp end do nowait
  !$omp do schedule(auto)
  do i = 1, 100, 1
    b(i) = i
  enddo
  !$omp end do nowait
  !$omp barrier
  !$omp do schedule(auto)
  do i = 1, 100, 1
    b(i) = b(i) + 1
  enddo
  !$omp end do nowait
  !$omp do schedule(auto)
  do i = 1, 100, 1
    a(i) = a(i) + 1
  enddo
  !$omp end do nowait
  !$omp end parallel"""
    out = fortran_writer(psyir)
    assert correct in out
