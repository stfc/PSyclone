# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2021, Science and Technology Facilities Council.
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
# Author J. Henrichs, Bureau of Meteorology
# Modified by R. W. Ford and S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''Performs pytest tests on the psyclone.psyir.backend.fortran and c module'''

from __future__ import absolute_import

from psyclone.psyir.nodes import Assignment, Reference
from psyclone.psyir.symbols import DataSymbol, REAL_TYPE
from psyclone.psyir.backend.c import CWriter
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import create_schedule, get_invoke


# ----------------------------------------------------------------------------
def test_nemo_omp_parallel():
    '''Tests if an OpenMP parallel directive in NEMO is handled correctly.
    '''
    # Generate fparser2 parse tree from Fortran code.
    code = '''
        module test
        contains
        subroutine tmp()
          integer :: i, a
          integer, dimension(:) :: b
          do i = 1, 20, 2
            a = 2 * i
            b(i) = b(i) + a
          enddo
        end subroutine tmp
        end module test'''
    schedule = create_schedule(code, "tmp")
    from psyclone.transformations import OMPParallelTrans

    # Now apply a parallel transform
    omp_par = OMPParallelTrans()
    # Note that the loop is not handled as nemo kernel, so the
    # omp node-type-check will find the assignment statement and
    # prevent application of omp parallel to the loop. So
    # disable the node type check so that omp parallel is applied.
    omp_par.apply(schedule[0], {"node-type-check": False})

    fvisitor = FortranWriter()
    result = fvisitor(schedule)
    correct = '''!$omp parallel default(shared) private(a,i)
  do i = 1, 20, 2
    a = 2 * i
    b(i) = b(i) + a
  enddo
  !$omp end parallel'''
    assert correct in result

    cvisitor = CWriter()
    result = cvisitor(schedule[0])
    correct = '''#pragma omp parallel default(shared) private(a,i)
{
  for(i=1; i<=20; i+=2)
  {
    a = (2 * i);
    b[i] = (b[i] + a);
  }
}'''
    result = cvisitor(schedule[0])
    assert correct in result


# ----------------------------------------------------------------------------
def replace_child_with_assignment(node):
    '''Since at this stage not all node types are supported,
    this function is used to replace the first child of the
    given node with a simple assignment statement ('a=b').
    This allows all tests to compare all output of the visitor
    pattern (even though in some cases the code might not
    compile, e.g. assignment as child of an OMP DO directive)
    # TODO #440 tracks this
    :param node: the node whose child is replaced.
    :type node: :py:class:`psyclone.psyir.nodes.Node`
    '''

    # Create a simple 'a=b' assignment statement for all tests
    lhs = Reference(DataSymbol('a', REAL_TYPE))
    rhs = Reference(DataSymbol('b', REAL_TYPE))
    assignment = Assignment.create(lhs, rhs)
    node.children[0] = assignment


# ----------------------------------------------------------------------------
def test_gocean_omp_parallel():
    '''Test that an OMP PARALLEL directive in a 'classical' API (gocean here)
    is created correctly.
    '''

    from psyclone.transformations import OMPParallelTrans

    _, invoke = get_invoke("single_invoke.f90", "gocean1.0",
                           idx=0, dist_mem=False)

    omp = OMPParallelTrans()
    _, _ = omp.apply(invoke.schedule[0])

    # Now remove the GOKern (since it's not yet supported in the
    # visitor pattern) and replace it with a simple assignment
    # TODO: #440 tracks this
    replace_child_with_assignment(invoke.schedule[0].dir_body)

    # omp_sched is a GOInvokeSchedule, which is not yet supported.
    # So only convert starting from the OMPParallelDirective
    fvisitor = FortranWriter()
    result = fvisitor(invoke.schedule[0])
    correct = '''!$omp parallel default(shared)
a = b
!$omp end parallel'''
    assert correct in result

    cvisitor = CWriter()
    # Remove newlines for easier RE matching
    result = cvisitor(invoke.schedule[0])
    correct = '''#pragma omp parallel default(shared)
{
  a = b;
}'''
    result = cvisitor(invoke.schedule[0])
    assert correct in result


# ----------------------------------------------------------------------------
def test_nemo_omp_do():
    '''Tests if an OpenMP do directive in NEMO is handled correctly.
    '''
    # Generate fparser2 parse tree from Fortran code.
    code = '''
        module test
        contains
        subroutine tmp()
          integer :: i, a
          integer, dimension(:) :: b
          do i = 1, 20, 2
            a = 2 * i
            b(i) = b(i) + a
          enddo
        end subroutine tmp
        end module test'''
    schedule = create_schedule(code, "tmp")
    from psyclone.transformations import OMPLoopTrans

    # Now apply a parallel transform
    omp_loop = OMPLoopTrans()
    omp_loop.apply(schedule[0])

    fvisitor = FortranWriter()
    result = fvisitor(schedule)
    correct = '''  !$omp do schedule(static)
  do i = 1, 20, 2
    a = 2 * i
    b(i) = b(i) + a
  enddo
  !$omp end do'''
    assert correct in result

    cvisitor = CWriter()
    result = cvisitor(schedule[0])
    correct = '''#pragma omp do schedule(static)
{
  for(i=1; i<=20; i+=2)
  {
    a = (2 * i);
    b[i] = (b[i] + a);
  }
}'''
    assert correct in result


# ----------------------------------------------------------------------------
def test_gocean_omp_do():
    '''Test that an OMP DO directive in a 'classical' API (gocean here)
    is created correctly.
    '''

    from psyclone.transformations import OMPLoopTrans

    _, invoke = get_invoke("single_invoke.f90", "gocean1.0",
                           idx=0, dist_mem=False)
    omp = OMPLoopTrans()
    _, _ = omp.apply(invoke.schedule[0])

    # Now remove the GOKern (since it's not yet supported in the
    # visitor pattern) and replace it with a simple assignment.
    # While this is invalid usage of OMP (omp must have a loop,
    # not an assignment inside), it is necessary because GOLoops
    # are not supported yet, and it is sufficient to test that the
    # visitor pattern creates correct OMP DO directives.
    # TODO #440 fixes this.
    replace_child_with_assignment(invoke.schedule[0].dir_body)
    fvisitor = FortranWriter()
    # GOInvokeSchedule is not yet supported, so start with
    # the OMP node:
    result = fvisitor(invoke.schedule[0])
    correct = '''!$omp do schedule(static)
a = b
!$omp end do'''
    assert correct in result

    cvisitor = CWriter()
    # Remove newlines for easier RE matching
    result = cvisitor(invoke.schedule[0])
    correct = '''#pragma omp do schedule(static)
{
  a = b;
}'''
    assert correct in result
