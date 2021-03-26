# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''Performs pytest tests on the support for OpenACC directives in the
   psyclone.psyir.backend.fortran and c modules. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.nodes import Assignment, Reference
from psyclone.psyir.symbols import DataSymbol, REAL_TYPE
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.backend.c import CWriter
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.transformations import (ACCKernelsTrans, ACCDataTrans,
                                      ACCParallelTrans)
from psyclone.nemo import NemoInvokeSchedule
from psyclone.tests.utilities import create_schedule, get_invoke


NEMO_TEST_CODE = '''
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


# ----------------------------------------------------------------------------
def test_nemo_acc_kernels():
    '''Tests if an OpenACC kernels directive in NEMO is handled correctly.
    '''
    # Generate fparser2 parse tree from Fortran code.
    schedule = create_schedule(NEMO_TEST_CODE, "tmp")
    # Currently the creation of PSyIR for the NEMO API does not quite
    # produce the expected structure so replace the root KernelSchedule
    # with a NemoInvokeSchedule so that it is accepted by the transformation.
    # TODO #737
    nemo_sched = NemoInvokeSchedule.create("tmp", schedule.symbol_table,
                                           schedule.pop_all_children())
    # Now apply a kernels transform
    ktrans = ACCKernelsTrans()
    ktrans.apply(nemo_sched[0])

    fvisitor = FortranWriter()
    result = fvisitor(nemo_sched)
    print(result)
    correct = '''!$acc kernels
do i = 1, 20, 2
  a = 2 * i
  b(i) = b(i) + a
enddo
!$acc end kernels'''
    assert correct in result

    cvisitor = CWriter()
    with pytest.raises(VisitorError) as err:
        _ = cvisitor(nemo_sched[0])
    assert "Unsupported node 'ACCKernelsDirective' found" in str(err.value)


# ----------------------------------------------------------------------------
def test_nemo_acc_parallel():
    '''Tests that an OpenACC parallel directive in NEMO is handled correctly.
    '''
    # Generate fparser2 parse tree from Fortran code.
    schedule = create_schedule(NEMO_TEST_CODE, "tmp")
    # Currently the creation of PSyIR for the NEMO API does not quite
    # produce the expected structure so replace the root KernelSchedule
    # with a NemoInvokeSchedule so that it is accepted by the transformation.
    # TODO #737
    nemo_sched = NemoInvokeSchedule.create("tmp", schedule.symbol_table,
                                           schedule.pop_all_children())
    # Now apply an ACC parallel transform
    dtrans = ACCDataTrans()
    ktrans = ACCParallelTrans()

    ktrans.apply(nemo_sched[0])
    dtrans.apply(nemo_sched[0])

    fvisitor = FortranWriter()
    result = fvisitor(nemo_sched)

    correct = '''!$acc data copy(b)
!$acc begin parallel default(present)
do i = 1, 20, 2
  a = 2 * i
  b(i) = b(i) + a
enddo
!$acc end parallel
!$acc end data'''
    assert correct in result

    cvisitor = CWriter()
    with pytest.raises(VisitorError) as err:
        _ = cvisitor(nemo_sched[0])
    assert "Unsupported node 'ACCDataDirective' found" in str(err.value)


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
    assignment.parent = node
    node.children[0] = assignment


# ----------------------------------------------------------------------------
def test_gocean_acc_parallel():
    '''Test that an ACC PARALLEL directive in a 'classical' API (gocean here)
    is created correctly.

    '''
    _, invoke = get_invoke("single_invoke.f90", "gocean1.0",
                           idx=0, dist_mem=False)

    ptrans = ACCParallelTrans()
    sched, _ = ptrans.apply(invoke.schedule[0])

    # Now remove the GOKern (since it's not yet supported in the
    # visitor pattern) and replace it with a simple assignment
    # TODO: #440 tracks this
    replace_child_with_assignment(sched[0].dir_body)

    # omp_sched is a GOInvokeSchedule, which is not yet supported.
    # So only convert starting from the OMPParallelDirective
    fvisitor = FortranWriter()
    result = fvisitor(sched[0])
    correct = '''!$acc begin parallel default(present)
a = b
!$acc end parallel'''
    assert correct in result

    cvisitor = CWriter()
    with pytest.raises(VisitorError) as err:
        _ = cvisitor(sched[0])
    assert "Unsupported node 'ACCParallelDirective' found" in str(err.value)
