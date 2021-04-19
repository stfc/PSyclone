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
from fparser.common.readfortran import FortranStringReader
from psyclone.psyGen import PSyFactory, TransInfo, Directive
from psyclone.psyir.nodes import Assignment, Reference, Loop
from psyclone.psyir.symbols import DataSymbol, REAL_TYPE
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.backend.c import CWriter
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.transformations import (ACCKernelsTrans, ACCDataTrans,
                                      ACCParallelTrans)
from psyclone.tests.utilities import get_invoke


NEMO_TEST_CODE = '''
module test
contains
subroutine tmp()
  integer :: i, a
  integer, dimension(:) :: b, c, d
  do i = 1, 20, 2
    a = 2 * i + d(i)
    c(i) = a
    b(i) = b(i) + a + c(i)
  enddo
end subroutine tmp
end module test'''

DOUBLE_LOOP = ("program do_loop\n"
               "use kind_params, only: wp\n"
               "integer :: ji, jj\n"
               "integer, parameter :: jpi=16, jpj=16\n"
               "real(kind=wp) :: sto_tmp(jpi, jpj)\n"
               "do jj = 1,jpj\n"
               "  do ji = 1,jpi\n"
               "    sto_tmp(ji, jj) = 1.0d0\n"
               "  end do\n"
               "end do\n"
               "end program do_loop\n")


# ----------------------------------------------------------------------------
def test_acc_data_region(parser):
    ''' Test that an ACCDataDirective node generates the expected code. '''
    # Generate fparser2 parse tree from Fortran code.
    reader = FortranStringReader(NEMO_TEST_CODE)
    code = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(code)
    sched = psy.invokes.invoke_list[0].schedule
    dtrans = ACCDataTrans()
    dtrans.apply(sched)
    fvisitor = FortranWriter()
    result = fvisitor(sched)
    assert ("  !$acc data copyin(d) copyout(c) copy(b)\n"
            "  do i = 1, 20, 2\n" in result)
    assert ("  enddo\n"
            "  !$acc end data\n" in result)
    assigns = sched.walk(Assignment)
    # Remove the read from array 'd'
    assigns[0].detach()
    result = fvisitor(sched)
    assert ("  !$acc data copyout(c) copy(b)\n"
            "  do i = 1, 20, 2\n" in result)
    # Remove the readwrite of array 'b'
    assigns[2].detach()
    result = fvisitor(sched)
    assert ("  !$acc data copyout(c)\n"
            "  do i = 1, 20, 2\n" in result)


# ----------------------------------------------------------------------------
def test_acc_data_region_no_struct(parser):
    '''
    Test that we refuse to generate code if a data region includes references
    to structures.
    '''
    reader = FortranStringReader('''
module test
  use some_mod, only: grid_type
  type(grid_type) :: grid
contains
subroutine tmp()
  integer :: i
  integer, dimension(:) :: b

  do i = 1, 20, 2
    b(i) = b(i) + i + grid%flag
    grid%data(i) = i
  enddo
end subroutine tmp
end module test''')
    code = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(code)
    sched = psy.invokes.invoke_list[0].schedule
    dtrans = ACCDataTrans()
    dtrans.apply(sched)
    fvisitor = FortranWriter()
    with pytest.raises(NotImplementedError) as err:
        _ = fvisitor(sched)
    # Allow for the vagaries of py2 output versus py3
    err_msg = str(err.value).replace("u'", "'")
    assert ("Structure (derived-type) references are not yet supported within "
            "OpenACC data regions but found: ['grid%flag', 'grid%data(i)']" in
            err_msg)


# ----------------------------------------------------------------------------
@pytest.mark.parametrize("default_present, expected",
                         [(True, " default(present)"), (False, "")])
def test_nemo_acc_kernels(default_present, expected, parser):
    '''
    Tests that an OpenACC kernels directive is handled correctly in the
    NEMO API.
    '''
    # Generate fparser2 parse tree from Fortran code.
    reader = FortranStringReader(NEMO_TEST_CODE)
    code = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(code)
    nemo_sched = psy.invokes.invoke_list[0].schedule

    # Now apply a kernels transform
    ktrans = ACCKernelsTrans()
    options = {"default_present": default_present}
    ktrans.apply(nemo_sched[0], options)

    fvisitor = FortranWriter()
    result = fvisitor(nemo_sched)
    correct = '''  !$acc kernels{0}
  do i = 1, 20, 2
    a = 2 * i + d(i)
    c(i) = a
    b(i) = b(i) + a + c(i)
  enddo
  !$acc end kernels'''.format(expected)
    assert correct in result

    cvisitor = CWriter()
    with pytest.raises(VisitorError) as err:
        _ = cvisitor(nemo_sched[0])
    assert "Unsupported node 'ACCKernelsDirective' found" in str(err.value)


# ----------------------------------------------------------------------------
def test_nemo_acc_parallel(parser):
    '''Tests that an OpenACC parallel directive in NEMO is handled correctly.
    '''
    # Generate fparser2 parse tree from Fortran code.
    reader = FortranStringReader(NEMO_TEST_CODE)
    code = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(code)
    nemo_sched = psy.invokes.invoke_list[0].schedule

    # Now apply an ACC parallel transform
    dtrans = ACCDataTrans()
    ktrans = ACCParallelTrans()

    ktrans.apply(nemo_sched[0])
    dtrans.apply(nemo_sched[0])

    fort_writer = FortranWriter()
    result = fort_writer(nemo_sched)

    correct = '''  !$acc begin parallel default(present)
  do i = 1, 20, 2
    a = 2 * i + d(i)
    c(i) = a
    b(i) = b(i) + a + c(i)
  enddo
  !$acc end parallel'''
    assert correct in result

    cvisitor = CWriter()
    with pytest.raises(VisitorError) as err:
        _ = cvisitor(nemo_sched[0])
    assert "Unsupported node 'ACCDataDirective' found" in str(err.value)


# ----------------------------------------------------------------------------
def test_acc_loop(parser):
    ''' Tests that an OpenACC loop directive is handled correctly. '''
    reader = FortranStringReader(DOUBLE_LOOP)
    code = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ACCLoopTrans')
    # An ACC Loop must be within a KERNELS or PARALLEL region
    kernels_trans = TransInfo().get_trans_name('ACCKernelsTrans')
    kernels_trans.apply(schedule.children)
    loops = schedule[0].walk(Loop)
    _ = acc_trans.apply(loops[0], {"sequential": True})
    fort_writer = FortranWriter()
    result = fort_writer(schedule)
    assert ("  !$acc kernels\n"
            "  !$acc loop seq\n"
            "  do jj = 1, jpj, 1\n" in result)
    loop_dir = loops[0].ancestor(Directive)
    # Rather than keep apply the transformation with different options,
    # change the internal state of the Directive directly.
    loop_dir._sequential = False
    result = fort_writer(schedule)
    assert ("  !$acc kernels\n"
            "  !$acc loop independent\n"
            "  do jj = 1, jpj, 1\n" in result)
    loop_dir._collapse = 2
    result = fort_writer(schedule)
    assert ("  !$acc kernels\n"
            "  !$acc loop independent collapse(2)\n"
            "  do jj = 1, jpj, 1\n" in result)
    loop_dir._independent = False
    result = fort_writer(schedule)
    assert ("  !$acc kernels\n"
            "  !$acc loop collapse(2)\n"
            "  do jj = 1, jpj, 1\n" in result)
    loop_dir._collapse = None
    result = fort_writer(schedule)
    assert ("  !$acc kernels\n"
            "  !$acc loop\n"
            "  do jj = 1, jpj, 1\n" in result)


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
