# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2020, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''Module containing py.test tests for the transformation of the PSy
   representation of NEMO code using the OpenACC 'kernels' directive.

'''

from __future__ import print_function, absolute_import
import pytest

from fparser.common.readfortran import FortranStringReader

from psyclone.psyGen import ACCKernelsDirective, Loop, PSyFactory
from psyclone.psyir.transformations import TransformationError, ProfileTrans
from psyclone.transformations import ACCKernelsTrans, ACCLoopTrans
from psyclone.psyir.nodes import Assignment
from psyclone.errors import GenerationError


# The PSyclone API under test
API = "nemo"

EXPLICIT_LOOP = ("program do_loop\n"
                 "integer :: ji\n"
                 "integer, parameter :: jpj=32\n"
                 "real(kind=wp) :: sto_tmp(jpj)\n"
                 "do ji = 1,jpj\n"
                 "  sto_tmp(ji) = 1.0d0\n"
                 "end do\n"
                 "end program do_loop\n")


def test_kernels_view(parser, capsys):
    ''' Test the ACCKernelsDirective.view() method. '''
    code = parser(FortranStringReader(EXPLICIT_LOOP))
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = ACCKernelsTrans()
    schedule, _ = acc_trans.apply(schedule.children,
                                  {"default_present": True})
    schedule.view()
    output, _ = capsys.readouterr()
    assert "[ACC Kernels]" in output


def test_kernels_dag_name(parser):
    ''' Check that we get the correct name for a DAG node for an OpenACC
    kernels directive. '''
    code = parser(FortranStringReader(EXPLICIT_LOOP))
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = ACCKernelsTrans()
    schedule, _ = acc_trans.apply(schedule.children,
                                  {"default_present": True})
    assert schedule.children[0].dag_name == "ACC_kernels_1"


def test_kernels_single_node(parser):
    ''' Check that we can apply the ACCKernelsTrans to a single node
    instead of to a list of nodes. '''
    code = parser(FortranStringReader(EXPLICIT_LOOP))
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = ACCKernelsTrans()
    acc_trans.apply(schedule[0], {"default_present": True})
    assert isinstance(schedule[0], ACCKernelsDirective)


def test_no_kernels_error(parser):
    ''' Check that the transformation rejects an attempt to put things
    that aren't kernels inside a kernels region. '''
    reader = FortranStringReader("program write_out\n"
                                 "integer :: ji, jpj\n"
                                 "real(kind=wp) :: sto_tmp(5)\n"
                                 "do ji = 1,jpj\n"
                                 "read(*,*) sto_tmp(ji)\n"
                                 "end do\n"
                                 "do ji = 1,jpj\n"
                                 "write(*,*) sto_tmp(ji)\n"
                                 "end do\n"
                                 "end program write_out\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = ACCKernelsTrans()
    with pytest.raises(TransformationError) as err:
        _, _ = acc_trans.apply(schedule.children[0:2],
                               {"default_present": True})
    assert ("cannot be enclosed by a ACCKernelsTrans transformation"
            in str(err.value))


def test_no_loops(parser):
    ''' Check that the transformation refuses to generate a kernels region
    if it contains no loops. '''
    reader = FortranStringReader("program no_loop\n"
                                 "integer :: jpk\n"
                                 "jpk = 30\n"
                                 "end program no_loop\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = ACCKernelsTrans()
    with pytest.raises(TransformationError) as err:
        _, _ = acc_trans.apply(schedule.children[0:1],
                               {"default_present": True})
    assert ("must enclose at least one loop or array range but none were "
            "found" in str(err.value))


def test_implicit_loop(parser):
    ''' Check that the transformation generates correct code when applied
    to an implicit loop. '''
    reader = FortranStringReader("program implicit_loop\n"
                                 "real(kind=wp) :: sto_tmp(5,5)\n"
                                 "sto_tmp(:,:) = 0.0_wp\n"
                                 "end program implicit_loop\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = ACCKernelsTrans()
    schedule, _ = acc_trans.apply(schedule.children[0:1],
                                  {"default_present": True})
    gen_code = str(psy.gen)
    assert ("  !$ACC KERNELS DEFAULT(PRESENT)\n"
            "  sto_tmp(:, :) = 0.0_wp\n"
            "  !$ACC END KERNELS\n" in gen_code)


def test_multikern_if(parser):
    ''' Check that we can include an if-block containing multiple
    loops within a kernels region. '''
    reader = FortranStringReader("program implicit_loop\n"
                                 "logical :: do_this\n"
                                 "integer :: jk\n"
                                 "real(kind=wp) :: sto_tmp(5)\n"
                                 "if(do_this)then\n"
                                 "do jk = 1, 3\n"
                                 "  sto_tmp(jk) = jk\n"
                                 "end do\n"
                                 "else\n"
                                 "do jk = 1, 5\n"
                                 "  sto_tmp(jk) = jk\n"
                                 "end do\n"
                                 "end if\n"
                                 "end program implicit_loop\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = ACCKernelsTrans()
    schedule, _ = acc_trans.apply(schedule.children[0:1],
                                  {"default_present": True})
    gen_code = str(psy.gen).lower()
    assert ("!$acc kernels default(present)\n"
            "  if (do_this) then\n"
            "    do jk = 1, 3\n" in gen_code)
    assert ("    end do\n"
            "  end if\n"
            "  !$acc end kernels\n"
            "end program implicit_loop" in gen_code)


def test_kernels_within_if(parser):
    ''' Check that we can put a kernels region within an if block. '''
    reader = FortranStringReader("program if_then\n"
                                 "logical :: do_this\n"
                                 "integer :: ji, jpi\n"
                                 "real :: fld(:), fld2d(:,:)\n"
                                 "if(do_this)then\n"
                                 "  do ji=1,jpi\n"
                                 "    fld(ji) = 1.0\n"
                                 "  end do\n"
                                 "else\n"
                                 "  fld2d(:,:) = 0.0\n"
                                 "end if\n"
                                 "end program if_then\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = ACCKernelsTrans()

    schedule, _ = acc_trans.apply(schedule.children[0].if_body,
                                  {"default_present": True})
    schedule, _ = acc_trans.apply(schedule.children[0].else_body,
                                  {"default_present": True})
    new_code = str(psy.gen)
    assert ("  IF (do_this) THEN\n"
            "    !$ACC KERNELS DEFAULT(PRESENT)\n"
            "    DO ji = 1, jpi\n" in new_code)
    assert ("    END DO\n"
            "    !$ACC END KERNELS\n"
            "  ELSE\n"
            "    !$ACC KERNELS DEFAULT(PRESENT)\n"
            "    fld2d(:, :) = 0.0\n"
            "    !$ACC END KERNELS\n"
            "  END IF\n" in new_code)


def test_no_code_block_kernels(parser):
    ''' Check that we reject attempts to enclose CodeBlocks within a
    Kernels region. '''
    reader = FortranStringReader("program cb_mix\n"
                                 "  integer :: ji, jpi\n"
                                 "  real :: fld(:)\n"
                                 "  do ji=1,jpi\n"
                                 "    fld(ji) = 1.0\n"
                                 "  end do\n"
                                 "  write(*,*) 'Hello'\n"
                                 "end program cb_mix\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = ACCKernelsTrans()
    with pytest.raises(TransformationError) as err:
        _, _ = acc_trans.apply(schedule.children)
    assert ("'CodeBlock' cannot be enclosed by a ACCKernelsTrans "
            in str(err.value))


def test_no_default_present(parser):
    ''' Check that we can create a kernels region with no 'default(present)'
    clause (as we will want to do when using managed memory). '''
    reader = FortranStringReader(EXPLICIT_LOOP)
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = ACCKernelsTrans()
    _, _ = acc_trans.apply(schedule.children, {"default_present": False})
    gen_code = str(psy.gen)
    assert "!$ACC KERNELS\n" in gen_code


def test_kernels_around_where_construct(parser):
    ''' Check that we can put a WHERE construct inside a KERNELS region. '''
    reader = FortranStringReader("program where_test\n"
                                 "  integer :: flag\n"
                                 "  real :: a(:,:), b(:,:)\n"
                                 "  where (a(:,:) < flag)\n"
                                 "    b(:,:) = 0.0\n"
                                 "  end where\n"
                                 "end program where_test\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = ACCKernelsTrans()
    sched, _ = acc_trans.apply(schedule)
    assert isinstance(sched[0], ACCKernelsDirective)
    assert isinstance(sched[0].dir_body[0], Loop)
    new_code = str(psy.gen)
    assert ("  !$ACC KERNELS\n"
            "  WHERE (a(:, :) < flag)" in new_code)
    assert ("  END WHERE\n"
            "  !$ACC END KERNELS\n" in new_code)


def test_kernels_around_where_stmt(parser):
    ''' Check that we can put a WHERE statement inside a KERNELS region. '''
    reader = FortranStringReader("program where_test\n"
                                 "  integer :: flag\n"
                                 "  real :: a(:,:), b(:,:), c(:,:)\n"
                                 "  a(:,:) = 1.0\n"
                                 "  where (a(:,:) < flag) b(:,:) = 0.0\n"
                                 "  c(:,:) = 1.0\n"
                                 "end program where_test\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = ACCKernelsTrans()
    acc_trans.apply([schedule[1]])
    new_code = str(psy.gen)
    assert ("  a(:, :) = 1.0\n"
            "  !$ACC KERNELS\n"
            "  WHERE (a(:, :) < flag) b(:, :) = 0.0\n"
            "  !$ACC END KERNELS\n"
            "  c(:, :) = 1.0\n" in new_code)


def test_loop_inside_kernels(parser):
    ''' Check that we can put an ACC LOOP directive inside a KERNELS
    region. '''
    code = parser(FortranStringReader(EXPLICIT_LOOP))
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = ACCKernelsTrans()
    acc_trans.apply([schedule[0]])
    loop_trans = ACCLoopTrans()
    loop_trans.apply(schedule[0].dir_body[0])
    output = str(psy.gen).lower()
    assert ("  !$acc kernels\n"
            "  !$acc loop independent\n"
            "  do ji = 1, jpj\n" in output)
    assert ("  end do\n"
            "  !$acc end kernels\n" in output)


def test_two_loops_inside_kernels(parser):
    ''' Check that we can mark-up one or both loops inside a KERNELS
    region containing two loops. '''
    reader = FortranStringReader("program two_loops\n"
                                 "  integer :: ji\n"
                                 "  real :: array(10)\n"
                                 "  do ji = 1, 10\n"
                                 "    array(ji) = 1.0\n"
                                 "  end do\n"
                                 "  do ji = 1, 5\n"
                                 "    array(ji) = 2.0*array(ji)\n"
                                 "  end do\n"
                                 "end program two_loops\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    # Enclose both loops within a KERNELS region
    acc_trans = ACCKernelsTrans()
    acc_trans.apply(schedule[0:2])
    # Apply a loop transformation to just the first loop
    loop_trans = ACCLoopTrans()
    loop_trans.apply(schedule[0].dir_body[0])
    output = str(psy.gen).lower()
    assert ("  !$acc kernels\n"
            "  !$acc loop independent\n"
            "  do ji = 1, 10\n" in output)
    assert ("  end do\n"
            "  !$acc end kernels\n"
            "end program" in output)
    loop_trans.apply(schedule[0].dir_body[1])
    output = str(psy.gen).lower()
    assert ("  !$acc loop independent\n"
            "  do ji = 1, 5\n" in output)
    assert ("  end do\n"
            "  !$acc end kernels\n"
            "end program" in output)


def test_loop_after_implicit_kernels(parser):
    ''' Test the addition of a loop directive after some implicit loops
    within a kernels region. '''
    reader = FortranStringReader("program two_loops\n"
                                 "  integer :: ji\n"
                                 "  real :: array(10,10)\n"
                                 "  array(:,:) = -1.0\n"
                                 "  do ji = 1, 5\n"
                                 "    array(ji,1) = 2.0*array(ji,2)\n"
                                 "  end do\n"
                                 "end program two_loops\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = ACCKernelsTrans()
    loop_trans = ACCLoopTrans()
    acc_trans.apply(schedule[0:2])
    loop_trans.apply(schedule[0].dir_body[1])
    output = str(psy.gen).lower()
    assert ("  !$acc kernels\n"
            "  array(:, :) = - 1.0\n"
            "  !$acc loop independent\n"
            "  do ji = 1, 5\n" in output)
    assert ("  end do\n"
            "  !$acc end kernels\n"
            "end program" in output)


def test_no_psydata_in_kernels(parser, monkeypatch):
    ''' Check that we refuse to generate code when a kernels region
    contains PSyData calls. '''
    code = parser(FortranStringReader(EXPLICIT_LOOP))
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    ptrans = ProfileTrans()
    acc_trans = ACCKernelsTrans()
    acc_trans.apply(schedule[0])
    # Attempt to put a profiling call within the loop
    assign = schedule.walk(Assignment)[0]
    with pytest.raises(TransformationError) as err:
        ptrans.apply(assign)
    assert ("A PSyData node cannot be inserted inside an OpenACC region"
            in str(err.value))
    # Monkeypatch the validate() method so as to avoid the checking
    # that it does
    monkeypatch.setattr(ptrans, "validate", lambda x, y: None)
    ptrans.apply(assign)
    # Check that an appropriate error is raised at code-generation time
    with pytest.raises(GenerationError) as err:
        _ = psy.gen
    assert ("Cannot include calls to PSyData routines within OpenACC "
            "regions" in str(err.value))
