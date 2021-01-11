# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2020, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab

''' Module containing py.test tests for the transformation of
    the PSy representation of NEMO code '''

from __future__ import print_function, absolute_import
import pytest
from fparser.common.readfortran import FortranStringReader
from psyclone.psyGen import TransInfo, PSyFactory
from psyclone.errors import InternalError, GenerationError
from psyclone.tests.utilities import get_invoke
from psyclone import nemo
from psyclone.transformations import OMPLoopTrans, OMPParallelTrans
from psyclone.psyGen import OMPDoDirective

# Constants
API = "nemo"


def test_omp_explicit_gen():
    ''' Check code generation for a single explicit loop containing
    a kernel. '''
    psy, invoke_info = get_invoke("explicit_do.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    omp_trans = TransInfo().get_trans_name('OMPParallelLoopTrans')

    for loop in schedule.loops():
        kernel = loop.kernel
        if kernel and loop.loop_type == "levels":
            schedule, _ = omp_trans.apply(loop)
    gen_code = str(psy.gen).lower()

    expected = (
        "program explicit_do\n"
        "  implicit none\n"
        "  integer :: ji, jj, jk\n"
        "  integer, parameter :: jpi = 2, jpj = 4, jpk = 6\n"
        "  real :: r\n"
        "  real, dimension(jpi, jpj, jpk) :: umask\n"
        "  !$omp parallel do default(shared), private(ji,jj,jk), "
        "schedule(static)\n"
        "  do jk = 1, jpk\n"
        "    do jj = 1, jpj\n"
        "      do ji = 1, jpi\n"
        "        umask(ji, jj, jk) = ji * jj * jk / r\n"
        "      end do\n"
        "    end do\n"
        "  end do\n"
        "  !$omp end parallel do\n"
        "end program explicit_do")
    assert expected in gen_code
    # Check that calling gen a second time gives the same code
    gen_code = str(psy.gen).lower()
    assert expected in gen_code


def test_omp_private_declaration():
    ''' Check code generation and private/shared declaration when
    an assignment is parallelised. In this case the code is like:
    !$omp parallel default(shared), private()
    jpk = 100
    do k=1, jpk ...
    enddo
    !$omp end parallel
    do k=1, jpk ...

    In this case jpk should not be declared private, since then it
    is not defined in the next loop.'''

    psy, invoke_info = get_invoke("explicit_do_two_loops.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    omp_parallel = TransInfo().get_trans_name('OMPParallelTrans')

    # Apply "omp parallel" around one assignment to a scalar variable
    # and a loop using this variable as loop boundary. Parallelising an
    # assignment statement is not allowed by default, so we need to disable
    # the node type check in order to apply the omp parallel transform.
    omp_parallel.apply(schedule.children[0:2], {'node-type-check': False})
    expected = "!$omp parallel default(shared), private(ji,jj,jk)"

    gen_code = str(psy.gen).lower()
    assert expected in gen_code


def test_omp_parallel():
    ''' Check insertion of an OpenMP parallel region containing a single,
    explicit loop. '''
    otrans = OMPParallelTrans()
    psy, invoke_info = get_invoke("explicit_do.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    schedule, _ = otrans.apply([schedule[0]])
    gen_code = str(psy.gen).lower()
    assert ("  !$omp parallel default(shared), private(ji,jj,jk)\n"
            "  do jk = 1, jpk\n"
            "    do jj = 1, jpj\n"
            "      do ji = 1, jpi\n"
            "        umask(ji, jj, jk) = ji * jj * jk / r\n"
            "      end do\n"
            "    end do\n"
            "  end do\n"
            "  !$omp end parallel\n" in gen_code)


def test_omp_add_region_invalid_data_move():
    ''' Check that _add_region() raises the expected error if an invalid
    value for data_movement is supplied. '''
    otrans = OMPParallelTrans()
    _, invoke_info = get_invoke("explicit_do.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    schedule, _ = otrans.apply([schedule[0]])
    ompdir = schedule[0]
    with pytest.raises(InternalError) as err:
        ompdir._add_region("DATA", "END DATA", data_movement="analyse")
    assert ("the data_movement='analyse' option is only valid for an "
            "OpenACC directive" in str(err.value))


def test_omp_parallel_multi():
    ''' Check insertion of an OpenMP parallel region containing more than
    one node. '''
    from psyclone.psyGen import OMPParallelDirective
    otrans = OMPParallelTrans()
    psy, invoke_info = get_invoke("imperfect_nest.f90", api=API, idx=0)
    schedule = invoke_info.schedule

    # Apply the OMP Parallel transformation so as to enclose the last two
    # loop nests (Python's slice notation is such that the expression below
    # gives elements 2-3).
    new_sched, _ = otrans.apply(schedule[0].loop_body[2:4])
    gen_code = str(psy.gen).lower()
    assert ("    !$omp parallel default(shared), private(ji,jj,zabe1,zcof1,"
            "zmsku)\n"
            "    do jj = 1, jpjm1\n"
            "      do ji = 1, jpim1\n"
            "        zabe1 = pahu(ji, jj, jk) * e2_e1u(ji, jj) * "
            "e3u_n(ji, jj, jk)\n" in gen_code)
    assert ("    do jj = 2, jpjm1\n"
            "      do ji = 2, jpim1\n"
            "        pta(ji, jj, jk, jn) = pta(ji, jj, jk, jn) + "
            "zsign * (zftu(ji, jj, jk) - zftu(ji - 1, jj, jk) + "
            "zftv(ji, jj, jk) - zftv(ji, jj - 1, jk)) * r1_e1e2t(ji, jj) / "
            "e3t_n(ji, jj, jk)\n"
            "      end do\n"
            "    end do\n"
            "    !$omp end parallel\n" in gen_code)
    directive = new_sched[0].loop_body[2]
    assert isinstance(directive, OMPParallelDirective)

    # Check that further calls to the update() method don't change the
    # stored AST.
    old_ast = directive.ast
    directive.update()
    assert old_ast is directive.ast


def test_omp_do_missing_region(parser):
    ''' Check that the correct error is raised if an OMPDoDirective is
    found outside an OMP parallel region at code-generation time. '''
    reader = FortranStringReader("program do_loop\n"
                                 "integer :: ji\n"
                                 "integer, parameter :: jpj=32\n"
                                 "real :: sto_tmp(jpj)\n"
                                 "do ji = 1,jpj\n"
                                 "  sto_tmp(ji) = 1.0d0\n"
                                 "end do\n"
                                 "end program do_loop\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    loop_trans = OMPLoopTrans()
    loop_trans.apply(schedule[0])
    with pytest.raises(GenerationError) as err:
        str(psy.gen)
    assert ("OMPDoDirective must be inside an OMP parallel region but could "
            "not find an ancestor OMPParallelDirective" in str(err.value))


def test_omp_do_update():
    '''Check the OMPDoDirective update function.'''
    psy, invoke = get_invoke("imperfect_nest.f90", api=API, idx=0)
    schedule = invoke.schedule
    par_trans = OMPParallelTrans()
    loop_trans = OMPLoopTrans()
    new_sched, _ = par_trans.apply(schedule[0].loop_body[1]
                                   .else_body[0].else_body[0])
    new_sched, _ = loop_trans.apply(new_sched[0].loop_body[1]
                                    .else_body[0].else_body[0].dir_body[0])
    gen_code = str(psy.gen).lower()
    correct = '''      !$omp parallel default(shared), private(ji,jj)
      !$omp do schedule(static)
      do jj = 1, jpj, 1
        do ji = 1, jpi, 1
          zdkt(ji, jj) = (ptb(ji, jj, jk - 1, jn) - ptb(ji, jj, jk, jn)) * \
wmask(ji, jj, jk)
        end do
      end do
      !$omp end do
      !$omp end parallel'''
    assert correct in gen_code
    directive = new_sched[0].loop_body[1].else_body[0].else_body[0]\
        .dir_body[0]
    assert isinstance(directive, OMPDoDirective)

    # Call update a second time and make sure that this does not
    # trigger the whole update process again, and we get the same ast
    old_ast = directive.ast
    directive.update()
    assert directive.ast is old_ast

    # Remove the existing AST, so we can do more tests:
    directive.ast = None
    # Make the schedule invalid by adding a second child to the
    # OMPParallelDoDirective
    directive.dir_body.children.append(new_sched[0].loop_body[3])

    with pytest.raises(GenerationError) as err:
        _ = directive.update()
    assert ("An OpenMP DO can only be applied to a single loop but "
            "this Node has 2 children:" in str(err.value))


def test_omp_parallel_errs():
    ''' Check that we raise the expected errors when incorrectly attempting
    to add an OpenMP parallel region containing more than one node. '''
    otrans = OMPParallelTrans()
    psy, invoke_info = get_invoke("imperfect_nest.f90", api=API, idx=0)
    schedule = invoke_info.schedule

    # Apply the OMP Parallel transformation so as to enclose the last two
    # loop nests (Python's slice notation is such that the expression below
    # gives elements 2-3).
    new_sched, _ = otrans.apply(schedule[0].loop_body[2:4])
    directive = new_sched[0].loop_body[2]
    # Break the AST by deleting some of it
    _ = new_sched[0].ast.content.remove(directive.children[0].ast)
    with pytest.raises(InternalError) as err:
        _ = psy.gen
    assert ("Failed to find locations to insert begin/end directives" in
            str(err.value))


def test_omp_do_children_err():
    ''' Tests that we raise the expected error when an OpenMP parallel do
    directive has more than one child. '''
    from psyclone.transformations import OMPParallelLoopTrans
    from psyclone.psyGen import OMPParallelDoDirective
    otrans = OMPParallelLoopTrans()
    psy, invoke_info = get_invoke("imperfect_nest.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    new_sched, _ = otrans.apply(schedule[0].loop_body[2])
    directive = new_sched[0].loop_body[2]
    assert isinstance(directive, OMPParallelDoDirective)
    # Make the schedule invalid by adding a second child to the
    # OMPParallelDoDirective
    directive.dir_body.children.append(new_sched[0].loop_body[3])
    with pytest.raises(GenerationError) as err:
        _ = psy.gen
    assert ("An OpenMP PARALLEL DO can only be applied to a single loop but "
            "this Node has 2 children:" in str(err.value))


def test_omp_do_within_if():
    ''' Check that we can insert an OpenMP parallel do within an if block. '''
    from psyclone.transformations import OMPParallelLoopTrans
    otrans = OMPParallelLoopTrans()
    psy, invoke_info = get_invoke("imperfect_nest.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    loop = schedule[0].loop_body[1].else_body[0].else_body[0]
    assert isinstance(loop, nemo.NemoLoop)
    # Apply the transformation to a loop within an else clause
    schedule, _ = otrans.apply(loop)
    gen = str(psy.gen).lower()
    expected = (
        "    else\n"
        "      !$omp parallel do default(shared), private(ji,jj), "
        "schedule(static)\n"
        "      do jj = 1, jpj, 1\n"
        "        do ji = 1, jpi, 1\n"
        "          zdkt(ji, jj) = (ptb(ji, jj, jk - 1, jn) - "
        "ptb(ji, jj, jk, jn)) * wmask(ji, jj, jk)\n"
        "        end do\n"
        "      end do\n"
        "      !$omp end parallel do\n"
        "    end if\n")
    assert expected in gen
