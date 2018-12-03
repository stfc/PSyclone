# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2018, Science and Technology Facilities Council.
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
# Authors: R. Ford and A. R. Porter, STFC Daresbury Lab

''' Module containing py.test tests for the transformation of
    the PSy representation of NEMO code '''

from __future__ import print_function, absolute_import
import os
import pytest
from fparser.two import Fortran2003
from fparser.common.readfortran import FortranStringReader
from psyclone.parse import parse
from psyclone.psyGen import PSyFactory, TransInfo, InternalError, \
    GenerationError
from psyclone.transformations import TransformationError
from psyclone import nemo

# Constants
API = "nemo"
# Location of the Fortran files associated with these tests
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")


def test_omp_explicit_gen():
    ''' Check code generation for a single explicit loop containing
    a kernel. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "explicit_do.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.get('explicit_do').schedule
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
        "  integer :: jpi, jpj, jpk\n"
        "  real, dimension(jpi, jpj, jpk) :: umask\n"
        "\n"
        "  ! test code with explicit nemo-style do loop\n"
        "  !$omp parallel do default(shared), private(jk,jj,ji), "
        "schedule(static)\n"
        "  do jk = 1, jpk\n"
        "    do jj = 1, jpj\n"
        "      do ji = 1, jpi\n"
        "        umask(ji, jj, jk) = ji * jj * jk / r\n"
        "      end do\n"
        "    end do\n"
        "  end do\n"
        "  !$omp end parallel do\n"
        "\n"
        "end program explicit_do")
    assert expected in gen_code
    # Check that calling gen a second time gives the same code
    gen_code = str(psy.gen).lower()
    assert expected in gen_code


def test_omp_parallel():
    ''' Check insertion of an OpenMP parallel region containing a single,
    explicit loop. '''
    from psyclone.transformations import OMPParallelTrans
    otrans = OMPParallelTrans()
    _, invoke_info = parse(os.path.join(BASE_PATH, "explicit_do.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.get('explicit_do').schedule
    schedule, _ = otrans.apply([schedule.children[0]])
    gen_code = str(psy.gen).lower()
    assert ("  !$omp parallel default(shared), private(jk,jj,ji)\n"
            "  do jk = 1, jpk\n"
            "    do jj = 1, jpj\n"
            "      do ji = 1, jpi\n"
            "        umask(ji, jj, jk) = ji * jj * jk / r\n"
            "      end do\n"
            "    end do\n"
            "  end do\n"
            "  !$omp end parallel\n" in gen_code)


def test_omp_parallel_multi():
    ''' Check insertion of an OpenMP parallel region containing more than
    one node. '''
    from psyclone.transformations import OMPParallelTrans
    from psyclone.psyGen import OMPParallelDirective
    otrans = OMPParallelTrans()
    _, invoke_info = parse(os.path.join(BASE_PATH, "imperfect_nest.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.get('imperfect_nest').schedule
    schedule.view()
    # Apply the OMP Parallel transformation so as to enclose the last two
    # loop nests (Python's slice notation is such that the expression below
    # gives elements 2-3).
    new_sched, _ = otrans.apply(schedule.children[0].children[2:4])
    new_sched.view()
    gen_code = str(psy.gen).lower()
    assert ("    !$omp parallel default(shared), private(jj,ji)\n"
            "    do jj = 1, jpjm1\n"
            "      do ji = 1, fs_jpim1\n"
            "        zabe1 = pahu(ji, jj, jk) * e2_e1u(ji, jj) * "
            "e3u_n(ji, jj, jk)\n" in gen_code)
    assert ("    do jj = 2, jpjm1\n"
            "      do ji = fs_2, fs_jpim1\n"
            "        pta(ji, jj, jk, jn) = pta(ji, jj, jk, jn) + "
            "zsign * (zftu(ji, jj, jk) - zftu(ji - 1, jj, jk) + "
            "zftv(ji, jj, jk) - zftv(ji, jj - 1, jk)) * r1_e1e2t(ji, jj) / "
            "e3t_n(ji, jj, jk)\n"
            "      end do\n"
            "    end do\n"
            "    !$omp end parallel\n" in gen_code)
    # Check that further calls to the update() method don't change the
    # stored AST.
    directive = new_sched.children[0].children[2]
    assert isinstance(directive, OMPParallelDirective)
    old_ast = directive._ast
    directive.update()
    assert old_ast is directive._ast


def test_omp_parallel_errs():
    ''' Check that we raise the expected errors when incorrectly attempting
    to add an OpenMP parallel region containing more than one node. '''
    from psyclone.transformations import OMPParallelTrans
    otrans = OMPParallelTrans()
    _, invoke_info = parse(os.path.join(BASE_PATH, "imperfect_nest.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.get('imperfect_nest').schedule
    schedule.view()
    # Apply the OMP Parallel transformation so as to enclose the last two
    # loop nests (Python's slice notation is such that the expression below
    # gives elements 2-3).
    new_sched, _ = otrans.apply(schedule.children[0].children[2:4])
    directive = new_sched.children[0].children[2]
    # Break the AST by deleting some of it
    _ = new_sched.children[0]._ast.content.remove(directive._children[0]._ast)
    with pytest.raises(InternalError) as err:
        _ = psy.gen
    assert ("Failed to find locations to insert begin/end directives" in
            str(err))


def test_omp_do_children_err():
    ''' Tests that we raise the expected error when an OpenMP parallel do
    directive has more than one child. '''
    from psyclone.transformations import OMPParallelLoopTrans
    from psyclone.psyGen import OMPParallelDoDirective
    otrans = OMPParallelLoopTrans()
    _, invoke_info = parse(os.path.join(BASE_PATH, "imperfect_nest.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.get('imperfect_nest').schedule
    new_sched, _ = otrans.apply(schedule.children[0].children[2])
    directive = new_sched.children[0].children[2]
    assert isinstance(directive, OMPParallelDoDirective)
    # Make the schedule invalid by adding a second child to the
    # OMPParallelDoDirective
    directive.children.append(new_sched.children[0].children[3])
    with pytest.raises(GenerationError) as err:
        _ = psy.gen
    assert ("An OpenMP PARALLEL DO can only be applied to a single loop but "
            "this Node has 2 children:" in str(err))


def test_omp_do_missing_parent(monkeypatch):
    ''' Check that we raise the expected error when we cannot find the
    parent node in the fparser2 AST. '''
    from psyclone.transformations import OMPParallelLoopTrans
    otrans = OMPParallelLoopTrans()
    _, invoke_info = parse(os.path.join(BASE_PATH, "imperfect_nest.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.get('imperfect_nest').schedule
    schedule, _ = otrans.apply(schedule.children[0])
    # Remove the reference to the fparser2 AST from the Schedule node
    monkeypatch.setattr(schedule, "_ast", None)
    with pytest.raises(InternalError) as err:
        _ = psy.gen
    assert ("Failed to find parent node in which to insert OpenMP parallel "
            "do directive" in str(err))


def test_omp_do_within_if():
    ''' Check that we can insert an OpenMP parallel do within an if block. '''
    from psyclone.transformations import OMPParallelLoopTrans
    otrans = OMPParallelLoopTrans()
    _, invoke_info = parse(os.path.join(BASE_PATH, "imperfect_nest.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.get('imperfect_nest').schedule
    # Apply the transformation to a loop within an else clause
    schedule, _ = otrans.apply(schedule.children[0].children[1].
                               children[2].children[0])
    gen = str(psy.gen)
    expected = (
        "    ELSE\n"
        "      !$omp parallel do default(shared), private(jj,ji), "
        "schedule(static)\n"
        "      DO jj = 1, jpj\n"
        "        DO ji = 1, jpi\n"
        "          zdkt(ji, jj) = (ptb(ji, jj, jk - 1, jn) - "
        "ptb(ji, jj, jk, jn)) * wmask(ji, jj, jk)\n"
        "        END DO\n"
        "      END DO\n"
        "      !$omp end parallel do\n"
        "    END IF\n")
    assert expected in gen


@pytest.mark.xfail(reason="Explicit loop transformation not implemented.")
def test_implicit_loop_sched1():
    ''' Check that we get the correct schedule for an implicit loop '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "implicit_do.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    exp_trans = TransInfo().get_trans_name('NemoExplicitLoopTrans')
    assert isinstance(psy, nemo.NemoPSy)
    sched = psy.invokes.invoke_list[0].schedule
    assert isinstance(sched.children[0], nemo.NemoImplicitLoop)
    sched.view()
    new_sched, _ = exp_trans.apply(sched.children[0])
    loops = sched.walk(sched.children, nemo.NemoLoop)
    assert len(loops) == 3
    kerns = sched.kern_calls()
    assert len(kerns) == 1


@pytest.mark.xfail(reason="Explicit loop transformation not implemented.")
def test_implicit_loop_sched2():
    ''' Check that we get the correct schedule for an explicit loop over
    levels containing an implicit loop over the i-j slab '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "explicit_over_implicit.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    exp_trans = TransInfo().get_trans_name('NemoExplicitLoopTrans')
    sched = psy.invokes.invoke_list[0].schedule
    sched.view()
    new_sched, _ = exp_trans.apply(sched.children[0])
    # We should have 3 loops (one from the explicit loop over levels and
    # the other two from the implicit loops over ji and jj).
    loops = sched.walk(sched.children, nemo.NemoLoop)
    assert len(loops) == 3
    kerns = sched.kern_calls()
    assert len(kerns) == 1


@pytest.mark.xfail(reason="Test needs updating once transformation "
                   "implemented")
def test_unrecognised_implicit():
    ''' Check that we raise the expected error if we encounter an
    unrecognised form of implicit loop. '''
    from psyclone.nemo import NemoImplicitLoop, NemoInvoke
    from fparser.two.parser import ParserFactory
    from fparser.two.utils import walk_ast
    exp_trans = TransInfo().get_trans_name('NemoExplicitLoopTrans')
    # Array syntax used in an unsupported index location
    parser = ParserFactory().create()
    reader = FortranStringReader("program test_prog\n"
                                 "real, dimension(3,3,3,3) :: umask\n"
                                 "umask(:, :, :, :) = 0.0D0\n"
                                 "end program test_prog\n")
    prog = parser(reader)
    psy = PSyFactory(API).create(prog)
    sched = psy.invokes.invoke_list[0].schedule
    with pytest.raises(TransformationError) as err:
        exp_trans.apply(sched.children[0])
    assert ("Array section in unsupported dimension (4) for code "
            "'umask(:, :, :, :) = 0.0D0'" in str(err))
    # and now for the case where the Program unit doesn't have a
    # specification section to modify. This is hard to trigger
    # so we manually construct some objects and put them together
    # to create an artificial example...
    reader = FortranStringReader("umask(:, :, :) = 0.0D0")
    assign = Fortran2003.Assignment_Stmt(reader)
    reader = FortranStringReader("program atest\nreal :: umask(1,1,1,1)\n"
                                 "umask(:, :, :) = 0.0\nend program atest")
    prog = Fortran2003.Program_Unit(reader)
    invoke = NemoInvoke(prog, name="atest")
    loop = NemoImplicitLoop.__new__(NemoImplicitLoop)
    loop._parent = None
    loop.invoke = invoke
    loop.root.invoke._ast = prog
    spec = walk_ast(prog.content, [Fortran2003.Specification_Part])
    prog.content.remove(spec[0])
    with pytest.raises(InternalError) as err:
        loop.__init__(assign)
    assert "No specification part found for routine atest" in str(err)


@pytest.mark.xfail(reason="Error will be raised by new transformation")
def test_implicit_range_err():
    ''' Check that we raise the expected error if we encounter an implicit
    loop with an explicit range (since we don't yet support that). '''
    # Array syntax with an explicit range
    reader = FortranStringReader("umask(1:jpi, 1, :) = 0.0D0")
    assign = Fortran2003.Assignment_Stmt(reader)
    with pytest.raises(NotImplementedError) as err:
        nemo.NemoImplicitLoop(assign)
    assert ("Support for implicit loops with specified bounds is not yet "
            "implemented: 'umask(1 : jpi, 1, :) = 0.0D0'" in str(err))


@pytest.mark.xfail(reason="Error will be raised by new transformation")
def test_implicit_loop_different_rank():
    ''' Test that we reject implicit loops if the index positions of the
    colons differs. This is a restriction that could be lifted by
    using e.g. SIZE(zvab, 1) as the upper loop limit or (with a lot more
    work) by interrogating the parsed code to figure out the loop bound. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "array_section_index_mismatch.f90"),
                           api=API, line_length=False)
    with pytest.raises(NotImplementedError) as err:
        _ = PSyFactory(API, distributed_memory=False).create(invoke_info)
        assert ("implicit loops are restricted to cases where all array "
                "range specifications occur" in str(err))
