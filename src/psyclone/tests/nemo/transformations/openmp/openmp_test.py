# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2026, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab

''' Module containing py.test tests for the transformation of
    the PSy representation of NEMO code '''

import os
import pytest
from psyclone.errors import GenerationError
from psyclone.psyGen import TransInfo
from psyclone.psyir.nodes import OMPDoDirective, OMPParallelDirective, Loop
from psyclone.transformations import OMPLoopTrans, OMPParallelTrans, \
    OMPParallelLoopTrans

NEMO_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                              "..", "..", "test_files")


def test_omp_explicit_gen(fortran_reader, fortran_writer):
    ''' Check code generation for a single explicit loop referenced by
    loop type as it may be done for the NEMO application. '''
    psyir = fortran_reader.psyir_from_file(
        os.path.join(NEMO_BASE_PATH, "explicit_do.f90")
    )
    schedule = psyir.children[0]
    omp_trans = TransInfo().get_trans_name('OMPParallelLoopTrans')
    Loop.set_loop_type_inference_rules({
            "lon": {"variable": "ji"},
            "lat": {"variable": "jj"},
            "levels": {"variable": "jk"}
    })

    for loop in schedule.loops():
        if loop.loop_type == "levels":
            omp_trans.apply(loop)

    expected = (
        "program explicit_do\n"
        "  integer, parameter :: jpi = 2\n"
        "  integer, parameter :: jpj = 4\n"
        "  integer, parameter :: jpk = 6\n"
        "  integer :: ji\n"
        "  integer :: jj\n"
        "  integer :: jk\n"
        "  real :: r\n"
        "  real, dimension(jpi,jpj,jpk) :: umask\n"
        "\n"
        "  !$omp parallel do default(shared) private(ji,jj,jk) "
        "schedule(auto)\n"
        "  do jk = 1, jpk, 1\n"
        "    do jj = 1, jpj, 1\n"
        "      do ji = 1, jpi, 1\n"
        "        umask(ji,jj,jk) = ji * jj * jk / r\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n"
        "  !$omp end parallel do\n"
        "\n"
        "end program explicit_do")
    assert expected in fortran_writer(psyir).lower()
    # Check that calling gen a second time gives the same code
    assert expected in fortran_writer(psyir).lower()


def test_omp_private_declaration(fortran_reader, fortran_writer):
    ''' Check code generation and private/shared declaration when
    an assignment is parallelised. In this case the code is like:
    !$omp parallel default(shared) private()
    jpk = 100
    do k=1, jpk ...
    enddo
    !$omp end parallel
    do k=1, jpk ...

    In this case jpk should not be declared private, since then it
    is not defined in the next loop.'''

    psyir = fortran_reader.psyir_from_file(
        os.path.join(NEMO_BASE_PATH, "explicit_do_two_loops.f90")
    )
    schedule = psyir.children[0]
    omp_parallel = TransInfo().get_trans_name('OMPParallelTrans')

    # Apply "omp parallel" around one assignment to a scalar variable
    # and a loop using this variable as loop boundary. Parallelising an
    # assignment statement is not allowed by default, so we need to disable
    # the node type check in order to apply the omp parallel transform.
    omp_parallel.apply(schedule.children[0:2], {'node-type-check': False})
    expected = "!$omp parallel default(shared) private(ji,jj,jk)"
    assert expected in fortran_writer(psyir)


def test_omp_parallel(fortran_reader, fortran_writer):
    ''' Check insertion of an OpenMP parallel region containing a single,
    explicit loop. '''
    psyir = fortran_reader.psyir_from_file(
        os.path.join(NEMO_BASE_PATH, "explicit_do.f90")
    )
    schedule = psyir.children[0]
    otrans = OMPParallelTrans()
    otrans.apply([schedule[0]])
    assert ("  !$omp parallel default(shared) private(ji,jj,jk)\n"
            "  do jk = 1, jpk, 1\n"
            "    do jj = 1, jpj, 1\n"
            "      do ji = 1, jpi, 1\n"
            "        umask(ji,jj,jk) = ji * jj * jk / r\n"
            "      enddo\n"
            "    enddo\n"
            "  enddo\n"
            "  !$omp end parallel\n" in fortran_writer(psyir).lower())


def test_omp_parallel_multi(fortran_reader, fortran_writer):
    ''' Check insertion of an OpenMP parallel region containing more than
    one node. '''
    otrans = OMPParallelTrans()
    psyir = fortran_reader.psyir_from_file(
        os.path.join(NEMO_BASE_PATH, "imperfect_nest.f90")
    )
    schedule = psyir.children[0]

    # Apply the OMP Parallel transformation so as to enclose the last two
    # loop nests (Python's slice notation is such that the expression below
    # gives elements 2-3).
    otrans.apply(schedule[0].loop_body[2:4])
    code = fortran_writer(psyir).lower()
    assert ("    !$omp parallel default(shared) private(ji,jj,zabe1,zcof1,"
            "zmsku)\n"
            "    do jj = 1, jpjm1, 1\n"
            "      do ji = 1, jpim1, 1\n"
            "        zabe1 = pahu(ji,jj,jk) * e2_e1u(ji,jj) * "
            "e3u_n(ji,jj,jk)\n" in code)
    assert ("    do jj = 2, jpjm1, 1\n"
            "      do ji = 2, jpim1, 1\n"
            "        pta(ji,jj,jk,jn) = pta(ji,jj,jk,jn) + "
            "zsign * (zftu(ji,jj,jk) - zftu(ji - 1,jj,jk) + "
            "zftv(ji,jj,jk) - zftv(ji,jj - 1,jk)) * r1_e1e2t(ji,jj) / "
            "e3t_n(ji,jj,jk)\n"
            "      enddo\n"
            "    enddo\n"
            "    !$omp end parallel\n" in code)
    directive = schedule[0].loop_body[2]
    assert isinstance(directive, OMPParallelDirective)


def test_omp_do_missing_region(fortran_reader):
    ''' Check that the correct error is raised if an OMPDoDirective is
    found outside an OMP parallel region at code-generation time. '''
    psyir = fortran_reader.psyir_from_source(
                "program do_loop\n"
                "integer :: ji\n"
                "integer, parameter :: jpj=32\n"
                "real :: sto_tmp(jpj)\n"
                "do ji = 1,jpj\n"
                "  sto_tmp(ji) = 1.0d0\n"
                "end do\n"
                "end program do_loop\n")
    schedule = psyir.children[0]
    loop_trans = OMPLoopTrans()
    loop_trans.apply(schedule[0])
    with pytest.raises(GenerationError) as err:
        schedule[0].validate_global_constraints()
    assert ("OMPDoDirective must be inside an OMP parallel region but could "
            "not find an ancestor OMPParallelDirective" in str(err.value))


def test_omp_do_code_gen(fortran_reader, fortran_writer):
    '''Check the OMPDoDirective generates the correct code.'''
    psyir = fortran_reader.psyir_from_file(
        os.path.join(NEMO_BASE_PATH, "imperfect_nest.f90")
    )
    schedule = psyir.children[0]
    par_trans = OMPParallelTrans()
    loop_trans = OMPLoopTrans()
    par_trans.apply(schedule[0].loop_body[1]
                    .else_body[0].else_body[0])
    loop_trans.apply(schedule[0].loop_body[1]
                     .else_body[0].else_body[0].dir_body[0])
    code = fortran_writer(psyir).lower()
    correct = '''        !$omp parallel default(shared) private(ji,jj)
        !$omp do schedule(auto)
        do jj = 1, jpj, 1
          do ji = 1, jpi, 1
            zdkt(ji,jj) = (ptb(ji,jj,jk - 1,jn) - ptb(ji,jj,jk,jn)) * \
wmask(ji,jj,jk)
          enddo
        enddo
        !$omp end do
        !$omp end parallel'''
    assert correct in code
    directive = schedule[0].loop_body[1].else_body[0].else_body[0].dir_body[0]
    assert isinstance(directive, OMPDoDirective)


def test_omp_do_within_if(fortran_reader, fortran_writer):
    ''' Check that we can insert an OpenMP parallel do within an if block. '''
    otrans = OMPParallelLoopTrans()
    psyir = fortran_reader.psyir_from_file(
        os.path.join(NEMO_BASE_PATH, "imperfect_nest.f90")
    )
    schedule = psyir.children[0]
    loop = schedule[0].loop_body[1].else_body[0].else_body[0]
    assert isinstance(loop, Loop)
    # Apply the transformation to a loop within an else clause
    otrans.apply(loop)
    gen = fortran_writer(psyir).lower()
    expected = (
        "      else\n"
        "        !$omp parallel do default(shared) private(ji,jj) "
        "schedule(auto)\n"
        "        do jj = 1, jpj, 1\n"
        "          do ji = 1, jpi, 1\n"
        "            zdkt(ji,jj) = (ptb(ji,jj,jk - 1,jn) - "
        "ptb(ji,jj,jk,jn)) * wmask(ji,jj,jk)\n"
        "          enddo\n"
        "        enddo\n"
        "        !$omp end parallel do\n"
        "      end if\n")
    assert expected in gen
