# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council.
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
# Modified: J. G. Wallwork, Met Office

'''Module containing py.test tests for the transformation of the PSy
   representation of NEMO code using the OpenACC loop directive.

'''

import pytest
from fparser.common.readfortran import FortranStringReader
from psyclone.psyGen import PSyFactory, TransInfo
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.nodes import Loop
from psyclone.errors import GenerationError


# The PSyclone API under test
API = "nemo"


def test_missing_enclosing_region(parser):
    ''' Check that applying the loop transformation to code without
    any enclosing parallel or kernels region results in a
    code-generation error. '''
    reader = FortranStringReader("program do_loop\n"
                                 "integer :: ji\n"
                                 "integer, parameter :: jpj=64\n"
                                 "real :: sto_tmp(jpj)\n"
                                 "do ji = 1,jpj\n"
                                 "  sto_tmp(ji) = 1.0d0\n"
                                 "end do\n"
                                 "end program do_loop\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ACCLoopTrans')
    acc_trans.apply(schedule[0])
    with pytest.raises(GenerationError) as err:
        str(psy.gen)
    assert ("ACCLoopDirective in routine 'do_loop' must either have an "
            "ACCParallelDirective or ACCKernelsDirective as an ancestor in "
            "the Schedule or the routine must contain an ACCRoutineDirective"
            in str(err.value))


def test_explicit_loop(parser):
    ''' Check that we can apply the transformation to an explicit loop. '''
    reader = FortranStringReader("program do_loop\n"
                                 "integer :: ji\n"
                                 "integer, parameter :: jpj=13\n"
                                 "real :: sto_tmp(jpj), sto_tmp2(jpj)\n"
                                 "do ji = 1,jpj\n"
                                 "  sto_tmp(ji) = 1.0d0\n"
                                 "end do\n"
                                 "do ji = 1,jpj\n"
                                 "  sto_tmp2(ji) = 1.0d0\n"
                                 "end do\n"
                                 "end program do_loop\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ACCLoopTrans')
    para_trans = TransInfo().get_trans_name('ACCParallelTrans')
    data_trans = TransInfo().get_trans_name('ACCDataTrans')
    para_trans.apply(schedule.children)
    acc_trans.apply(schedule[0].dir_body[0])
    acc_trans.apply(schedule[0].dir_body[1], {"independent": False})
    data_trans.apply(schedule)

    code = str(psy.gen).lower()
    assert ("program do_loop\n"
            "  integer, parameter :: jpj = 13\n"
            "  integer :: ji\n"
            "  real, dimension(jpj) :: sto_tmp\n"
            "  real, dimension(jpj) :: sto_tmp2\n"
            "\n"
            "  !$acc data copyout(sto_tmp,sto_tmp2)\n"
            "  !$acc parallel default(present)\n"
            "  !$acc loop independent\n"
            "  do ji = 1, jpj, 1\n"
            "    sto_tmp(ji) = 1.0d0\n"
            "  enddo\n"
            "  !$acc loop\n"
            "  do ji = 1, jpj, 1\n"
            "    sto_tmp2(ji) = 1.0d0\n"
            "  enddo\n"
            "  !$acc end parallel\n"
            "  !$acc end data\n"
            "\n"
            "end program do_loop" in code)


SINGLE_LOOP = ("program do_loop\n"
               "use kind_params_mod, only: wp\n"
               "integer :: ji\n"
               "integer, parameter :: jpj=12\n"
               "real(kind=wp) :: sto_tmp(jpj)\n"
               "do ji = 1,jpj\n"
               "  sto_tmp(ji) = 1.0d0\n"
               "end do\n"
               "end program do_loop\n")

DOUBLE_LOOP = ("program do_loop\n"
               "use kind_params_mod, only: wp\n"
               "integer :: ji, jj\n"
               "integer, parameter :: jpi=16, jpj=16\n"
               "real(kind=wp) :: sto_tmp(jpi, jpj)\n"
               "do jj = 1,jpj\n"
               "  do ji = 1,jpi\n"
               "    sto_tmp(ji, jj) = 1.0d0\n"
               "  end do\n"
               "end do\n"
               "end program do_loop\n")


def test_seq_loop(parser):
    ''' Check that we can apply the transformation with the 'sequential'
    clause. '''
    reader = FortranStringReader(SINGLE_LOOP)
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ACCLoopTrans')
    # An ACC Loop must be within a KERNELS or PARALLEL region
    kernels_trans = TransInfo().get_trans_name('ACCKernelsTrans')
    kernels_trans.apply(schedule.children)
    loops = schedule[0].walk(Loop)
    acc_trans.apply(loops[0], {"sequential": True})
    code = str(psy.gen).lower()
    assert ("  real(kind=wp), dimension(jpj) :: sto_tmp\n"
            "\n"
            "  !$acc kernels\n"
            "  !$acc loop seq\n"
            "  do ji = 1, jpj, 1\n" in code)


@pytest.mark.parametrize("clause", ["gang", "vector"])
def test_loop_clauses(parser, clause):
    ''' Check that we can apply the transformation with different
    clauses for independent loops. '''
    reader = FortranStringReader(SINGLE_LOOP)
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ACCLoopTrans')
    # An ACC Loop must be within a KERNELS or PARALLEL region
    kernels_trans = TransInfo().get_trans_name('ACCKernelsTrans')
    kernels_trans.apply(schedule.children)
    loops = schedule[0].walk(Loop)
    acc_trans.apply(loops[0], {clause: True})
    code = str(psy.gen).lower()
    assert ("  real(kind=wp), dimension(jpj) :: sto_tmp\n"
            "\n"
            "  !$acc kernels\n"
            f"  !$acc loop {clause} independent\n"
            "  do ji = 1, jpj, 1\n" in code)


def test_collapse(parser):
    ''' Check that we can apply the loop transformation with the 'collapse'
    clause. '''
    reader = FortranStringReader(DOUBLE_LOOP)
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ACCLoopTrans')
    # An ACC Loop must be within a KERNELS or PARALLEL region
    kernels_trans = TransInfo().get_trans_name('ACCKernelsTrans')
    kernels_trans.apply(schedule.children)
    loops = schedule[0].walk(Loop)
    acc_trans.apply(loops[0], {"collapse": 2})
    code = str(psy.gen).lower()
    assert ("  real(kind=wp), dimension(jpi,jpj) :: sto_tmp\n"
            "\n"
            "  !$acc kernels\n"
            "  !$acc loop independent collapse(2)\n"
            "  do jj = 1, jpj, 1\n"
            "    do ji = 1, jpi, 1\n" in code)


def test_collapse_err(parser):
    ''' Check that attempting to apply the loop transformation with a
    'collapse' depth creater than the number of nested loops raises an
    error. '''
    reader = FortranStringReader(DOUBLE_LOOP)
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ACCLoopTrans')
    with pytest.raises(TransformationError) as err:
        acc_trans.apply(schedule.children[0], {"collapse": 3})
    assert ("Cannot apply COLLAPSE(3) clause to a loop nest containing "
            "only 2 loops" in str(err.value))
