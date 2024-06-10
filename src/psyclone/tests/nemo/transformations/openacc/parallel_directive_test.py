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

'''Module containing py.test tests for the transformation of the PSy
   representation of NEMO code using the OpenACC parallel directive.

'''

import pytest

from fparser.common.readfortran import FortranStringReader
from psyclone.psyGen import PSyFactory, TransInfo
from psyclone.tests.utilities import Compile
from psyclone.transformations import TransformationError


# The PSyclone API under test
API = "nemo"


SINGLE_LOOP = ("program do_loop\n"
               "integer :: ji\n"
               "integer, parameter :: jpj=128\n"
               "real :: sto_tmp(jpj)\n"
               "do ji = 1,jpj\n"
               "  sto_tmp(ji) = 1.0d0\n"
               "end do\n"
               "end program do_loop\n")


def test_parallel_single_loop(parser, tmpdir):
    ''' Check that we can apply the transformation to a single, explicit
    loop. '''
    reader = FortranStringReader(SINGLE_LOOP)
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    data_trans = TransInfo().get_trans_name('ACCDataTrans')
    acc_trans = TransInfo().get_trans_name('ACCParallelTrans')
    acc_trans.apply(schedule[0:1])
    data_trans.apply(schedule[0])
    code = str(psy.gen).lower()

    assert ("program do_loop\n"
            "  integer, parameter :: jpj = 128\n"
            "  integer :: ji\n"
            "  real, dimension(jpj) :: sto_tmp\n"
            "\n"
            "  !$acc data copyout(sto_tmp)\n"
            "  !$acc parallel default(present)\n"
            "  do ji = 1, jpj, 1\n"
            "    sto_tmp(ji) = 1.0d0\n"
            "  enddo\n"
            "  !$acc end parallel\n"
            "  !$acc end data\n"
            "\n"
            "end program do_loop" in code)
    assert Compile(tmpdir).string_compiles(code)


def test_parallel_single_loop_with_no_default_present_clause(parser, tmpdir):
    ''' Check that we can apply the transformation to a single, explicit
    loop, wihtout the default present clause '''
    reader = FortranStringReader(SINGLE_LOOP)
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ACCParallelTrans')

    with pytest.raises(TransformationError) as err:
        acc_trans.apply(schedule[0:1], options={"default_present": 3})
    assert ("The provided 'default_present' option must be a boolean, "
            "but found '3'." in str(err.value))

    acc_trans.apply(schedule[0:1], options={"default_present": False})
    code = str(psy.gen).lower()

    assert ("program do_loop\n"
            "  integer, parameter :: jpj = 128\n"
            "  integer :: ji\n"
            "  real, dimension(jpj) :: sto_tmp\n"
            "\n"
            "  !$acc parallel\n"
            "  do ji = 1, jpj, 1\n"
            "    sto_tmp(ji) = 1.0d0\n"
            "  enddo\n"
            "  !$acc end parallel\n"
            "\n"
            "end program do_loop" in code)
    assert Compile(tmpdir).string_compiles(code)


def test_parallel_two_loops(parser, tmpdir):
    ''' Check that we can enclose two loops within a parallel region. '''
    reader = FortranStringReader("program do_loop\n"
                                 "integer :: ji\n"
                                 "integer, parameter :: jpi=11\n"
                                 "real :: sto_tmp(jpi), sto_tmp2(jpi)\n"
                                 "do ji = 1,jpi\n"
                                 "  sto_tmp(ji) = 1.0d0\n"
                                 "end do\n"
                                 "do ji = 1,jpi\n"
                                 "  sto_tmp2(ji) = 1.0d0\n"
                                 "end do\n"
                                 "end program do_loop\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    data_trans = TransInfo().get_trans_name('ACCDataTrans')
    acc_trans = TransInfo().get_trans_name('ACCParallelTrans')
    acc_trans.apply(schedule[0:2])
    data_trans.apply(schedule[0])
    code = str(psy.gen).lower()
    assert ("program do_loop\n"
            "  integer, parameter :: jpi = 11\n"
            "  integer :: ji\n"
            "  real, dimension(jpi) :: sto_tmp\n"
            "  real, dimension(jpi) :: sto_tmp2\n"
            "\n"
            "  !$acc data copyout(sto_tmp,sto_tmp2)\n"
            "  !$acc parallel default(present)\n"
            "  do ji = 1, jpi, 1\n"
            "    sto_tmp(ji) = 1.0d0\n"
            "  enddo\n"
            "  do ji = 1, jpi, 1\n"
            "    sto_tmp2(ji) = 1.0d0\n"
            "  enddo\n"
            "  !$acc end parallel\n"
            "  !$acc end data\n"
            "\n"
            "end program do_loop" in code)
    assert Compile(tmpdir).string_compiles(code)


def test_parallel_if_block(parser, tmpdir):
    ''' Check that we can enclose an IF-block within a parallel region. '''
    reader = FortranStringReader("program do_loop\n"
                                 "integer :: ji\n"
                                 "integer, parameter :: jpi=64\n"
                                 "logical :: init\n"
                                 "real :: sto_tmp(jpi), sto_tmp2(jpi)\n"
                                 "if(init)then\n"
                                 "  do ji = 1,jpi\n"
                                 "    sto_tmp(ji) = 1.0d0\n"
                                 "  end do\n"
                                 "else\n"
                                 "  do ji = 1,jpi\n"
                                 "    sto_tmp2(ji) = 1.0d0\n"
                                 "  end do\n"
                                 "end if\n"
                                 "end program do_loop\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    data_trans = TransInfo().get_trans_name('ACCDataTrans')
    acc_trans = TransInfo().get_trans_name('ACCParallelTrans')
    acc_trans.apply(schedule[0:1])
    data_trans.apply(schedule[0])
    code = str(psy.gen).lower()
    assert ("  !$acc data copyout(sto_tmp,sto_tmp2)\n"
            "  !$acc parallel default(present)\n"
            "  if (init) then\n"
            "    do ji = 1, jpi, 1\n" in code)
    assert ("    enddo\n"
            "  end if\n"
            "  !$acc end parallel\n"
            "  !$acc end data\n" in code)
    assert Compile(tmpdir).string_compiles(code)
