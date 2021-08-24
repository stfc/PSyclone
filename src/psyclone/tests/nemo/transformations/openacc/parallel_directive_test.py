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
# Authors: R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab

'''Module containing py.test tests for the transformation of the PSy
   representation of NEMO code using the OpenACC parallel directive.

'''

from __future__ import print_function, absolute_import
from fparser.common.readfortran import FortranStringReader
from psyclone.psyGen import PSyFactory, TransInfo
from psyclone.psyir.nodes import ACCParallelDirective


# The PSyclone API under test
API = "nemo"


SINGLE_LOOP = ("program do_loop\n"
               "integer :: ji\n"
               "integer, parameter :: jpj=128\n"
               "real(kind=wp) :: sto_tmp(jpj)\n"
               "do ji = 1,jpj\n"
               "  sto_tmp(ji) = 1.0d0\n"
               "end do\n"
               "end program do_loop\n")


def test_parallel_single_loop(parser):
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
    code = str(psy.gen)

    assert ("PROGRAM do_loop\n"
            "  INTEGER :: ji\n"
            "  INTEGER, PARAMETER :: jpj = 128\n"
            "  REAL(KIND = wp) :: sto_tmp(jpj)\n"
            "  !$ACC DATA COPYOUT(sto_tmp)\n"
            "  !$ACC PARALLEL DEFAULT(PRESENT)\n"
            "  DO ji = 1, jpj\n"
            "    sto_tmp(ji) = 1.0D0\n"
            "  END DO\n"
            "  !$ACC END PARALLEL\n"
            "  !$ACC END DATA\n"
            "END PROGRAM do_loop" in code)


def test_parallel_two_loops(parser):
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
    code = str(psy.gen)
    assert ("PROGRAM do_loop\n"
            "  INTEGER :: ji\n"
            "  INTEGER, PARAMETER :: jpi = 11\n"
            "  REAL :: sto_tmp(jpi), sto_tmp2(jpi)\n"
            "  !$ACC DATA COPYOUT(sto_tmp,sto_tmp2)\n"
            "  !$ACC PARALLEL DEFAULT(PRESENT)\n"
            "  DO ji = 1, jpi\n"
            "    sto_tmp(ji) = 1.0D0\n"
            "  END DO\n"
            "  DO ji = 1, jpi\n"
            "    sto_tmp2(ji) = 1.0D0\n"
            "  END DO\n"
            "  !$ACC END PARALLEL\n"
            "  !$ACC END DATA\n"
            "END PROGRAM do_loop" in code)


def test_parallel_if_block(parser):
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
    code = str(psy.gen)
    assert ("  !$ACC DATA COPYOUT(sto_tmp,sto_tmp2)\n"
            "  !$ACC PARALLEL DEFAULT(PRESENT)\n"
            "  IF (init) THEN\n"
            "    DO ji = 1, jpi\n" in code)
    assert ("    END DO\n"
            "  END IF\n"
            "  !$ACC END PARALLEL\n"
            "  !$ACC END DATA\n" in code)


def test_parallel_repeat_update(parser):
    ''' Check that calling ACCParallelDirective.update() a 2nd time
    does not alter the fparser2 parse tree. '''
    reader = FortranStringReader(SINGLE_LOOP)
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    data_trans = TransInfo().get_trans_name('ACCDataTrans')
    acc_trans = TransInfo().get_trans_name('ACCParallelTrans')
    acc_trans.apply(schedule.children[0:1])
    data_trans.apply(schedule[0])
    accdir = schedule[0].dir_body[0]
    assert isinstance(accdir, ACCParallelDirective)
    assert accdir._ast is None
    # Generate the code in order to trigger the update of the fparser2 tree
    _ = str(psy.gen)
    # Store the content of a part of the fparser2 parse tree
    orig_content = accdir._ast.parent.content[:]
    # Call update() a second time and then check that nothing has changed
    accdir.update()
    for idx, item in enumerate(orig_content):
        assert item is accdir._ast.parent.content[idx]
