# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2019, Science and Technology Facilities Council.
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
import os
import pytest
from fparser.common.readfortran import FortranStringReader
from psyclone.psyGen import PSyFactory, TransInfo
from psyclone.transformations import TransformationError


# The PSyclone API under test
API = "nemo"


def test_no_kernels_error(parser):
    ''' Check that the transformation rejects an attempt to put things
    that aren't kernels inside a kernels region. '''
    reader = FortranStringReader("program write_out\n"
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
    acc_trans = TransInfo().get_trans_name('ACCKernelsTrans')
    with pytest.raises(TransformationError) as err:
        _, _ = acc_trans.apply(schedule.children[0:2])
    assert "must contain blah blah" in str(err)


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
    schedule.view()
    acc_trans = TransInfo().get_trans_name('ACCKernelsTrans')
    with pytest.raises(TransformationError) as err:
        _, _ = acc_trans.apply(schedule.children[0:1])
    assert "must contain blah blah" in str(err)


def test_implicit_loop(parser):
    ''' Check that the transformation generates correct code when applied
    to an implicit loop. '''
    reader = FortranStringReader("program implicit_loop\n"
                                 "real(kind=wp) :: sto_tmp(5)\n"
                                 "sto_tmp(:) = 0.0_wp\n"
                                 "end program implicit_loop\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    schedule.view()
    acc_trans = TransInfo().get_trans_name('ACCKernelsTrans')
    schedule, _ = acc_trans.apply(schedule.children[0:1])
    gen_code = str(psy.gen)
    print(gen_code)
    assert 0


def test_multikern_if(parser):
    ''' Check that we can include an if-block containing multiple
    loops within a kernels region. '''
    reader = FortranStringReader("program implicit_loop\n"
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
    acc_trans = TransInfo().get_trans_name('ACCKernelsTrans')
    schedule, _ = acc_trans.apply(schedule.children[0:1], default_present=True)
    gen_code = str(psy.gen).lower()
    assert ("!$acc kernels default(present)\n"
            "  if (do_this) then\n"
            "    do jk = 1, 3\n" in gen_code)
    assert ("    end do\n"
            "  end if\n"
            "  !$acc end kernels\n"
            "end program implicit_loop" in gen_code)
