# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council.
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
# Authors R. W. Ford and S. Siso, STFC Daresbury Lab

'''Module containing tests for the NemoAllArrayRange2LoopTrans
transformation.'''

from __future__ import absolute_import

import pytest

from psyclone.psyir.nodes import Assignment
from psyclone.psyGen import Transformation
from psyclone.psyir.transformations import TransformationError
from psyclone.domain.nemo.transformations import NemoAllArrayRange2LoopTrans
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import get_invoke

# Constants
API = "nemo"


def test_transform():
    '''Check that it is possible to create an instance of
    NemoArrayRange2LoopTrans and that it is a Transformation.

    '''
    assert isinstance(NemoAllArrayRange2LoopTrans(), Transformation)


def test_transform_apply_mixed_implicit_do():
    '''Check that the PSyIR is transformed as expected for a lat,lon,levs
    loop with some of its indices accessed using array notation and
    some using explicit loops.  The resultant Fortran code is used to
    confirm the transformation has worked correctly.

    '''
    _, invoke_info = get_invoke("explicit_over_implicit.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[0].loop_body[0]
    trans = NemoAllArrayRange2LoopTrans()
    trans.apply(assignment)
    writer = FortranWriter()
    result = writer(schedule)
    expected = (
        "  do jk = 1, jpk, 1\n"
        "    do jj = 1, jpj, 1\n"
        "      do ji = 1, jpi, 1\n"
        "        umask(ji,jj,jk) = vmask(ji,jj,jk) + 1.0\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo")
    assert expected in result


def test_apply_multi_assignments():
    '''Check that the apply() method can be called for multiple assigments
    with and without ranges.

    '''
    _, invoke_info = get_invoke("array_syntax.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    trans = NemoAllArrayRange2LoopTrans()
    for assignment in schedule.walk(Assignment):
        trans.apply(assignment)
    writer = FortranWriter()
    result = writer(schedule)
    expected = (
        "  do jk = 1, jpk, 1\n"
        "    do jj = 1, jpj, 1\n"
        "      do ji = 1, jpi, 1\n"
        "        zftv(ji,jj,jk) = 0.0e0\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n"
        "  if (l_ptr) then\n"
        "    call dia_ptr_hst(jn, 'ldf', -zftv(:,:,:))\n"
        "  end if\n"
        "  call dia_ptr_hst(jn, 'ldf', -zftv(:,:,:))\n"
        "  do jj = 1, jpj, 1\n"
        "    do ji = 1, jpi, 1\n"
        "      zftu(ji,jj,1) = 1.0e0\n"
        "    enddo\n"
        "  enddo\n"
        "  do jj = 1, jpj, 1\n"
        "    do ji = 1, jpi, 1\n"
        "      tmask(ji,jj) = jpi\n"
        "    enddo\n"
        "  enddo\n")
    assert expected in result


def test_apply_calls_validate():
    '''Check that the apply() method calls the validate method.'''
    trans = NemoAllArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.apply(None)
    assert("Error in NemoAllArrayRange2LoopTrans transformation. The "
           "supplied node argument should be a PSyIR Assignment, but "
           "found 'NoneType'." in str(info.value))


def test_str():
    '''Test that the str of an instance of the NemoAllArrayRange2LoopTrans
    class returns the expected value.

    '''
    assert (str(NemoAllArrayRange2LoopTrans()) == "Convert all array ranges "
            "in a PSyIR assignment into PSyIR NemoLoops.")


def test_name():
    '''Check that the name property of the NemoAllArrayRange2LoopTrans
    class returns the expected value.

    '''
    assert (NemoAllArrayRange2LoopTrans().name ==
            "NemoAllArrayRange2LoopTrans")


def test_validate_assignment():
    '''Check that the validate method raises an exception if the supplied
    argument is not an Assignment node.

    '''
    trans = NemoAllArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(None)
    assert("Error in NemoAllArrayRange2LoopTrans transformation. The "
           "supplied node argument should be a PSyIR Assignment, but "
           "found 'NoneType'." in str(info.value))
