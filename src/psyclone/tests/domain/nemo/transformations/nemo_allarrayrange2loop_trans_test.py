# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
        "    do idx = LBOUND(umask, dim=2), UBOUND(umask, dim=2), 1\n"
        "      do idx_1 = LBOUND(umask, dim=1), UBOUND(umask, dim=1), 1\n"
        "        umask(idx_1,idx,jk) = vmask(idx_1,idx,jk) + 1.0\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo")
    assert expected in result


def test_apply_multi_assignments():
    '''Check that the apply() method can be called for multiple assignments
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
        "  do idx = LBOUND(zftv, dim=3), UBOUND(zftv, dim=3), 1\n"
        "    do idx_1 = LBOUND(zftv, dim=2), UBOUND(zftv, dim=2), 1\n"
        "      do idx_2 = LBOUND(zftv, dim=1), UBOUND(zftv, dim=1), 1\n"
        "        zftv(idx_2,idx_1,idx) = 0.0d0\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo\n"
        "  if (l_ptr) then\n"
        "    call dia_ptr_hst(jn, 'ldf', -zftv(:,:,:))\n"
        "  end if\n"
        "  call dia_ptr_hst(jn, 'ldf', -zftv(:,:,:))\n"
        "  do idx_3 = LBOUND(zftu, dim=2), UBOUND(zftu, dim=2), 1\n"
        "    do idx_4 = LBOUND(zftu, dim=1), UBOUND(zftu, dim=1), 1\n"
        "      zftu(idx_4,idx_3,1) = 1.0d0\n"
        "    enddo\n"
        "  enddo\n"
        "  do idx_5 = LBOUND(tmask, dim=2), UBOUND(tmask, dim=2), 1\n"
        "    do idx_6 = LBOUND(tmask, dim=1), UBOUND(tmask, dim=1), 1\n"
        "      tmask(idx_6,idx_5) = jpi\n"
        "    enddo\n"
        "  enddo\n")
    assert expected in result


def test_apply_with_structures(fortran_reader, fortran_writer):
    '''Check that the PSyIR is transformed as expected when the
    expressions contain a mix of arrays and structures in the same
    reference.

    '''
    trans = NemoAllArrayRange2LoopTrans()

    # The outer dimension is already set
    psyir = fortran_reader.psyir_from_source('''
    subroutine test
        use my_variables
        integer, parameter :: constant = 3
        integer :: jk
        base%field(constant)%array(:,:,jk) = 1
    end subroutine test
    ''')
    assignment = psyir.walk(Assignment)[0]
    trans.apply(assignment)
    result = fortran_writer(assignment)
    assert "base%field(constant)%array(idx_1,idx,jk) = 1" in result

    # The inner dimension is already set
    psyir = fortran_reader.psyir_from_source('''
    subroutine test
        use my_variables
        integer, parameter :: jf = 3, jpi = 3, jpim1 = 1
        ptab(jf)%pt2d(jpi,:,:) = ptab(jf)%pt2d(jpim1,:,:)
    end subroutine test
    ''')
    assignment = psyir.walk(Assignment)[0]
    trans.apply(assignment)
    result = fortran_writer(assignment)
    assert ("ptab(jf)%pt2d(jpi,idx_1,idx) = "
            "ptab(jf)%pt2d(jpim1,idx_1,idx)") in result


def test_apply_option_verbose(fortran_reader, capsys):
    '''Check that the transformation with the verbose option provides more
    information about why the last attempt to convert the array range into
    a loop was not successful.

    '''
    trans = NemoAllArrayRange2LoopTrans()

    psyir = fortran_reader.psyir_from_source('''
    subroutine test
        use my_variables
        integer, parameter :: constant = 3
        integer :: jk, a, b
        integer, dimension(3) :: array
        a = b
        array(:) = my_func()
    end subroutine test
    ''')

    # Basic cases like assignment 1 are skipped
    assignment = psyir.walk(Assignment)[0]
    trans.apply(assignment, options={'verbose': True})
    out, _ = capsys.readouterr()
    assert out == ""

    # Other cases like assignment 2 are printed to stdout because they are due
    # to PSyclone limitations, in this case my_func is a codeblock because
    # currently the Fortran reader only generate functions if it can see its
    # declaration.
    assignment = psyir.walk(Assignment)[1]
    trans.apply(assignment, options={'verbose': True})
    out, _ = capsys.readouterr()
    assert ("Error in NemoArrayRange2LoopTrans transformation. This "
            "transformation does not support array assignments that contain "
            "a CodeBlock anywhere in the expression, but found:\n"
            "array(:) = my_func()" in out)


def test_apply_calls_validate():
    '''Check that the apply() method calls the validate method.'''
    trans = NemoAllArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.apply(None)
    assert ("Error in NemoAllArrayRange2LoopTrans transformation. The "
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
    assert ("Error in NemoAllArrayRange2LoopTrans transformation. The "
            "supplied node argument should be a PSyIR Assignment, but "
            "found 'NoneType'." in str(info.value))
