# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2023, Science and Technology Facilities Council.
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
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''
A module to perform pytest tests on the
psyad/domain/lfric/lfric_adjoint.py file.

'''
import pytest

from psyclone.errors import InternalError
from psyclone.psyad.domain.lfric import generate_lfric_adjoint


def test_generate_lfric_adjoint_no_container_error(fortran_reader):
    '''
    Check that generate_lfric_adjoint raises the expected error when
    provided PSyIR that does not have a Container.

    '''
    psyir = fortran_reader.psyir_from_source("""\
program test
  implicit none
  integer :: var1, var2
  var1 = var2
end program test
""")
    with pytest.raises(InternalError) as err:
        generate_lfric_adjoint(psyir, ["var1", "var2"])
    assert ("An LFRic kernel must be within a Container but the supplied "
            "PSyIR does not contain one." in str(err.value))


def test_generate_lfric_adjoint_no_routines_error(fortran_reader):
    '''
    Check that generate_lfric_adjoint raises the expected error when
    provided PSyIR that does not contain any Routines.

    '''
    psyir = fortran_reader.psyir_from_source("""\
module test
  implicit none
  integer :: var1
end module test
""")
    with pytest.raises(InternalError) as err:
        generate_lfric_adjoint(psyir, ["var1", "var2"])
    assert "The supplied PSyIR does not contain any routines" in str(err.value)


def test_generate_lfric_adjoint_multi_kernel(fortran_reader, fortran_writer):
    '''Check that generate_lfric_adjoint creates the expected code when there
    are multiple kernels in a module.

    '''
    tl_fortran_str = (
        "module test_mod\n"
        "  contains\n"
        "  subroutine kern1()\n"
        "    real :: psyir_tmp, psyir_tmp_1\n"
        "    psyir_tmp = psyir_tmp_1\n"
        "  end subroutine kern1\n"
        "  subroutine kern2()\n"
        "    real :: psyir_tmp, psyir_tmp_1\n"
        "    psyir_tmp = psyir_tmp_1\n"
        "  end subroutine kern2\n"
        "  subroutine kern3()\n"
        "    real :: psyir_tmp, psyir_tmp_1\n"
        "    psyir_tmp = psyir_tmp_1\n"
        "  end subroutine kern3\n"
        "end module test_mod\n")
    expected = (
        "module adj_test_mod\n"
        "  implicit none\n"
        "  public\n\n"
        "  contains\n"
        "  subroutine adj_kern1()\n"
        "    real :: psyir_tmp\n"
        "    real :: psyir_tmp_1\n\n"
        "    psyir_tmp = 0.0\n"
        "    psyir_tmp_1 = 0.0\n"
        "    psyir_tmp_1 = psyir_tmp_1 + psyir_tmp\n"
        "    psyir_tmp = 0.0\n\n"
        "  end subroutine adj_kern1\n"
        "  subroutine adj_kern2()\n"
        "    real :: psyir_tmp\n"
        "    real :: psyir_tmp_1\n\n"
        "    psyir_tmp = 0.0\n"
        "    psyir_tmp_1 = 0.0\n"
        "    psyir_tmp_1 = psyir_tmp_1 + psyir_tmp\n"
        "    psyir_tmp = 0.0\n\n"
        "  end subroutine adj_kern2\n"
        "  subroutine adj_kern3()\n"
        "    real :: psyir_tmp\n"
        "    real :: psyir_tmp_1\n\n"
        "    psyir_tmp = 0.0\n"
        "    psyir_tmp_1 = 0.0\n"
        "    psyir_tmp_1 = psyir_tmp_1 + psyir_tmp\n"
        "    psyir_tmp = 0.0\n\n"
        "  end subroutine adj_kern3\n\n"
        "end module adj_test_mod\n")
    psyir = fortran_reader.psyir_from_source(tl_fortran_str)
    ad_psyir = generate_lfric_adjoint(psyir, ["psyir_tmp", "psyir_tmp_1"])
    ad_fortran_str = fortran_writer(ad_psyir)
    assert ad_fortran_str == expected
