# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council
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

'''A module to perform pytest tests on the code in the script.py file
within the psyad/transformations directory

'''
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyad.transformations.script import preprocess_trans
from psyclone.tests.utilities import Compile


def test_script_no_change():
    '''test that the script does not modify the PSyIR if the script
    transformations are not applicable to its contents.

    '''
    code = (
        "program test\n"
        "  integer :: a\n\n"
        "  a = 0.0\n\n"
        "end program test\n")
    reader = FortranReader()
    psyir = reader.psyir_from_source(code)
    preprocess_trans(psyir)
    writer = FortranWriter()
    result = writer(psyir)
    assert result == code


def test_script_dotproduct(tmpdir):
    '''test that the script replaces a dotproduct with equivalent code.'''
    code = (
        "program test\n"
        "real :: a, b(10), c(10)\n"
        "a = dot_product(b(:), c(:))\n"
        "end program test\n")
    expected = (
        "program test\n"
        "  real :: a\n"
        "  real, dimension(10) :: b\n"
        "  real, dimension(10) :: c\n"
        "  integer :: i\n"
        "  real :: res_dot_product\n\n"
        "  res_dot_product = 0.0\n"
        "  do i = 1, 10, 1\n"
        "    res_dot_product = res_dot_product + b(i) * c(i)\n"
        "  enddo\n"
        "  a = res_dot_product\n\n"
        "end program test\n")
    reader = FortranReader()
    psyir = reader.psyir_from_source(code)
    preprocess_trans(psyir)
    writer = FortranWriter()
    result = writer(psyir)
    assert result == expected
    assert Compile(tmpdir).string_compiles(result)
