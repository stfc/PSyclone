# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2019, Science and Technology Facilities Council.
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

''' Module containing py.test tests for the generation of Fortran from
    the PSy representation of NEMO code. '''

from __future__ import print_function, absolute_import
import os
import pytest
from fparser.common.readfortran import FortranStringReader
from psyclone.psyGen import PSyFactory

# Constants
API = "nemo"
# Location of the Fortran files associated with these tests
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")


def test_api_no_alg():
    ''' Checks that generate works OK for an API which doesn't have an
    Algorithm layer '''
    from psyclone.generator import generate
    alg, psy = generate(os.path.join(BASE_PATH, "explicit_do.f90"),
                        api="nemo")
    assert alg is None
    code = str(psy).lower()
    assert "program explicit_do" in code
    assert "integer :: ji" in code
    assert "real, dimension(jpi, jpj, jpk) :: umask" in code
    assert "end program explicit_do" in code


def test_utf_char(tmpdir):
    ''' Check that we generate the PSy layer OK when the original Fortran
    code contains UTF characters with no representation in the ASCII
    character set. '''
    from psyclone.generator import main
    test_file = os.path.join(BASE_PATH, "utf_char.f90")
    tmp_file = os.path.join(str(tmpdir), "test_psy.f90")
    main(["-api", "nemo", "-opsy", tmp_file, test_file])
    assert os.path.isfile(tmp_file)


def test_psyir_backend_program(parser, psyir_backend):
    ''' Check that using the Fortran backend to the PSyIR works for a
    simple example consisting of a single Program. '''
    reader = FortranStringReader("program fake_kern\n"
                                 "integer :: ji, jpj\n"
                                 "real(kind=wp) :: sto_tmp(5)\n"
                                 "do ji = 1,jpj\n"
                                 "sto_tmp(ji) = 1.0\n"
                                 "end do\n"
                                 "end program fake_kern\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    code = str(psy.gen).lower()
    assert "program fake_kern" in code
    assert "do ji = 1" in code
    assert "sto_tmp(ji)=1.0" in code
    assert "enddo" in code
    assert "end program fake_kern" in code


def test_psyir_backend_module(parser, psyir_backend):
    ''' Check that using the Fortran backend to the PSyIR works for a
    an example consisting of a module. '''
    reader = FortranStringReader("module my_mod\n"
                                 "contains\n"
                                 "subroutine my_kern()\n"
                                 "integer :: ji, jpj\n"
                                 "real(kind=wp) :: sto_tmp(5)\n"
                                 "do ji = 1,jpj\n"
                                 "sto_tmp(ji) = 1.0\n"
                                 "end do\n"
                                 "end subroutine my_kern\n"
                                 "end module my_mod\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    code = str(psy.gen).lower()
    print(code)
    assert "subroutine my_kern" in code
    assert "do ji = 1" in code
    assert "sto_tmp(ji)=1.0" in code
    assert "enddo" in code
    assert "end subroutine my_kern" in code
