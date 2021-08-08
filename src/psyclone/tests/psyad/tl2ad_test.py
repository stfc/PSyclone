# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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

'''A module to perform pytest tests on the code in the tl2ad.py file
within the psyad directory.

'''
from __future__ import print_function, absolute_import
import logging
import six
import pytest

from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyad import generate_adjoint_str, generate_adjoint


# 1: generate_adjoint_str function

# expected output
@pytest.mark.xfail(reason="issue #1235: caplog returns an empty string in "
                   "github actions.", strict=False)
def test_generate_adjoint_str(caplog):
    '''Test that the generate_adjoint_str() function works as expected
    including logging.

    '''
    tl_code = (
        "program test\n"
        "integer :: a\n"
        "a = 0.0\n"
        "end program test\n")
    expected = (
        "program test\n"
        "  integer :: a\n\n"
        "  a = 0.0\n\n"
        "end program test\n")

    with caplog.at_level(logging.INFO):
        result = generate_adjoint_str(tl_code, ["a"])
    assert expected in result

    with caplog.at_level(logging.DEBUG):
        result = generate_adjoint_str(tl_code, ["a"])

    assert "DEBUG    psyclone.psyad.tl2ad:tl2ad.py:60" in caplog.text
    assert tl_code in caplog.text
    assert "DEBUG    psyclone.psyad.tl2ad:tl2ad.py:76" in caplog.text
    assert expected in caplog.text
    assert expected in result


# 2: generate_adjoint function
def test_generate_adjoint():
    '''Test that the generate_adjoint() function works as expected.'''

    tl_fortran_str = (
        "program test\n"
        "integer :: a\n"
        "a = 0.0\n"
        "end program test\n")
    expected_ad_fortran_str = (
        "program test\n"
        "  integer :: a\n\n"
        "  a = 0.0\n\n"
        "end program test\n")
    reader = FortranReader()
    tl_psyir = reader.psyir_from_source(tl_fortran_str)

    ad_psyir = generate_adjoint(tl_psyir, ["a"])

    writer = FortranWriter()
    ad_fortran_str = writer(ad_psyir)
    assert expected_ad_fortran_str in ad_fortran_str


# generate_adjoint function logging
@pytest.mark.xfail(reason="issue #1235: caplog returns an empty string in "
                   "github actions.", strict=False)
def test_generate_adjoint_logging(caplog):
    '''Test that logging works as expected in the generate_adjoint()
    function.

    '''
    tl_fortran_str = (
        "program test\n"
        "integer :: a\n"
        "a = 0.0\n"
        "end program test\n")
    expected_ad_fortran_str = (
        "program test\n"
        "  integer :: a\n\n"
        "  a = 0.0\n\n"
        "end program test\n")
    reader = FortranReader()
    tl_psyir = reader.psyir_from_source(tl_fortran_str)

    with caplog.at_level(logging.INFO):
        ad_psyir = generate_adjoint(tl_psyir, ["a"])
    assert caplog.text == ""

    writer = FortranWriter()
    ad_fortran_str = writer(ad_psyir)
    assert expected_ad_fortran_str in ad_fortran_str

    with caplog.at_level(logging.DEBUG):
        ad_psyir = generate_adjoint(tl_psyir, ["a"])
    # Python2 and 3 report different line numbers
    if six.PY2:
        line_number = 98
    else:
        line_number = 97
    assert (
        "DEBUG    psyclone.psyad.tl2ad:tl2ad.py:{0} Translating from TL to AD."
        "".format(line_number)
        in caplog.text)

    ad_fortran_str = writer(ad_psyir)
    assert expected_ad_fortran_str in ad_fortran_str
