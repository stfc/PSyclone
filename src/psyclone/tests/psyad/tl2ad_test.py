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
import pytest
import six

from psyclone.errors import InternalError
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Container, FileContainer, Return, Routine
from psyclone.psyir.symbols import SymbolTable
from psyclone.psyad import generate_adjoint_str, generate_adjoint
from psyclone.psyad.tl2ad import _find_container


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
        result, test_harness = generate_adjoint_str(tl_code)

    assert caplog.text == ""
    assert expected in result
    assert test_harness is None

    with caplog.at_level(logging.DEBUG):
        result, test_harness = generate_adjoint_str(tl_code)

    assert "DEBUG    psyclone.psyad.tl2ad:tl2ad.py:58" in caplog.text
    assert tl_code in caplog.text
    assert "DEBUG    psyclone.psyad.tl2ad:tl2ad.py:74" in caplog.text
    assert expected in caplog.text
    assert expected in result
    assert test_harness is None


def test_find_container():
    ''' Tests for the internal, helper function _find_container(). '''
    assert _find_container(Return()) is None
    assert _find_container(FileContainer("test")) is None
    cont = Container("my_mod")
    assert _find_container(cont) is cont
    cont.addchild(FileContainer("test"))
    with pytest.raises(InternalError) as err:
        _find_container(cont)
    assert ("The supplied PSyIR contains two Containers but the innermost is "
            "a FileContainer. This should not be possible" in str(err.value))
    cont = Container("my_mod")
    cont.addchild(Container("another_mod"))
    with pytest.raises(NotImplementedError) as err:
        _find_container(cont)
    assert ("supplied PSyIR contains two Containers and the outermost one is "
            "not a FileContainer. This is not supported." in str(err.value))
    file_cont = FileContainer("test")
    cont = Container("my_mod")
    file_cont.addchild(cont)
    assert _find_container(file_cont) is cont
    file_cont.addchild(cont.copy())
    with pytest.raises(NotImplementedError) as err:
        _find_container(file_cont)
    assert ("The supplied PSyIR contains more than two Containers. This is "
            "not supported." in str(err.value))


# 2: generate_adjoint function
def test_generate_adjoint(fortran_reader):
    '''Test that the generate_adjoint() function works as expected.'''

    tl_fortran_str = (
        "program test\n"
        "integer :: a\n"
        "a = 0.0\n"
        "end program test\n")
    expected_ad_fortran_str = (
        "program test_adj\n"
        "  integer :: a\n\n"
        "  a = 0.0\n\n"
        "end program test_adj\n")
    tl_psyir = fortran_reader.psyir_from_source(tl_fortran_str)

    ad_psyir = generate_adjoint(tl_psyir)

    writer = FortranWriter()
    ad_fortran_str = writer(ad_psyir)
    assert expected_ad_fortran_str in ad_fortran_str


def test_generate_adjoint_errors():
    ''' Check that generate_adjoint() raises the expected exceptions when
    given invalid input. '''
    # Only a FileContainer
    psyir = FileContainer("test_file")
    with pytest.raises(InternalError) as err:
        generate_adjoint(psyir)
    assert ("The supplied PSyIR does not contain any routines." in
            str(err.value))
    with pytest.raises(InternalError) as err:
        generate_adjoint(Container.create("test_mod", SymbolTable(),
                                          [psyir.copy()]))
    assert ("The supplied PSyIR contains two Containers but the innermost is "
            "a FileContainer. This should not be possible" in str(err.value))
    # No kernel code
    cont = Container("test_mod")
    with pytest.raises(InternalError) as err:
        generate_adjoint(cont)
    assert ("The supplied PSyIR does not contain any routines." in
            str(err.value))
    # Only one routine is permitted
    cont.addchild(Routine.create("my_kern1", SymbolTable(), [Return()]))
    cont.addchild(Routine.create("my_kern2", SymbolTable(), [Return()]))
    with pytest.raises(NotImplementedError) as err:
        generate_adjoint(cont)
    assert ("The supplied Fortran must contain one and only one routine but "
            "found: ['my_kern1', 'my_kern2']" in str(err.value))


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
        ad_psyir = generate_adjoint(tl_psyir)
    assert caplog.text == ""

    writer = FortranWriter()
    ad_fortran_str = writer(ad_psyir)
    assert expected_ad_fortran_str in ad_fortran_str

    with caplog.at_level(logging.DEBUG):
        ad_psyir = generate_adjoint(tl_psyir)
    # Python2 and 3 report different line numbers
    if six.PY2:
        line_number = 96
    else:
        line_number = 95
    assert (
        "DEBUG    psyclone.psyad.tl2ad:tl2ad.py:{0} Translation from generic "
        "PSyIR to LFRic-specific PSyIR should be done now.".format(line_number)
        in caplog.text)
    assert (
        "DEBUG    psyclone.psyad.tl2ad:tl2ad.py:100 Transformation from TL to "
        "AD should be done now." in caplog.text)

    ad_fortran_str = writer(ad_psyir)
    assert expected_ad_fortran_str in ad_fortran_str
