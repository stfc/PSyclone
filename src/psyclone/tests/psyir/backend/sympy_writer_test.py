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
# Author: J. Henrichs, Bureau of Meteorology

''' Module containing py.test tests the SymPy writer.'''

from __future__ import print_function, absolute_import

import pytest

from psyclone.errors import InternalError
from psyclone.psyir.backend.sympy_writer import SymPyWriter
from psyclone.psyir.nodes import Literal
from psyclone.psyir.symbols import BOOLEAN_TYPE, CHARACTER_TYPE


def test_sym_writer_boolean():
    '''Test that booleans are written in the way that SymPy accepts.
    '''
    sympy_writer = SymPyWriter()
    lit = Literal("true", BOOLEAN_TYPE)
    assert sympy_writer(lit) == "True"
    lit = Literal("false", BOOLEAN_TYPE)
    assert sympy_writer(lit) == "False"


def test_sym_writer_character():
    '''Test that characters are rejected.
    '''

    sympy_writer = SymPyWriter()
    lit = Literal("bla", CHARACTER_TYPE)

    with pytest.raises(InternalError) as err:
        sympy_writer(lit)

    assert "SymPy cannot handle strings like 'bla'." in str(err.value)


@pytest.mark.parametrize("expressions", [("2", "2"),
                                         ("123_4", "123"),
                                         ("456_8", "456"),
                                         ("123_xx", "123")
                                         ])
def test_sym_writer_int_constants(fortran_reader, expressions):
    '''Test that integer constants are handled, including precision
    specifications (either as int or as a name).
    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = '''program test_prog
                integer :: x
                x = {0}
                end program test_prog '''.format(expressions[0])
    psyir = fortran_reader.psyir_from_source(source)
    # psyir is a FileContainer, its first children the program, and its
    # first children the assignment, of which we take the right hand side
    lit = psyir.children[0].children[0].rhs

    sympy_writer = SymPyWriter()
    assert sympy_writer(lit) == expressions[1]


@pytest.mark.parametrize("expressions", [("3.1415926535897932384626",
                                          "3.1415926535897932384626"),
                                         ("1.23E5", "1.23e5"),
                                         ("1.23D5", "1.23e5"),
                                         ("1.0E+3", "1.0e+3"),
                                         ("1.0", "1.0"),
                                         ("0.01E-3", "0.01e-3"),
                                         ("3.14e-2", "3.14e-2")
                                         ])
def test_sym_writer_real_constants(fortran_reader, expressions):
    '''Test that real constants are handled, including precision
    specifications (either as int or as a name).
    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = '''program test_prog
                integer :: x
                x = {0}
                end program test_prog '''.format(expressions[0])

    psyir = fortran_reader.psyir_from_source(source)
    lit = psyir.children[0].children[0].rhs
    sympy_writer = SymPyWriter()
    assert sympy_writer(lit) == expressions[1]
