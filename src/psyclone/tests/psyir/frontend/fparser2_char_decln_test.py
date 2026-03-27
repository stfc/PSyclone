# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
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
# Author: A. R. Porter
# -----------------------------------------------------------------------------

''' Performs pytest tests on the handling of character declarations in the
    fparser2 PSyIR frontend. '''

import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.common.sourceinfo import FortranFormat
from fparser.two import Fortran2003

from psyclone.psyir.frontend.fparser2 import (
    Fparser2Reader)
from psyclone.psyir.nodes import Reference, Routine
from psyclone.psyir.symbols import (
    ScalarType, Symbol, UnsupportedFortranType)


@pytest.mark.usefixtures("f2008_parser")
@pytest.mark.parametrize("len_expr,length,kind",
                         [("", "1", ScalarType.Precision.UNDEFINED),
                          ("(len=3)", "3", ScalarType.Precision.UNDEFINED),
                          ("(3)", "3", ScalarType.Precision.UNDEFINED),
                          ("*3", "3", ScalarType.Precision.UNDEFINED),
                          ("*(3)", "3", ScalarType.Precision.UNDEFINED),
                          ("(len=2*max_len)", "2 * max_len",
                           ScalarType.Precision.UNDEFINED),
                          ("*(2*max_len)", "2 * max_len",
                           ScalarType.Precision.UNDEFINED),
                          ("(len=:)", "COLON", ScalarType.Precision.UNDEFINED),
                          ("(:)", "COLON", ScalarType.Precision.UNDEFINED),
                          ("*(:)", "COLON", ScalarType.Precision.UNDEFINED),
                          ("(len=*)", "ASTERISK",
                           ScalarType.Precision.UNDEFINED),
                          ("(*)", "ASTERISK", ScalarType.Precision.UNDEFINED),
                          ("*(*)", "ASTERISK", ScalarType.Precision.UNDEFINED),
                          ("(len=3, kind=ckind)", "3",
                           Reference(Symbol("ckind"))),
                          ("(len=*, kind=ckind)", "ASTERISK",
                           Reference(Symbol("ckind")))])
def test_char_decln_length_handling(len_expr, length, kind):
    '''
    Test the handling of kind and length specifiers.
    '''
    fake_parent = Routine.create("dummy_schedule")
    symtab = fake_parent.symbol_table
    processor = Fparser2Reader()

    # Test simple declarations
    reader = FortranStringReader(f"character{len_expr} :: l1")
    # Set reader to free format (otherwise this is a comment in fixed format)
    reader.set_format(FortranFormat(True, True))
    fparser2spec = Fortran2003.Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l1_var = symtab.lookup("l1")
    assert l1_var.datatype.intrinsic == ScalarType.Intrinsic.CHARACTER
    assert l1_var.datatype.precision == kind
    assert l1_var.datatype.length.debug_string() == length


@pytest.mark.usefixtures("f2008_parser")
def test_char_decln_with_char_kind():
    '''
    Check that we get the expected UnsupportedFortranType if the kind is
    specified using a character literal.

    '''
    fake_parent = Routine.create("dummy_schedule")
    symtab = fake_parent.symbol_table
    processor = Fparser2Reader()
    reader = FortranStringReader("character(len=3, kind=KIND('h')) :: l1")
    # Set reader to free format (otherwise this is a comment in fixed format)
    reader.set_format(FortranFormat(True, True))
    fparser2spec = Fortran2003.Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l1_var = symtab.lookup("l1")
    assert isinstance(l1_var.datatype, UnsupportedFortranType)


@pytest.mark.usefixtures("f2008_parser")
def test_char_len_inline():
    '''
    Check that specifying the character length of an individual entity is
    supported and correctly overrides any length in the base declaration.

    '''
    fake_parent = Routine.create("dummy_schedule")
    symtab = fake_parent.symbol_table
    processor = Fparser2Reader()
    reader = FortranStringReader(
        "character*5 :: l1*3, l2*(MAX_LEN), l3, l4*(:)")
    # Set reader to free format (otherwise this is a comment in fixed format)
    reader.set_format(FortranFormat(True, True))
    fparser2spec = Fortran2003.Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l1_var = symtab.lookup("l1")
    assert l1_var.datatype.length.value == "3"
    l2_var = symtab.lookup("l2")
    assert l2_var.datatype.length.symbol is symtab.lookup("MAX_LEN")
    assert symtab.lookup("l3").datatype.length.value == "5"
    assert (symtab.lookup("l4").datatype.length ==
            ScalarType.CharLengthParameter.COLON)
