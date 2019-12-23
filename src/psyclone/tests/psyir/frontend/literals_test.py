# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab


''' Performs py.test tests on the support for literals in the fparser2
    PSyIR front-end '''

from __future__ import absolute_import
import pytest
from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.symbols import DataType
from psyclone.psyGen import Node, Literal, CodeBlock


@pytest.mark.parametrize("code, dtype", [("'hello'", DataType.CHARACTER),
                                         ("1", DataType.INTEGER),
                                         ("1.0", DataType.REAL),
                                         (".tRue.", DataType.BOOLEAN),
                                         (".false.", DataType.BOOLEAN)])
def test_handling_literal(f2008_parser, code, dtype):
    ''' Check that the fparser2 frontend can handle literals of all
    supported datatypes. Note that signed literals are represented in the
    PSyIR as a Unary operation on an unsigned literal. '''
    reader = FortranStringReader("x=" + code)
    astmt = Fortran2003.Assignment_Stmt(reader)
    fake_parent = Node()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [astmt])
    assert not fake_parent.walk(CodeBlock)
    literal = fake_parent.children[0].children[1]
    assert isinstance(literal, Literal)
    assert literal.datatype == dtype
    if dtype != DataType.BOOLEAN:
        assert literal.value == code
    else:
        assert literal.value == ("true" in code.lower())


def test_literal_datatype():
    ''' Tests for the setting (via constructor) and getter of the
    Literal.datatype property. '''
    with pytest.raises(TypeError) as err:
        Literal("1", 1)
    assert ("datatype of a Literal must be an instance of psyir.symbols."
            "DataType but got" in str(err.value))
    with pytest.raises(ValueError) as err:
        Literal("1", DataType.DEFERRED)
    assert "datatype of a Literal must be one of" in str(err.value)
    lval = Literal(False, DataType.BOOLEAN)
    assert lval.value is False
    with pytest.raises(TypeError) as err:
        Literal("true", DataType.BOOLEAN)
    assert ("boolean Literal must be supplied with a value that is a bool "
            "but got" in str(err.value))
    with pytest.raises(TypeError) as err:
        Literal(True, DataType.INTEGER)
    assert ("non-boolean Literal must be supplied with a value encoded as a "
            "string but got" in str(err.value))


def test_number_handler():
    ''' Check that the number_handler raises a NotImplementedError for an
    unrecognised fparser2 node. '''
    processor = Fparser2Reader()
    fake_parent = Node()
    reader = FortranStringReader("(1.0, 1.0)")
    with pytest.raises(NotImplementedError):
        processor._number_handler(
            Fortran2003.Complex_Literal_Constant(reader), fake_parent)
