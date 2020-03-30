# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council.
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
from psyclone.psyir.symbols import ScalarType, DataSymbol, DeferredType
from psyclone.psyir.nodes import Node, Literal, CodeBlock
from psyclone.errors import InternalError


@pytest.mark.parametrize("code, dtype",
                         [("'hello'", ScalarType.Name.CHARACTER),
                          ("1", ScalarType.Name.INTEGER),
                          ("1.0", ScalarType.Name.REAL),
                          (".tRue.", ScalarType.Name.BOOLEAN),
                          (".false.", ScalarType.Name.BOOLEAN)])
@pytest.mark.usefixtures("f2008_parser")
def test_handling_literal(code, dtype):
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
    assert literal.datatype.name == dtype
    if dtype != ScalarType.Name.BOOLEAN:
        assert literal.value == code
    else:
        assert literal.value == code.lower()[1:-1]  # Remove wrapping dots


@pytest.mark.parametrize("value,dprecision,dname",
                         [("0.0", "rdef", ScalarType.Name.REAL),
                          ("1", "idef", ScalarType.Name.INTEGER),
                          ("'hello'", "cdef", ScalarType.Name.CHARACTER),
                          (".tRue.", "ldef", ScalarType.Name.BOOLEAN),
                          (".false.", "ldef", ScalarType.Name.BOOLEAN)])
@pytest.mark.usefixtures("f2008_parser")
def test_handling_literal_precision_1(value, dprecision, dname):
    '''Check that the fparser2 frontend can handle literals with a
    specified precision kind symbol.

    '''
    if dname == ScalarType.Name.CHARACTER:
        code = "x={0}_{1}".format(dprecision, value)
    else:
        code = "x={0}_{1}".format(value, dprecision)
    reader = FortranStringReader(code)
    astmt = Fortran2003.Assignment_Stmt(reader)
    fake_parent = Node()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [astmt])
    assert not fake_parent.walk(CodeBlock)
    literal = fake_parent.children[0].children[1]
    assert isinstance(literal, Literal)
    assert literal.datatype.name == dname
    if dname == ScalarType.Name.BOOLEAN:
        assert ".{0}.".format(literal.value) == value.lower()
    else:
        assert literal.value == value
    assert isinstance(literal.datatype.precision, DataSymbol)
    assert literal.datatype.precision.name == dprecision
    assert isinstance(literal.datatype.precision.datatype, DeferredType)


@pytest.mark.parametrize("value,dprecision,dname",
                         [("0.0", 16, ScalarType.Name.REAL),
                          ("1", 8, ScalarType.Name.INTEGER),
                          ("'hello'", 1, ScalarType.Name.CHARACTER),
                          (".tRue.", 4, ScalarType.Name.BOOLEAN),
                          (".false.", 8, ScalarType.Name.BOOLEAN)])
@pytest.mark.usefixtures("f2008_parser")
def test_handling_literal_precision_2(value, dprecision, dname):
    '''Check that the fparser2 frontend can handle literals with a
    specified precision value.

    '''
    if dname == ScalarType.Name.CHARACTER:
        code = "x={0}_{1}".format(dprecision, value)
    else:
        code = "x={0}_{1}".format(value, dprecision)
    reader = FortranStringReader(code)
    astmt = Fortran2003.Assignment_Stmt(reader)
    fake_parent = Node()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [astmt])
    assert not fake_parent.walk(CodeBlock)
    literal = fake_parent.children[0].children[1]
    assert isinstance(literal, Literal)
    assert literal.datatype.name == dname
    if dname == ScalarType.Name.BOOLEAN:
        assert ".{0}.".format(literal.value) == value.lower()
    else:
        assert literal.value == value
    assert isinstance(literal.datatype.precision, int)
    assert literal.datatype.precision == dprecision


@pytest.mark.parametrize("value,dprecision",
                         [("0.0D0", ScalarType.Precision.DOUBLE),
                          ("0.0d0", ScalarType.Precision.DOUBLE),
                          ("0.0E0", ScalarType.Precision.SINGLE),
                          ("0.0e0", ScalarType.Precision.SINGLE)])
@pytest.mark.usefixtures("f2008_parser")
def test_handling_literal_precision_3(value,dprecision):
    '''Check that the fparser2 frontend can handle literals with a
    precision value specified by the exponent.

    '''
    code = "x={0}".format(value)
    reader = FortranStringReader(code)
    astmt = Fortran2003.Assignment_Stmt(reader)
    fake_parent = Node()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [astmt])
    assert not fake_parent.walk(CodeBlock)
    literal = fake_parent.children[0].children[1]
    assert isinstance(literal, Literal)
    assert literal.value.lower() == value.lower()
    assert literal.datatype.name == ScalarType.Name.REAL
    assert literal.datatype.precision == dprecision


@pytest.mark.usefixtures("f2008_parser")
def test_handling_invalid_logic_literal():
    ''' Test that a logic fparser2 literal with an invalid value produces
    an error.'''
    from psyclone.errors import GenerationError
    reader = FortranStringReader("x = .true.")
    astmt = Fortran2003.Assignment_Stmt(reader)
    astmt.items[2].items = ('invalid', None)
    fake_parent = Node()
    processor = Fparser2Reader()
    with pytest.raises(GenerationError) as error:
        processor.process_nodes(fake_parent, [astmt])
    assert "Expected to find '.true.' or '.false.' as fparser2 logical " \
        "literal, but found 'invalid' instead." in str(error.value)


def test_number_handler():
    ''' Check that the number_handler raises a NotImplementedError for an
    unrecognised fparser2 node. '''
    processor = Fparser2Reader()
    fake_parent = Node()
    reader = FortranStringReader("(1.0, 1.0)")
    with pytest.raises(NotImplementedError):
        processor._number_handler(
            Fortran2003.Complex_Literal_Constant(reader), fake_parent)

        
# The get_literal_precision() function is covered by the
# test_handling_literal_precision_{1-3} tests above, apart from
# invalid arguments which are tested here.
@pytest.mark.usefixtures("f2008_parser")
def test_get_literal_precision():
    '''Make sure the get_literal_precision function in fparser2.py behaves
    as expected when the arguments are invalid.

    '''
    from psyclone.psyir.frontend.fparser2 import get_literal_precision
    with pytest.raises(InternalError) as excinfo:
        _ = get_literal_precision(None, None)
    assert ("Unsupported literal type 'NoneType' found in "
            "get_literal_precision." in str(excinfo.value))
    code = "x=0.0"
    reader = FortranStringReader(code)
    astmt = Fortran2003.Assignment_Stmt(reader)
    fparser2_literal = astmt.children[2]
    with pytest.raises(InternalError) as excinfo:
        _ = get_literal_precision(fparser2_literal, None)
    assert ("Expecting argument psyir_node to be a PSyIR Node but found "
            "'NoneType' in get_literal_precision." in str(excinfo.value))
