# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab


''' Performs py.test tests on the support for literals in the fparser2
    PSyIR front-end '''

import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003

from psyclone.errors import InternalError, GenerationError
from psyclone.psyir.frontend import fparser2
from psyclone.psyir.frontend.fparser2 import Fparser2Reader, \
    get_literal_precision
from psyclone.psyir.nodes import (
    Node, Literal, CodeBlock, Schedule, Assignment, Routine)
from psyclone.psyir.symbols import (
    ScalarType, DataSymbol, INTEGER_TYPE, UnsupportedFortranType,
    SymbolTable)


@pytest.mark.parametrize("code, dtype",
                         [("'hello'", ScalarType.Intrinsic.CHARACTER),
                          ('''"('hello: ',3A)"''',
                           ScalarType.Intrinsic.CHARACTER),
                          ('"hello"', ScalarType.Intrinsic.CHARACTER),
                          ("1", ScalarType.Intrinsic.INTEGER),
                          ("1.0", ScalarType.Intrinsic.REAL),
                          (".tRue.", ScalarType.Intrinsic.BOOLEAN),
                          (".false.", ScalarType.Intrinsic.BOOLEAN)])
@pytest.mark.usefixtures("f2008_parser")
def test_handling_literal(code, dtype):
    ''' Check that the fparser2 frontend can handle literals of all
    supported datatypes. Note that signed literals are represented in the
    PSyIR as a Unary operation on an unsigned literal.

    Note that because of fparser issue #295 we must include the quotation marks
    with supplied character literals. Once that issue is done, these can
    be removed.

    '''
    reader = FortranStringReader("x=" + code)
    astmt = Fortran2003.Assignment_Stmt(reader)
    fake_parent = Schedule()
    # Ensure the symbol table has an entry for "x"
    fake_parent.symbol_table.add(
        DataSymbol("x", UnsupportedFortranType("blah :: x")))
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [astmt])
    assert not fake_parent.walk(CodeBlock)
    literal = fake_parent.children[0].children[1]
    assert isinstance(literal, Literal)
    assert literal.datatype.intrinsic == dtype
    if dtype == ScalarType.Intrinsic.BOOLEAN:
        # Remove wrapping dots and lower the case
        assert literal.value == code.lower()[1:-1]
    elif dtype == ScalarType.Intrinsic.CHARACTER:
        # Remove wrapping quotes
        assert literal.value == code[1:-1]
    else:
        assert literal.value == code


def test_handling_literal_char(fortran_reader):
    ''' Check that the special cases where '' must be interpreted as ' and
    "" as " result in CodeBlocks. (See Note 4.12 in the Fortran 2003
    standard.) Also tests that any empty string is handled correctly. '''
    code = """program my_prog
  implicit none
  character(len=32) :: my_str1, my_str2, my_str3, my_str4, fmt_str
  my_str1 = 'a cat''s mat'
  my_str2 = "a cat""s mat"
  my_str3 = "a cat''s mat"
  my_str4 = 'a cat""s mat'
  fmt_str = "('Let''s see a cat''s mat')"
  my_str3 = ''
  my_str4 = ""

end program my_prog
"""
    prog = fortran_reader.psyir_from_source(code)
    assigns = prog.walk(Assignment)
    for assign in assigns[:5]:
        assert isinstance(assign.rhs, CodeBlock)
    for assign in assigns[5:]:
        assert isinstance(assign.rhs, Literal)
        assert assign.rhs.datatype.intrinsic == ScalarType.Intrinsic.CHARACTER
        assert assign.rhs.value == ""


@pytest.mark.usefixtures("f2008_parser")
def test_literal_char_without_quotes_error():
    ''' Test that the check in the handler that the provided string is quoted
    works as expected. This will need to be changed once fparser #295 is
    done. '''
    reader = FortranStringReader("x = 'hello'")
    astmt = Fortran2003.Assignment_Stmt(reader)
    # Edit the resulting parse tree to remove the quotes
    astmt.children[2].items = ("hello", None)
    sched = Schedule()
    sched.symbol_table.add(
        DataSymbol("x", UnsupportedFortranType("blah :: x")))
    processor = Fparser2Reader()
    with pytest.raises(InternalError) as err:
        processor.process_nodes(sched, [astmt])
    assert ("Char literal handler expects a quoted value but got: >>hello<<"
            in str(err.value))


@pytest.mark.parametrize("value,dprecision,intrinsic",
                         [("0.0", "rdef", ScalarType.Intrinsic.REAL),
                          ("1", "idef", ScalarType.Intrinsic.INTEGER),
                          ("'hEllo'", "cdef", ScalarType.Intrinsic.CHARACTER),
                          (".tRue.", "ldef", ScalarType.Intrinsic.BOOLEAN),
                          (".false.", "ldef", ScalarType.Intrinsic.BOOLEAN)])
@pytest.mark.usefixtures("f2008_parser")
def test_handling_literal_precision_1(value, dprecision, intrinsic):
    '''Check that the fparser2 frontend can handle literals with a
    specified precision kind symbol.

    '''
    if intrinsic == ScalarType.Intrinsic.CHARACTER:
        code = f"x={dprecision}_{value}"
    else:
        code = f"x={value}_{dprecision}"
    reader = FortranStringReader(code)
    astmt = Fortran2003.Assignment_Stmt(reader)
    fake_parent = Routine.create("fake", SymbolTable(), [])
    # Ensure the symbol table has an entry for "x"
    fake_parent.symbol_table.add(
        DataSymbol("x", ScalarType(ScalarType.Intrinsic.INTEGER, 4)))
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [astmt])
    assert not fake_parent.walk(CodeBlock)
    literal = fake_parent.children[0].children[1]
    assert isinstance(literal, Literal)
    assert literal.datatype.intrinsic == intrinsic
    if intrinsic == ScalarType.Intrinsic.BOOLEAN:
        assert f".{literal.value}." == value.lower()
    elif intrinsic == ScalarType.Intrinsic.CHARACTER:
        assert f"'{literal.value}'" == value
    else:
        assert literal.value == value
    assert isinstance(literal.datatype.precision, DataSymbol)
    assert literal.datatype.precision.name == dprecision
    assert isinstance(literal.datatype.precision.datatype,
                      ScalarType)
    assert (literal.datatype.precision.datatype.intrinsic ==
            ScalarType.Intrinsic.INTEGER)
    assert (fake_parent.symbol_table.lookup(dprecision) is
            literal.datatype.precision)


@pytest.mark.parametrize("value,dprecision,intrinsic",
                         [("0.0", 16, ScalarType.Intrinsic.REAL),
                          ("1", 8, ScalarType.Intrinsic.INTEGER),
                          ("'hellO'", 1, ScalarType.Intrinsic.CHARACTER),
                          (".tRue.", 4, ScalarType.Intrinsic.BOOLEAN),
                          (".false.", 8, ScalarType.Intrinsic.BOOLEAN)])
@pytest.mark.usefixtures("f2008_parser")
def test_handling_literal_precision_2(value, dprecision, intrinsic):
    '''Check that the fparser2 frontend can handle literals with a
    specified precision value.

    '''
    if intrinsic == ScalarType.Intrinsic.CHARACTER:
        code = f"x={dprecision}_{value}"
    else:
        code = f"x={value}_{dprecision}"
    reader = FortranStringReader(code)
    astmt = Fortran2003.Assignment_Stmt(reader)
    fake_parent = Schedule()
    # Ensure the symbol table has an entry for "x"
    fake_parent.symbol_table.add(
        DataSymbol("x", ScalarType(ScalarType.Intrinsic.INTEGER, 4)))
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [astmt])
    assert not fake_parent.walk(CodeBlock)
    literal = fake_parent.children[0].children[1]
    assert isinstance(literal, Literal)
    assert literal.datatype.intrinsic == intrinsic
    if intrinsic == ScalarType.Intrinsic.BOOLEAN:
        assert f".{literal.value}." == value.lower()
    elif intrinsic == ScalarType.Intrinsic.CHARACTER:
        assert f"'{literal.value}'" == value
    else:
        assert literal.value == value
    assert isinstance(literal.datatype.precision, int)
    assert literal.datatype.precision == dprecision


@pytest.mark.parametrize("value,dprecision",
                         [("0.0D0", ScalarType.Precision.DOUBLE),
                          ("0.0d0", ScalarType.Precision.DOUBLE),
                          ("0.0E0", ScalarType.Precision.SINGLE),
                          ("0.0e0", ScalarType.Precision.SINGLE)])
@pytest.mark.usefixtures("f2008_parser", "disable_declaration_check")
def test_handling_literal_precision_3(value, dprecision):
    '''Check that the fparser2 frontend can handle literals with a
    precision value specified by the exponent. The literal value

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    should always use a lower case "e" for the exponent.
    '''
    code = f"x={value}"
    reader = FortranStringReader(code)
    astmt = Fortran2003.Assignment_Stmt(reader)
    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [astmt])
    assert not fake_parent.walk(CodeBlock)
    literal = fake_parent.children[0].children[1]
    assert isinstance(literal, Literal)
    assert literal.value.lower() == "0.0e0"
    assert literal.datatype.intrinsic == ScalarType.Intrinsic.REAL
    assert literal.datatype.precision == dprecision


@pytest.mark.parametrize("value,result",
                         [(".3", "0.3"), (".3e4", "0.3e4")])
@pytest.mark.usefixtures("f2008_parser", "disable_declaration_check")
def test_literal_constant_value_format(value, result):
    '''Test that the Fortran real literal value format which does not have
    a digit before the decimal point is modified to include a "0"
    e.g. ".3" -> "0.3", "-.3e4" -> "-0.3e4"

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    reader = FortranStringReader(f"a = {value}")
    astmt = Fortran2003.Assignment_Stmt(reader)
    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [astmt])
    literal = fake_parent.children[0].children[1]
    assert isinstance(literal, Literal)
    assert literal.value == result
    assert literal.datatype.intrinsic == ScalarType.Intrinsic.REAL


@pytest.mark.usefixtures("f2008_parser", "disable_declaration_check")
def test_handling_invalid_logic_literal():
    ''' Test that a logic fparser2 literal with an invalid value produces
    an error.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    reader = FortranStringReader("x = .true.")
    astmt = Fortran2003.Assignment_Stmt(reader)
    astmt.items[2].items = ('invalid', None)
    fake_parent = Schedule()
    processor = Fparser2Reader()
    with pytest.raises(GenerationError) as error:
        processor.process_nodes(fake_parent, [astmt])
    assert "Expected to find '.true.' or '.false.' as fparser2 logical " \
        "literal, but found 'invalid' instead." in str(error.value)


def test_number_handler():
    ''' Check that the number_handler raises a NotImplementedError for an
    unrecognised fparser2 node. '''
    processor = Fparser2Reader()
    fake_parent = Schedule()
    reader = FortranStringReader("(1.0, 1.0)")
    with pytest.raises(NotImplementedError):
        processor._number_handler(
            Fortran2003.Complex_Literal_Constant(reader), fake_parent)


# The get_literal_precision() function is covered by the
# test_handling_literal_precision_{1-3} tests above, apart from
# invalid arguments and unsupported datatypes which are tested in the
# next two tests.
@pytest.mark.usefixtures("f2008_parser")
def test_get_literal_precision():
    '''Make sure the get_literal_precision function in fparser2.py behaves
    as expected when the arguments are invalid.

    '''
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
    assert ("Expecting argument psyir_literal_parent to be a PSyIR Node but "
            "found 'NoneType' in get_literal_precision." in str(excinfo.value))


@pytest.mark.usefixtures("f2008_parser")
def test_get_literal_precision_type(monkeypatch):
    '''Make sure the get_literal_precision function in fparser2.py behaves
    as expected when an unsupported datatype is found

    '''
    monkeypatch.setattr(fparser2, "CONSTANT_TYPE_MAP", {})
    code = "x=0.0"
    reader = FortranStringReader(code)
    astmt = Fortran2003.Assignment_Stmt(reader)
    fparser2_literal = astmt.children[2]
    with pytest.raises(NotImplementedError) as excinfo:
        _ = get_literal_precision(fparser2_literal, Node())
    assert ("Could not process Real_Literal_Constant. Only 'real', 'integer', "
            "'logical' and 'character' intrinsic types are supported."
            in str(excinfo.value))


@pytest.mark.usefixtures("f2008_parser")
def test_get_literal_precision_missing_table():
    ''' Check that get_literal_precision raises the expected error if it
    fails to find a symbol table. '''
    code = "x=0.0_rdef"
    reader = FortranStringReader(code)
    astmt = Fortran2003.Assignment_Stmt(reader)
    # Pass get_literal_precision just a Literal() (which does not have an
    # associated symbol table).
    with pytest.raises(InternalError) as err:
        get_literal_precision(astmt.children[2], Literal("1", INTEGER_TYPE))
    assert ("Failed to find a symbol table to which to add the kind"
            in str(err.value))
