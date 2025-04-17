# Modified work Copyright (c) 2017-2024 Science and Technology
# Facilities Council.
# Original work Copyright (c) 1999-2008 Pearu Peterson
#
# All rights reserved.
#
# Modifications made as part of the fparser project are distributed
# under the following license:
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# --------------------------------------------------------------------

# The original software (in the f2py project) was distributed under
# the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:

#   a. Redistributions of source code must retain the above copyright notice,
#      this list of conditions and the following disclaimer.
#   b. Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#   c. Neither the name of the F2PY project nor the names of its
#      contributors may be used to endorse or promote products derived from
#      this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.

"""
Module containing py.test tests for Fortran 2003 language constructs
"""

import pytest

from fparser.two.Fortran2003 import *
from fparser.two import Fortran2003
from fparser.two.symbol_table import SYMBOL_TABLES
from fparser.two.utils import NoMatchError
from fparser.two import utils
from fparser.api import get_reader


@pytest.fixture(autouse=True)
def auto_f2003_create(f2003_create):
    """Since all of the tests in this file need the `f2003_create` fixture,
    this fixture simply adds 'autouse=True' to it."""


def assert_raises(exc, fcls, string):
    """
    Asserts that the appropriate error is raised when a Fortran
    string is not parsed correctly.

    :param exc: the error to be raised
    :type exc: :py:class:'fparser.two.utils.NoMatchError'
    :param fcls: the class of Fortran object to create
    :type fcls: names of classes deriving from `:py:class:Base` or str
    :param string: (source of) Fortran string to parse
    :type string: str or :py:class:`FortranReaderBase`
    """
    try:
        fcls(string)
        raise AssertionError("Expected {0} but got nothing".format(exc))
    except exc:
        pass


#
# SECTION 2
#


def test_specification_part():
    """Tests for parsing specification-part (R204)."""
    reader = get_reader(
        """\
    integer a"""
    )
    tcls = Specification_Part
    obj = tcls(reader)
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "INTEGER :: a"
    assert (
        repr(obj) == "Specification_Part(Type_Declaration_Stmt("
        "Intrinsic_Type_Spec('INTEGER', None), None, "
        "Entity_Decl_List(',', (Entity_Decl(Name('a'), None, None, "
        "None),))))"
    )
    # Check that parent information is correctly setup
    assert obj.parent == None
    assert obj.content[0].parent is obj
    assert obj.content[0].items[0].parent is obj.content[0]
    assert obj.content[0].items[2].parent is obj.content[0]
    assert obj.content[0].items[2].items[0].parent is obj.content[0].items[2]

    obj = tcls(
        get_reader(
            """\
type a
end type a
type b
end type b
"""
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert "TYPE :: a\nEND TYPE a\nTYPE :: b\nEND TYPE b" in str(obj)


#
# SECTION  3
#


def test_constant():
    """Tests that various types of constant expressions are parsed
    correctly (R305). The example here is for Literal_Constant
    subclass. Other literal constant types are tested separately.
    """
    tcls = Constant
    obj = tcls(".false.")
    assert isinstance(obj, Logical_Literal_Constant), repr(obj)
    assert str(obj) == ".FALSE."


def test_literal_constant():
    """Tests that various types of literal constant expressions are
    parsed correctly (R306).
    """
    tcls = Literal_Constant
    obj = tcls("35")
    assert isinstance(obj, Int_Literal_Constant), repr(obj)
    assert str(obj) == "35"

    obj = tcls("2.85e-13")
    assert isinstance(obj, Real_Literal_Constant), repr(obj)
    assert str(obj) == "2.85E-13"

    obj = tcls("(PI,-2.0E-3)")
    assert isinstance(obj, Complex_Literal_Constant), repr(obj)
    assert str(obj) == "(PI, -2.0E-3)"

    obj = tcls(".true.")
    assert isinstance(obj, Logical_Literal_Constant), repr(obj)
    assert str(obj) == ".TRUE."

    obj = tcls("'(3(A5,1X))'")
    assert isinstance(obj, Char_Literal_Constant), repr(obj)
    assert str(obj) == "'(3(A5,1X))'"

    obj = tcls('B"01011101"')
    assert isinstance(obj, Binary_Constant), repr(obj)
    assert str(obj) == 'B"01011101"'


#
# SECTION 4
#


def test_type_param_value():  # R402
    tcls = Type_Param_Value
    obj = tcls("*")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "*"
    assert repr(obj) == "Type_Param_Value('*')"

    obj = tcls(":")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == ":"

    obj = tcls("1+2")
    assert isinstance(obj, Level_2_Expr), repr(obj)
    assert str(obj) == "1 + 2"


def test_intrinsic_type_spec():  # R403
    tcls = Intrinsic_Type_Spec
    obj = tcls("INTEGER")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "INTEGER"
    assert repr(obj) == "Intrinsic_Type_Spec('INTEGER', None)"

    obj = tcls("Integer*2")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "INTEGER*2"

    obj = tcls("real*2")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "REAL*2"

    obj = tcls("logical*2")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "LOGICAL*2"

    obj = tcls("complex*2")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "COMPLEX*2"

    obj = tcls("character*2")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "CHARACTER*2"

    obj = tcls("double complex")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "DOUBLE COMPLEX"

    obj = tcls("double  precision")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "DOUBLE PRECISION"


def test_signed_int_literal_constant():  # R405
    # pylint: disable=invalid-name

    tcls = Signed_Int_Literal_Constant
    obj = tcls("1")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "1"
    assert repr(obj) == "%s('1', None)" % (tcls.__name__)

    obj = tcls("+ 21_2")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "+21_2"
    assert repr(obj) == "%s('+21', '2')" % (tcls.__name__)

    obj = tcls("-21_SHORT")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "-21_SHORT"

    obj = tcls("21_short")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "21_short"

    obj = tcls("+1976354279568241_8")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "+1976354279568241_8"


def test_int_literal_constant():  # R406
    tcls = Int_Literal_Constant
    obj = tcls("1")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "1"
    assert repr(obj) == "%s('1', None)" % (tcls.__name__)

    obj = tcls("21_2")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "21_2"
    assert repr(obj) == "%s('21', '2')" % (tcls.__name__)

    obj = tcls("21_SHORT")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "21_SHORT"

    obj = tcls("21_short")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "21_short"

    obj = tcls("1976354279568241_8")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "1976354279568241_8"


def test_binary_constant():  # R412
    tcls = Boz_Literal_Constant
    bcls = Binary_Constant
    obj = tcls('B"01"')
    assert isinstance(obj, bcls), repr(obj)
    assert str(obj) == 'B"01"'
    assert repr(obj) == "%s('B\"01\"')" % (bcls.__name__)


def test_octal_constant():  # R413
    tcls = Boz_Literal_Constant
    ocls = Octal_Constant
    obj = tcls('O"017"')
    assert isinstance(obj, ocls), repr(obj)
    assert str(obj) == 'O"017"'
    assert repr(obj) == "%s('O\"017\"')" % (ocls.__name__)


def test_hex_constant():  # R414
    tcls = Boz_Literal_Constant
    zcls = Hex_Constant
    obj = tcls('Z"01A"')
    assert isinstance(obj, zcls), repr(obj)
    assert str(obj) == 'Z"01A"'
    assert repr(obj) == "%s('Z\"01A\"')" % (zcls.__name__)


def test_signed_real_literal_constant():
    """Tests that various formats of a signed ("+", "-") real
    literal constant are parsed correctly (R416)."""
    # pylint: disable=invalid-name

    tcls = Signed_Real_Literal_Constant
    obj = tcls("12.78")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "12.78"
    assert repr(obj) == "%s('12.78', None)" % (tcls.__name__)

    obj = tcls("+12.78_8")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "+12.78_8"
    assert repr(obj) == "%s('+12.78', '8')" % (tcls.__name__)

    obj = tcls("- 12.")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "-12."

    obj = tcls(".123")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == ".123"

    obj = tcls("3E4")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "3E4"

    # Tests for double precision format identifier ("D"),
    # exponential format identifier ("E")
    # and numerically defined precision ("8")
    real_str = [
        "1.6D3",
        "-1.6d-3",
        "1.6E3",
        "1.6e+3",
        "+1.6e-3",
        "-1.6E-3",
        "+1.6E3_8",
        "-1.6e-3_8",
    ]
    for rstr in real_str:
        obj = tcls(rstr)
        assert isinstance(obj, tcls), repr(obj)
        assert str(obj) == rstr.upper()

    obj = tcls("10.9E7_QUAD")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "10.9E7_QUAD"

    obj = tcls("-10.9e-17_quad")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "-10.9E-17_quad"


def test_real_literal_constant():
    """Tests that various formats of a real literal constant
    are parsed correctly (R417)."""

    tcls = Real_Literal_Constant
    obj = tcls("12.78")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "12.78"
    assert repr(obj) == "%s('12.78', None)" % (tcls.__name__)

    obj = tcls("12.78_8")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "12.78_8"
    assert repr(obj) == "%s('12.78', '8')" % (tcls.__name__)

    obj = tcls("12.")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "12."

    obj = tcls(".123")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == ".123"

    obj = tcls("3E4")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "3E4"

    # Tests for double precision format identifier ("D"),
    # exponential format identifier ("E")
    # and numerically defined precision ("8")
    real_str = [
        "0.0D+0",
        "1.6d3",
        "1.6D-3",
        "1.6E3",
        "1.6E+3",
        "1.6E-3",
        "1.6e3_8",
        "1.6E-3_8",
    ]
    for rstr in real_str:
        obj = tcls(rstr)
        assert isinstance(obj, tcls), repr(obj)
        assert str(obj) == rstr.upper()

    obj = tcls("10.9E7_QUAD")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "10.9E7_QUAD"

    obj = tcls("10.9e-17_quad")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "10.9E-17_quad"


def test_char_selector():  # R424
    tcls = Char_Selector
    obj = tcls("(len=2, kind=8)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "(LEN = 2, KIND = 8)"
    assert (
        repr(obj) == "Char_Selector(Int_Literal_Constant('2', None), "
        "Int_Literal_Constant('8', None))"
    )

    obj = tcls("(2, kind=8)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "(LEN = 2, KIND = 8)"

    obj = tcls("(2, 8)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "(LEN = 2, KIND = 8)"

    obj = tcls("(kind=8)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "(KIND = 8)"

    obj = tcls("(kind=8,len=2)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "(LEN = 2, KIND = 8)"


def test_complex_literal_constant():  # R421
    tcls = Complex_Literal_Constant
    obj = tcls("(1.0, -1.0)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "(1.0, -1.0)"
    assert (
        repr(obj) == "Complex_Literal_Constant(Signed_Real_Literal_Constant("
        "'1.0', None), Signed_Real_Literal_Constant('-1.0', None))"
    )

    obj = tcls("(3,3.1E6)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "(3, 3.1E6)"

    obj = tcls("(4.0_4, 3.6E7_8)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "(4.0_4, 3.6E7_8)"

    obj = tcls("( 0., PI)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "(0., PI)"


def test_type_name():  # C424
    tcls = Type_Name
    obj = tcls("a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a"
    assert repr(obj) == "Type_Name('a')"

    assert_raises(NoMatchError, tcls, "integer")
    assert_raises(NoMatchError, tcls, "doubleprecision")


def test_length_selector():  # R425
    tcls = Length_Selector
    obj = tcls("( len = *)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "(LEN = *)"
    assert repr(obj) == "Length_Selector('(', Type_Param_Value('*'), ')')"

    obj = tcls("*2,")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "*2"


def test_char_length():  # R426
    tcls = Char_Length
    obj = tcls("(1)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "(1)"
    assert repr(obj) == "Char_Length('(', Int_Literal_Constant('1', None), ')')"

    obj = tcls("1")
    assert isinstance(obj, Int_Literal_Constant), repr(obj)
    assert str(obj) == "1"

    obj = tcls("(*)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "(*)"

    obj = tcls("(:)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "(:)"


def test_logical_literal_constant():  # R428
    tcls = Logical_Literal_Constant
    obj = tcls(".TRUE.")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == ".TRUE."
    assert repr(obj) == "%s('.TRUE.', None)" % (tcls.__name__)

    obj = tcls(".True.")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == ".TRUE."

    obj = tcls(".FALSE.")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == ".FALSE."

    obj = tcls(".false.")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == ".FALSE."

    obj = tcls(".TRUE._HA")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == ".TRUE._HA"


def test_type_attr_spec():  # R431
    tcls = Type_Attr_Spec
    obj = tcls("abstract")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "ABSTRACT"
    assert repr(obj) == "Type_Attr_Spec('ABSTRACT', None)"

    obj = tcls("bind (c )")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "BIND(C)"

    obj = tcls("extends(a)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "EXTENDS(a)"

    obj = tcls("private")
    assert isinstance(obj, Access_Spec), repr(obj)
    assert str(obj) == "PRIVATE"


def test_end_type_stmt():  # R433
    tcls = End_Type_Stmt
    obj = tcls("end type")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "END TYPE"
    assert repr(obj) == "End_Type_Stmt('TYPE', None)"

    obj = tcls("end type  a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "END TYPE a"


def test_sequence_stmt():  # R434
    tcls = Sequence_Stmt
    obj = tcls("sequence")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "SEQUENCE"
    assert repr(obj) == "Sequence_Stmt('SEQUENCE')"


def test_type_param_def_stmt():  # R435
    tcls = Type_Param_Def_Stmt
    obj = tcls("integer ,kind :: a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "INTEGER, KIND :: a"
    assert (
        repr(obj) == "Type_Param_Def_Stmt(None, Type_Param_Attr_Spec('KIND'), "
        "Type_Param_Decl_List(',', (Name('a'),)))"
    )

    obj = tcls("integer*2 ,len :: a=3, b=2+c")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "INTEGER*2, LEN :: a = 3, b = 2 + c"


def test_type_param_decl():  # R436
    tcls = Type_Param_Decl
    obj = tcls("a=2")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a = 2"
    assert (
        repr(obj) == "Type_Param_Decl(Name('a'), '=', Int_Literal_Constant('2', None))"
    )

    obj = tcls("a")
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == "a"


def test_type_param_attr_spec():  # R437
    tcls = Type_Param_Attr_Spec
    obj = tcls("kind")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "KIND"
    assert repr(obj) == "Type_Param_Attr_Spec('KIND')"

    obj = tcls("len")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "LEN"


def test_component_attr_spec():  # R441
    tcls = Component_Attr_Spec
    obj = tcls("pointer")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "POINTER"
    assert repr(obj) == "Component_Attr_Spec('POINTER')"

    obj = tcls("allocatable")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "ALLOCATABLE"

    obj = tcls("dimension(a)")
    assert isinstance(obj, Dimension_Component_Attr_Spec), repr(obj)
    assert str(obj) == "DIMENSION(a)"

    obj = tcls("private")
    assert isinstance(obj, Access_Spec), repr(obj)
    assert str(obj) == "PRIVATE"


def test_component_decl():  # R442
    tcls = Component_Decl
    obj = tcls("a(1)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a(1)"
    assert (
        repr(obj) == "Component_Decl(Name('a'), Explicit_Shape_Spec_List(',', "
        "(Explicit_Shape_Spec(None, Int_Literal_Constant('1', None)),)), "
        "None, None)"
    )

    obj = tcls("a(1)*(3)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a(1)*(3)"

    obj = tcls("a(1)*(3) = 2")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a(1)*(3) = 2"

    obj = tcls("a(1) => NULL")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a(1) => NULL"


def test_proc_component_def_stmt():  # R445
    tcls = Proc_Component_Def_Stmt
    obj = tcls("procedure(), pointer :: a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "PROCEDURE(), POINTER :: a"

    obj = tcls("procedure(real*8), pointer, pass(n) :: a, b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "PROCEDURE(REAL*8), POINTER, PASS(n) :: a, b"


def test_private_components_stmt():
    """Tests that declaration of PRIVATE components in a type definition
    is parsed correctly (R447)."""
    tcls = Private_Components_Stmt
    obj = tcls("private")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "PRIVATE"
    assert repr(obj) == "Private_Components_Stmt('PRIVATE')"

    # Statement not 'private'
    with pytest.raises(NoMatchError) as excinfo:
        _ = tcls("public")
    assert "Private_Components_Stmt: 'public'" in str(excinfo.value)


def test_type_bound_procedure_part():
    """Tests for type-bound procedure (R448)."""
    tcls = Type_Bound_Procedure_Part
    obj = tcls(
        get_reader(
            """\
contains
procedure, pass :: length => point_length"""
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert "CONTAINS\nPROCEDURE, PASS :: length => point_length" in str(obj)


def test_proc_binding_stmt():  # R450
    tcls = Proc_Binding_Stmt
    obj = tcls("procedure, pass :: length => point_length")
    assert isinstance(obj, Specific_Binding), repr(obj)
    assert str(obj) == "PROCEDURE, PASS :: length => point_length"


def test_generic_binding():  # R452
    tcls = Generic_Binding
    obj = tcls("generic :: a => b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "GENERIC :: a => b"

    obj = tcls("generic, private :: read(formatted) => b,c")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "GENERIC, PRIVATE :: READ(FORMATTED) => b, c"


def test_final_binding():  # R454
    tcls = Final_Binding
    obj = tcls("final a, b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "FINAL :: a, b"
    assert (
        repr(obj) == "Final_Binding('FINAL', Final_Subroutine_Name_List(',', "
        "(Name('a'), Name('b'))))"
    )

    obj = tcls("final::a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "FINAL :: a"


def test_derived_type_spec():  # R455
    tcls = Derived_Type_Spec
    obj = tcls("a(b)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a(b)"
    assert repr(obj) == (
        "Derived_Type_Spec(Type_Name('a'), " "Type_Param_Spec_List(',', (Name('b'),)))"
    )

    obj = tcls("a(b,c,g=1)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a(b, c, g = 1)"

    obj = tcls("a")
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == "a"

    obj = tcls("a()")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a()"


def test_type_param_spec():  # R456
    tcls = Type_Param_Spec
    obj = tcls("a=1")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a = 1"
    assert repr(obj) == "Type_Param_Spec(Name('a'), Int_Literal_Constant('1', None))"

    obj = tcls("k=a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "k = a"

    obj = tcls("k=:")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "k = :"


def test_type_param_spec_list():  # R456-list
    tcls = Type_Param_Spec_List
    obj = tcls("a,b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a, b"
    assert repr(obj) == "Type_Param_Spec_List(',', (Name('a'), Name('b')))"

    obj = tcls("a")
    assert isinstance(obj, tcls), repr(obj)

    obj = tcls("k=a,c,g=1")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "k = a, c, g = 1"


def test_structure_constructor():  # R457
    tcls = Structure_Constructor
    obj = tcls("t()")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "t()"
    assert repr(obj) == "Structure_Constructor(Type_Name('t'), None)"

    obj = tcls("t(s=1, a=2)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "t(s = 1, a = 2)"


def test_component_spec():  # R458
    tcls = Component_Spec
    obj = tcls("k=a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "k = a"
    assert repr(obj) == "Component_Spec(Name('k'), Name('a'))"

    obj = tcls("a")
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == "a"

    obj = tcls("a % b")
    assert isinstance(obj, Proc_Component_Ref), repr(obj)
    assert str(obj) == "a % b"

    obj = tcls("s =a % b")
    assert isinstance(obj, Component_Spec), repr(obj)
    assert str(obj) == "s = a % b"


def test_component_spec_list():  # R458-list
    tcls = Component_Spec_List
    obj = tcls("k=a, b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "k = a, b"
    assert (
        repr(obj) == "Component_Spec_List(',', (Component_Spec(Name('k'), "
        "Name('a')), Name('b')))"
    )

    obj = tcls("k=a, c")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "k = a, c"


def test_enum_def():  # R460
    tcls = Enum_Def
    obj = tcls(
        get_reader(
            """\
enum, bind(c)
enumerator :: red = 4, blue = 9
enumerator yellow
end enum
    """
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert (
        str(obj) == "ENUM, BIND(C)\n  ENUMERATOR :: red = 4, blue = 9\n"
        "  ENUMERATOR :: yellow\nEND ENUM"
    )


def test_enum_def_stmt():  # R461
    tcls = Enum_Def_Stmt
    obj = tcls("enum, bind(c)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "ENUM, BIND(C)"


def test_array_constructor():  # R465
    tcls = Array_Constructor
    obj = tcls("(/a/)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "(/a/)"
    assert repr(obj) == (
        "Array_Constructor('(/', Ac_Value_List(',', " "(Name('a'),)), '/)')"
    )

    obj = tcls("[a]")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "[a]"
    assert repr(obj) == (
        "Array_Constructor('[', Ac_Value_List(',', " "(Name('a'),)), ']')"
    )

    obj = tcls("[integer::a]")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "[INTEGER :: a]"

    obj = tcls("[integer::a,b]")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "[INTEGER :: a, b]"


def test_ac_spec():  # R466
    tcls = Ac_Spec
    obj = tcls("integer ::")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "INTEGER ::"
    assert repr(obj) == "Ac_Spec(Intrinsic_Type_Spec('INTEGER', None), None)"

    obj = tcls("integer :: a,b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "INTEGER :: a, b"

    obj = tcls("a,b")
    assert isinstance(obj, Ac_Value_List), repr(obj)
    assert str(obj) == "a, b"

    obj = tcls("integer :: a, (a, b, n = 1, 5)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "INTEGER :: a, (a, b, n = 1, 5)"


def test_ac_value_list():  # R469-list
    tcls = Ac_Value_List
    obj = tcls("a, b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a, b"
    assert repr(obj) == "Ac_Value_List(',', (Name('a'), Name('b')))"

    obj = tcls("a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a"


def test_ac_implied_do():  # R470
    tcls = Ac_Implied_Do
    obj = tcls("( a, b, n = 1, 5 )")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "(a, b, n = 1, 5)"
    assert (
        repr(obj) == "Ac_Implied_Do(Ac_Value_List(',', (Name('a'), Name('b'))), "
        "Ac_Implied_Do_Control(Name('n'), [Int_Literal_Constant('1', "
        "None), Int_Literal_Constant('5', None)]))"
    )


def test_ac_implied_do_control():  # R471
    tcls = Ac_Implied_Do_Control
    obj = tcls("n = 3, 5")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "n = 3, 5"
    assert (
        repr(obj) == "Ac_Implied_Do_Control(Name('n'), [Int_Literal_Constant('3', "
        "None), Int_Literal_Constant('5', None)])"
    )

    obj = tcls("n = 3+1, 5, 1")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "n = 3 + 1, 5, 1"


#
# SECTION 5
#


def test_declaration_type_spec():  # R502
    tcls = Declaration_Type_Spec
    obj = tcls("Integer*2")
    assert isinstance(obj, Intrinsic_Type_Spec), repr(obj)
    assert str(obj) == "INTEGER*2"

    obj = tcls("type(foo)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "TYPE(foo)"
    assert repr(obj) == "Declaration_Type_Spec('TYPE', Type_Name('foo'))"

    # No content should not match.
    with pytest.raises(NoMatchError):
        obj = tcls("")


def test_attr_spec():  # R503
    tcls = Attr_Spec
    obj = tcls("allocatable")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "ALLOCATABLE"

    obj = tcls("dimension(a)")
    assert isinstance(obj, Dimension_Attr_Spec), repr(obj)
    assert str(obj) == "DIMENSION(a)"


def test_dimension_attr_spec():  # R503.d
    tcls = Dimension_Attr_Spec
    obj = tcls("dimension(a)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "DIMENSION(a)"
    assert (
        repr(obj) == "Dimension_Attr_Spec('DIMENSION', Explicit_Shape_Spec_List(',', "
        "(Explicit_Shape_Spec(None, Name('a')),)))"
    )


def test_intent_attr_spec():  # R503.f
    tcls = Intent_Attr_Spec
    obj = tcls("intent(in)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "INTENT(IN)"
    assert repr(obj) == "Intent_Attr_Spec('INTENT', Intent_Spec('IN'))"


def test_target_entity_decl():
    tcls = Target_Entity_Decl
    obj = tcls("a(1)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a(1)"
    assert (
        repr(obj) == "Target_Entity_Decl(Name('a'), Explicit_Shape_Spec_List(',', "
        "(Explicit_Shape_Spec(None, Int_Literal_Constant('1', None)),)), "
        "None, None)"
    )


def test_access_spec():  # R508
    tcls = Access_Spec
    obj = tcls("private")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "PRIVATE"
    assert repr(obj) == "Access_Spec('PRIVATE')"

    obj = tcls("public")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "PUBLIC"


def test_language_binding_spec():  # R509
    tcls = Language_Binding_Spec
    obj = tcls("bind(c)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "BIND(C)"
    assert repr(obj) == "Language_Binding_Spec(None)"

    obj = tcls('bind(c, name="hey")')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == 'BIND(C, NAME = "hey")'


def test_explicit_shape_spec():  # R511
    tcls = Explicit_Shape_Spec
    obj = tcls("a:b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a : b"
    assert repr(obj) == "Explicit_Shape_Spec(Name('a'), Name('b'))"

    obj = tcls("a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a"


def test_upper_bound():  # R513
    tcls = Upper_Bound
    obj = tcls("a")
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == "a"

    assert_raises(NoMatchError, tcls, "*")


def test_assumed_shape_spec():  # R514
    tcls = Assumed_Shape_Spec
    obj = tcls(":")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == ":"
    assert repr(obj) == "Assumed_Shape_Spec(None, None)"

    obj = tcls("a :")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a :"


def test_deferred_shape_spec():  # R515
    tcls = Deferred_Shape_Spec
    obj = tcls(":")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == ":"
    assert repr(obj) == "Deferred_Shape_Spec(None, None)"


def test_assumed_size_spec():  # R516
    tcls = Assumed_Size_Spec
    obj = tcls("*")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "*"
    assert repr(obj) == "Assumed_Size_Spec(None, None)"

    obj = tcls("1:*")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "1 : *"

    obj = tcls("a,1:*")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a, 1 : *"

    obj = tcls("a:b,1:*")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a : b, 1 : *"


def test_access_stmt():  # R518
    tcls = Access_Stmt
    obj = tcls("private")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "PRIVATE"
    assert repr(obj) == "Access_Stmt('PRIVATE', None)"

    obj = tcls("public a,b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "PUBLIC :: a, b"

    obj = tcls("public ::a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "PUBLIC :: a"


def test_data_stmt():  # R524
    tcls = Data_Stmt
    obj = tcls('DATA YOURNAME % AGE, YOURNAME % NAME / 35, "FRED BROWN" /')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == 'DATA YOURNAME % AGE, YOURNAME % NAME / 35, "FRED BROWN" /'

    obj = tcls('DATA NAME / "JOHN DOE" / MILES / 10 * 0 /')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == 'DATA NAME / "JOHN DOE" /, MILES / 10 * 0 /'

    obj = tcls("DATA MYNAME / PERSON (21, 'JOHN SMITH') /")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "DATA MYNAME / PERSON(21, 'JOHN SMITH') /"


def test_data_stmt_set():  # R525
    tcls = Data_Stmt_Set
    obj = tcls('MILES / 10 * "2/3" /')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == 'MILES / 10 * "2/3" /'


def test_data_implied_do():  # R527
    tcls = Data_Implied_Do
    obj = tcls("((SKEW (K, J), J = 1, K), K = 1, 100)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "((SKEW(K, J), J = 1, K), K = 1, 100)"


# R531-R534 are trivial


def test_dimension_stmt():  # R535
    tcls = Dimension_Stmt
    obj = tcls("dimension :: a(5)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "DIMENSION :: a(5)"
    assert (
        repr(obj) == "Dimension_Stmt([(Name('a'), Explicit_Shape_Spec_List(',', "
        "(Explicit_Shape_Spec(None, Int_Literal_Constant('5', "
        "None)),)))])"
    )

    obj = tcls("dimension a(n,m), b(:), c(2:n), d(*), e(n, 2:*)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "DIMENSION :: a(n, m), b(:), c(2 : n), d(*), e(n, 2 : *)"


def test_intent_stmt():  # R536
    tcls = Intent_Stmt
    obj = tcls("intent(in) :: a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "INTENT(IN) :: a"
    assert repr(obj) == (
        "Intent_Stmt(Intent_Spec('IN'), " "Dummy_Arg_Name_List(',', (Name('a'),)))"
    )

    obj = tcls("intent(out) a, b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "INTENT(OUT) :: a, b"
    assert (
        repr(obj) == "Intent_Stmt(Intent_Spec('OUT'), Dummy_Arg_Name_List(',', "
        "(Name('a'), Name('b'))))"
    )


def test_optional_stmt():  # R537
    tcls = Optional_Stmt
    obj = tcls("optional :: a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "OPTIONAL :: a"
    assert repr(obj) == (
        "Optional_Stmt('OPTIONAL', " "Dummy_Arg_Name_List(',', (Name('a'),)))"
    )

    obj = tcls("optional :: a, b, c")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "OPTIONAL :: a, b, c"
    assert (
        repr(obj) == "Optional_Stmt('OPTIONAL', Dummy_Arg_Name_List(',', (Name('a'), "
        "Name('b'), Name('c'))))"
    )


def test_parameter_stmt():  # R538
    tcls = Parameter_Stmt
    obj = tcls("parameter(a=1)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "PARAMETER(a = 1)"
    assert (
        repr(obj) == "Parameter_Stmt('PARAMETER', Named_Constant_Def_List(',', "
        "(Named_Constant_Def(Name('a'), Int_Literal_Constant('1', "
        "None)),)))"
    )

    obj = tcls("parameter(a=1, b=a+2)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "PARAMETER(a = 1, b = a + 2)"

    obj = tcls("PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "PARAMETER(ONE = 1.0D+0, ZERO = 0.0D+0)"


def test_named_constant_def():  # R539
    tcls = Named_Constant_Def
    obj = tcls("a=1")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a = 1"
    assert repr(obj) == "Named_Constant_Def(Name('a'), Int_Literal_Constant('1', None))"


def test_pointer_stmt():  # R540
    tcls = Pointer_Stmt
    obj = tcls("pointer a(:), b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "POINTER :: a(:), b"
    assert (
        repr(obj) == "Pointer_Stmt('POINTER', Pointer_Decl_List(',', "
        "(Pointer_Decl(Name('a'), Deferred_Shape_Spec_List(',', "
        "(Deferred_Shape_Spec(None, None),))), Name('b'))))"
    )


def test_pointer_decl():  # R541
    tcls = Pointer_Decl
    obj = tcls("a(:)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a(:)"
    assert (
        repr(obj) == "Pointer_Decl(Name('a'), Deferred_Shape_Spec_List(',', "
        "(Deferred_Shape_Spec(None, None),)))"
    )

    obj = tcls("a(:,:)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a(:, :)"


def test_protected_stmt():  # R542
    tcls = Protected_Stmt
    obj = tcls("protected a,b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "PROTECTED :: a, b"
    assert (
        repr(obj) == "Protected_Stmt('PROTECTED', Entity_Name_List(',', (Name('a'), "
        "Name('b'))))"
    )

    obj = tcls("protected ::a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "PROTECTED :: a"
    assert repr(obj) == (
        "Protected_Stmt('PROTECTED', " "Entity_Name_List(',', (Name('a'),)))"
    )


def test_save_stmt():  # R543
    tcls = Save_Stmt
    obj = tcls("save")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "SAVE"
    assert repr(obj) == "Save_Stmt('SAVE', None)"

    obj = tcls("save a, b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "SAVE :: a, b"
    assert (
        repr(obj) == "Save_Stmt('SAVE', "
        "Saved_Entity_List(',', (Name('a'), Name('b'))))"
    )

    obj = tcls("save :: /a/ , b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "SAVE :: /a/, b"
    assert (
        repr(obj) == "Save_Stmt('SAVE', Saved_Entity_List(',', (Saved_Entity('/', "
        "Name('a'), '/'), Name('b'))))"
    )


def test_saved_entity():  # R544
    tcls = Saved_Entity
    obj = tcls("a")
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == "a"
    assert repr(obj) == "Name('a')"

    obj = tcls("/a/")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "/a/"
    assert repr(obj) == "Saved_Entity('/', Name('a'), '/')"


# R545 is trivial


def test_target_stmt():  # R546
    tcls = Target_Stmt
    obj = tcls("target a, b(1000, 1000)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "TARGET :: a, b(1000, 1000)"

    obj = tcls("target :: a, c")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "TARGET :: a, c"


def test_value_stmt():  # R547
    tcls = Value_Stmt
    obj = tcls("value a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "VALUE :: a"

    obj = tcls("value:: a, c")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "VALUE :: a, c"


def test_volatile_stmt():  # R548
    tcls = Volatile_Stmt
    obj = tcls("volatile a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "VOLATILE :: a"

    obj = tcls("volatile :: a, c")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "VOLATILE :: a, c"


def test_implicit_stmt():  # R549
    tcls = Implicit_Stmt
    obj = tcls("implicitnone")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "IMPLICIT NONE"
    assert repr(obj) == "Implicit_Stmt('NONE')"

    obj = tcls("implicit real(a-d), double precision(r-t,x), type(a) (y-z)")
    assert isinstance(obj, tcls), repr(obj)
    assert (
        str(obj) == "IMPLICIT REAL(A - D), DOUBLE PRECISION(R - T, X), "
        "TYPE(a)(Y - Z)"
    )


def test_implicit_spec():  # R550
    tcls = Implicit_Spec
    obj = tcls("integer (a-z)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "INTEGER(A - Z)"
    assert (
        repr(obj) == "Implicit_Spec(Intrinsic_Type_Spec('INTEGER', None), "
        "Letter_Spec_List(',', (Letter_Spec('A', 'Z'),)))"
    )

    obj = tcls("double  complex (r,d-g)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "DOUBLE COMPLEX(R, D - G)"


def test_letter_spec():  # R551
    tcls = Letter_Spec
    obj = tcls("a-z")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "A - Z"
    assert repr(obj) == "Letter_Spec('A', 'Z')"

    obj = tcls("d")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "D"


def test_equivalence_stmt():  # R554
    tcls = Equivalence_Stmt
    obj = tcls("equivalence (a, b ,z)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "EQUIVALENCE(a, b, z)"
    assert (
        repr(obj) == "Equivalence_Stmt('EQUIVALENCE', Equivalence_Set_List(',', "
        "(Equivalence_Set(Name('a'), Equivalence_Object_List(',', "
        "(Name('b'), Name('z')))),)))"
    )

    obj = tcls("equivalence (a, b ,z),(b,l)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "EQUIVALENCE(a, b, z), (b, l)"


def test_common_stmt():  # R557
    tcls = Common_Stmt
    obj = tcls("common a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "COMMON // a"
    assert repr(obj) == (
        "Common_Stmt([(None, " "Common_Block_Object_List(',', (Name('a'),)))])"
    )

    obj = tcls("common // a,b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "COMMON // a, b"

    obj = tcls("common /name/ a,b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "COMMON /name/ a, b"

    obj = tcls("common /name/ a,b(4,5) // c, /ljuks/ g(2)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "COMMON /name/ a, b(4, 5) // c /ljuks/ g(2)"


def test_common_block_object():  # R558
    tcls = Common_Block_Object
    obj = tcls("a(2)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a(2)"
    assert (
        repr(obj) == "Common_Block_Object(Name('a'), Explicit_Shape_Spec_List(',', "
        "(Explicit_Shape_Spec(None, Int_Literal_Constant('2', None)),)))"
    )

    obj = tcls("a")
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == "a"


#
# SECTION 6
#


def test_substring():  # R609
    tcls = Substring
    obj = tcls("a(:)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a(:)"
    assert repr(obj) == ("Substring(Name('a'), Substring_Range(None," " None))")

    obj = tcls("a(1:2)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a(1 : 2)"
    assert (
        repr(obj) == "Substring(Name('a'), Substring_Range(Int_Literal_Constant('1',"
        " None), Int_Literal_Constant('2', None)))"
    )


def test_substring_range():  # R611
    tcls = Substring_Range
    obj = tcls(":")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == ":"
    assert repr(obj) == "Substring_Range(None, None)"

    obj = tcls("a+1:")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a + 1 :"

    obj = tcls("a+1: c/foo(g)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a + 1 : c / foo(g)"

    obj = tcls("a:b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a : b"
    assert repr(obj) == "Substring_Range(Name('a'), Name('b'))"

    obj = tcls("a:")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a :"

    obj = tcls(":b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == ": b"


def test_part_ref():  # R613
    tcls = Part_Ref
    obj = tcls("a")
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == "a"


def test_type_param_inquiry():  # R615
    tcls = Type_Param_Inquiry
    obj = tcls("a % b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a % b"
    assert repr(obj) == "Type_Param_Inquiry(Name('a'), '%', Name('b'))"


def test_array_section():  # R617
    tcls = Array_Section
    obj = tcls("a(:)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a(:)"
    assert repr(obj) == "Array_Section(Name('a'), " "Substring_Range(None, None))"

    obj = tcls("a(2:)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a(2 :)"


def test_section_subscript():  # R619
    tcls = Section_Subscript

    obj = tcls("1:2")
    assert isinstance(obj, Subscript_Triplet), repr(obj)
    assert str(obj) == "1 : 2"

    obj = tcls("zzz")
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == "zzz"


def test_section_subscript_list():  # R619-list
    tcls = Section_Subscript_List
    obj = tcls("a,2")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a, 2"
    assert (
        repr(obj) == "Section_Subscript_List(',', (Name('a'), Int_Literal_Constant("
        "'2', None)))"
    )

    obj = tcls("::1")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == ": : 1"

    obj = tcls("::1, 3")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == ": : 1, 3"


def test_subscript_triplet():  # R620
    tcls = Subscript_Triplet
    obj = tcls("a:b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a : b"
    assert repr(obj) == "Subscript_Triplet(Name('a'), Name('b'), None)"

    obj = tcls("a:b:1")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a : b : 1"

    obj = tcls(":")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == ":"

    obj = tcls("::5")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == ": : 5"

    obj = tcls(":5")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == ": 5"

    obj = tcls("a+1 :")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a + 1 :"


def test_allocate_stmt():  # R623
    tcls = Allocate_Stmt
    obj = tcls("allocate(a,b)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "ALLOCATE(a, b)"

    obj = tcls("allocate(real::a)")
    assert str(obj) == "ALLOCATE(REAL::a)"

    obj = tcls("allocate(real(kind=8)::a, stat=b, source=c//d)")
    assert str(obj) == "ALLOCATE(REAL(KIND = 8)::a, STAT = b, SOURCE = c // d)"


def test_alloc_opt():  # R624
    tcls = Alloc_Opt
    obj = tcls("stat=a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "STAT = a"
    assert repr(obj) == "Alloc_Opt('STAT', Name('a'))"


def test_nullify_stmt():  # R633
    tcls = Nullify_Stmt
    obj = tcls("nullify (a)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "NULLIFY(a)"
    assert repr(obj) == (
        "Nullify_Stmt('NULLIFY', " "Pointer_Object_List(',', (Name('a'),)))"
    )

    obj = tcls("nullify (a,c)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "NULLIFY(a, c)"


def test_deallocate_stmt():  # R635
    tcls = Deallocate_Stmt
    obj = tcls("deallocate (a)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "DEALLOCATE(a)"

    obj = tcls("deallocate (a,stat=b)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "DEALLOCATE(a, STAT = b)"
    obj = tcls("deallocate (a,c,stat=b,errmsg=d)")
    assert str(obj) == "DEALLOCATE(a, c, STAT = b, ERRMSG = d)"


#
# SECTION 7
#


def test_level_1_expr():  # R702
    tcls = Level_1_Expr
    obj = tcls(".hey. a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == ".HEY. a"
    assert repr(obj) == "Level_1_Expr('.HEY.', Name('a'))"

    obj = tcls(".false.")
    assert isinstance(obj, Logical_Literal_Constant), repr(obj)


def test_mult_operand():  # R704
    tcls = Mult_Operand
    obj = tcls("a**b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a ** b"
    assert repr(obj) == "Mult_Operand(Name('a'), '**', Name('b'))"

    obj = tcls("a**2")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a ** 2"

    obj = tcls("(a+b)**2")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "(a + b) ** 2"

    obj = tcls("0.0E-1")
    assert isinstance(obj, Real_Literal_Constant), repr(obj)
    assert str(obj) == "0.0E-1"


def test_level_2_expr():  # R706
    tcls = Level_2_Expr
    obj = tcls("a+b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a + b"
    assert repr(obj) == "Level_2_Expr(Name('a'), '+', Name('b'))"

    obj = tcls("a-b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a - b"

    obj = tcls("a+b+c")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a + b + c"

    obj = tcls("+a")
    assert isinstance(obj, Level_2_Unary_Expr), repr(obj)
    assert str(obj) == "+ a"

    obj = tcls("+1")
    assert isinstance(obj, Level_2_Unary_Expr), repr(obj)
    assert str(obj) == "+ 1"

    obj = tcls("+a+b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "+ a + b"

    obj = tcls("0.0E-1")
    assert isinstance(obj, Real_Literal_Constant), repr(obj)
    assert str(obj) == "0.0E-1"


def test_level_2_unary_expr():  # R706.c
    tcls = Level_2_Unary_Expr
    obj = tcls("+a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "+ a"
    assert repr(obj) == "Level_2_Unary_Expr('+', Name('a'))"

    obj = tcls("-a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "- a"

    obj = tcls("+1")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "+ 1"

    obj = tcls("0.0E-1")
    assert isinstance(obj, Real_Literal_Constant), repr(obj)
    assert str(obj) == "0.0E-1"


def test_level_3_expr():  # R710
    tcls = Level_3_Expr
    obj = tcls("a//b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a // b"
    assert repr(obj) == "Level_3_Expr(Name('a'), '//', Name('b'))"

    obj = tcls('"a"//"b"')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == '"a" // "b"'


def test_level_4_expr():  # R712
    tcls = Level_4_Expr
    obj = tcls("a.eq.b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a .EQ. b"
    assert repr(obj) == "Level_4_Expr(Name('a'), '.EQ.', Name('b'))"

    obj = tcls("a.ne.b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a .NE. b"

    obj = tcls("a.lt.b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a .LT. b"

    obj = tcls("a.gt.b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a .GT. b"

    obj = tcls("a.ge.b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a .GE. b"

    obj = tcls("a==b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a == b"

    obj = tcls("a/=b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a /= b"

    obj = tcls("a<b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a < b"

    obj = tcls("a<=b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a <= b"

    obj = tcls("a>=b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a >= b"

    obj = tcls("a>b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a > b"


def test_and_operand():  # R714
    tcls = And_Operand
    obj = tcls(".not.a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == ".NOT. a"
    assert repr(obj) == "And_Operand('.NOT.', Name('a'))"


def test_or_operand():  # R715
    tcls = Or_Operand
    obj = tcls("a.and.b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a .AND. b"
    assert repr(obj) == "Or_Operand(Name('a'), '.AND.', Name('b'))"


def test_equiv_operand():  # R716
    tcls = Equiv_Operand
    obj = tcls("a.or.b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a .OR. b"
    assert repr(obj) == "Equiv_Operand(Name('a'), '.OR.', Name('b'))"


def test_level_5_expr():  # R717
    tcls = Level_5_Expr
    obj = tcls("a.eqv.b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a .EQV. b"
    assert repr(obj) == "Level_5_Expr(Name('a'), '.EQV.', Name('b'))"

    obj = tcls("a.neqv.b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a .NEQV. b"

    obj = tcls("a.eq.b")
    assert isinstance(obj, Level_4_Expr), repr(obj)
    assert str(obj) == "a .EQ. b"


def test_expr():  # R722
    tcls = Expr
    obj = tcls("a .op. b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a .OP. b"
    assert repr(obj) == "Expr(Name('a'), '.OP.', Name('b'))"

    obj = tcls("a")
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == "a"

    obj = tcls("3.e2")
    assert isinstance(obj, Real_Literal_Constant), repr(obj)

    obj = tcls("0.0E-1")
    assert isinstance(obj, Real_Literal_Constant), repr(obj)
    assert str(obj) == "0.0E-1"

    obj = tcls("123")
    assert isinstance(obj, Int_Literal_Constant), repr(obj)
    assert str(obj) == "123"

    obj = tcls(".false.")
    assert isinstance(obj, Logical_Literal_Constant), repr(obj)
    assert str(obj) == ".FALSE."

    assert_raises(NoMatchError, Scalar_Int_Expr, "a,b")


def test_logical_initialization_expr():  # R733
    # pylint: disable=invalid-name

    tcls = Logical_Initialization_Expr
    obj = tcls(".false.")
    assert isinstance(obj, Logical_Literal_Constant), repr(obj)
    assert str(obj) == ".FALSE."


def test_assignment_stmt():
    """Tests for the Assignment_Stmt class (R734)."""
    tcls = Assignment_Stmt
    obj = tcls("a = b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a = b"
    assert repr(obj) == "Assignment_Stmt(Name('a'), '=', Name('b'))"

    obj = tcls("a(3:4) = b+c")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a(3 : 4) = b + c"

    obj = tcls("a%c = b+c")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a % c = b + c"

    obj = tcls("a = .FALSE.")
    assert isinstance(obj, tcls), repr(obj)
    assert (
        repr(obj) == "Assignment_Stmt(Name('a'), '=', Logical_Literal_Constant("
        "'.FALSE.', None))"
    )

    obj = tcls("a(n)(k:m) = 5")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a(n)(k : m) = 5"

    obj = tcls("b = a + 1d-8")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "b = a + 1D-8"

    obj = tcls("b = a + 1d-8 + 1.1e+3")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "b = a + 1D-8 + 1.1E+3"

    # Extra white space around a part-ref
    obj = tcls("zdepth(:) = ((gdept_1d(:) ))")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "zdepth(:) = ((gdept_1d(:)))"
    obj = tcls("zdepth(:) = (( gdept_1d(:) ))")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "zdepth(:) = ((gdept_1d(:)))"
    obj = tcls("zdepth(:) = ( ( gdept_1d(:) ) )")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "zdepth(:) = ((gdept_1d(:)))"
    obj = tcls("zdepth(:) = ( gdept_1d(:) ) ")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "zdepth(:) = (gdept_1d(:))"


@pytest.mark.usefixtures("fake_symbol_table")
def test_pointer_assignment_stmt():  # R735
    tcls = Pointer_Assignment_Stmt
    obj = tcls("new_node % left => current_node")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "new_node % left => current_node"

    obj = tcls("simple_name => target_structure % substruct % component")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "simple_name => target_structure % substruct % component"

    for stmt in """\
PTR => NULL()
ROW => MAT2D(N, :)
WINDOW => MAT2D(I - 1 : I + 1, J - 1 : J + 1)
POINTER_OBJECT => POINTER_FUNCTION(ARG_1, ARG_2)
EVERY_OTHER => VECTOR(1 : N : 2)
WINDOW2(0 :, 0 :) => MAT2D(ML : MU, NL : NU)
P => BESSEL
STRUCT % COMPONENT => BESSEL""".split(
        "\n"
    ):
        obj = tcls(stmt)
        assert isinstance(obj, tcls), repr(obj)
        assert str(obj) == stmt


def test_proc_component_ref():  # R741
    tcls = Proc_Component_Ref
    obj = tcls("a % b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a % b"
    assert repr(obj) == "Proc_Component_Ref(Name('a'), '%', Name('b'))"


def test_where_stmt():  # R743
    tcls = Where_Stmt
    obj = tcls("where (a) c=2")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "WHERE (a) c = 2"
    assert (
        repr(obj) == "Where_Stmt(Name('a'), Assignment_Stmt(Name('c'), '=', "
        "Int_Literal_Constant('2', None)))"
    )


def test_where_construct_stmt():  # R745
    tcls = Where_Construct_Stmt
    obj = tcls("where (a)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "WHERE (a)"
    assert repr(obj) == "Where_Construct_Stmt(Name('a'))"


@pytest.mark.usefixtures("fake_symbol_table")
def test_forall_construct():  # R752
    tcls = Forall_Construct
    obj = tcls(
        get_reader(
            """\
    forall (i = 1:10, j = 1:10, b(i, j) /= 0.0)
      a(i, j) = real (i + j - 2)
      b(i, j) = a(i, j) + b(i, j) * real (i * j)
    end forall
    """
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert (
        str(obj) == "FORALL(i = 1 : 10, j = 1 : 10, b(i, j) /= 0.0)\n"
        "  a(i, j) = REAL(i + j - 2)\n  b(i, j) = a(i, j) + "
        "b(i, j) * REAL(i * j)\nEND FORALL"
    )

    obj = tcls(
        get_reader(
            """\
    n: forall (x = 1:5:2, j = 1:4)
      a(x, j) = j
    end forall n
    """
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "n:FORALL(x = 1 : 5 : 2, j = 1 : 4)\n  a(x, j) = j\nEND FORALL n"


def test_forall_triplet_spec():  # R755
    tcls = Forall_Triplet_Spec
    obj = tcls("n = 1: 2")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "n = 1 : 2"

    obj = tcls("n = f(x): 2-b:a+1")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "n = f(x) : 2 - b : a + 1"


#
# SECTION 8
#


def test_if_nonblock_do():
    """Tests that conditional nonblock DO construct is parsed correctly."""
    tcls = If_Construct

    obj = tcls(
        get_reader(
            """\
if (expr) then
   do  20  i = 1, 3
     a = 1
     do  20  j = 1, 3
       a = 2
       do  20  k = 1, 3
         a = 3
20 rotm(i,j) = r2(j,i)
endif
"""
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert len(obj.content) == 3, repr(obj)
    obj = obj.content[1]
    assert isinstance(obj, Action_Term_Do_Construct), repr(obj)
    assert str(obj) == (
        "DO 20 i = 1, 3\n  a = 1\n  DO 20 j = 1, 3\n    a = 2\n    "
        "DO 20 k = 1, 3\n      a = 3\n20 rotm(i, j) = r2(j, i)"
    )

    obj = tcls(
        get_reader(
            """\
if (expr) then
    do  50  i = n, m, -1
  50 call foo(a)
endif"""
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert len(obj.content) == 3, repr(obj)
    obj = obj.content[1]
    assert isinstance(obj, Action_Term_Do_Construct), repr(obj)


def test_case_selector():  # R813
    tcls = Case_Selector
    obj = tcls("default")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "DEFAULT"

    obj = tcls("(2)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "(2)"

    obj = tcls("(2:3, c+2:, :-a)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "(2 : 3, c + 2 :, : - a)"


def test_select_type_stmt():  # R822
    tcls = Select_Type_Stmt
    obj = tcls("select type(a=>b)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "SELECT TYPE(a=>b)"

    obj = tcls("select type(a)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "SELECT TYPE(a)"


def test_label_do_stmt():
    """Tests that labeled DO statement is parsed correctly (R828)."""
    tcls = Label_Do_Stmt
    obj = tcls("do 12")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "DO 12"
    assert repr(obj) == "Label_Do_Stmt(None, Label('12'), None)"


def test_continue_stmt():  # R848
    tcls = Continue_Stmt
    obj = tcls("continue")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "CONTINUE"
    assert repr(obj) == "Continue_Stmt('CONTINUE')"


@pytest.mark.parametrize("standard_only", [True, False])
def test_stop_stmt_standard_2003(standard_only, monkeypatch):
    """Test that stop statements are parsed correctly [R849].
    It tests both pure 2003 standard compliance, but also
    that negative numbers and string concatenations are accepted.
    """
    if standard_only:
        # Disable the stop-stmt extension for this test to verify
        # that really only standard expressions are accepted
        monkeypatch.setattr(utils, "_EXTENSIONS", [])

    tcls = Stop_Stmt
    obj = tcls("stop")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "STOP"

    obj = tcls("stop 123")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "STOP 123"

    obj = tcls("stop   'hey you'")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "STOP 'hey you'"

    # This should not be accepted even with the extension enabled:
    with pytest.raises(NoMatchError) as excinfo:
        tcls("stop 12 .and. 34")
    assert "Stop_Stmt: 'stop 12 .and. 34'" in str(excinfo.value)

    if standard_only:
        # This should not be accepted according to F2003
        with pytest.raises(NoMatchError) as excinfo:
            tcls('stop "123"//"456"')
        assert 'Stop_Stmt: \'stop "123"//"456"' in str(excinfo.value)

        # This should not be accepted according to F2003
        with pytest.raises(NoMatchError) as excinfo:
            tcls("stop -321")
        assert "Stop_Stmt: 'stop -321'" in str(excinfo.value)

    else:
        # Test the F2003 standard extensions, which should
        # accept these expressions
        obj = tcls('stop "123"//"456"')
        assert isinstance(obj, tcls), repr(obj)
        assert str(obj) == 'STOP "123" // "456"'

        obj = tcls("stop -321")
        assert isinstance(obj, tcls), repr(obj)
        assert str(obj) == "STOP - 321"


#
# SECTION 9
#


def test_io_unit():  # R901
    tcls = Io_Unit
    obj = tcls("*")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "*"

    obj = tcls("a")
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == "a"


def test_read_stmt():
    """Tests that we successfully parse various forms of
    READ statement (R910)."""
    tcls = Read_Stmt
    obj = tcls("read(123)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "READ(123)"

    obj = tcls("read(123) a")
    assert str(obj) == "READ(123) a"
    obj = tcls("read(123) a(  2)")
    assert str(obj) == "READ(123) a(2)"

    obj = tcls("read*, a(  2), b")
    assert str(obj) == "READ *, a(2), b"
    assert repr(obj) == (
        "Read_Stmt(None, Format('*'), Output_Item_List(',', "
        "(Part_Ref(Name('a'), Section_Subscript_List(',', "
        "(Int_Literal_Constant('2', None),))), Name('b'))))"
    )
    # With format specified by label number
    obj = tcls("READ 13, a(2)")
    assert str(obj) == "READ 13, a(2)"
    assert (
        repr(obj) == "Read_Stmt(None, Label('13'), Output_Item_List(',', "
        "(Part_Ref(Name('a'), Section_Subscript_List(',', "
        "(Int_Literal_Constant('2', None),))),)))"
    )

    # If there is no preceding "FMT=" or "NML=" then there is no way of
    # knowing whether the second argument is a format string or a namelist
    # without determining the actual type of the argument.
    obj = tcls("read(123, a_namelist_or_format)")
    assert str(obj) == "READ(123, a_namelist_or_format)"
    assert repr(obj) == (
        "Read_Stmt(Io_Control_Spec_List(',', "
        "(Io_Control_Spec(None, Int_Literal_Constant('123', "
        "None)), Io_Control_Spec(None, "
        "Name('a_namelist_or_format')))), None, None)"
    )


def test_print_stmt():  # R912
    tcls = Print_Stmt
    obj = tcls("print 123")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "PRINT 123"
    assert repr(obj) == "Print_Stmt(Label('123'), None)"

    obj = tcls('print *,"a=",a')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == 'PRINT *, "a=", a'


def test_format():  # R914
    tcls = Format
    obj = tcls("*")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "*"
    assert repr(obj) == "Format('*')"

    obj = tcls("a")
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == "a"

    obj = tcls("123")
    assert isinstance(obj, Label), repr(obj)
    assert str(obj) == "123"


def test_io_implied_do():  # R917
    tcls = Io_Implied_Do
    obj = tcls("(a, i=1,2)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "(a, i = 1, 2)"

    obj = tcls("((i+j,j=3,4,1), i=1,2)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "((i + j, j = 3, 4, 1), i = 1, 2)"


def test_io_implied_do_control():  # R919
    tcls = Io_Implied_Do_Control
    obj = tcls("i=1,2")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "i = 1, 2"

    obj = tcls("i=f(2),2-1,a+2")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "i = f(2), 2 - 1, a + 2"


def test_wait_stmt():  # R921
    tcls = Wait_Stmt
    obj = tcls("wait (123)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "WAIT(UNIT = 123)"


def test_wait_spec():  # R922
    tcls = Wait_Spec
    obj = tcls("123")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "UNIT = 123"
    assert repr(obj) == "Wait_Spec('UNIT', Int_Literal_Constant('123', None))"

    obj = tcls("err=1")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "ERR = 1"


def test_backspace_stmt():  # R923
    tcls = Backspace_Stmt
    obj = tcls("backspace 1")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "BACKSPACE 1"

    obj = tcls("backspace  (unit=1,err=2)")
    assert str(obj) == "BACKSPACE(UNIT = 1, ERR = 2)"


def test_endfile_stmt():  # R924
    tcls = Endfile_Stmt
    obj = tcls("endfile 1")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "ENDFILE 1"

    obj = tcls("endfile  (unit=1,err=2)")
    assert str(obj) == "ENDFILE(UNIT = 1, ERR = 2)"


def test_rewind_stmt():  # R925
    tcls = Rewind_Stmt
    obj = tcls("rewind 1")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "REWIND 1"

    obj = tcls("rewind  (unit=1,err=2)")
    assert str(obj) == "REWIND(UNIT = 1, ERR = 2)"


def test_position_spec():  # R926
    tcls = Position_Spec
    obj = tcls("1")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "UNIT = 1"
    obj = tcls("unit=1")
    assert str(obj) == "UNIT = 1"
    obj = tcls("err=2")
    assert str(obj) == "ERR = 2"
    obj = tcls("iomsg=a")
    assert str(obj) == "IOMSG = a"
    obj = tcls("iostat=a")
    assert str(obj) == "IOSTAT = a"


def test_flush_stmt():  # R927
    tcls = Flush_Stmt
    obj = tcls("flush 1")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "FLUSH 1"

    obj = tcls("flush  (unit=1,err=2)")
    assert str(obj) == "FLUSH(UNIT = 1, ERR = 2)"


def test_flush_spec():  # R928
    tcls = Flush_Spec
    obj = tcls("1")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "UNIT = 1"
    obj = tcls("unit=1")
    assert str(obj) == "UNIT = 1"
    obj = tcls("err=2")
    assert str(obj) == "ERR = 2"
    obj = tcls("iomsg=a")
    assert str(obj) == "IOMSG = a"
    obj = tcls("iostat=a")
    assert str(obj) == "IOSTAT = a"


def test_inquire_stmt():
    """Tests for the INQUIRE statement (R929)."""
    tcls = Inquire_Stmt
    obj = tcls("inquire(1,file=a)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "INQUIRE(UNIT = 1, FILE = a)"
    obj = tcls("inquire(iolength=n) a, b")
    assert str(obj) == "INQUIRE(IOLENGTH=n) a, b"
    obj = tcls("inquire(unit=get_unit, opened=llopn)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "INQUIRE(UNIT = get_unit, OPENED = llopn)"


def test_inquire_spec():
    """Tests that we recognise the various possible forms of
    entries in an inquire list (R930)."""
    tcls = Inquire_Spec
    obj = tcls("1")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "UNIT = 1"
    obj = tcls("file=fn")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "FILE = fn"

    obj = tcls("access=a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "ACCESS = a"

    obj = tcls("opened=a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "OPENED = a"

    obj = tcls("sequential=a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "SEQUENTIAL = a"

    obj = tcls("direct=a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "DIRECT = a"


def test_inquire_spec_list():
    """Tests that we recognise the various possible forms of
    inquire list (R930)."""
    # Inquire_Spec_List is generated at runtime in Fortran2003.py
    tcls = Inquire_Spec_List

    obj = tcls('unit=23, file="a_file.dat"')
    assert isinstance(obj, tcls)
    assert str(obj) == 'UNIT = 23, FILE = "a_file.dat"'

    # Invalid list (afile= instead of file=)
    with pytest.raises(NoMatchError) as excinfo:
        _ = tcls('unit=23, afile="a_file.dat"')
    assert "Inquire_Spec_List: 'unit=23, afile=" in str(excinfo.value)


def test_connect_spec():
    """Tests for individual elements of Connect_Spec (R905)."""
    tcls = Connect_Spec
    # Incorrect name for a member of the list
    with pytest.raises(NoMatchError) as excinfo:
        _ = tcls("afile='a_file.dat'")
    assert "Connect_Spec: 'afile=" in str(excinfo.value)


def test_connect_spec_list():
    """
    Tests that we correctly parse the various valid forms of
    connect specification (R905).
    """
    tcls = Connect_Spec_List
    obj = tcls("22, access='direct'")
    assert isinstance(obj, tcls)
    assert str(obj) == "UNIT = 22, ACCESS = 'direct'"

    obj = tcls("22, action='read'")
    assert isinstance(obj, tcls)
    assert str(obj) == "UNIT = 22, ACTION = 'read'"

    obj = tcls("22, asynchronous='YES'")
    assert isinstance(obj, tcls)
    assert str(obj) == "UNIT = 22, ASYNCHRONOUS = 'YES'"

    obj = tcls("22, blank='NULL'")
    assert isinstance(obj, tcls)
    assert str(obj) == "UNIT = 22, BLANK = 'NULL'"

    obj = tcls("22, decimal='COMMA'")
    assert isinstance(obj, tcls)
    assert str(obj) == "UNIT = 22, DECIMAL = 'COMMA'"

    obj = tcls("22, delim='APOSTROPHE'")
    assert isinstance(obj, tcls)
    assert str(obj) == "UNIT = 22, DELIM = 'APOSTROPHE'"

    obj = tcls("22, err=109")
    assert isinstance(obj, tcls)
    assert str(obj) == "UNIT = 22, ERR = 109"

    obj = tcls("22, encoding='DEFAULT'")
    assert isinstance(obj, tcls)
    assert str(obj) == "UNIT = 22, ENCODING = 'DEFAULT'"

    obj = tcls("22, file='a_file.dat'")
    assert isinstance(obj, tcls)
    assert str(obj) == "UNIT = 22, FILE = 'a_file.dat'"

    obj = tcls("22, file='a_file.dat', form='FORMATTED'")
    assert isinstance(obj, tcls)
    assert str(obj) == "UNIT = 22, FILE = 'a_file.dat', FORM = 'FORMATTED'"

    obj = tcls("22, file='a_file.dat', iomsg=my_string")
    assert isinstance(obj, tcls)
    assert str(obj) == "UNIT = 22, FILE = 'a_file.dat', IOMSG = my_string"

    obj = tcls("22, file='a_file.dat', iostat=ierr")
    assert isinstance(obj, tcls)
    assert str(obj) == "UNIT = 22, FILE = 'a_file.dat', IOSTAT = ierr"

    obj = tcls("22, file='a_file.dat', pad='YES'")
    assert isinstance(obj, tcls)
    assert str(obj) == "UNIT = 22, FILE = 'a_file.dat', PAD = 'YES'"

    obj = tcls("22, file='a_file.dat', position='APPEND'")
    assert isinstance(obj, tcls)
    assert str(obj) == "UNIT = 22, FILE = 'a_file.dat', POSITION = 'APPEND'"

    obj = tcls("22, file='a_file.dat', recl=100")
    assert isinstance(obj, tcls)
    assert str(obj) == "UNIT = 22, FILE = 'a_file.dat', RECL = 100"

    obj = tcls("22, file='a_file.dat', round='UP'")
    assert isinstance(obj, tcls)
    assert str(obj) == "UNIT = 22, FILE = 'a_file.dat', ROUND = 'UP'"

    obj = tcls("22, file='a_file.dat', sign='PLUS'")
    assert isinstance(obj, tcls)
    assert str(obj) == "UNIT = 22, FILE = 'a_file.dat', SIGN = 'PLUS'"

    obj = tcls("22, file='a_file.dat', sign='PLUS', status='OLD'")
    assert isinstance(obj, tcls)
    assert str(obj) == (
        "UNIT = 22, FILE = 'a_file.dat', SIGN = 'PLUS', " "STATUS = 'OLD'"
    )

    # Incorrect name for a member of the list
    with pytest.raises(NoMatchError) as excinfo:
        _ = tcls("unit=22, afile='a_file.dat', sign='PLUS', status='OLD'")
    assert "Connect_Spec_List: 'unit=22, afile=" in str(excinfo.value)


#
# SECTION 10
#


def test_format_stmt():  # R1001
    tcls = Format_Stmt
    obj = tcls("format (3f9.4)")
    assert isinstance(obj, tcls), repr(type(obj))
    assert str(obj) == "FORMAT(3F9.4)"
    obj = tcls("format (' ',3f9.4)")
    assert isinstance(obj, tcls), repr(type(obj))
    assert str(obj) == "FORMAT(' ', 3F9.4)"

    obj = tcls("format(i6,f12.6,2x,f12.6)")
    assert isinstance(obj, tcls), repr(type(obj))
    assert str(obj) == "FORMAT(I6, F12.6, 2X, F12.6)"

    obj = tcls("format(' Enter smth',$)")
    assert isinstance(obj, tcls), repr(type(obj))
    assert str(obj) == "FORMAT(' Enter smth', $)"

    obj = tcls("format(/'a' /'b')")
    assert isinstance(obj, tcls), repr(type(obj))
    assert str(obj) == "FORMAT(/, 'a', /, 'b')"

    obj = tcls("format('a:':' b')")
    assert isinstance(obj, tcls), repr(type(obj))
    assert str(obj) == "FORMAT('a:', :, ' b')"

    return  # TODO
    obj = tcls("format('text=','  '")
    assert str(obj) == ""


def test_format_specification():  # R1002
    tcls = Format_Specification
    obj = tcls("(3f9.4, 2f8.1)")
    assert isinstance(obj, tcls), repr(type(obj))
    assert str(obj) == "(3F9.4, 2F8.1)"

    obj = tcls("(' ', 2f8.1)")
    assert isinstance(obj, tcls), repr(type(obj))
    assert str(obj) == "(' ', 2F8.1)"


def test_format_item():  # R1003
    tcls = Format_Item
    obj = tcls("3f9.4")
    assert isinstance(obj, tcls), repr(type(obj))
    assert str(obj) == "3F9.4"

    obj = tcls("' '")
    assert isinstance(obj, Char_Literal_Constant), repr(type(obj))
    assert str(obj) == "' '"

    obj = tcls("i4/")
    assert isinstance(obj, Format_Item_C1002), repr(type(obj))
    assert str(obj) == "I4, /"

    obj = tcls("3f12.6/")
    assert str(obj) == "3F12.6, /"

    obj = tcls("3d12.6/")
    assert str(obj) == "3D12.6, /"

    # D specifier must be Dw.d so must have a decimal point
    with pytest.raises(NoMatchError):
        _ = tcls("3d12/")

    obj = tcls("3e12.6/")
    assert str(obj) == "3E12.6, /"

    obj = tcls("3e12.6e2/")
    assert str(obj) == "3E12.6E2, /"

    # Scientific format
    obj = tcls("3es12.6/")
    assert str(obj) == "3ES12.6, /"

    # Engineering format
    obj = tcls("3en12.6/")
    assert str(obj) == "3EN12.6, /"

    # Must have a decimal point
    with pytest.raises(NoMatchError):
        _ = tcls("3en12/")

    # Engineering format specifying number of digits in exponent
    obj = tcls("3en12.6e3/")
    assert str(obj) == "3EN12.6E3, /"

    obj = tcls("/' '")
    assert str(obj) == "/, ' '"

    obj = tcls("' '/")
    assert str(obj) == "' ', /"

    obj = tcls("' '/' '")
    assert str(obj) == "' ', /, ' '"

    obj = tcls("'(5X,\"q_mesh =\",4F12.8)'")
    assert isinstance(obj, Char_Literal_Constant)

    obj = tcls("3/' '")
    assert str(obj) == "3/, ' '"


def test_data_edit_desc():
    """Tests for matching Edit Descriptors (R1005)."""
    tcls = Data_Edit_Desc
    obj = tcls("I3")
    assert str(obj) == "I3"

    obj = tcls("I3.2")
    assert str(obj) == "I3.2"

    obj = tcls("O3.2")
    assert str(obj) == "O3.2"

    obj = tcls("Z3.2")
    assert str(obj) == "Z3.2"

    obj = tcls("L3")
    assert str(obj) == "L3"

    with pytest.raises(NoMatchError) as excinfo:
        _ = tcls("L3.2")
    assert "Data_Edit_Desc: 'L3.2'" in str(excinfo.value)

    obj = tcls("A3")
    assert str(obj) == "A3"

    with pytest.raises(NoMatchError) as excinfo:
        _ = tcls("A3.2")
    assert "Data_Edit_Desc: 'A3.2'" in str(excinfo.value)

    obj = tcls("DT'a_name'")
    assert str(obj) == "DT'a_name'"

    obj = tcls("DT'a_name'(3,-2)")
    assert str(obj) == "DT'a_name'(3, -2)"

    with pytest.raises(NoMatchError) as excinfo:
        _ = tcls("DT'a_name'()")
    assert """Data_Edit_Desc: \'DT\'a_name\'()\'""" in str(excinfo.value)


def test_format_item_list():  # R1002, R1003
    tcls = Format_Item_List
    obj = tcls("3f9.4")
    assert isinstance(obj, Format_Item_List), repr(type(obj))
    assert str(obj) == "3F9.4"

    obj = tcls("3f9.4, 2f8.1")
    assert isinstance(obj, Format_Item_List), repr(type(obj))
    assert str(obj) == "3F9.4, 2F8.1"

    obj = tcls("' ', 2f8.1")
    assert isinstance(obj, Format_Item_List), repr(type(obj))
    assert str(obj) == "' ', 2F8.1"

    obj = tcls("' ', ' '")
    assert str(obj) == "' ', ' '"

    obj = tcls("3(3f8.2, :), (A)")
    assert str(obj) == "3(3F8.2, :), (A)"


#
# SECTION 11
#


def test_main_program0():
    """Test for R1101 when the program statement is not supplied. This
    case matches with the Main_Program0 class.

    """
    obj0 = Fortran2003.Main_Program0(
        get_reader(
            """\
end
    """
        )
    )
    assert isinstance(obj0, Fortran2003.Main_Program0), repr(obj0)
    assert str(obj0) == "END"

    obj0 = Fortran2003.Main_Program0(
        get_reader(
            """\
contains
  function foo()
  end
end
    """
        )
    )
    assert isinstance(obj0, Fortran2003.Main_Program0), repr(obj0)
    assert str(obj0) == "CONTAINS\nFUNCTION foo()\nEND\nEND"

    # Check that the expected symbol table is created and populated
    obj0 = Fortran2003.Main_Program0(
        get_reader(
            """\
integer :: i
i = 9
end
    """
        )
    )
    assert isinstance(obj0, Fortran2003.Main_Program0), repr(obj0)
    assert str(obj0) == "INTEGER :: i\n  i = 9\nEND"
    table = SYMBOL_TABLES.lookup("fparser2:main_program")
    assert table.lookup("i")


def test_invalid_main_program0():
    """Test for when the Main_Program0 class fails to match. We should
    get a NoMatchError and no symbol table."""
    with pytest.raises(NoMatchError):
        _ = Fortran2003.Main_Program0(get_reader("integer :: i\n" "i = 9\n" "en\n"))
    # Ensure that no symbol table has been created
    assert SYMBOL_TABLES._symbol_tables == {}


def test_module():  # R1104
    tcls = Module
    obj = tcls(
        get_reader(
            """\
module m
end
    """
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "MODULE m\nEND"

    obj = tcls(
        get_reader(
            """\
module m
type a
end type
type b
end type b
end
    """
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert (
        str(obj) == "MODULE m\n  TYPE :: a\n  END TYPE\n  TYPE :: b\n  END TYPE b"
        "\nEND"
    )


def test_module_subprogram_part():  # R1107
    tcls = Module_Subprogram_Part
    obj = tcls(
        get_reader(
            """\
contains
  subroutine foo(a)
  real a
  a = 1.0
  end
    """,
            isfree=True,
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "CONTAINS\nSUBROUTINE foo(a)\n  REAL :: a" "\n  a = 1.0\nEND"


def test_module_nature():
    """Tests that a module nature statement is parsed correctly
    (INTRINSIC or NON_INTRINSIC allowed, R1110)."""
    tcls = Module_Nature
    obj = tcls("intrinsic")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "INTRINSIC"
    assert repr(obj) == "Module_Nature('INTRINSIC')"

    obj = tcls("non_intrinsic")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "NON_INTRINSIC"
    assert repr(obj) == "Module_Nature('NON_INTRINSIC')"

    # Incorrect module nature
    with pytest.raises(NoMatchError) as excinfo:
        _ = tcls("other_nature")
    assert "Module_Nature: 'other_nature'" in str(excinfo.value)


@pytest.mark.xfail(reason="Match fails with multiple spaces, see issue #197")
def test_block_data():  # R1116
    tcls = Block_Data
    obj = tcls(
        get_reader(
            """\
block data a
real b
end block data
    """
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "BLOCK DATA a\n  REAL :: b\nEND BLOCK DATA"

    tcls = Block_Data
    obj = tcls(
        get_reader(
            """\
block     data a
end block     data a
    """
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "BLOCK DATA a\nEND BLOCK DATA a"


#
# SECTION 12
#


def test_interface_block():  # R1201
    tcls = Interface_Block
    obj = tcls(
        get_reader(
            """\
interface
end interface"""
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "INTERFACE\nEND INTERFACE"

    obj = tcls(
        get_reader(
            """\
abstract interface
procedure a
module procedure b,c
end interface
"""
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert (
        str(obj) == "ABSTRACT INTERFACE\n  MODULE PROCEDURE a\n  MODULE PROCEDURE b, "
        "c\nEND INTERFACE"
    )


def test_interface_specification():  # R1202
    tcls = Interface_Specification
    obj = tcls(
        get_reader(
            """\
    function foo()
    end
    """
        )
    )
    assert isinstance(obj, Function_Body), repr(obj)
    assert str(obj) == "FUNCTION foo()\nEND"


def test_interface_stmt():  # R1203
    tcls = Interface_Stmt
    obj = tcls("interface")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "INTERFACE"

    obj = tcls("interface assignment(=)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "INTERFACE ASSIGNMENT(=)"

    obj = tcls("abstract interface")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "ABSTRACT INTERFACE"


def test_end_interface_stmt():  # R1204
    tcls = End_Interface_Stmt
    obj = tcls("end interface")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "END INTERFACE"

    obj = tcls("end interface read(formatted)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "END INTERFACE READ(FORMATTED)"

    obj = tcls("end interface assignment(=)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "END INTERFACE ASSIGNMENT(=)"


def test_interface_body():  # R1205
    tcls = Interface_Body
    obj = tcls(
        get_reader(
            """\
subroutine foo
end subroutine foo
"""
        )
    )
    assert isinstance(obj, Subroutine_Body), repr(obj)
    assert str(obj) == "SUBROUTINE foo\nEND SUBROUTINE foo"

    obj = tcls(
        get_reader(
            """\
function foo(a) result(c)
  real a, c
end
"""
        )
    )
    assert isinstance(obj, Function_Body), repr(obj)
    assert str(obj) == "FUNCTION foo(a) RESULT(c)\n  REAL :: a, c\nEND"


def test_subroutine_body():
    pass


def test_function_body():
    pass


def test_procedure_stmt():  # R1206
    tcls = Procedure_Stmt
    obj = tcls("module procedure a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "MODULE PROCEDURE a"

    obj = tcls("procedure a, b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "MODULE PROCEDURE a, b"

    # '::' is only valid from F2008 onwards
    with pytest.raises(NoMatchError):
        _ = tcls("procedure :: a")


def test_generic_spec():  # R1207
    tcls = Generic_Spec
    obj = tcls("a")
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == "a"
    obj = tcls("read(formatted)")
    assert isinstance(obj, Dtio_Generic_Spec), repr(obj)
    assert str(obj) == "READ(FORMATTED)"

    obj = tcls("assignment ( = )")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "ASSIGNMENT(=)"

    return  # TODO
    obj = tcls("operator(.foo.)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "OPERATOR(.foo.)"


def test_dtio_generic_spec():  # R1208
    tcls = Dtio_Generic_Spec
    obj = tcls("read   ( formatted )")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "READ(FORMATTED)"

    obj = tcls("write ( formatted )")
    assert str(obj) == "WRITE(FORMATTED)"
    obj = tcls("read   ( unformatted )")
    assert str(obj) == "READ(UNFORMATTED)"
    obj = tcls("write ( unformatted )")
    assert str(obj) == "WRITE(UNFORMATTED)"


def test_import_stmt():  # R1209
    tcls = Import_Stmt
    obj = tcls("import :: a, b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "IMPORT :: a, b"

    obj = tcls("import a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "IMPORT :: a"


def test_external_stmt():  # R1210
    tcls = External_Stmt
    obj = tcls("external :: a, b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "EXTERNAL :: a, b"

    obj = tcls("external a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "EXTERNAL :: a"


def test_procedure_declaration_stmt():  # R1211
    tcls = Procedure_Declaration_Stmt
    obj = tcls("procedure () a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "PROCEDURE() a"

    obj = tcls("procedure (n) a")
    assert str(obj) == "PROCEDURE(n) a"

    obj = tcls("procedure (real*8) a")
    assert str(obj) == "PROCEDURE(REAL*8) a"

    obj = tcls("procedure (real(kind=8)) a")
    assert str(obj) == "PROCEDURE(REAL(KIND = 8)) a"

    obj = tcls("procedure (real*8) :: a")
    assert str(obj) == "PROCEDURE(REAL*8) a"

    obj = tcls("procedure (real*8), intent(in), bind(c) :: a, b")
    assert str(obj) == "PROCEDURE(REAL*8), INTENT(IN), BIND(C) :: a, b"


@pytest.mark.parametrize(
    "procedure_attribute_input,expected_class,expected_string",
    [
        ("private", Access_Spec, "PRIVATE"),
        ("public", Access_Spec, "PUBLIC"),
        ("bind(c)", Language_Binding_Spec, "BIND(C)"),
        ('bind(c, name="foo")', Language_Binding_Spec, 'BIND(C, NAME = "foo")'),
        ("intent(in)", Proc_Attr_Spec, "INTENT(IN)"),
        ("intent(out)", Proc_Attr_Spec, "INTENT(OUT)"),
        ("intent(inout)", Proc_Attr_Spec, "INTENT(INOUT)"),
        ("optional", Proc_Attr_Spec, "OPTIONAL"),
        ("pointer", Proc_Attr_Spec, "POINTER"),
        ("protected", Proc_Attr_Spec, "PROTECTED"),
        ("save", Proc_Attr_Spec, "SAVE"),
    ],
)
def test_proc_attr_spec(procedure_attribute_input, expected_class, expected_string):
    """
    Tests the procedure attribute specification as outlined in #R1213 of
    ISO/IEC 1539-1:2010.
    """
    unit_under_test = Proc_Attr_Spec

    result = unit_under_test(procedure_attribute_input)
    assert isinstance(result, expected_class)
    assert str(result) == expected_string


def test_proc_decl():  # R1214
    tcls = Proc_Decl
    obj = tcls("a => NULL")
    assert isinstance(obj, tcls)
    assert str(obj) == "a => NULL"

    obj = tcls("a")
    assert isinstance(obj, Name), repr(type(obj))
    assert str(obj) == "a"


def test_intrinsic_stmt():  # R1216
    tcls = Intrinsic_Stmt
    obj = tcls("intrinsic :: a, b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "INTRINSIC :: a, b"
    obj = tcls("intrinsic a, b")
    assert str(obj) == "INTRINSIC :: a, b"

    obj = tcls("intrinsic a")
    assert str(obj) == "INTRINSIC :: a"


def test_function_reference():  # R1217
    tcls = Function_Reference
    obj = tcls("f()")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "f()"
    assert repr(obj) == "Function_Reference(Name('f'), None)"

    obj = tcls("f(2,k=1,a)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "f(2, k = 1, a)"


def test_call_stmt():  # R1218
    tcls = Call_Stmt
    obj = tcls("call a")
    assert isinstance(obj, tcls)
    assert str(obj) == "CALL a"

    obj = tcls("call a()")
    assert str(obj) == "CALL a"

    obj = tcls("call a(b,c)")
    assert str(obj) == "CALL a(b, c)"


def test_procedure_designator():  # R1219
    tcls = Procedure_Designator
    obj = tcls("a%b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a % b"
    assert repr(obj) == "Procedure_Designator(Name('a'), '%', Name('b'))"


def test_actual_arg_spec():  # R1220
    tcls = Actual_Arg_Spec
    obj = tcls("k=a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "k = a"
    assert repr(obj) == "Actual_Arg_Spec(Name('k'), Name('a'))"

    obj = tcls("a")
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == "a"


def test_actual_arg_spec_list():
    tcls = Actual_Arg_Spec_List
    obj = tcls("a,b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a, b"
    assert repr(obj) == "Actual_Arg_Spec_List(',', (Name('a'), Name('b')))"

    obj = tcls("a = k")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a = k"

    obj = tcls("a = k,b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a = k, b"

    obj = tcls("a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a"


def test_alt_return_spec():  # R1222
    tcls = Alt_Return_Spec
    obj = tcls("* 123")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "*123"
    assert repr(obj) == "Alt_Return_Spec(Label('123'))"


def test_function_subprogram():  # R1223
    reader = get_reader(
        """\
    function foo()
    end function foo"""
    )
    tcls = Function_Subprogram
    obj = tcls(reader)
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "FUNCTION foo()\nEND FUNCTION foo"
    assert (
        repr(obj) == "Function_Subprogram(Function_Stmt(None, Name('foo'), None, None),"
        " End_Function_Stmt('FUNCTION', Name('foo')))"
    )

    reader = get_reader(
        """\
    pure real function foo(a) result(b) bind(c)
    integer a
    end function foo"""
    )
    tcls = Function_Subprogram
    obj = tcls(reader)
    assert isinstance(obj, tcls), repr(obj)
    assert (
        str(obj) == "PURE REAL FUNCTION foo(a) RESULT(b) BIND(C)\n  INTEGER :: "
        "a\nEND FUNCTION foo"
    )


def test_function_stmt():  # R1224
    """Check that rule R1224 (function-stmt) is parsed correctly."""

    tcls = Function_Stmt
    obj = tcls("function foo()")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "FUNCTION foo()"
    assert repr(obj) == "Function_Stmt(None, Name('foo'), None, None)"

    obj = tcls("function foo(a,b)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "FUNCTION foo(a, b)"
    assert (
        repr(obj) == "Function_Stmt(None, Name('foo'), Dummy_Arg_List(',', "
        "(Name('a'), Name('b'))), None)"
    )

    obj = tcls("function foo(a)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "FUNCTION foo(a)"

    obj = tcls("real function foo(a)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "REAL FUNCTION foo(a)"

    obj = tcls("real recursive function foo(a)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "REAL RECURSIVE FUNCTION foo(a)"

    obj = tcls("real function foo(a) bind(c)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "REAL FUNCTION foo(a) BIND(C)"

    obj = tcls("real function foo(a) result (b)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "REAL FUNCTION foo(a) RESULT(b)"

    obj = tcls("real function foo(a) bind(c) result(b)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "REAL FUNCTION foo(a) RESULT(b) BIND(C)"

    obj = tcls("elemental real function foo(a) result(b)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "ELEMENTAL REAL FUNCTION foo(a) RESULT(b)"

    obj = tcls("type(ELEMENTAL_type) function foo(a) bind(c)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "TYPE(ELEMENTAL_type) FUNCTION foo(a) BIND(C)"

    # Constraint C1242. A prefix shall not specify ELEMENTAL if
    # proc-language-binding-spec appears in the function-stmt or
    # subroutine-stmt.
    with pytest.raises(NoMatchError):
        _ = tcls("elemental real function foo() bind(c)")
    with pytest.raises(NoMatchError):
        _ = tcls("elemental real function foo() bind(c) result(b)")
    with pytest.raises(NoMatchError):
        _ = tcls("elemental real function foo() result(b) bind(c)")


def test_dummy_arg_name():  # R1226
    tcls = Dummy_Arg_Name
    obj = tcls("a")
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == "a"


@pytest.mark.parametrize(
    "procedure_prefix_input,expected_class,expected_string",
    [
        ("integer", Intrinsic_Type_Spec, "INTEGER"),
        ("integer * 2", Intrinsic_Type_Spec, "INTEGER*2"),
        ("real", Intrinsic_Type_Spec, "REAL"),
        ("double complex", Intrinsic_Type_Spec, "DOUBLE COMPLEX"),
        ("complex", Intrinsic_Type_Spec, "COMPLEX"),
        ("character", Intrinsic_Type_Spec, "CHARACTER"),
        ("logical", Intrinsic_Type_Spec, "LOGICAL"),
        ("type(foo)", Declaration_Type_Spec, "TYPE(foo)"),
        ("class(bar)", Declaration_Type_Spec, "CLASS(bar)"),
        ("class(*)", Declaration_Type_Spec, "CLASS(*)"),
        ("elemental", Prefix_Spec, "ELEMENTAL"),
        ("impure", Prefix_Spec, "IMPURE"),
        ("module", Prefix_Spec, "MODULE"),
        ("pure", Prefix_Spec, "PURE"),
        ("recursive", Prefix_Spec, "RECURSIVE"),
    ],
)
def test_prefix_spec(procedure_prefix_input, expected_class, expected_string):  # R1226
    unit_under_test = Prefix_Spec
    result = unit_under_test(procedure_prefix_input)
    assert isinstance(result, expected_class), repr(result)
    assert str(result) == expected_string


def test_suffix():  # R1229
    tcls = Suffix

    obj = tcls("bind(c)")
    assert isinstance(obj, Language_Binding_Spec), repr(obj)
    assert str(obj) == "BIND(C)"
    assert repr(obj) == "Language_Binding_Spec(None)"

    obj = tcls("result(a)")
    assert isinstance(obj, Suffix), repr(obj)
    assert str(obj) == "RESULT(a)"

    obj = tcls("bind(c) result(a)")
    assert isinstance(obj, Suffix), repr(obj)
    assert str(obj) == "RESULT(a) BIND(C)"

    obj = tcls("result(a) bind(c)")
    assert isinstance(obj, Suffix), repr(obj)
    assert str(obj) == "RESULT(a) BIND(C)"


def test_end_function_stmt():  # R1230
    tcls = End_Function_Stmt
    obj = tcls("end")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "END"

    obj = tcls("endfunction")
    assert str(obj) == "END FUNCTION"

    obj = tcls("endfunction foo")
    assert str(obj) == "END FUNCTION foo"


def test_subroutine_subprogram():  # R1231
    reader = get_reader(
        """\
    subroutine foo
    end subroutine foo"""
    )
    tcls = Subroutine_Subprogram
    obj = tcls(reader)
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "SUBROUTINE foo\nEND SUBROUTINE foo"
    assert (
        repr(obj) == "Subroutine_Subprogram(Subroutine_Stmt(None, "
        "Name('foo'), None, None), End_Subroutine_Stmt('SUBROUTINE', "
        "Name('foo')))"
    )

    reader = get_reader(
        """\
    subroutine foo
    integer a
    end subroutine foo"""
    )
    tcls = Subroutine_Subprogram
    obj = tcls(reader)
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "SUBROUTINE foo\n  INTEGER :: a\nEND SUBROUTINE foo"


def test_subroutine_stmt():  # R1232
    """Check that rule R1232 (subroutine-stmt) is parsed correctly."""

    tcls = Subroutine_Stmt
    obj = tcls("subroutine foo")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "SUBROUTINE foo"
    assert repr(obj) == "Subroutine_Stmt(None, Name('foo'), None, None)"

    obj = tcls("pure subroutine foo")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "PURE SUBROUTINE foo"

    obj = tcls("pure subroutine foo(a,b)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "PURE SUBROUTINE foo(a, b)"

    obj = tcls("subroutine foo() bind(c)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "SUBROUTINE foo BIND(C)"

    obj = tcls("subroutine foo(a)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "SUBROUTINE foo(a)"

    obj = tcls("subroutine foo(a, b)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "SUBROUTINE foo(a, b)"

    obj = tcls("subroutine foo(a,*)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "SUBROUTINE foo(a, *)"

    obj = tcls("subroutine foo(*)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "SUBROUTINE foo(*)"

    # Constraint C1242. A prefix shall not specify ELEMENTAL if
    # proc-language-binding-spec appears in the function-stmt or
    # subroutine-stmt.
    with pytest.raises(NoMatchError):
        _ = tcls("elemental module subroutine foo() bind(c)")


def test_dummy_arg():  # R1233
    tcls = Dummy_Arg
    obj = tcls("a")
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == "a"
    obj = tcls("*")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "*"


def test_end_subroutine_stmt():  # R1234
    tcls = End_Subroutine_Stmt
    obj = tcls("end subroutine foo")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "END SUBROUTINE foo"
    assert repr(obj) == "End_Subroutine_Stmt('SUBROUTINE', Name('foo'))"

    obj = tcls("end")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "END"

    obj = tcls("endsubroutine")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "END SUBROUTINE"


def test_entry_stmt():  # R1235
    tcls = Entry_Stmt
    obj = tcls("entry a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "ENTRY a()"

    obj = tcls("entry a()")
    assert str(obj) == "ENTRY a()"

    obj = tcls("entry a(b, c)")
    assert str(obj) == "ENTRY a(b, c)"

    obj = tcls("entry a(b, c) bind(c)")
    assert str(obj) == "ENTRY a(b, c) BIND(C)"


def test_return_stmt():  # R1236
    tcls = Return_Stmt
    obj = tcls("return")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "RETURN"
    assert repr(obj) == "Return_Stmt(None)"


def test_contains():  # R1237
    tcls = Contains_Stmt
    obj = tcls("Contains")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "CONTAINS"
    assert repr(obj) == "Contains_Stmt('CONTAINS')"


if 0:
    NOF_NEEDED_TESTS = 0
    NOF_NEEDED_MATCH = 0
    TOTAL_NEEDS = 0
    TOTAL_CLASSES = 0
    for NAME in dir():
        # pylint: disable=undefined-variable
        OBJ = ast.literal_eval(NAME)
        if not isinstance(OBJ, ClassType):
            continue
        if not issubclass(OBJ, Base):
            continue
        CLSNAME = OBJ.__name__
        if CLSNAME.endswith("Base"):
            continue
        TOTAL_CLASSES += 1
        SUBCLASS_NAMES = OBJ.__dict__.get("subclass_names", None)
        USE_NAMES = OBJ.__dict__.get("use_names", None)
        if not USE_NAMES:
            continue
        MATCH = OBJ.__dict__.get("match", None)
        try:
            TEST_CLS = ast.literal_eval("test_{0}".format(CLSNAME))
        except NameError:
            TEST_CLS = None
        TOTAL_NEEDS += 1
        if MATCH is None:
            if TEST_CLS is None:
                print("Needs tests:", CLSNAME)
                print("Needs match implementation:", CLSNAME)
                NOF_NEEDED_TESTS += 1
                NOF_NEEDED_MATCH += 1
            else:
                print("Needs match implementation:", CLSNAME)
                NOF_NEEDED_MATCH += 1
        else:
            if TEST_CLS is None:
                print("Needs tests:", CLSNAME)
                NOF_NEEDED_TESTS += 1
        continue
    print("-----")
    print("Nof match implementation needs:", NOF_NEEDED_MATCH, "out of", TOTAL_NEEDS)
    print("Nof tests needs:", NOF_NEEDED_TESTS, "out of", TOTAL_NEEDS)
    print("Total number of classes:", TOTAL_CLASSES)
    print("-----")
