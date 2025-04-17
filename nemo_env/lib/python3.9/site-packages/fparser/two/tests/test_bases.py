# Copyright (c) 2017-2021 Science and Technology Facilities Council.

# All rights reserved.

# Modifications made as part of the fparser project are distributed
# under the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:

# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.

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

""" Module containing pytest tests for fparser2 base classes """

import pytest
from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.symbol_table import SYMBOL_TABLES
from fparser.two.utils import NoMatchError, BlockBase, walk


@pytest.mark.usefixtures("f2003_create")
def test_keywordvaluebase_errors():
    """Unit tests for the KeywordValueBase class to check that it rejects
    invalid input"""
    from fparser.two.Fortran2003 import (
        KeywordValueBase,
        Io_Unit,
        Format,
        Char_Literal_Constant,
    )

    lhs_cls = "UNIT"
    rhs_cls = Io_Unit
    obj = KeywordValueBase.match(lhs_cls, rhs_cls, "  ", require_lhs=False)
    assert obj is None
    obj = KeywordValueBase.match(lhs_cls, rhs_cls, " = 36 ")
    assert obj is None
    _, obj = KeywordValueBase.match("FMT", Format, "'(A)'", require_lhs=False)
    assert isinstance(obj, Char_Literal_Constant)
    _, obj = KeywordValueBase.match("FMT", Format, "FMT='(A)'", require_lhs=False)
    assert isinstance(obj, Char_Literal_Constant)
    _, obj = KeywordValueBase.match(
        "FMT", Format, "FMT='(\"my_var =  \", (A))'", require_lhs=False
    )
    assert isinstance(obj, Char_Literal_Constant)
    lhs, obj = KeywordValueBase.match(
        "FMT", Format, "FMT='(\"my_var =  \", (A))'", require_lhs=True
    )
    assert lhs == "FMT"
    assert isinstance(obj, Char_Literal_Constant)
    # Try to trigger an error by specifying that a "LHS =" is required but
    # then omitting it while having the RHS contain a '=' character
    obj = KeywordValueBase.match(
        "FMT", Format, "'(\"my_var =  \", (A))'", require_lhs=True
    )
    assert obj is None
    # Check with a valid Fortran name on the lhs but not what the match
    # is requesting.
    with pytest.raises(NoMatchError) as err:
        _ = KeywordValueBase.match("FERMAT", Format, "FMT='(A)'", require_lhs=False)
    assert "FMT='(A)'" in str(err.value)


@pytest.mark.usefixtures("f2003_create")
def test_read_stmt_errors():
    """Unit tests for the Read class to ensure it rejects invalid
    inputs"""
    from fparser.two.Fortran2003 import Read_Stmt

    # Missing closing parenthesis
    obj = Read_Stmt.match("READ(unit=23")
    assert obj is None
    # Missing arguments
    obj = Read_Stmt.match("READ()")
    assert obj is None
    obj = Read_Stmt.match("READ")
    assert obj is None
    # Wrong argument type
    obj = Read_Stmt.match("READ a_var")
    assert obj is None
    obj = Read_Stmt.match("READ 13")
    assert obj is None
    # Missing comma
    obj = Read_Stmt.match("READ * a_var")
    assert obj is None
    # Missing value/variable after comma
    obj = Read_Stmt.match("READ 13, ")
    assert obj is None


@pytest.mark.usefixtures("f2003_create")
def test_io_ctrl_spec_list_errors():
    """Unit tests for the Io_Control_Spec_List class to ensure it
    rejects invalid input"""
    from fparser.two.Fortran2003 import Io_Control_Spec_List

    # Positional arg following named arg
    obj = Io_Control_Spec_List.match("unit=23, namvar")
    assert obj is None


@pytest.mark.usefixtures("f2003_create")
def test_io_ctrl_spec_errors():
    """Unit tests for the Io_Control_Spec class to ensure it
    rejects invalid input"""
    from fparser.two.Fortran2003 import Io_Control_Spec

    # An argument with a name that is not valid within an IO control
    # description
    obj = Io_Control_Spec.match("not_unit=23")
    assert obj is None


@pytest.mark.usefixtures("f2003_create")
def test_blockbase_tofortran_non_ascii():
    """Check that the tofortran() method works when we have a program
    containing non-ascii characters within a sub-class of BlockBase. We
    use a Case Construct for this purpose."""
    from fparser.two.Fortran2003 import Program, Case_Construct

    code = (
        "program my_test\n"
        "! A comment outside the select block\n"
        "SELECT CASE(iflag)\n"
        "CASE(  30  )\n"
        "  IF(lwp) WRITE(*,*) ' for e1=1\xb0'\n"
        "END SELECT\n"
        "end program\n"
    )
    reader = FortranStringReader(code, ignore_comments=False)
    obj = Program(reader)
    bbase = walk(obj.content, Case_Construct)[0]
    # Explicitly call tofortran() on the BlockBase class.
    out_str = BlockBase.tofortran(bbase)
    assert "for e1=1" in out_str
