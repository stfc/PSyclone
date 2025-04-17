# Copyright (c) 2018-2021 Science and Technology Facilities Council.

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

"""Test Fortran 2008 rule R1116 and its constraints C1112 and C1114.

    submodule is submodule-stmt
                 [ specification-part ]
                 [ module-subprogram-part ]
                 end-submodule-stmt

    C1112 A submodule specification-part shall not contain a
    format-stmt, entry-stmt, or stmt-function-stmt.

    C1114 If a submodule-name appears in the end-submodule-stmt, it
    shall be identical to the one in the submodule-stmt.

"""

import pytest
from fparser.api import get_reader
from fparser.two.Fortran2008 import Submodule
from fparser.two.symbol_table import SYMBOL_TABLES
from fparser.two.utils import NoMatchError


def test_submodule(f2008_create):
    """Test the parsing of a minimal submodule."""
    reader = get_reader(
        """\
      submodule (foobar) bar
      end
      """
    )
    ast = Submodule(reader)
    assert "SUBMODULE (foobar) bar\n" "END" in str(ast)
    # A new symbol table should have been created
    assert "bar" in SYMBOL_TABLES._symbol_tables


@pytest.mark.usefixtures("fake_symbol_table")
def test_submodule_sp(f2008_create):
    """Test the parsing of a minimal submodule with a specification
    part.

    """
    reader = get_reader(
        """\
      submodule (foobar) bar
        use empty
      end
      """
    )
    ast = Submodule(reader)
    assert "SUBMODULE (foobar) bar\n" "  USE empty\n" "END" in str(ast)


def test_submodule_msp(f2008_create):
    """Test the parsing of a minimal submodule with a module subprogram
    part.

    """
    reader = get_reader(
        """\
      submodule (foobar) bar
      contains
        subroutine info()
        end subroutine info
      end
      """
    )
    ast = Submodule(reader)
    assert (
        "SUBMODULE (foobar) bar\n"
        "  CONTAINS\n"
        "  SUBROUTINE info\n"
        "  END SUBROUTINE info\n"
        "END" in str(ast)
    )
    # A new symbol table should have been created
    assert "bar" in SYMBOL_TABLES._symbol_tables
    assert "info" == SYMBOL_TABLES.lookup("bar").children[0].name


@pytest.mark.usefixtures("fake_symbol_table")
def test_submodule_both(f2008_create):
    """Test the parsing of a minimal submodule with a specification part
    and a module subprogram part.

    """
    reader = get_reader(
        """\
      submodule (foobar) bar
      use empty
      contains
        subroutine info()
        end subroutine info
      end
      """
    )
    ast = Submodule(reader)
    assert (
        "SUBMODULE (foobar) bar\n"
        "  USE empty\n"
        "  CONTAINS\n"
        "  SUBROUTINE info\n"
        "  END SUBROUTINE info\n"
        "END" in str(ast)
    )


# constraint C1112 format statement


def test_submodule_format_error1(f2008_create):
    """C1112: Test an exception is raised if a format statement is
    specified in a submodule. The first place it can occur is in the
    implicit part of the specification.

    """
    reader = get_reader(
        """\
      submodule (foobar) bar
      1 format(a)
      end
      """
    )
    with pytest.raises(NoMatchError) as excinfo:
        dummy_ = Submodule(reader)
    assert "at line 2\n" ">>>      1 format(a)\n" in str(excinfo.value)


def test_submodule_format_error2(f2008_create):
    """C1112: Test an exception is raised if a format statement is
    specified in a submodule. The second place it can occur is in the
    declaration part of the specification.

    """
    reader = get_reader(
        """\
      submodule (foobar) bar
      contains
      1 format(a)
      end
      """
    )
    with pytest.raises(NoMatchError) as excinfo:
        dummy_ = Submodule(reader)
    assert "at line 3\n" ">>>      1 format(a)\n" in str(excinfo.value)


# constraint C1112 entry statement


def test_submodule_entry_error1(f2008_create):
    """C1112: Test an exception is raised if an entry statement is
    specified in a submodule. The first place it can occur is in the
    implicit part of the specification.

    """
    reader = get_reader(
        """\
      submodule (foobar) bar
      entry here
      end
      """
    )
    with pytest.raises(NoMatchError) as excinfo:
        dummy_ = Submodule(reader)
    assert "at line 2\n" ">>>      entry here\n" in str(excinfo.value)


def test_submodule_entry_error2(f2008_create):
    """C1112: Test an exception is raised if an entry statement is
    specified in a submodule. The second place it can occur is in the
    declaration part of the specification.

    """
    reader = get_reader(
        """\
      submodule (foobar) bar
      contains
      entry here
      end
      """
    )
    with pytest.raises(NoMatchError) as excinfo:
        dummy_ = Submodule(reader)
    assert "at line 3\n" ">>>      entry here\n" in str(excinfo.value)


# constraint C1112 statement-function statement


def test_submodule_stmt_func_error(f2008_create):
    """C1112: Test an exception is raised if a statement-function
    statement is specified in a submodule. The only place it could
    validly occur is in the declaration part of the specification.

    """
    reader = get_reader(
        """\
      submodule (foobar) bar
      contains
      statefunc(x) = x*2
      end
      """
    )
    with pytest.raises(NoMatchError) as excinfo:
        dummy_ = Submodule(reader)
    assert "at line 3\n" ">>>      statefunc(x) = x*2\n" in str(excinfo.value)


# constraint C1114


def test_submodule_samename(f2008_create):
    """Test the parsing of a submodule with the same name in the start and
    end statements: C1114.

    """
    reader = get_reader(
        """\
      submodule (foobar) bar
      end submodule bar
      """
    )
    ast = Submodule(reader)
    assert "SUBMODULE (foobar) bar\n" "END SUBMODULE bar" in str(ast)


def test_submodule_differentname(f2008_create):
    """Test an exception is raised if the end submodule statement has a
    different name to that of the submodule statement : C1114.

    """
    reader = get_reader(
        """\
      submodule (foobar) bar
      end submodule error
      """
    )
    with pytest.raises(SystemExit):
        dummy_ = Submodule(reader)
