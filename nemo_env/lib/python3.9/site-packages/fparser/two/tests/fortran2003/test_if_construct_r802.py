# This Python file uses the following encoding: utf-8
# Copyright (c) 2020-2022 Science and Technology Facilities Council.

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

""" pytest module for the Fortran2003 If Construct - R802. Note that
    this requires re-implementing and extending to fully cover the
    language specification (#232). """

import pytest
from fparser.api import get_reader
from fparser.common.readfortran import FortranStringReader
from fparser.two.Fortran2003 import If_Construct
from fparser.two.utils import FortranSyntaxError


@pytest.mark.usefixtures("f2003_create", "fake_symbol_table")
def test_if_construct():
    """Basic tests for the if construct."""
    tcls = If_Construct
    obj = tcls(
        get_reader(
            """\
if (expr) then
  a = 1
end if
    """
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "IF (expr) THEN\n  a = 1\nEND IF"

    obj = tcls(
        get_reader(
            """\
name: if (expr) then
  a = 1
end if name
    """
        )
    )

    assert str(obj) == "name:IF (expr) THEN\n  a = 1\nEND IF name"

    obj = tcls(
        get_reader(
            """\
if (expr) then
  a = 1
  if (expr2) then
    a = 2
  endif
  a = 3
end if
    """
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert (
        str(obj) == "IF (expr) THEN\n  a = 1\n  IF (expr2) THEN\n    a = 2\n"
        "  END IF\n  a = 3\nEND IF"
    )

    obj = tcls(
        get_reader(
            """\
if (expr) then
  a = 1
else if (expr2) then
  a = 2
end if
    """
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "IF (expr) THEN\n  a = 1\nELSE IF (expr2) THEN\n  a = 2\nEND IF"

    obj = tcls(
        get_reader(
            """\
if (expr) then
  a = 1
else
  a = 2
end if
    """
        )
    )
    assert str(obj) == "IF (expr) THEN\n  a = 1\nELSE\n  a = 2\nEND IF"

    obj = tcls(
        get_reader(
            """\
if (expr) then
  a = 1
else if (expr2) then
  a = 2
else
  a = 3
end if
    """
        )
    )
    assert (
        str(obj) == "IF (expr) THEN\n  a = 1\nELSE IF (expr2) THEN\n  a = 2\n"
        "ELSE\n  a = 3\nEND IF"
    )

    obj = tcls(
        get_reader(
            """\
named: if (expr) then
  a = 1
else named
  a = 2
end if named
    """
        )
    )
    assert (
        str(obj) == "named:IF (expr) THEN\n  a = 1\nELSE named\n  a = 2\nEND IF named"
    )

    obj = tcls(
        get_reader(
            """\
named: if (expr) then
  a = 1
  named2: if (expr2) then
    a = 2
  end if named2
end if named
"""
        )
    )
    assert (
        str(obj) == "named:IF (expr) THEN\n  a = 1\n  named2:IF (expr2) THEN\n"
        "    a = 2\n  END IF named2\nEND IF named"
    )

    obj = tcls(
        get_reader(
            """\
if (expr) then
  a = 1
else if (expr2) then
  a = 2
else if (expr3) then
  a = 3
end if
    """
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert (
        str(obj) == "IF (expr) THEN\n  a = 1\nELSE IF (expr2) THEN\n  a = 2\n"
        "ELSE IF (expr3) THEN\n  a = 3\nEND IF"
    )

    obj = tcls(
        get_reader(
            """\
        if (dxmx .gt. 0d0) then
          diff = 0
          do  80  k = 1, n
   80     diff = max(diff,abs(xnew(k)-xin(k)))
          if (diff .gt. dxmx) then
            betx = dxmx/diff

            call awrit3(' broyj:  max shift = %1;3g'//
     .        ' is larger than dxmx = %1;3g.  Scale by %1;3g',
     .        ' ',80,i1mach(2),diff,dxmx,dxmx/diff)

            do  82  k = 1, n
   82       xnew(k) = xin(k) + betx*(xnew(k)-xin(k))
          endif
        endif"""
        )
    )
    assert isinstance(obj, tcls)


@pytest.mark.usefixtures("f2003_create")
def test_ifconstruct_tofortran_non_ascii():
    """Check that the tofortran() method works when the if-construct contains
    a character string with non-ascii characters."""
    code = "IF(iflag)THEN\n" "  WRITE(*,*) ' for e1=1\xb0'\n" "END IF\n"
    reader = FortranStringReader(code, ignore_comments=False)
    obj = If_Construct(reader)
    out_str = str(obj)
    assert "for e1=1" in out_str


def test_if_construct_wrong_name(f2003_create, fake_symbol_table):
    """Check named 'if' constructs have correct start/end names"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        If_Construct(
            get_reader(
                """\
            name: if (expr) then
                a = 1
            end if wrong"""
            )
        )
    assert exc_info.value.args[0].endswith("Expecting name 'name', got 'wrong'")


def test_if_construct_missing_start_name(f2003_create, fake_symbol_table):
    """Check named 'if' constructs have correct start/end names"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        If_Construct(
            get_reader(
                """\
            if (expr) then
                a = 1
            end if name"""
            )
        )
    assert exc_info.value.args[0].endswith(
        "Name 'name' has no corresponding starting name"
    )


def test_if_construct_missing_end_name(f2003_create, fake_symbol_table):
    """Check named 'if' constructs have correct start/end names"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        If_Construct(
            get_reader(
                """\
            name: if (expr) then
                a = 1
            end if"""
            )
        )
    assert exc_info.value.args[0].endswith("Expecting name 'name' but none given")


def test_if_construct_else_wrong_end_name(f2003_create, fake_symbol_table):
    """Check named 'if' constructs have correct start/end names"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        If_Construct(
            get_reader(
                """\
            name: if (expr) then
                a = 1
            else
                a = 2
            end if wrong"""
            )
        )
    assert exc_info.value.args[0].endswith("Expecting name 'name', got 'wrong'")


def test_if_construct_else_wrong_name(f2003_create, fake_symbol_table):
    """Check named 'if' constructs have correct start/end names"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        If_Construct(
            get_reader(
                """\
            name: if (expr) then
                a = 1
            else wrong
                a = 2
            end if name"""
            )
        )
    assert exc_info.value.args[0].endswith("Expecting name 'name', got 'wrong'")


def test_if_construct_else_if_wrong_name(f2003_create, fake_symbol_table):
    """Check named 'if' constructs have correct start/end names"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        If_Construct(
            get_reader(
                """\
            name: if (expr) then
                a = 1
            else if (other_expr) then wrong
                a = 2
            end if name"""
            )
        )
    assert exc_info.value.args[0].endswith("Expecting name 'name', got 'wrong'")
