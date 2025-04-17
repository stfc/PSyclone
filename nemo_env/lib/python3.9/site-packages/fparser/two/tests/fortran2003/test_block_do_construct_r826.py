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

""" pytest module for the Fortran2003 Do Construct - R826."""

import pytest
from fparser.api import get_reader
from fparser.common.readfortran import FortranStringReader
from fparser.two.utils import FortranSyntaxError
from fparser.two.Fortran2003 import (
    Block_Label_Do_Construct,
    Block_Nonlabel_Do_Construct,
)


@pytest.mark.usefixtures("f2003_create")
def test_block_label_do_construct():
    """Tests that block labeled DO construct
    is parsed correctly (R826_1)."""
    tcls = Block_Label_Do_Construct

    obj = tcls(
        get_reader(
            """\
      do 12
        a = 1
 12   continue
    """
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "DO 12\n  a = 1\n12 CONTINUE"

    obj = tcls(
        get_reader(
            """\
      foo: do 21, i=1,10
        a = 1
 21   end do foo
    """
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "foo:DO 21 , i = 1, 10\n  a = 1\n21 END DO foo"

    obj = tcls(
        get_reader(
            """
      do 51 while (a < 10)
        a = a + 1
 51   continue
    """
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "DO 51 WHILE (a < 10)\n  a = a + 1\n51 CONTINUE"

    obj = tcls(
        get_reader(
            """
      do 52
        a = a + 1
        if (a > 10) exit
 52   continue
    """
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "DO 52\n  a = a + 1\n  IF (a > 10) EXIT\n52 CONTINUE"

    obj = tcls(
        get_reader(
            """\
      do 12
        do 13
          a = 1
 13   continue
 12   continue
    """
        )
    )
    assert str(obj) == "DO 12\n  DO 13\n    a = 1\n13 CONTINUE\n12 CONTINUE"
    assert len(obj.content) == 3, repr(len(obj.content))
    assert str(obj.content[1]) == "DO 13\n  a = 1\n13 CONTINUE"

    obj = tcls(
        get_reader(
            """
      do 52, i = 1,10
        do 53, while (j /= n)
        j = j + i
 53   continue
 52   continue
    """
        )
    )
    assert len(obj.content) == 3, repr(len(obj.content))
    assert str(obj) == (
        "DO 52 , i = 1, 10\n  DO 53 , WHILE (j /= n)\n"
        "    j = j + i\n53 CONTINUE\n52 CONTINUE"
    )
    assert str(obj.content[1]) == ("DO 53 , WHILE (j /= n)\n  j = j + i\n53 CONTINUE")


@pytest.mark.usefixtures("f2003_create")
def test_block_nonlabel_do_construct():
    # pylint: disable=invalid-name
    """Tests that block nonlabeled DO construct is parsed
    correctly (R826_2)"""
    tcls = Block_Nonlabel_Do_Construct

    obj = tcls(
        get_reader(
            """\
      do i=1,10
        a = 1
      end do
    """
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "DO i = 1, 10\n  a = 1\nEND DO"

    obj = tcls(
        get_reader(
            """\
      do while (a < 10)
        a = a + 1
      end do
    """
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "DO WHILE (a < 10)\n  a = a + 1\nEND DO"

    obj = tcls(
        get_reader(
            """
      do
        a = a - 1
        if (a < 10) exit
      end do
    """
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "DO\n  a = a - 1\n  IF (a < 10) EXIT\nEND DO"
    assert len(obj.content) == 4, repr(len(obj.content))
    assert str(obj.content[2]) == "IF (a < 10) EXIT"

    obj = tcls(
        get_reader(
            """\
      foo:do i=1,10
        a = 1
      end do foo
    """
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "foo:DO i = 1, 10\n  a = 1\nEND DO foo"

    obj = tcls(
        get_reader(
            """\
      foo:do while (a < 10)
        a = a + 1
      end do foo
    """
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "foo:DO WHILE (a < 10)\n  a = a + 1\nEND DO foo"

    obj = tcls(
        get_reader(
            """\
      do j=1,2
      foo:do i=1,10
        a = 1
      end do foo
      end do
    """
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == (
        "DO j = 1, 2\n" "  foo:DO i = 1, 10\n    a = 1\n  END DO foo\nEND DO"
    )

    obj = tcls(
        get_reader(
            """
      do while (j >= n)
      bar:do i=1,10
        a = i + j
      end do bar
      j = j - 1
      end do
    """
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == (
        "DO WHILE (j >= n)\n"
        "  bar:DO i = 1, 10\n    a = i + j\n  END DO bar\n"
        "  j = j - 1\nEND DO"
    )

    obj = tcls(
        get_reader(
            """
      do, i = 1,10
      bar: do, while (j /= n)
        a = i - j
      end do bar
      end do
    """
        )
    )
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == (
        "DO , i = 1, 10\n"
        "  bar:DO , WHILE (j /= n)\n    a = i - j\n  END DO bar\n"
        "END DO"
    )
    assert len(obj.content) == 3, repr(len(obj.content))
    assert str(obj.content[1]) == ("bar:DO , WHILE (j /= n)\n  a = i - j\nEND DO bar")


@pytest.mark.usefixtures("f2003_create")
def test_doconstruct_tofortran_non_ascii():
    """Check that the tofortran() method works when the do-construct contains
    a character string with non-ascii characters."""
    code = "DO i=1,10\n" "  WRITE(*,*) ' for e1=1\xb0'\n" "END DO\n"
    reader = FortranStringReader(code, ignore_comments=False)
    obj = Block_Nonlabel_Do_Construct(reader)
    out_str = str(obj)
    assert "for e1=1" in out_str


def test_do_construct_wrong_name(f2003_create, fake_symbol_table):
    """Check that named 'do' block has correct name at end of block"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        Block_Nonlabel_Do_Construct(
            get_reader(
                """\
            name: do
                a = 1
            end do wrong"""
            )
        )
    assert exc_info.value.args[0].endswith("Expecting name 'name', got 'wrong'")


def test_do_construct_missing_start_name(f2003_create, fake_symbol_table):
    """Check that named 'do' block has correct name at end of block"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        Block_Nonlabel_Do_Construct(
            get_reader(
                """\
            do
                a = 1
            end do name"""
            )
        )
    assert exc_info.value.args[0].endswith(
        "Name 'name' has no corresponding starting name"
    )


def test_do_construct_missing_end_name(f2003_create, fake_symbol_table):
    """Check that named 'do' block has correct name at end of block"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        Block_Nonlabel_Do_Construct(
            get_reader(
                """\
            name: do
                a = 1
            end do"""
            )
        )
    assert exc_info.value.args[0].endswith("Expecting name 'name' but none given")
