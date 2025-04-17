# Copyright (c) 2022-2023 Science and Technology Facilities Council.

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

"""Module containing pytest tests for the support of the Fortran2008
Block construct."""

import re
import pytest

from fparser.api import get_reader
from fparser.two.Fortran2008 import Block_Construct, Block_Stmt
from fparser.two.symbol_table import SYMBOL_TABLES
from fparser.two.utils import FortranSyntaxError, ScopingRegionMixin, walk


def test_block():
    """Test that the Block_Construct matches as expected."""
    block = Block_Construct(
        get_reader(
            """\
            block
               integer :: b = 4
               a = 1 + b
            end block
            """
        )
    )
    assert isinstance(block.children[0], Block_Stmt)
    assert isinstance(block.children[0], ScopingRegionMixin)
    name = block.children[0].get_scope_name()
    assert re.match(r"block:[\d+]", name)
    assert "BLOCK\n  INTEGER :: b = 4\n  a = 1 + b\nEND BLOCK" in str(block)


@pytest.mark.parametrize(
    "before, after", [("", ""), ("b = 2.0 * b", ""), ("", "b = 2.0 * b")]
)
def test_block_new_scope(f2008_parser, before, after):
    """Test that a Block_Construct creates a new scoping region."""
    block = f2008_parser(
        get_reader(
            f"""\
            program foo
            integer :: b = 3
            {before}
            block
               integer :: b = 4
               a = 1 + b
            end block
            {after}
            end program foo
            """
        )
    )

    assert "BLOCK\nINTEGER :: b = 4\na = 1 + b\nEND BLOCK" in str(block).replace(
        "  ", ""
    )
    tables = SYMBOL_TABLES
    assert list(tables._symbol_tables.keys()) == ["foo"]
    table = SYMBOL_TABLES.lookup("foo")
    assert len(table.children) == 1
    assert re.match(r"block:[\d+]", table.children[0].name)


def test_block_in_if(f2008_parser):
    """Test that a Block may appear inside an IF."""
    ptree = f2008_parser(
        get_reader(
            """\
            program foo
            integer :: b = 3
            if (b == 2) then
              block
                real :: tmp
                tmp = ATAN(0.5)
                b = NINT(tmp)
              end block
            end if
            end program foo
            """
        )
    )
    blocks = walk([ptree], Block_Construct)
    assert len(blocks) == 1


def test_named_block():
    """
    Test that a named block construct is correctly captured and also
    reproduced.

    """
    block = Block_Construct(
        get_reader(
            """\
            foo: block
               integer :: b = 4
               a = 1 + b
            end block foo
            """
        )
    )

    assert "foo:BLOCK\n  INTEGER :: b = 4\n  a = 1 + b\nEND BLOCK foo" in str(block)


def test_end_block_missing_start_name():  # C808
    """
    Test Constraint 808 - that a name on the 'end block' must correspond
    with the same name on the 'block'.

    """
    with pytest.raises(FortranSyntaxError) as err:
        Block_Construct(
            get_reader(
                """\
                block
                end block foo
                """
            )
        )
    assert "Name 'foo' has no corresponding starting name" in str(err)


def test_end_block_missing_end_name():  # C808
    """
    Test that a named block that is missing a name on its 'end block' statement
    results in a syntax error.

    """
    with pytest.raises(FortranSyntaxError) as err:
        Block_Construct(
            get_reader(
                """\
                foo: block
                end block
                """
            )
        )
    assert "Expecting name 'foo' but none given" in str(err)


def test_end_block_wrong_name():  # C808
    """
    Test that an incorrect name on the end block statement results in a
    syntax error.

    """
    with pytest.raises(FortranSyntaxError) as err:
        Block_Construct(
            get_reader(
                """\
                foo: block
                end block bar
                """
            )
        )
    assert "Expecting name 'foo', got 'bar'" in str(err)


def test_block_in_subroutine(f2008_parser):
    """
    Check that we get two, nested symbol tables when a routine contains
    a Block construct.

    """
    code = """\
            program my_prog
            real :: a
            a = -1.0
            if (a < 0.0) then
             rocking: block
               real :: b
               b = 42.0
               a = b
             end block rocking
            else
             block
               real :: c
               c = 42.0 / 5.0
               a = 10.0 * c
             end block
            end if
            end program my_prog
            """
    _ = f2008_parser(get_reader(code))
    tables = SYMBOL_TABLES
    assert list(tables._symbol_tables.keys()) == ["my_prog"]
    table = SYMBOL_TABLES.lookup("my_prog")
    assert len(table.children) == 2
    assert table.children[0].name == "rocking"
    assert re.match(r"block:[\d+]", table.children[1].name)
