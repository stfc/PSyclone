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
Critical construct."""


import pytest

from fparser.api import get_reader
from fparser.two.Fortran2003 import Assignment_Stmt
from fparser.two.Fortran2008 import Critical_Construct, Critical_Stmt, End_Critical_Stmt
from fparser.two.utils import FortranSyntaxError


def test_critical():
    """Test that a basic critical construct is correctly constructed."""
    critical = Critical_Construct(
        get_reader(
            """\
            critical
               a = 1 + b
            end critical
            """
        )
    )
    assert isinstance(critical.children[0], Critical_Stmt)
    assert isinstance(critical.children[1], Assignment_Stmt)
    assert isinstance(critical.children[2], End_Critical_Stmt)
    assert critical.children[0].get_start_name() is None
    assert "CRITICAL\n  a = 1 + b\nEND CRITICAL" in str(critical)


def test_named_critical():
    """Test that a named critical construct is matched correctly and that
    its name can be queried."""
    critical = Critical_Construct(
        get_reader(
            """\
            foo: critical
               a = 1 + b
            end critical foo
            """
        )
    )
    assert critical.children[0].get_start_name() == "foo"
    assert "foo:CRITICAL\n  a = 1 + b\nEND CRITICAL foo" in str(critical)


def test_end_critical_missing_start_name():  # C809
    """Check that a critical construct with an end name but no start name
    results in a syntax error (C809)."""
    with pytest.raises(FortranSyntaxError) as err:
        Critical_Construct(
            get_reader(
                """\
                critical
                end critical foo
                """
            )
        )
    assert "Name 'foo' has no corresponding starting name" in str(err)


def test_end_critical_missing_end_name():  # C809
    """Test that a named critical construct with the name omitted from
    the end critical results in a syntax error (C809)."""
    with pytest.raises(FortranSyntaxError) as err:
        Critical_Construct(
            get_reader(
                """\
                foo: critical
                end critical
                """
            )
        )
    assert "Expecting name 'foo' but none given" in str(err)


def test_end_critical_wrong_name():  # C809
    """Test that mismatched start and end names result in a syntax error (C809)"""
    with pytest.raises(FortranSyntaxError) as err:
        Critical_Construct(
            get_reader(
                """\
                foo: critical
                end critical bar
                """
            )
        )
    assert "Expecting name 'foo', got 'bar'" in str(err)
