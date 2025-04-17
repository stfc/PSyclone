# Copyright (c) 2019 Science and Technology Facilities Council

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

"""Test Fortran Include Statement: This file tests the parsing of an
include statement. Whilst include is not part of the standard Fortran
rules (the include should include code as the code is being parsed)
there are cases where users might like to keep the include statement
in the Fortran parse tree and output it again.

"""

import pytest
from fparser.api import get_reader
from fparser.two.Fortran2003 import Include_Stmt, InternalError
from fparser.two.utils import NoMatchError


def test_include_stmt(f2003_create):
    """Check that a basic include statement is parsed
    correctly. Input separately as a string and as a reader object

    """

    def check_include(reader):
        """Internal helper function to avoid code replication."""
        ast = Include_Stmt(reader)
        assert "INCLUDE 'my-non-existant-file.inc'" in str(ast)
        assert repr(ast).replace("u'", "'") == (
            "Include_Stmt(Include_Filename(" "'my-non-existant-file.inc'))"
        )

    line = "include 'my-non-existant-file.inc'"
    check_include(line)
    reader = get_reader(line)
    check_include(reader)


def test_spaces(f2003_create):
    """Check that spaces are allowed before and after an include keyword
    as well as after the file string.

    """
    line = " include 'my-non-existant-file.inc' "
    ast = Include_Stmt(line)
    assert "INCLUDE 'my-non-existant-file.inc'" in str(ast)


def test_no_space(f2003_create):
    """Check that no space is required between the include keyword and the
    file string.

    """
    line = "include'my-non-existant-file.inc'"
    ast = Include_Stmt(line)
    assert "INCLUDE 'my-non-existant-file.inc'" in str(ast)


def test_case(f2003_create):
    """Check that different case is allowed for the include keyword."""
    line = "InClUdE 'my-non-existant-file.inc'"
    ast = Include_Stmt(line)
    assert "INCLUDE 'my-non-existant-file.inc'" in str(ast)


def test_double_quotes(f2003_create):
    """Check that double quotes are allowed for the file string."""
    line = 'include "my-non-existant-file.inc"'
    ast = Include_Stmt(line)
    assert "INCLUDE 'my-non-existant-file.inc'" in str(ast)


def test_errors(f2003_create):
    """Check that syntax errors produce a NoMatchError exception."""
    for line in [
        None,
        "",
        "  ",
        "includ",
        "includ 'x'",
        "include",
        "include ''",
        "include \"x'",
        "include 'x\"",
        "include 'xxx",
        'include "xxx',
        "include xxx'",
        'include xxx"',
        "include x'x'",
        "include 'x'x",
        "x include 'x'",
    ]:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Include_Stmt(line)
        assert "Include_Stmt: '{0}'".format(line) in str(excinfo.value)


def test_include_filename_error(f2003_create, monkeypatch):
    """Check that we raise an InternalError if a return from
    Include_Filename is None or an empty string. This should never
    happen as any matching errors would cause this class to raise an
    exception.

    """

    monkeypatch.setattr(
        "fparser.two.Fortran2003.Include_Filename", lambda file_name: None
    )
    line = "include ' '"
    with pytest.raises(InternalError) as excinfo:
        _ = Include_Stmt(line)
    assert ("Include_Filename should never return None or an empty " "name") in str(
        excinfo.value
    )
