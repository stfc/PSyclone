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

"""Module which provides pytest fixtures for use by files in this
directory

"""
import pytest
from fparser.two.parser import ParserFactory
from fparser.two.symbol_table import SYMBOL_TABLES


@pytest.fixture
def f2003_create():
    """Create a fortran 2003 parser class hierarchy"""
    _ = ParserFactory().create(std="f2003")


@pytest.fixture
def f2003_parser():
    """Create a Fortran 2003 parser class hierarchy and return the parser
    for usage in tests.

    :return: a Program class (not object) for use with the Fortran reader.
    :rtype: :py:class:`fparser.two.Fortran2003.Program`
    """
    return ParserFactory().create(std="f2003")


@pytest.fixture(name="clear_symbol_table", autouse=True)
def clear_symbol_tables_fixture():
    """Clear-up any existing symbol-table hierarchy."""
    SYMBOL_TABLES.clear()


@pytest.fixture(name="fake_symbol_table")
def setup_symbol_table_fixture():
    """Creates a current scope for those tests that would otherwise
    not have one."""
    SYMBOL_TABLES.enter_scope("fixture_scope")
    yield
    SYMBOL_TABLES.exit_scope()
