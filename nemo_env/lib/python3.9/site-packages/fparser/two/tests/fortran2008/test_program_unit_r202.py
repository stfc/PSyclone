# Copyright (c) 2018 Science and Technology Facilities Council

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

"""Test Fortran 2008 rule R202 : This file tests the addition of
submodules in Fortran2008 for the Program-unit rule.

"""

import pytest
from fparser.two.utils import NoMatchError
from fparser.api import get_reader
from fparser.two.Fortran2008 import Program_Unit


def test_other(f2008_create):
    """Test that something other than submodule can still be parsed
    i.e. by adding submodule we've not broken the existing
    program-unit options.

    """
    reader = get_reader(
        """\
      subroutine test()
      end subroutine
      """
    )
    ast = Program_Unit(reader)
    assert "SUBROUTINE test\n" "END SUBROUTINE" in str(ast)


def test_submodule(f2008_create):
    """Test that submodule as a top-level program unit can be parsed."""
    reader = get_reader(
        """\
      submodule (foobar) bar
      end
      """
    )
    ast = Program_Unit(reader)
    assert "SUBMODULE (foobar) bar\n" "END" in str(ast)


def test_submodule_nomatch(f2008_create):
    """Test an exception is raised if there is a syntax error."""
    reader = get_reader(
        """\
      submod (foobar) bar
      end
      """
    )
    with pytest.raises(NoMatchError) as excinfo:
        dummy_ = Program_Unit(reader)
    assert "at line 1\n>>>      submod (foobar) bar\n" in str(excinfo.value)
