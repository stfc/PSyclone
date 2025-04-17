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

"""Test Fortran 2003 rules R703 and R723 : This file tests the support
for a Defined Unary Operator and a Defined Binary Operator,
respectively. Both rules are included in a single test as, in terms of
fparser implementation, they are identical. Therefore, to avoid code
duplication we use a fixture.

"""

import pytest
from fparser.two.utils import NoMatchError


def test_defined_operator(f2003_create, op_type):
    """Check that basic unary and binary operators are parsed correctly."""
    for line in [".myoperator.", "  .myoperator.  ", "." + 63 * "a" + "."]:
        ast = op_type(line)
        target = line.strip().upper()
        assert target in str(ast)
        assert repr(ast) == "Defined_Op('{0}')".format(target)


def test_syntax_error(f2003_create, op_type):
    """Test that NoMatchError is raised for various syntax errors."""
    for line in ["", "  ", "x", ".x", "x.", "..", ".,.", ". x.", ".x .", ".x x."]:
        with pytest.raises(NoMatchError) as excinfo:
            _ = op_type(line)
        assert "_Op: '{0}'".format(line) in str(excinfo.value)


def test_c703(f2003_create, op_type):
    """Test that we do not match if the name is more than 63 characters
    long, the name matches an existing intrinsic operator (e.g. .gt.),
    or the name matches an existing logical literal constant
    (e.g. .false.).

    """
    for line in ["." + 64 * "a" + ".", ".eq.", ".not.", ".false.", ".FALSE."]:
        with pytest.raises(NoMatchError) as excinfo:
            _ = op_type(line)
        assert "_Op: '{0}'".format(line) in str(excinfo.value)
