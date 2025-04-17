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

"""Test Fortran 2003 rule R312 : This file tests the support for an
extended intrinsic operator.

"""

import pytest
from fparser.two.Fortran2003 import Extended_Intrinsic_Op
from fparser.two.utils import NoMatchError


def test_extended_intrinsic_op(f2003_create):
    """Check that correct extended intrinsic op input is parsed. No need
    to test them all as they will be tested in the pattern_tools tests
    but we do it anyway.

    """
    for myinput in [
        "**",
        "*",
        "/",
        "+",
        "-",
        "//",
        ".eq.",
        ".NE.",
        ".lt.",
        ".LE.",
        ".gt.",
        ".GE.",
        "==",
        "/=",
        "<",
        "<=",
        ">",
        ">=",
        ".not.",
        ".AND.",
        ".or.",
        ".eQv.",
        ".NeqV.",
    ]:
        ast = Extended_Intrinsic_Op(myinput)
        assert myinput in str(ast)
        assert repr(ast) == "Extended_Intrinsic_Op('{0}')".format(myinput)


def test_parse_errors(f2003_create):
    """The full set of errors will be checked in the pattern_tools
    tests. We just check a few here to make sure the correct
    NoMatchError exception is raised at this level.

    """
    for myinput in ["", "  ", "***", "///", "eq.", ".eq", "eq", "= ="]:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Extended_Intrinsic_Op(myinput)
        assert "Extended_Intrinsic_Op: '{0}'".format(myinput) in str(excinfo.value)
