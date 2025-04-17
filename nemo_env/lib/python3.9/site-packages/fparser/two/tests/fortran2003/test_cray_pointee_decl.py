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

"""Test Fortran 2003 Cray-pointers: This file tests the support for a
Cray-pointee declaration.

"""

import pytest
from fparser.two.Fortran2003 import Cray_Pointee_Decl
from fparser.two.utils import NoMatchError


def test_cray_pointee_decl(f2003_create):
    """Check that Cray-pointee declarations are parsed correctly."""
    for myinput in [
        "a(n)",
        "a(0 : n)",
        "a(n, m)",
        "a(5, *)",
        "a(*)",
        "a(0 : 1, 2 : *)",
    ]:
        ast = Cray_Pointee_Decl(myinput)
        assert myinput in str(ast)


def test_errors(f2003_create):
    """Check that syntax errors produce a NoMatchError exception."""
    for myinput in ["", "  ", "a", "a2)", "a(2", "a()"]:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Cray_Pointee_Decl(myinput)
        assert "Cray_Pointee_Decl: '{0}'".format(myinput) in str(excinfo.value)


def test_unsupported(f2003_create):
    """Check that unsupported assumed shape declarations produce a
    NoMatchError exception.

    """
    for myinput in ["a(:)", "a(2,:)"]:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Cray_Pointee_Decl(myinput)
        assert "Cray_Pointee_Decl: '{0}'".format(myinput) in str(excinfo.value)
