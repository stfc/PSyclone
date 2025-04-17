# Copyright (c) 2022 Science and Technology Facilities Council

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

"""Test Fortran 2003 rule R1238 : This file tests support for the
Stmt_Function_Stmt class.

"""
import pytest

from fparser.two.Fortran2003 import Stmt_Function_Stmt
from fparser.two.utils import NoMatchError


def test_stmt_function_stmt():
    """Test that the stmt_function_stmt class can be created, raises the
    expected exception and outputs the expected string
    representation.

    """
    tcls = Stmt_Function_Stmt

    # no args
    obj = tcls("a()=b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a () = b"
    assert obj.tostr() == "a () = b"

    # args
    obj = tcls("a(x)=b")
    assert obj.tostr() == "a (x) = b"

    with pytest.raises(NoMatchError) as info:
        obj = tcls("hello")
    assert "Stmt_Function_Stmt: 'hello'" in str(info.value)
    with pytest.raises(NoMatchError) as info:
        obj = tcls("a()=")
    assert "Stmt_Function_Stmt: 'a()='" in str(info.value)
    with pytest.raises(NoMatchError) as info:
        obj = tcls("=a")
    assert "Stmt_Function_Stmt: '=a'" in str(info.value)
    with pytest.raises(NoMatchError) as info:
        obj = tcls("a)=b")
    assert "Stmt_Function_Stmt: 'a)=b'" in str(info.value)
    with pytest.raises(NoMatchError) as info:
        obj = tcls("()=b")
    assert "Stmt_Function_Stmt: '()=b'" in str(info.value)
