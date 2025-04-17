# This Python file uses the following encoding: utf-8
# Copyright (c) 2022 Science and Technology Facilities Council.
#
# All rights reserved.
#
# Modifications made as part of the fparser project are distributed
# under the following license:
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
#
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

""" pytest module for the Fortran2003 Allocate Statement - R263. """

import pytest
from fparser.two.utils import NoMatchError
from fparser.two.Fortran2003 import Allocate_Stmt, Alloc_Opt, Alloc_Opt_List


@pytest.mark.usefixtures("f2003_create")
def test_allocate_stmt():
    """Tests for the allocate statement: R623."""
    tcls = Allocate_Stmt
    assert tcls.alloc_opt_list() == Alloc_Opt_List

    obj = tcls("allocate(a,b)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "ALLOCATE(a, b)"

    obj = tcls("allocate(real::a)")
    assert str(obj) == "ALLOCATE(REAL::a)"

    obj = tcls("allocate(real(kind=8)::a, stat=b, source=c//d)")
    assert str(obj) == "ALLOCATE(REAL(KIND = 8)::a, STAT = b, SOURCE = c // d)"


@pytest.mark.usefixtures("f2003_create")
def test_allocate_no_match():
    """Tests that the expected NoMatchError is raised if there are problems."""
    tcls = Allocate_Stmt
    # Missing parenthesis.
    with pytest.raises(NoMatchError) as err:
        tcls("allocate(var(3)")
    assert "allocate(var(3)" in str(err.value)
    with pytest.raises(NoMatchError) as err:
        tcls("allocate var(3))")
    assert "allocate var(3))" in str(err.value)
    # Misspelt key word.
    with pytest.raises(NoMatchError) as err:
        tcls("allocte(var(3))")
    assert "allocte(var(3))" in str(err.value)
    # No arguments.
    with pytest.raises(NoMatchError) as err:
        tcls("allocate()")
    assert "allocate()" in str(err.value)
    # Missing positional argument.
    with pytest.raises(NoMatchError) as err:
        tcls("allocate(stat=ierr)")
    assert "allocate(stat=ierr)" in str(err.value)


@pytest.mark.usefixtures("f2003_create")
def test_alloc_opt():
    """Tests for the various forms of alloc-opt: R624."""
    tcls = Alloc_Opt
    obj = tcls("stat=a")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "STAT = a"
    assert repr(obj) == "Alloc_Opt('STAT', Name('a'))"
    obj = tcls("errmsg=my_msg")
    assert str(obj) == "ERRMSG = my_msg"
    obj = tcls("source=b")
    assert str(obj) == "SOURCE = b"
    # Check for a failed match - use 'mold' as that's a Fortran2008 addition
    # so should not match here.
    with pytest.raises(NoMatchError):
        tcls("MOLD=b")
    with pytest.raises(NoMatchError):
        tcls("value")
