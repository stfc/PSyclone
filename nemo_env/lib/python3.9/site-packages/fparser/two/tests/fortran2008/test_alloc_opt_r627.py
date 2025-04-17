# Copyright (c) 2022 Science and Technology Facilities Council.

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

"""Test Fortran 2008 rule R627

    alloc-opt is ERRMSG = errmsg-variable
              or MOLD = source-expr
              or SOURCE = source-expr
              or STAT = stat-variable

The only difference to 2003 is the addition of 'MOLD' so that's what we
test for.

"""

import pytest
from fparser.two.Fortran2008 import Allocate_Stmt, Alloc_Opt, Alloc_Opt_List


@pytest.mark.usefixtures("f2008_create")
def test_alloc_opt():
    """Test that Alloc_Opt supports the F2008 addition of 'MOLD'."""
    obj = Alloc_Opt("MOLD=c")
    assert isinstance(obj, Alloc_Opt), repr(obj)
    assert str(obj) == "MOLD = c"


@pytest.mark.usefixtures("f2008_create")
def test_allocate_stmt():
    """Check that the Fortran2008 version of allocate has picked up the
    version of Alloc_Opt that supports MOLD."""
    obj = Allocate_Stmt("allocate(b, mold=c)")
    assert obj.alloc_opt_list() == Alloc_Opt_List
    assert isinstance(obj, Allocate_Stmt), repr(obj)
    assert str(obj) == "ALLOCATE(b, MOLD = c)"
    obj = Allocate_Stmt("allocate(b, mold=c, stat=ierr)")
    assert isinstance(obj, Allocate_Stmt), repr(obj)
    assert str(obj) == "ALLOCATE(b, MOLD = c, STAT = ierr)"
