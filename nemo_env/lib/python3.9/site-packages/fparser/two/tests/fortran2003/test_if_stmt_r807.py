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

"""Test Fortran 2003 rule R807

    if-stmt is IF ( scalar-logical-expr ) action-stmt
"""

import pytest
from fparser.two.Fortran2003 import Stop_Stmt, If_Stmt, Assignment_Stmt
from fparser.two.utils import walk, NoMatchError


@pytest.mark.usefixtures("f2003_create")
def test_simple_stop():
    """Test matching of a valid string."""
    result = If_Stmt("IF (5 > 3) STOP")
    assert isinstance(result, If_Stmt)
    assert walk(result, Stop_Stmt)


@pytest.mark.usefixtures("f2003_create")
def test_simple_assignment():
    """Test matching of a valid string."""
    result = If_Stmt("IF (A < B) A = B")
    assert isinstance(result, If_Stmt)
    assignments = walk(result, Assignment_Stmt)
    assert len(assignments) == 1
    assert str(assignments[0]) == "A = B"


@pytest.mark.usefixtures("f2003_create")
def test_error1():
    """Test invalid syntax doesn't match."""
    with pytest.raises(NoMatchError) as excinfo:
        _ = If_Stmt("IFF (5 > 3) STOP")
    assert "If_Stmt: 'IFF (5 > 3) STOP'" in str(excinfo.value)


@pytest.mark.usefixtures("f2003_create")
def test_error2():
    """Test invalid syntax doesn't match."""
    with pytest.raises(NoMatchError) as excinfo:
        _ = If_Stmt("IF 5 > 3 STOP")
    assert "If_Stmt: 'IF 5 > 3 STOP'" in str(excinfo.value)


@pytest.mark.usefixtures("f2003_create")
def test_error3():
    """Test invalid syntax doesn't match."""
    with pytest.raises(NoMatchError) as excinfo:
        _ = If_Stmt("IF (5 > 3] STOP")
    assert "If_Stmt: 'IF (5 > 3] STOP'" in str(excinfo.value)


@pytest.mark.usefixtures("f2003_create")
def test_error4():
    """Test invalid syntax doesn't match."""
    with pytest.raises(NoMatchError) as excinfo:
        _ = If_Stmt("IF (A = B) A = 0")
    assert "If_Stmt: 'IF (A = B) A = 0'" in str(excinfo.value)
