# This Python file uses the following encoding: utf-8
# Copyright (c) 2020 Science and Technology Facilities Council.

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

""" pytest module for the Fortran2003 Non-block Do Construct - R835. Note that
    this requires re-implementing and extending to fully cover the
    language specification (#232)."""

import pytest
from fparser.api import get_reader
from fparser.two.Fortran2003 import Action_Term_Do_Construct, Nonblock_Do_Construct


@pytest.mark.usefixtures("f2003_create")
def test_nonblock_do_construct():
    """Tests that nonblock DO construct is parsed correctly (R835)."""
    tcls = Nonblock_Do_Construct
    obj = tcls(
        get_reader(
            """\
      do  20,  i = 1, 3
 20     rotm(i,j) = r2(j,i)
    """
        )
    )
    assert isinstance(obj, Action_Term_Do_Construct), repr(obj)
    assert str(obj) == "DO 20 , i = 1, 3\n20 rotm(i, j) = r2(j, i)"

    # Without the comma after the label
    obj = tcls(
        get_reader(
            """\
      do  20  i = 1, 3
 20     rotm(i,j) = r2(j,i)
    """
        )
    )
    assert isinstance(obj, Action_Term_Do_Construct), repr(obj)
    assert str(obj) == "DO 20 i = 1, 3\n20 rotm(i, j) = r2(j, i)"

    obj = tcls(
        get_reader(
            """\
    do  50,  i = n, m, -1
  50 call foo(a)
    """
        )
    )
    assert isinstance(obj, Action_Term_Do_Construct), repr(obj)
    assert str(obj) == "DO 50 , i = n, m, - 1\n50 CALL foo(a)"


@pytest.mark.usefixtures("f2003_create")
def test_outer_shared_do_construct():
    """Test for parsing of an outer-shared do construct (R839)."""
    tcls = Nonblock_Do_Construct
    obj = tcls(
        get_reader(
            """\
      do  20,  i = 1, 3
      k = 3
      do  20,  j = 1, 3
      l = 3
 20     rotm(i,j) = r2(j,i)
    """
        )
    )
    assert isinstance(obj, Action_Term_Do_Construct), repr(obj)
    assert str(obj) == (
        "DO 20 , i = 1, 3\n  k = 3\n  DO 20 , j = 1, 3\n    l = 3\n"
        "20 rotm(i, j) = r2(j, i)"
    )


@pytest.mark.usefixtures("f2003_create")
def test_nonblock_do_construct_tofortran_non_ascii():
    """Check that the tofortran() method works when the non-block
    do-construct contains a character string with non-ascii characters."""
    from fparser.common.readfortran import FortranStringReader
    from fparser.common.sourceinfo import FortranFormat

    code = "      DO 50\n" " 50   WRITE(*,*) ' for e1=1\xb0'\n"
    reader = FortranStringReader(code)
    # Ensure reader in in 'fixed-format' mode
    reader.set_format(FortranFormat(False, True))
    obj = Nonblock_Do_Construct(reader)
    out_str = str(obj)
    assert "for e1=1" in out_str
