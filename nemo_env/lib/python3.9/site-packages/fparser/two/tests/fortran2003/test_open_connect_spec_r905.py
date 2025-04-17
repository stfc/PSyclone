# Modified work Copyright (c) 2017-2024 Science and Technology
# Facilities Council.
# Original work Copyright (c) 1999-2008 Pearu Peterson
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

# --------------------------------------------------------------------

# The original software (in the f2py project) was distributed under
# the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:

#   a. Redistributions of source code must retain the above copyright notice,
#      this list of conditions and the following disclaimer.
#   b. Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#   c. Neither the name of the F2PY project nor the names of its
#      contributors may be used to endorse or promote products derived from
#      this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.

"""
Module containing py.test tests for the connect-spec of a Fortran2003 OPEN
statement (R905).

"""

import pytest

from fparser.two import Fortran2003, utils

# from fparser.two.Fortran2003 import Open_Stmt


@pytest.mark.parametrize(
    "keyword",
    [
        "ACCESS",
        "ACTION",
        "ASYNCHRONOUS",
        "BLANK",
        "CONVERT",
        "DECIMAL",
        "DELIM",
        "ENCODING",
        "FORM",
        "PAD",
        "POSITION",
        "ROUND",
        "SIGN",
        "STATUS",
    ],
)
def test_connect_spec_char_keyword_args(keyword):
    """Test that fparser2 supports the various keywords specified in R905
    for the connect-spec that expect a scalar, character value. Note
    that CONVERT is not actually part of the standard but is supported
    by at least Gnu, Intel and Cray.

    """
    tcls = Fortran2003.Connect_Spec
    obj = tcls(f"{keyword}='some-arg'")
    assert isinstance(obj, tcls)
    assert str(obj) == f"{keyword.upper()} = 'some-arg'"


def test_connect_spec_non_char_expr():
    """Test the support for all keywords which expect something other than
    a char expression."""
    tcls = Fortran2003.Connect_Spec
    obj = tcls("ERR = 905")
    assert isinstance(obj.items[1], Fortran2003.Label)
    obj = tcls("FILE = 'a_file.txt'")
    assert isinstance(obj.items[1], Fortran2003.Char_Literal_Constant)
    obj = tcls("IOSTAT=ierr")
    assert isinstance(obj.items[1], Fortran2003.Name)
    obj = tcls("IOMSG=msg")
    assert isinstance(obj.items[1], Fortran2003.Name)
    obj = tcls("RECL=28")
    assert isinstance(obj.items[1], Fortran2003.Int_Literal_Constant)
    obj = tcls("UNIT=23")
    assert isinstance(obj.items[1], Fortran2003.Int_Literal_Constant)
    # Wrong type of argument.
    with pytest.raises(utils.NoMatchError) as err:
        _ = tcls("UNIT='Newtons'")
    assert "Connect_Spec: 'UNIT" in str(err)


def test_convert_support_disabled(monkeypatch):
    """Test that the support for the CONVERT keyword may be disabled."""
    monkeypatch.setattr(utils, "_EXTENSIONS", [])
    with pytest.raises(utils.NoMatchError) as err:
        _ = Fortran2003.Connect_Spec("CONVERT='big-endian'")
    assert "Connect_Spec: 'CONVERT=" in str(err)
