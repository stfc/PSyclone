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

"""Test Fortran 2003 rule R1111 : This file tests the support for the
Rename class.

"""

from fparser.two.Fortran2003 import Rename


def test_rename():
    """
    Check basic functionality of the Rename class.
    """
    tcls = Rename
    obj = tcls("a=>b")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a => b"
    assert obj.children[0] is None

    obj = tcls("operator(.foo.)=>operator(.bar.)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "OPERATOR(.FOO.) => OPERATOR(.BAR.)"
    assert obj.children[0] == "OPERATOR"

    obj = tcls("operator_1=>operator_2")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "operator_1 => operator_2"
    assert obj.children[0] is None


def test_rename_nomatch():
    """
    Check that various syntax errors result in no match.
    """
    assert Rename.match("operator(.foo.) = operator(.bar.)") is None
    assert Rename.match("operator(.foo.) => ") is None
    assert Rename.match("operator(.foo.) => operator_1") is None
    assert Rename.match("operator() => operator(.bar.)") is None
