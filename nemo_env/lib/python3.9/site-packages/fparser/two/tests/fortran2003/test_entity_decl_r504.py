# Modified work Copyright (c) 2017-2022 Science and Technology
# Facilities Council.
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

"""Test Fortran 2003 rule R504: entity-decl

"""

from fparser.two.Fortran2003 import Entity_Decl, Name

import pytest


def test_entity_decl_repr():
    tcls = Entity_Decl
    obj = tcls("a(1)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "a(1)"
    assert (
        repr(obj) == "Entity_Decl(Name('a'), Explicit_Shape_Spec_List(',', "
        "(Explicit_Shape_Spec(None, Int_Literal_Constant('1', None)),)), "
        "None, None)"
    )


@pytest.mark.parametrize(
    ("declaration, expected_str"),
    [
        ("a(1)*(3)", "a(1)*(3)"),
        ("a(1)*(3) = 2", "a(1)*(3) = 2"),
        ("a = 2", "a = 2"),
        ("a=2", "a = 2"),
        ('a = "abc "', 'a = "abc "'),
        ("a = .true.", "a = .TRUE."),
    ],
)
def test_entity_decl_str(declaration, expected_str):
    """Test the string representations of various entity declarations"""
    obj = Entity_Decl(declaration)
    assert isinstance(obj, Entity_Decl), repr(obj)
    assert str(obj) == expected_str


def test_entity_decl_name():  # 504
    """Test we can get the name of an entity declaration"""
    obj = Entity_Decl("a(1) = 2")
    assert obj.get_name() == Name("a")
