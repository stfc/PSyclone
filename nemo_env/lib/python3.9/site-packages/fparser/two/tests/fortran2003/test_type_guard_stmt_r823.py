# Copyright (c) 2023 Science and Technology Facilities Council

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

"""Test Fortran 2003 rule R823

        type-guard-stmt is TYPE IS ( type-spec ) [ select-construct-name ]
                        or CLASS IS ( type-spec ) [ select-construct-name ]
                        or CLASS DEFAULT [ select-construct-name ]

"""
import pytest

from fparser.two.Fortran2003 import Type_Guard_Stmt
from fparser.two.utils import NoMatchError


@pytest.mark.usefixtures("f2003_create")
def test_type_is():
    """Test that the match works as expected when trying to match 'type
    is'

    """
    # Space before 'type is'
    tcls = Type_Guard_Stmt
    obj = tcls("  type is (real)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "TYPE IS (REAL)"

    # Mixed case keyword
    obj = tcls("TyPe Is (mytype)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "TYPE IS (mytype)"

    # Invalid 'type' keyword
    with pytest.raises(NoMatchError) as info:
        _ = tcls("TYP Is (mytype)")
    assert "Type_Guard_Stmt: 'TYP Is (mytype)'" in str(info.value)

    # Invalid 'is' keyword
    with pytest.raises(NoMatchError) as info:
        _ = tcls("TYPE IZ (mytype)")
    assert "Type_Guard_Stmt: 'TYPE IZ (mytype)'" in str(info.value)


@pytest.mark.usefixtures("f2003_create")
def test_class_is():
    """Test that the match works as expected when trying to match 'class
    is'

    """
    # Space before 'class is'
    tcls = Type_Guard_Stmt
    obj = tcls("  class is (real)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "CLASS IS (REAL)"

    # Mixed case keyword
    obj = tcls("ClAsS Is (mytype)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "CLASS IS (mytype)"

    # Invalid 'class' keyword
    with pytest.raises(NoMatchError) as info:
        _ = tcls("CLAS IS (mytype)")
    assert "Type_Guard_Stmt: 'CLAS IS (mytype)'" in str(info.value)

    # Invalid 'is' keyword
    with pytest.raises(NoMatchError) as info:
        _ = tcls("CLASS IZ (mytype)")
    assert "Type_Guard_Stmt: 'CLASS IZ (mytype)'" in str(info.value)


@pytest.mark.usefixtures("f2003_create")
def test_default():
    """Test that the match works as expected when trying to match 'class
    default'

    """
    # Space before 'class default'
    tcls = Type_Guard_Stmt
    obj = tcls("  class default")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "CLASS DEFAULT"

    # Mixed case keyword
    obj = tcls("ClAsS DeFaUlT")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "CLASS DEFAULT"

    # Invalid 'class' keyword
    with pytest.raises(NoMatchError) as info:
        _ = tcls("CLAS DEFAULT")
    assert "Type_Guard_Stmt: 'CLAS DEFAULT'" in str(info.value)

    # Invalid 'default' keyword
    with pytest.raises(NoMatchError) as info:
        _ = tcls("CLASS DERFAULT")
    assert "Type_Guard_Stmt: 'CLASS DERFAULT'" in str(info.value)

    # Invalid optional name after CLASS DEFAULT
    with pytest.raises(NoMatchError) as info:
        _ = tcls("CLASS DEFAULT (mytype)")
    assert "Type_Guard_Stmt: 'CLASS DEFAULT (mytype)'" in str(info.value)

    # Valid name after CLASS DEFAULT
    obj = tcls("class default myname")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "CLASS DEFAULT myname"


@pytest.mark.usefixtures("f2003_create")
def test_invalid_keyword():
    """Test that the match fails if the initial matching keyword is not
    CLASS or TYPE.

    """
    tcls = Type_Guard_Stmt
    with pytest.raises(NoMatchError) as info:
        _ = tcls("invalid")
    assert "Type_Guard_Stmt: 'invalid'" in str(info.value)


@pytest.mark.usefixtures("f2003_create")
def test_clause():
    """Test matching works as expected when trying to match 'class is
    (mytype)'. Also covers case with 'type is' so no need for
    additional tests.

    """
    # invalid ( location
    tcls = Type_Guard_Stmt
    with pytest.raises(NoMatchError) as info:
        _ = tcls(" class is m(ytype)")
    assert "Type_Guard_Stmt: ' class is m(ytype)'" in str(info.value)

    # non-existant )
    with pytest.raises(NoMatchError) as info:
        _ = tcls(" class is (mytype")
    assert "Type_Guard_Stmt: ' class is (mytype'" in str(info.value)

    # no content in ()
    with pytest.raises(NoMatchError) as info:
        _ = tcls(" class is ()")
    assert "Type_Guard_Stmt: ' class is ()'" in str(info.value)

    # valid string
    obj = tcls(" class is (mytype)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "CLASS IS (mytype)"

    # valid string with optional name
    obj = tcls(" class is (mytype) name")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "CLASS IS (mytype) name"


@pytest.mark.usefixtures("f2003_create")
def test_tostr():
    """Test the tostr() method works as expected"""

    tcls = Type_Guard_Stmt
    obj = tcls("class default")
    assert obj.tostr() == "CLASS DEFAULT"

    obj = tcls("class default name")
    assert obj.tostr() == "CLASS DEFAULT name"

    obj = tcls("class is (real)")
    assert obj.tostr() == "CLASS IS (REAL)"

    obj = tcls("class is (real) name")
    assert obj.tostr() == "CLASS IS (REAL) name"

    obj = tcls("type is (real)")
    assert obj.tostr() == "TYPE IS (REAL)"

    obj = tcls("type is (real) name")
    assert obj.tostr() == "TYPE IS (REAL) name"


@pytest.mark.usefixtures("f2003_create")
def test_get_end_name():
    """Test the get_end_name method works as expected."""

    tcls = Type_Guard_Stmt
    obj = tcls("type is (real)")
    assert obj.get_end_name() is None

    obj = tcls("type is (real) name")
    assert obj.get_end_name() == "name"
