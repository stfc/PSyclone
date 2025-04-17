# Copyright (c) 2018-2020 Science and Technology Facilities Council

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

""" File containing unit tests for the WORDClsBase baseclass in utils.py """

import pytest
from fparser.two.utils import WORDClsBase, NoMatchError
from fparser.two.Fortran2003 import Name

# test WORDClsBase

# TODO: match: multiple matches (list of values)
# TODO: match: code when the input is not a string or list of values
# TODO: test tostr and tostr_a


def test_wordclsbase():
    """Test the wordclsbase match method with no optional arguments."""

    token = "TOKEN"
    name = "name"

    # token only, empty text no match
    text = ""
    result = WORDClsBase.match(token, Name, text)
    assert not result

    # token only, name too short no match
    text = "TOKE"
    result = WORDClsBase.match(token, Name, text)
    assert not result

    # token only, wrong name no match
    text = "wrong"
    result = WORDClsBase.match(token, Name, text)
    assert not result

    # token only, match
    text = token
    result = WORDClsBase.match(token, Name, text)
    assert str(result) == "('{0}', None)".format(token)

    # token only, mixed case match
    text = "ToKeN"
    result = WORDClsBase.match(token, Name, text)
    assert str(result) == "('{0}', None)".format(token)

    # token only, spaces match
    text = "  {0}  ".format(token)
    result = WORDClsBase.match(token, Name, text)
    assert str(result) == "('{0}', None)".format(token)

    # token and name no space no match
    text = "{0}{1}".format(token, name)
    result = WORDClsBase.match(token, Name, text)
    assert not result

    # token and name match
    text = "{0} {1}".format(token, name)
    result = WORDClsBase.match(token, Name, text)
    assert str(result) == "('{0}', Name('{1}'))".format(token, name)

    # token, :: and name no spaces no match
    text = "{0}::{1}".format(token, name)
    with pytest.raises(NoMatchError) as excinfo:
        result = WORDClsBase.match(token, Name, text)
    assert "Name: '::name'" in str(excinfo.value)

    # token, :: and name with spaces no match
    text = "{0} :: {1}".format(token, name)
    with pytest.raises(NoMatchError) as excinfo:
        result = WORDClsBase.match(token, Name, text)
    assert "Name: ':: name'" in str(excinfo.value)


@pytest.mark.xfail(reason="multiple names are treated as one and concatenated")
def test_wordclsbase_multi_names():
    """Create a separate test as these examples fail as the Name class
    happily strips out spaces. I need to work out whether this is an
    issue for this class or for the Name class itself.

    Test with 'require_cls' is 'False' (the default) which means that
    content for the class ('Name' in this case) is optional and test
    with 'require_cls' is 'True' which means that valid content should
    exist for the class.

    """

    token = "TOKEN"
    name = "name"

    # token and names no match
    text = "{0} {1} {1}".format(token, name)
    result = WORDClsBase.match(token, Name, text)
    assert not result

    # token and names no match
    text = "{0} {1} {1}".format(token, name)
    result = WORDClsBase.match(token, Name, text, require_cls=True)
    assert not result


def test_wordclsbase_optional_colons():
    """Test the wordclsbase match method with colons as an optional
    argument. No need to check in combination with the require_cls
    optional argument as the code is independent.

    """
    token = "TOKEN"
    name = "name"

    # token only - match
    text = token
    result = WORDClsBase.match(token, Name, text, colons=True)
    assert str(result) == "('{0}', None)".format(token)

    # token then name - match
    text = "{0} {1}".format(token, name)
    result = WORDClsBase.match(token, Name, text, colons=True)
    assert str(result) == "('{0}', Name('{1}'))".format(token, name)

    # token then :: (no spaces) with no following name - no match
    text = "{0}::".format(token)
    result = WORDClsBase.match(token, Name, text, colons=True)
    assert not result

    # token then :: with no following name - no match
    text = "{0} :: ".format(token)
    result = WORDClsBase.match(token, Name, text, colons=True)
    assert not result

    # token, :: then name (no spaces) - match
    text = "{0}::{1}".format(token, name)
    result = WORDClsBase.match(token, Name, text, colons=True)
    assert str(result) == "('{0}', Name('{1}'))".format(token, name)

    # token, :: then name - match
    text = "{0} :: {1}".format(token, name)
    result = WORDClsBase.match(token, Name, text, colons=True)
    assert str(result) == "('{0}', Name('{1}'))".format(token, name)


def test_wordclsbase_require_cls():
    """Test the wordclsbase match method with require_cls as an optional
    argument.

    """
    token = "TOKEN"
    name = "name"

    # token only - no match
    text = token
    result = WORDClsBase.match(token, Name, text, require_cls=True)
    assert not result

    # token then :: (no spaces) with no following name - no match
    text = "{0}::".format(token)
    result = WORDClsBase.match(token, Name, text, colons=True, require_cls=True)
    assert not result

    # token then :: with no following name - no match
    text = "{0} :: ".format(token)
    result = WORDClsBase.match(token, Name, text, colons=True, require_cls=True)
    assert not result

    # token then name - match
    text = "{0} {1}".format(token, name)
    result = WORDClsBase.match(token, Name, text, colons=True, require_cls=True)
    assert str(result) == "('{0}', Name('{1}'))".format(token, name)

    # token, :: then name (no spaces) - match
    text = "{0}::{1}".format(token, name)
    result = WORDClsBase.match(token, Name, text, colons=True, require_cls=True)
    assert str(result) == "('{0}', Name('{1}'))".format(token, name)

    # token, :: then name - match
    text = "{0} :: {1}".format(token, name)
    result = WORDClsBase.match(token, Name, text, colons=True, require_cls=True)
    assert str(result) == "('{0}', Name('{1}'))".format(token, name)
