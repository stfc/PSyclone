# Copyright (c) 2017-2022 Science and Technology Facilities Council

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

"""
Test the various utility functions

"""
import pytest

from fparser.common.utils import split_comma, ParseError


def test_split_comma():
    """Test the split_comma() function"""
    items = split_comma("hello, goodbye")
    print(items)
    assert items[0] == "hello"
    assert items[1] == "goodbye"
    # With trailing and leading white space
    items = split_comma("  hello, goodbye   ")
    print(items)
    assert items[0] == "hello"
    assert items[1] == "goodbye"
    items = split_comma("  ")
    assert not items


def test_split_comma_exceptions():
    """Test that we raise the expected exceptions if we don't supply
    the brackets in the right form"""
    with pytest.raises(ParseError) as excinfo:
        _ = split_comma("one, two", brackets="()")
    assert "brackets must be a tuple" in str(excinfo.value)
    with pytest.raises(ParseError) as excinfo:
        _ = split_comma("one, two", brackets=("()",))
    assert "brackets tuple must contain just two items" in str(excinfo.value)
    with pytest.raises(ParseError) as excinfo:
        _ = split_comma("one, two", brackets=("(", "(", "("))
    assert "brackets tuple must contain just two items" in str(excinfo.value)


def test_split_bracketed_list():
    """Test the splitting of a list bracketed with parentheses"""
    items = split_comma("(well(1), this(is), it)", brackets=("(", ")"))
    print(items)
    assert items[0] == "well(1)"
    assert items[1] == "this(is)"
    # With superfluous white space
    items = split_comma("  (  well(1), this(is), it  )  ", brackets=("(", ")"))
    print(items)
    assert items[0] == "well(1)"
    assert items[1] == "this(is)"
    assert items[2] == "it"
    # Square brackets
    items = split_comma("[well(1), this(is), it]", brackets=("[", "]"))
    print(items)
    assert items[0] == "well(1)"
    assert items[1] == "this(is)"
    assert items[2] == "it"
    # Mis-matched brackets
    items = split_comma("[well(1), this(is), it)", brackets=("[", "]"))
    assert not items


def test_extract_bracketed_list():
    """Test the extraction and parsing of a list within parentheses within
    a larger string"""
    from fparser.common.utils import extract_bracketed_list_items

    items = extract_bracketed_list_items("hello (this, is, a) test")
    assert items[0] == "this"
    assert items[1] == "is"
    assert items[2] == "a"


def test_extract_bracketed_list_err():
    """Test that we get the expected errors if the string passed into
    extract_bracketed_list_items() does not have the correct format"""
    from fparser.common.utils import extract_bracketed_list_items

    with pytest.raises(ParseError) as excinfo:
        _ = extract_bracketed_list_items("hello (this, is, wrong(")
    assert "more than one opening/closing parenthesis found" in str(excinfo.value)
    with pytest.raises(ParseError) as excinfo:
        _ = extract_bracketed_list_items("hello )this, is, wrong)")
    assert "more than one opening/closing parenthesis found" in str(excinfo.value)
    with pytest.raises(ParseError) as excinfo:
        _ = extract_bracketed_list_items("hello (this, is, wrong) (too)")
    assert "more than one opening/closing parenthesis found" in str(excinfo.value)
    with pytest.raises(ParseError) as excinfo:
        _ = extract_bracketed_list_items("hello )this, is, wrong( too")
    assert "failed to find expression within parentheses in" in str(excinfo.value)
