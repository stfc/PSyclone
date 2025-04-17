# Modified work Copyright (c) 2017-2021 Science and Technology
# Facilities Council.
# Original work Copyright (c) 1999-2008 Pearu Peterson

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

# Original author: Pearu Peterson <pearu@cens.ioc.ee>
# First version created: May 2006
# Modified by J. Henrichs, Bureau of Meteorology <joerg.henrichs@bom.gov.au>

"""
Test parsing single Fortran lines.

"""

import pytest

from fparser.common.splitline import (
    splitparen,
    splitquote,
    string_replace_map,
    StringReplaceDict,
)


def test_splitparen():
    """Unit tests for splitparen function."""
    assert splitparen("abc") == ["abc"]
    assert splitparen("abc(1)") == ["abc", "(1)"]
    assert splitparen("abc(1) xyz") == ["abc", "(1)", " xyz"]
    assert splitparen("a(b) = b(x,y(1)) b((a))") == [
        "a",
        "(b)",
        " = b",
        "(x,y(1))",
        " b",
        "((a))",
    ]
    # pylint: disable=anomalous-backslash-in-string
    assert splitparen(r"a(b) = b(x,y(1)) b\((a)\)") == [
        "a",
        "(b)",
        " = b",
        "(x,y(1))",
        " b\\(",
        "(a)",
        "\\)",
    ]
    # pylint: enable=anomalous-backslash-in-string
    assert splitparen("abc[1]") == ["abc", "[1]"]
    assert splitparen("abc[1,2,3]") == ["abc", "[1,2,3]"]
    assert splitparen("a[b] = b[x,y(1)] b((a))") == [
        "a",
        "[b]",
        " = b",
        "[x,y(1)]",
        " b",
        "((a))",
    ]
    # pylint: disable=anomalous-backslash-in-string
    assert splitparen(r"a[b] = b[x,y(1)] b\((a)\)") == [
        "a",
        "[b]",
        " = b",
        "[x,y(1)]",
        " b\\(",
        "(a)",
        "\\)",
    ]
    # pylint: enable=anomalous-backslash-in-string
    assert splitparen('integer a(3) = (/"a", "b", "c"/)') == [
        "integer a",
        "(3)",
        " = ",
        '(/"a", "b", "c"/)',
    ]
    assert splitparen('character(len=40) :: a(3) = (/"a[),", ",b,[(", "c,][)("/)') == [
        "character",
        "(len=40)",
        " :: a",
        "(3)",
        " = ",
        '(/"a[),", ",b,[(", "c,][)("/)',
    ]
    assert splitparen('integer a(3) = ["a", "b", "c"]') == [
        "integer a",
        "(3)",
        " = ",
        '["a", "b", "c"]',
    ]
    assert splitparen('character(len=40) :: a(3) = ["a[),", ",b,[(", "c,][)("]') == [
        "character",
        "(len=40)",
        " :: a",
        "(3)",
        " = ",
        '["a[),", ",b,[(", "c,][)("]',
    ]
    # pylint: disable=anomalous-backslash-in-string
    result = splitparen('a(1),b\\((2,3),c\\\\((1)),c"("')
    expected = ["a", "(1)", ",b\\(", "(2,3)", ",c\\\\", "((1))", ',c"("']
    # pylint: enable=anomalous-backslash-in-string
    assert result == expected
    # Useful for debugging:
    # for i in range(len(EXPECTED)):
    #     print i,l[i],EXPECTED[i],l[i]==EXPECTED[i]


def test_splitquote():
    """Tests splitquote function."""
    split_list, stopchar = splitquote('abc\\\' def"12\\"3""56"dfad\'a d\'')
    assert split_list == ["abc\\' def", '"12\\"3"', '"56"', "dfad", "'a d'"]
    assert stopchar is None
    result, stopchar = splitquote('abc\\\' def"12\\"3""56"dfad\'a d\'')
    assert result == ["abc\\' def", '"12\\"3"', '"56"', "dfad", "'a d'"]
    assert stopchar is None

    split_list, stopchar = splitquote("a'")
    assert split_list == ["a", "'"]
    assert stopchar == "'"

    split_list, stopchar = splitquote("a'b")
    assert split_list == ["a", "'b"]
    assert stopchar == "'"


@pytest.mark.parametrize(
    "test_str, result, result_map",
    [
        ("a()", "a()", {}),
        ("a(b + c)", "a(F2PY_EXPR_TUPLE_1)", {"F2PY_EXPR_TUPLE_1": "b + c"}),
        ("0.5d0*a", "F2PY_REAL_CONSTANT_1_*a", {"F2PY_REAL_CONSTANT_1_": "0.5d0"}),
        (".5d0*a", "F2PY_REAL_CONSTANT_1_*a", {"F2PY_REAL_CONSTANT_1_": ".5d0"}),
        (
            "a + 1.0e-10*c",
            "a + F2PY_REAL_CONSTANT_1_*c",
            {"F2PY_REAL_CONSTANT_1_": "1.0e-10"},
        ),
        (
            "a + 1.0e-10*c + 1.0e-10*d",
            "a + F2PY_REAL_CONSTANT_1_*c + F2PY_REAL_CONSTANT_1_*d",
            {"F2PY_REAL_CONSTANT_1_": "1.0e-10"},
        ),
        (
            "a + 1.0E-10*c + 1.0e-11*d",
            "a + F2PY_REAL_CONSTANT_1_*c + " "F2PY_REAL_CONSTANT_2_*d",
            {"F2PY_REAL_CONSTANT_1_": "1.0E-10", "F2PY_REAL_CONSTANT_2_": "1.0e-11"},
        ),
        ("a1e-3*1e3", "a1e-3*F2PY_REAL_CONSTANT_1_", {"F2PY_REAL_CONSTANT_1_": "1e3"}),
        (
            "3.0 - .32D+3",
            "3.0 - F2PY_REAL_CONSTANT_1_",
            {"F2PY_REAL_CONSTANT_1_": ".32D+3"},
        ),
        (
            "var=1.0d-3",
            "var=F2PY_REAL_CONSTANT_1_",
            {"F2PY_REAL_CONSTANT_1_": "1.0d-3"},
        ),
        (".5e3_wp*a", "F2PY_REAL_CONSTANT_1_*a", {"F2PY_REAL_CONSTANT_1_": ".5e3_wp"}),
        (
            "5.e+3_wp*a",
            "F2PY_REAL_CONSTANT_1_*a",
            {"F2PY_REAL_CONSTANT_1_": "5.e+3_wp"},
        ),
        (
            "IF(ABS( zds) <= 1.e-20_wp)   zds = 1.e-20_wp",
            "IF(F2PY_EXPR_TUPLE_1)   zds = F2PY_REAL_CONSTANT_1_",
            {
                "F2PY_EXPR_TUPLE_1": "ABS( zds) <= 1.e-20_wp",
                "F2PY_REAL_CONSTANT_1_": "1.e-20_wp",
            },
        ),
        (
            "1.e-1+2.e-1+3.e-1+4.e-1+5.e-1+6.e-1+7.e-1+8.e-1+9.e-1+1.1e-1+1.2e-2",
            "F2PY_REAL_CONSTANT_1_+F2PY_REAL_CONSTANT_2_+F2PY_REAL_CONSTANT_3_+"
            "F2PY_REAL_CONSTANT_4_+F2PY_REAL_CONSTANT_5_+F2PY_REAL_CONSTANT_6_+"
            "F2PY_REAL_CONSTANT_7_+F2PY_REAL_CONSTANT_8_+F2PY_REAL_CONSTANT_9_+"
            "F2PY_REAL_CONSTANT_10_+F2PY_REAL_CONSTANT_11_",
            {
                "F2PY_REAL_CONSTANT_1_": "1.e-1",
                "F2PY_REAL_CONSTANT_2_": "2.e-1",
                "F2PY_REAL_CONSTANT_3_": "3.e-1",
                "F2PY_REAL_CONSTANT_4_": "4.e-1",
                "F2PY_REAL_CONSTANT_5_": "5.e-1",
                "F2PY_REAL_CONSTANT_6_": "6.e-1",
                "F2PY_REAL_CONSTANT_7_": "7.e-1",
                "F2PY_REAL_CONSTANT_8_": "8.e-1",
                "F2PY_REAL_CONSTANT_9_": "9.e-1",
                "F2PY_REAL_CONSTANT_10_": "1.1e-1",
                "F2PY_REAL_CONSTANT_11_": "1.2e-2",
            },
        ),
        (
            "'value = 1.0d-3'",
            "'_F2PY_STRING_CONSTANT_1_'",
            {"_F2PY_STRING_CONSTANT_1_": "value = 1.0d-3"},
        ),
    ],
)
def test_string_replace_map(test_str, result, result_map):
    """Tests string_replace_map function for various expressions."""
    string, string_map = string_replace_map(test_str)
    assert string == result
    assert string_map == result_map
    assert string_map(string) == test_str


def test_string_replace_dict():
    """Tests for the StringReplaceDict class."""
    repmap = StringReplaceDict()
    assert repmap == {}
    repmap["F2PY_REAL_CONSTANT_1_"] = "a_value"
    new_line = repmap("some text with F2PY_REAL_CONSTANT_1_")
    assert new_line == "some text with a_value"
    repmap["F2PY_EXPR_TUPLE_1"] = "3 + 5"
    new_line = repmap("some text with F2PY_EXPR_TUPLE_1")
    assert new_line == "some text with 3 + 5"
    repmap["_F2PY_STRING_CONSTANT_1_"] = "blah = 0.01e+4"
    new_line = repmap("some text with _F2PY_STRING_CONSTANT_1_")
    assert new_line == "some text with blah = 0.01e+4"
    repmap["F2PY_EXPR_TUPLE_11"] = "0.5d0*val"
    new_line = repmap("text with F2PY_EXPR_TUPLE_11 and F2PY_EXPR_TUPLE_1")
    assert new_line == "text with 0.5d0*val and 3 + 5"
