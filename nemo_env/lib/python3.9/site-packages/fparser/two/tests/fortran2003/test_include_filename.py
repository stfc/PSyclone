# Copyright (c) 2019 Science and Technology Facilities Council

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

"""Test Fortran Include file name: This file tests the filename in an
include statement. Whilst include is not part of the standard Fortran
rules (a compiler should parse any code from an include as the code is
being parsed) there are cases where users might like to keep the
include statement in the Fortran parse tree and output it again.

The test for a valid file name is very lightweight as it is unclear
what would be invalid. The only things considered invalid are white
space at the beginning and/or end of the filename, or an empty string.

"""

import pytest
from fparser.two.Fortran2003 import Include_Filename
from fparser.two.utils import NoMatchError


def test_valid_include_filename(f2003_create):
    """Try some valid include statement path names and check that the
    output is what we expect.

    """
    for string in [
        "x",
        "/",
        ".",
        "./here.inc",
        "~/home/x.inc",
        "/local/y.inc",
        "/he/llo.world",
        "\\",
        "spaces are ok",
        "c:\\windogs\\include.bat",
    ]:
        ast = Include_Filename(string)
        assert str(ast) == string
        # repr will escape any backslashes that it finds.
        test_string = string.replace("\\", "\\\\")
        assert repr(ast) == "Include_Filename('{0}')".format(test_string)


def test_invalid_include_filename(f2003_create):
    """As the file name is not part of the Fortran standard, we apply very
    loose rules to what is valid. The only things we don't accept are
    an empty string or white space at the start or end of the string.

    """
    for string in ["", " ", " x.inc", "x.inc ", " x.inc "]:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Include_Filename(string)
        assert "Include_Filename: '{0}'".format(string) in str(excinfo.value)
