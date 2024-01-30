# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Authors R. W. Ford, A. R. Porter and N. Nobre, STFC Daresbury Lab

'''A module to perform pytest unit tests on the parse/utils.py
file.

'''
from __future__ import absolute_import
import tempfile

import pytest

from psyclone.parse.utils import check_line_length, parse_fp2, ParseError
from psyclone.errors import InternalError

# function check_line_length() tests


def test_no_file():
    '''Check that an exception is raised as expected when the file
    supplied to the check_line_length function does not exist.

    '''
    with pytest.raises(InternalError) as excinfo:
        check_line_length("file_does_not_exist")
    assert ("In utils.py:check_line_length: [Errno 2] No such file or "
            "directory: 'file_does_not_exist'") in str(excinfo.value)


def test_line_length_too_long():
    '''Check that a file containing a long comment
    raises a ParseError.

    '''
    with tempfile.NamedTemporaryFile(mode='w') as tmp_file:
        tmp_file.write(f'''
            ! A fortran line that is too long... {'a' * 100}
        ''')
        tmp_file.flush()
        with pytest.raises(ParseError) as excinfo:
            check_line_length(tmp_file.name)
    expected = 'file does not conform to the specified 132 line length limit'
    assert expected in str(excinfo.value)


def test_line_length_unicode():
    '''Check that a file containing unicode character comments
    parses correctly.

    Note: This test failed with Python >3,<3.7 before explicit codecs
          were defined in the open(filename, ...) call.
    '''
    kwargs = dict(encoding='utf8')
    with tempfile.NamedTemporaryFile(mode='w', **kwargs) as tmp_file:
        content = '''
            ! A fortran comment with a unicode character "\u2014"
        '''
        tmp_file.write(content)
        tmp_file.flush()

        assert check_line_length(tmp_file.name) is None

# function parse_fp2() tests


def test_parsefp2_invalid_file(tmpdir):
    '''Test that if there is an error finding the input file specified in
    filename argument then an exception is raised in the expected way.

    '''
    with pytest.raises(ParseError) as excinfo:
        _ = parse_fp2(str(tmpdir.join("does_not_exist.f90")))
    assert "Failed to parse file" in str(excinfo.value)
    assert "Error returned was ' [Errno 2] No such file or directory" \
        in str(excinfo.value)


def test_parsefp2_invalid_fortran(tmpdir):
    '''Test that if the Fortran contained in the file specified in the
    filename argument then an exception is raised in the expected
    way. *** Create the parse_tree in-place rather than running
    PSyclone. Once created make the parse_tree content invalid using
    monkeypatch.

    '''
    my_file = str(tmpdir.join("invalid.f90"))
    with open(my_file, "w", encoding="utf-8") as ffile:
        ffile.write("invalid Fortran code")
        ffile.close()
    with pytest.raises(ParseError) as excinfo:
        _ = parse_fp2(my_file)
    assert "Syntax error in file" in str(excinfo.value)
