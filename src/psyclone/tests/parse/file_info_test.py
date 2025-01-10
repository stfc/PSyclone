# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024-2025, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Laboratory.


'''Module containing tests for the FileInfo class.'''

import os
import pytest

from psyclone.parse import FileInfo


def test_file_info_constructor():
    '''
    Check that a FileInfo can be constructed and that its initial
    properties are set correctly.

    '''
    finfo = FileInfo("missing.txt")
    assert finfo._filename == "missing.txt"
    assert finfo._source_code is None
    assert finfo.filename == "missing.txt"
    assert finfo.basename == "missing"


def test_file_info_missing_file():
    '''
    Test FileInfo.source() raises the expected exception if the
    file cannot be found.

    '''
    finfo = FileInfo("missing.txt")
    with pytest.raises(FileNotFoundError) as err:
        _ = finfo.contents
    assert "'missing.txt'" in str(err.value)


def test_file_info_content(tmpdir):
    '''
    Check that FileInfo.content() successfully reads a file and that
    the results are cached.

    '''
    fname = os.path.join(tmpdir, "a_file.txt")
    content = "Just\nA\nTest"
    with open(fname, "w", encoding='utf-8') as fout:
        fout.write(content)
    finfo = FileInfo(fname)
    input1 = finfo.contents
    assert input1 == content
    # Check that the contents have been cached.
    input2 = finfo.contents
    assert input2 is input1


def test_file_info_decode_error(tmpdir):
    '''
    Check that FileInfo.contents() handles a decoding error when reading
    a file.

    '''
    fname = os.path.join(tmpdir, "a_file.txt")
    # Test content includes a byte that cannot be decoded as utf-8.
    content = "Ju\xb5st\nA\nTest"
    with open(fname, "w", encoding='latin') as fout:
        fout.write(content)
    finfo = FileInfo(fname)
    # Content of file has been read with problematic byte skipped.
    assert finfo.contents == "Just\nA\nTest"
