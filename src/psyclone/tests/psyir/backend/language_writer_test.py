# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Author: J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

'''Performs pytest tests on the psyclone.psyir.backend.fortran module'''

from __future__ import absolute_import

import pytest

from psyclone.psyir.backend.language_writer import LanguageWriter
from psyclone.tests.psyir.backend.fortran_test import test_fw_arrayreference, \
    test_fw_arrayreference_incomplete
from psyclone.tests.psyir.backend.c_test import test_cw_arraystructureref, \
    test_cw_structureref


def test_language_writer_constructor():
    '''Tests the constructor.

    '''
    _ = LanguageWriter(["(", ")"], "%")
    _ = LanguageWriter(["[", "]"], ".")


def test_language_writer_constructor_errors():
    '''Test that invalid parameters in the constructor are detected.
    '''
    for invalid_parenthesis in [123, "()", ['[', '[', ']']]:
        with pytest.raises(TypeError) as err:
            _ = LanguageWriter(invalid_parenthesis, "%")
        assert "Invalid array-parenthesis parameter, must be " \
               "a list of two strings, got '" in str(err.value)

    for invalid_structure_character in [123, []]:
        with pytest.raises(TypeError) as err:
            _ = LanguageWriter(["(", ")"], invalid_structure_character)
        assert "Invalid structure_character parameter, must be " \
               "a string of length 2, got '" in str(err.value)


def test_gen_dims_error():
    '''Check the _gen_dims method raises an exception if a symbol shape
    entry is not supported.

    '''
    writer = LanguageWriter(["(", ")"], "%")
    with pytest.raises(NotImplementedError) as err:
        writer.gen_dims([])
    assert "gen_dims() is abstract" in str(err.value)


def test_rest(fortran_reader, fortran_writer, tmpdir):
    '''This imports other tests to ensure a 100% coverage of the LanguageWriter
    when just running this test.
    '''

    # Test array references
    test_fw_arrayreference(fortran_reader, fortran_writer, tmpdir)
    # Tests errors with array references
    test_fw_arrayreference_incomplete(fortran_writer)

    # This test covers most structure related code in one call - use it:
    test_cw_structureref(fortran_reader)

    # This test covers most sarray tructure references related code:
    test_cw_arraystructureref(fortran_reader)
