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

''' Module containing pytest test that verifies proper behaviour if
fparser was not initialised.'''

from __future__ import absolute_import

import pytest

from fparser.two.parser import ParserFactory

from psyclone.gocean1p0 import GOLoop


@pytest.fixture(scope="function", autouse=True)
def clear_fparser():
    ''' The tests here assume that fparser has not been initialised.
    This is achieved by calling `_setup([])` with an empty list, which
    will remove all currently existing parser classes and functions.
    At the end of the tests re-initialse parser.
    '''

    # Remove all fparser classes and functions
    ParserFactory()._setup([])

    # Now execute all tests
    yield


def test_loop_bound_when_fparser_not_initialised():
    '''This reproduces #1272: a gocean custom loop boundary could
    not be parsed if the parser has not been initialised previously.
    Reproduce this bug by re-initialising fparser with an empty
    class list.
    '''
    GOLoop.add_bounds("go_offset_sw:go_ct:internal_we_halo:1:2:3:4")
