
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2018, Science and Technology Facilities Council.
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
# Authors: Joerg Henrichs, Bureau of Meteorology

'''This module tests AccessType.'''

from __future__ import absolute_import
import pytest
from psyclone.configuration import Config
from psyclone.core.access_type import AccessType


def test_generic():
    '''Generic tests.'''
    assert AccessType.get_size() == 5


def test_str():
    '''Tests conversion to an (API specific) string'''

    Config.get().api = "dynamo0.3"

    assert str(AccessType.INC) == "gh_inc"
    assert str(AccessType.WRITE) == "gh_write"
    assert str(AccessType.READ) == "gh_read"
    assert str(AccessType.READWRITE) == "gh_readwrite"
    assert str(AccessType.SUM) == "gh_sum"


def test_api_name():
    '''Tests api_name(). '''
    assert AccessType.INC.api_name() == "gh_inc"
    assert AccessType.WRITE.api_name() == "gh_write"
    assert AccessType.READ.api_name() == "gh_read"
    assert AccessType.READWRITE.api_name() == "gh_readwrite"
    assert AccessType.SUM.api_name() == "gh_sum"


def test_from_string():
    '''Test the from_string method.'''

    assert AccessType.from_string("inc") == AccessType.INC
    assert AccessType.from_string("write") == AccessType.WRITE
    assert AccessType.from_string("read") == AccessType.READ
    assert AccessType.from_string("readwrite") == AccessType.READWRITE
    assert AccessType.from_string("sum") == AccessType.SUM

    with pytest.raises(KeyError) as err:
        AccessType.from_string("invalid")
    assert "Unknown access type 'invalid'" in str(err)
