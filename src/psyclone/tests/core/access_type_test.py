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
# Author: Joerg Henrichs, Bureau of Meteorology
# Modified by R. W. Ford and N. Nobre, STFC Daresbury Lab

'''This module tests AccessType.'''

from __future__ import absolute_import
import pytest
from psyclone.configuration import Config
from psyclone.core.access_type import AccessType


def test_str():
    '''Tests conversion to a string.'''

    assert str(AccessType.READ) == "READ"
    assert str(AccessType.WRITE) == "WRITE"
    assert str(AccessType.READWRITE) == "READWRITE"
    assert str(AccessType.INC) == "INC"
    assert str(AccessType.READINC) == "READINC"
    assert str(AccessType.SUM) == "SUM"


def test_api_specific_name():
    '''Tests api_specific_name(), i.e. conversion to an
    API-specific string. '''

    Config.get().api = "dynamo0.3"

    assert AccessType.READ.api_specific_name() == "gh_read"
    assert AccessType.WRITE.api_specific_name() == "gh_write"
    assert AccessType.READWRITE.api_specific_name() == "gh_readwrite"
    assert AccessType.INC.api_specific_name() == "gh_inc"
    assert AccessType.READINC.api_specific_name() == "gh_readinc"
    assert AccessType.SUM.api_specific_name() == "gh_sum"
    assert AccessType.get_valid_reduction_modes() == [AccessType.SUM]
    assert AccessType.get_valid_reduction_names() == ["gh_sum"]
    # Use set to make this independent of the order:
    assert set(AccessType.all_write_accesses()) == set([AccessType.WRITE,
                                                        AccessType.READWRITE,
                                                        AccessType.INC,
                                                        AccessType.READINC,
                                                        AccessType.SUM])
    assert set(AccessType.all_read_accesses()) == set([AccessType.READ,
                                                       AccessType.READWRITE,
                                                       AccessType.READINC,
                                                       AccessType.INC])
    # Clean up the Config instance
    Config._instance = None


def test_from_string():
    '''Test the from_string method.'''

    assert AccessType.from_string("read") == AccessType.READ
    assert AccessType.from_string("write") == AccessType.WRITE
    assert AccessType.from_string("readwrite") == AccessType.READWRITE
    assert AccessType.from_string("inc") == AccessType.INC
    assert AccessType.from_string("readinc") == AccessType.READINC
    assert AccessType.from_string("sum") == AccessType.SUM
    assert AccessType.from_string("unknown") == AccessType.UNKNOWN

    with pytest.raises(ValueError) as err:
        AccessType.from_string("invalid")
    valid = [str(access).lower() for access in AccessType]
    assert (f"Unknown access type 'invalid'. Valid values are {valid}."
            in str(err.value))


def test_all_write_accesses():
    '''Test the all_write_accesses() method.'''

    all_write_accesses = AccessType.all_write_accesses()
    assert isinstance(all_write_accesses, list)
    assert len(all_write_accesses) == 5
    assert (len(all_write_accesses) ==
            len(set(all_write_accesses)))
    assert all(isinstance(write_access, AccessType)
               for write_access in all_write_accesses)


def test_all_read_accesses():
    '''Test the all_read_accesses() method.'''

    all_read_accesses = AccessType.all_read_accesses()
    assert isinstance(all_read_accesses, list)
    assert len(all_read_accesses) == 4
    assert (len(all_read_accesses) ==
            len(set(all_read_accesses)))
    assert all(isinstance(read_access, AccessType)
               for read_access in all_read_accesses)
