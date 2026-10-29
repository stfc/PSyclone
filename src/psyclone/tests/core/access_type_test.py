# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module tests AccessType.'''

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
    assert str(AccessType.REDUCTION) == "REDUCTION"
    assert str(AccessType.CALL) == "CALL"
    assert str(AccessType.INQUIRY) == "INQUIRY"
    assert str(AccessType.CONSTANT) == "CONSTANT"
    assert str(AccessType.UNKNOWN) == "UNKNOWN"


def test_api_specific_name():
    '''Tests api_specific_name(), i.e. conversion to an
    API-specific string. '''

    Config.get().api = "lfric"

    assert AccessType.READ.api_specific_name() == "gh_read"
    assert AccessType.WRITE.api_specific_name() == "gh_write"
    assert AccessType.READWRITE.api_specific_name() == "gh_readwrite"
    assert AccessType.INC.api_specific_name() == "gh_inc"
    assert AccessType.READINC.api_specific_name() == "gh_readinc"
    assert AccessType.REDUCTION.api_specific_name() == "gh_reduction"
    assert AccessType.CALL.api_specific_name() == "call"
    assert AccessType.INQUIRY.api_specific_name() == "inquiry"
    assert AccessType.CONSTANT.api_specific_name() == "constant"
    assert AccessType.UNKNOWN.api_specific_name() == "unknown"
    # Use set to make this independent of the order:
    assert set(AccessType.all_write_accesses()) == set([AccessType.WRITE,
                                                        AccessType.READWRITE,
                                                        AccessType.INC,
                                                        AccessType.READINC,
                                                        AccessType.REDUCTION])
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
    assert AccessType.from_string("reduction") == AccessType.REDUCTION
    assert AccessType.from_string("unknown") == AccessType.UNKNOWN
    assert AccessType.from_string("constant") == AccessType.CONSTANT

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
    # No duplications.
    assert (len(all_read_accesses) ==
            len(set(all_read_accesses)))
    assert all(isinstance(read_access, AccessType)
               for read_access in all_read_accesses)


def test_non_data_accesses():
    '''Test the non_data_accesses() method.'''
    accesses = AccessType.non_data_accesses()
    assert isinstance(accesses, list)
    # No duplications
    assert (len(accesses) == len(set(accesses)))
    assert all(isinstance(acc, AccessType) for acc in accesses)
    all_read_accesses = AccessType.all_read_accesses()
    all_write_accesses = AccessType.all_write_accesses()
    all_data_accesses = all_read_accesses + all_write_accesses
    for acc in accesses:
        assert acc not in all_data_accesses
