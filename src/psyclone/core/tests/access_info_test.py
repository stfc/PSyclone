# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council.
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

'''This module tests the various classes in core.access_info.'''

from __future__ import absolute_import
import pytest
from psyclone.core.access_info import AccessInfo, VariableAccessInfo, \
    VariablesAccessInfo
from psyclone.core.access_type import AccessType
from psyclone.psyGen import Node, InternalError


def test_access_info():
    '''Test the AccessInfo class.
    '''
    location = 12
    access_info = AccessInfo(AccessType.READ, location, Node())
    assert access_info.access_type == AccessType.READ
    assert access_info.location == location
    assert access_info.indices is None
    access_info.change_read_to_write()
    assert access_info.access_type == AccessType.WRITE
    with pytest.raises(InternalError) as err:
        access_info.change_read_to_write()
    assert "Trying to change variable to 'WRITE' which does not have "\
        "'READ' access." in str(err)

    access_info.indices = ["i"]
    assert access_info.indices == ["i"]

    access_info = AccessInfo(AccessType.UNKNOWN, location, Node())
    assert access_info.access_type == AccessType.UNKNOWN
    assert access_info.location == location
    assert access_info.indices is None

    access_info = AccessInfo(AccessType.UNKNOWN, location, Node(), ["i", "j"])
    assert access_info.access_type == AccessType.UNKNOWN
    assert access_info.location == location
    assert access_info.indices == ["i", "j"]


# -----------------------------------------------------------------------------
def test_variable_access_info():
    '''Test the VariableAccesInfo class, i.e. the class that manages a list
    of VariableInfo instances for one variable
    '''

    vai = VariableAccessInfo("var_name")
    assert vai.var_name == "var_name"
    assert vai.is_written() is False
    assert vai.is_read() is False
    assert vai.all_accesses == []

    vai.add_access(AccessType.READ, Node(), 2)
    assert vai.is_read()
    vai.change_read_to_write()
    assert not vai.is_read()
    assert vai.is_written()

    assert vai.all_accesses[0] == vai[0]
    with pytest.raises(IndexError) as err:
        _ = vai[1]

    # Add a READ access - now we should not be able to
    # change read to write anymore:
    vai.add_access(AccessType.READ, Node(), 1)
    with pytest.raises(InternalError) as err:
        vai.change_read_to_write()
    assert "Variable 'var_name' had 2 accesses listed, "\
           "not one in change_read_to_write." in str(err)


# -----------------------------------------------------------------------------
def test_variables_access_info():
    '''Test the implementation of VariablesAccessInfo, a class that manages
    a list of variables, each with a list of accesses.
    '''
    var_accesses = VariablesAccessInfo()
    node1 = Node()
    var_accesses.add_access("read", AccessType.READ, node1)
    node2 = Node()
    var_accesses.add_access("written", AccessType.WRITE, node2)
    assert str(var_accesses) == "read: READ, written: WRITE"

    var_accesses.next_location()
    node = Node()
    var_accesses.add_access("written", AccessType.WRITE, node)
    var_accesses.next_location()
    var_accesses.add_access("read_written", AccessType.WRITE, node)
    var_accesses.add_access("read_written", AccessType.READ, node)
    assert str(var_accesses) == "read: READ, read_written: READWRITE, "\
                                "written: WRITE"
    assert set(var_accesses.all_vars) == set(["read", "written",
                                              "read_written"])
    all_accesses = var_accesses["read"].all_accesses
    assert all_accesses[0].node == node1
    written_accesses = var_accesses["written"].all_accesses
    assert written_accesses[0].location == 0
    assert written_accesses[1].location == 1
    # Check that the location pointer is pointing to the next statement:
    assert var_accesses.location == 2

    # Create a new instance, which starts with statement number 999
    var_accesses2 = VariablesAccessInfo(999)
    var_accesses2.add_access("new_var", AccessType.READ, node)
    var_accesses2.add_access("written", AccessType.READ, node)
    new_var_accesses = var_accesses2["new_var"].all_accesses
    assert new_var_accesses[0].location == 999

    # Now merge the new instance with the previous instance:
    var_accesses.merge(var_accesses2)
    assert str(var_accesses) == "new_var: READ, read: READ, " \
                                "read_written: READWRITE, written: READWRITE"

    with pytest.raises(KeyError):
        _ = var_accesses["does_not_exist"]
    with pytest.raises(KeyError):
        var_accesses.is_read("does_not_exist")
    with pytest.raises(KeyError):
        var_accesses.is_written("does_not_exist")


# -----------------------------------------------------------------------------
def test_variables_access_info_merge():
    # pylint: disable=invalid-name
    '''Tests the merge operation of VariablesAccessInfo.
    '''
    # First create one instance representing for example:
    # a=b; c=d
    var_accesses1 = VariablesAccessInfo()
    node = Node()
    var_accesses1.add_access("b", AccessType.READ, node)
    var_accesses1.add_access("a", AccessType.WRITE, node)
    var_accesses1.next_location()
    var_accesses1.add_access("d", AccessType.READ, node)
    var_accesses1.add_access("c", AccessType.WRITE, node)
    c_accesses = var_accesses1["c"]
    assert len(c_accesses.all_accesses) == 1
    assert c_accesses[0].access_type == AccessType.WRITE

    # First create one instance representing for example:
    # e=f; g=h
    var_accesses2 = VariablesAccessInfo()
    var_accesses2.add_access("f", AccessType.READ, node)
    var_accesses2.add_access("e", AccessType.WRITE, node)
    var_accesses2.next_location()
    var_accesses2.add_access("h", AccessType.READ, node)
    var_accesses2.add_access("g", AccessType.WRITE, node)

    # Now merge the second instance into the first one
    var_accesses1.merge(var_accesses2)

    # The e=f access pattern should have the same location
    # as the c=d (since there is no next_location after
    # adding the b=a access):
    c_accesses = var_accesses1["c"]
    e_accesses = var_accesses1["e"]
    assert c_accesses[0].access_type == AccessType.WRITE
    assert e_accesses[0].access_type == AccessType.WRITE
    assert c_accesses[0].location == e_accesses[0].location

    # Test that the g=h part has a higher location than the
    # c=d data. This makes sure that merge() increases the
    # location number of accesses when merging.
    c_accesses = var_accesses1["c"]
    g_accesses = var_accesses1["g"]
    h_accesses = var_accesses1["h"]
    assert c_accesses[0].location < g_accesses[0].location
    assert g_accesses[0].location == h_accesses[0].location

    # Also make sure that the access location was properly increased
    # Originally we had locations 0,1. Then we merged accesses with
    # location 0,1 in - the one at 0 is merged with the current 1,
    # and the new location 1 increases the current location from
    # 1 to 2:
    # pylint: disable=protected-access
    assert var_accesses1._location == 2
