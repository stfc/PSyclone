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
# Modified: A. R. Porter, R. W. Ford and S. Siso, STFC Daresbury Laboratory

'''This module tests the various classes in the single_variable_access_info
module.'''


import pytest

from psyclone.core import (AccessInfo, ComponentIndices, Signature,
                           SingleVariableAccessInfo)
from psyclone.core.access_type import AccessType
from psyclone.errors import InternalError
from psyclone.psyir.nodes import Assignment, Node


def test_access_info():
    '''Test the AccessInfo class.
    '''
    location = 12
    access_info = AccessInfo(AccessType.READ, location, Node())
    assert access_info.access_type == AccessType.READ
    assert access_info.location == location
    assert access_info.component_indices.indices_lists == [[]]
    assert not access_info.is_array()
    assert str(access_info) == "READ(12)"
    access_info.change_read_to_write()
    assert str(access_info) == "WRITE(12)"
    assert access_info.access_type == AccessType.WRITE
    with pytest.raises(InternalError) as err:
        access_info.change_read_to_write()
    assert "Trying to change variable to 'WRITE' which does not have "\
        "'READ' access." in str(err.value)

    # Test setter and getter:
    component_indices = ComponentIndices([["i"]])
    access_info.component_indices = component_indices
    assert access_info.component_indices == component_indices
    assert access_info.is_array()

    access_info = AccessInfo(AccessType.UNKNOWN, location, Node())
    assert access_info.access_type == AccessType.UNKNOWN
    assert access_info.location == location
    assert access_info.component_indices.indices_lists == [[]]

    access_info = AccessInfo(AccessType.UNKNOWN, location, Node(),
                             [["i", "j"]])
    assert access_info.access_type == AccessType.UNKNOWN
    assert access_info.location == location
    assert access_info.component_indices.indices_lists == [["i", "j"]]

    access_info = AccessInfo(AccessType.UNKNOWN, location, Node(),
                             ComponentIndices([["i", "j"]]))
    assert access_info.access_type == AccessType.UNKNOWN
    assert access_info.location == location
    assert access_info.component_indices.indices_lists == [["i", "j"]]


# -----------------------------------------------------------------------------
def test_access_info_exceptions():
    '''Test that the right exceptions are raised.
    '''
    location = 12
    with pytest.raises(InternalError) as err:
        _ = AccessInfo(AccessType.READ, location, Node(),
                       component_indices=123)
    assert "Index object in ComponentIndices constructor must be None, " \
           "a list or list of lists, got '123'" in str(err.value)

    with pytest.raises(InternalError) as err:
        _ = AccessInfo(AccessType.READ, location, Node(),
                       component_indices=[[], 123])
    assert "ComponentIndices: Invalid list parameter '[[], 123]'" \
        in str(err.value)

    location = 1
    access_info = AccessInfo(AccessType.READ, location, Node())
    with pytest.raises(InternalError) as err:
        access_info.component_indices = 123
    assert "The component_indices object in the setter of AccessInfo must " \
           "be an instance of ComponentIndices, got '123'" in str(err.value)


# -----------------------------------------------------------------------------
def test_variable_access_info():
    '''Test the SingleVariableAccesInfo class, i.e. the class that manages a
    list of VariableInfo instances for one variable
    '''

    vai = SingleVariableAccessInfo(Signature("var_name"))
    assert vai.var_name == "var_name"
    assert str(vai) == "var_name:"
    assert vai.is_written() is False
    assert vai.is_written_first() is False
    assert vai.is_read() is False

    assert vai.all_accesses == []
    assert vai.all_read_accesses == []
    assert vai.all_write_accesses == []
    assert vai.signature == Signature("var_name")

    vai.add_access_with_location(AccessType.READ, 2, Node(),
                                 component_indices=None)
    assert str(vai) == "var_name:READ(2)"
    assert vai.is_read()
    assert vai.is_read_only()
    assert vai.all_read_accesses == [vai[0]]
    assert vai.all_write_accesses == []
    assert not vai.is_written()
    assert not vai.is_written_first()
    vai.change_read_to_write()
    assert not vai.is_read()
    assert vai.is_written()
    assert vai.is_written_first()
    assert not vai.is_read_only()
    assert vai.all_read_accesses == []
    assert vai.all_write_accesses == [vai[0]]

    # Now we have one write access, which we should not be able to
    # change to write again:
    with pytest.raises(InternalError) as err:
        vai.change_read_to_write()
    assert "Trying to change variable 'var_name' to 'WRITE' which "\
        "does not have 'READ' access." in str(err.value)

    assert vai.all_accesses[0] == vai[0]
    with pytest.raises(IndexError) as err:
        _ = vai[1]

    # Add a READ access - now we should not be able to
    # change read to write anymore:
    vai.add_access_with_location(AccessType.READ, 1, Node(),
                                 component_indices=None)
    with pytest.raises(InternalError) as err:
        vai.change_read_to_write()
    assert "Variable 'var_name' had 2 accesses listed, "\
           "not one in change_read_to_write." in str(err.value)

    # And make sure the variable is not read_only if a write is added
    vai.add_access_with_location(AccessType.WRITE, 3, Node(),
                                 component_indices=None)
    assert vai.is_read_only() is False
    assert vai.all_read_accesses == [vai[1]]
    assert vai.all_write_accesses == [vai[0], vai[2]]


# -----------------------------------------------------------------------------
def test_variable_access_info_is_array(fortran_reader):
    '''Test that the SingleVariableAccesInfo class handles arrays as expected.

    '''
    vai = SingleVariableAccessInfo(Signature("var_name"))
    # Add non array-like access:
    vai.add_access_with_location(AccessType.READ, 1, Node,
                                 component_indices=None)
    assert not vai.is_array()
    # Add array access:
    vai.add_access_with_location(AccessType.READ, 1, Node(), [[Node()]])
    assert vai.is_array()

    # Get some real nodes:
    code = '''program test_prog
              use some_mod
              real, dimension(5,5) :: b, c
              integer :: i
              a = b(i)
              end program test_prog'''
    psyir = fortran_reader.psyir_from_source(code)
    scalar_assignment = psyir.walk(Assignment)[0]
    rhs = scalar_assignment.rhs
    # Get the reference to i
    ref_i = rhs.children[0]

    vai = SingleVariableAccessInfo(Signature("b"))
    vai.add_access_with_location(AccessType.READ, 1, rhs,
                                 ComponentIndices([ref_i]))

    # Check that the access to "b[i]" is considered an array
    # when testing for access using "i"
    assert vai.is_array("i")
    # Check that the access to "b[i]" is not considered an array
    # when testing for access using "j"
    assert not vai.is_array("j")


# -----------------------------------------------------------------------------
def test_variable_access_info_read_write():
    '''Test the handling of READWRITE accesses. A READWRITE indicates both
    a read and a write access, but if a variable has a READ and a WRITE
    access, this is not one READWRITE access. A READWRITE access is only
    used in subroutine calls (depending on kernel metadata)
    '''

    vai = SingleVariableAccessInfo(Signature("var_name"))
    assert vai.has_read_write() is False
    assert vai.is_written_first() is False

    # Add a READ and WRITE access at the same location, and make sure it
    # is not reported as READWRITE access
    node = Node()
    vai.add_access_with_location(AccessType.READ, 2, node,
                                 component_indices=None)
    assert vai[0].node == node
    assert vai[0].location == 2
    # Test a single read access:
    assert vai.is_written_first() is False
    vai.add_access_with_location(AccessType.WRITE, 2, Node(),
                                 component_indices=None)
    assert vai.has_read_write() is False
    # This tests a read-then-write access:
    assert vai.is_written_first() is False

    vai.add_access_with_location(AccessType.READWRITE, 2, Node(),
                                 component_indices=None)
    assert vai.has_read_write()

    # Create a new instance, and add only one READWRITE access:
    vai = SingleVariableAccessInfo(Signature("var_name"))
    vai.add_access_with_location(AccessType.READWRITE, 2, Node(),
                                 component_indices=None)
    assert vai.has_read_write()
    assert vai.is_read()
    assert vai.is_written()


# -----------------------------------------------------------------------------
def test_is_written_before():
    '''Tests that the 'is_written_before' function works as expected.

    '''
    var_sig = Signature("a")
    accesses = SingleVariableAccessInfo(var_sig)
    node1 = Node()
    accesses.add_access_with_location(AccessType.READ, 1, node1, None)
    node2 = Node()
    accesses.add_access_with_location(AccessType.WRITE, 2, node2, None)
    node3 = Node()
    accesses.add_access_with_location(AccessType.WRITE, 3, node3, None)

    assert accesses.is_written_before(node1) is False
    assert accesses.is_written_before(node2) is False
    assert accesses.is_written_before(node3) is True

    with pytest.raises(ValueError) as err:
        accesses.is_written_before(Node())
    assert ("Reference not found in 'is_written_before' for variable "
            "'a'" in str(err.value))


# -----------------------------------------------------------------------------
def test_is_read_before():
    '''Tests that the 'is_read_before' function works as expected.

    '''
    var_sig = Signature("a")
    accesses = SingleVariableAccessInfo(var_sig)
    node1 = Node()
    accesses.add_access_with_location(AccessType.WRITE, 1, node1, None)
    node2 = Node()
    accesses.add_access_with_location(AccessType.READ, 1, node2, None)
    node3 = Node()
    accesses.add_access_with_location(AccessType.WRITE, 1, node3, None)

    assert accesses.is_read_before(node1) is False
    assert accesses.is_read_before(node2) is False
    assert accesses.is_read_before(node3) is True

    with pytest.raises(ValueError) as err:
        accesses.is_read_before(Node())
    assert ("Reference not found in 'is_read_before' for variable 'a'."
            in str(err.value))


# -----------------------------------------------------------------------------
def test_is_accessed_before():
    '''Tests that the 'is_accessed_before' function works as expected.

    '''

    # First check a write access before the specified node:
    var_sig = Signature("a")
    accesses = SingleVariableAccessInfo(var_sig)
    node1 = Node()
    accesses.add_access_with_location(AccessType.WRITE, 1, node1, None)
    node2 = Node()
    accesses.add_access_with_location(AccessType.READ, 2, node2, None)

    assert accesses.is_accessed_before(node1) is False
    assert accesses.is_accessed_before(node2) is True

    # Now test a read access before the specified node:
    accesses = SingleVariableAccessInfo(var_sig)
    accesses.add_access_with_location(AccessType.READ, 1, node1, None)
    accesses.add_access_with_location(AccessType.READ, 2, node2, None)

    assert accesses.is_accessed_before(node1) is False
    assert accesses.is_accessed_before(node2) is True

    with pytest.raises(ValueError) as err:
        accesses.is_accessed_before(Node())
    assert ("Reference not found in 'is_accessed_before' for variable 'a'."
            in str(err.value))
