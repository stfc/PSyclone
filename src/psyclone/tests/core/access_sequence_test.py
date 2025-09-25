# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2025, Science and Technology Facilities Council.
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

'''This module tests the various classes in the access_sequence module.'''


import pytest

from psyclone.core import (AccessInfo, ComponentIndices, Signature,
                           AccessSequence)
from psyclone.core.access_type import AccessType
from psyclone.errors import InternalError
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Assignment, Node, Reference, Return
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, Symbol


def test_access_info() -> None:
    '''Test the AccessInfo class.
    '''
    access_info = AccessInfo(AccessType.READ, Node())
    assert access_info.access_type == AccessType.READ
    assert access_info.component_indices.indices_lists == [[]]
    assert not access_info.has_indices()
    assert str(access_info) == "READ"
    access_info.change_read_to_write()
    assert str(access_info) == "WRITE"
    assert access_info.access_type == AccessType.WRITE
    with pytest.raises(InternalError) as err:
        access_info.change_read_to_write()
    assert "Trying to change variable to 'WRITE' which does not have "\
        "'READ' access." in str(err.value)

    # Test setter and getter:
    component_indices = ComponentIndices([["i"]])
    access_info.component_indices = component_indices
    assert access_info.component_indices == component_indices
    assert access_info.has_indices()

    access_info = AccessInfo(AccessType.UNKNOWN, Node())
    assert access_info.access_type == AccessType.UNKNOWN
    assert access_info.component_indices.indices_lists == [[]]

    access_info = AccessInfo(AccessType.UNKNOWN, Node(),
                             [["i", "j"]])
    assert access_info.access_type == AccessType.UNKNOWN
    assert access_info.component_indices.indices_lists == [["i", "j"]]

    access_info = AccessInfo(AccessType.UNKNOWN, Node(),
                             ComponentIndices([["i", "j"]]))
    assert access_info.access_type == AccessType.UNKNOWN
    assert access_info.component_indices.indices_lists == [["i", "j"]]

    assert access_info.is_data_access

    access_info = AccessInfo(AccessType.INQUIRY, Node())
    assert not access_info.is_data_access


# -----------------------------------------------------------------------------
def test_access_info_exceptions() -> None:
    '''Test that the right exceptions are raised.
    '''
    with pytest.raises(InternalError) as err:
        _ = AccessInfo(AccessType.READ, Node(),
                       component_indices=123)   # type: ignore[arg-type]
    assert "Index object in ComponentIndices constructor must be None, " \
           "a list or list of lists, got '123'" in str(err.value)

    with pytest.raises(InternalError) as err:
        _ = AccessInfo(AccessType.READ, Node(),
                       component_indices=[[], 123])   # type: ignore[list-item]
    assert "ComponentIndices: Invalid list parameter '[[], 123]'" \
        in str(err.value)

    access_info = AccessInfo(AccessType.READ, Node())
    with pytest.raises(InternalError) as err:
        access_info.component_indices = 123   # type: ignore[assignment]
    assert "The component_indices object in the setter of AccessInfo must " \
           "be an instance of ComponentIndices, got '123'" in str(err.value)


def test_access_info_description() -> None:
    '''
    Test for the description() method of AccessInfo.
    '''
    # When associated node is not a Statement.
    ainfo = AccessInfo(AccessType.READ, Node())
    assert "< node[] >" in ainfo.description.lower()
    # When it is a Statement.
    ainfo = AccessInfo(AccessType.READ, Return())
    assert "return" in ainfo.description.lower()
    # When it is a Symbol.
    osym = Symbol("something")
    asym = DataSymbol("test", INTEGER_TYPE, initial_value=Reference(osym))
    ainfo = AccessInfo(AccessType.INQUIRY, asym)
    assert ("definition of symbol 'test: datasymbol<scalar" in
            ainfo.description.lower())


# -----------------------------------------------------------------------------
def test_variable_access_sequence() -> None:
    '''Test the AccessSequence class, i.e. the class that manages a
    list of VariableInfo instances for one variable
    '''

    accesses = AccessSequence(Signature("var_name"))
    assert accesses.var_name == "var_name"
    assert str(accesses) == "var_name:[]"
    assert accesses.is_written() is False
    assert accesses.is_written_first() is False
    assert accesses.is_read() is False

    assert not accesses  # In python an empty list is falsy
    assert accesses.all_read_accesses == []
    assert accesses.all_write_accesses == []
    assert accesses.signature == Signature("var_name")

    accesses.add_access(AccessType.INQUIRY, Node(), component_indices=None)
    accesses.add_access(AccessType.READ, Node(), component_indices=None)
    assert str(accesses) == "var_name:[INQUIRY,READ]"
    assert accesses.str_access_summary() == "INQUIRY+READ"
    assert accesses.is_read()
    assert accesses.has_data_access()
    assert accesses.is_read_only()
    assert accesses.all_read_accesses == [accesses[1]]
    assert accesses.all_write_accesses == []
    assert not accesses.is_written()
    assert not accesses.is_written_first()
    assert not accesses.is_called()
    accesses.change_read_to_write()
    assert not accesses.is_read()
    assert accesses.is_written()
    assert accesses.is_written_first()
    assert not accesses.is_read_only()
    assert accesses.all_read_accesses == []
    assert accesses.all_write_accesses == [accesses[1]]

    # Now we have one write access, which we should not be able to
    # change to write again:
    with pytest.raises(InternalError) as err_internal:
        accesses.change_read_to_write()
    assert ("Variable 'var_name' has a 'WRITE' access. change_read_to_write() "
            "expects only inquiry accesses and a single 'READ' access."
            in str(err_internal.value))

    with pytest.raises(IndexError) as err:
        _ = accesses[2]
    assert "list index out of range" in str(err.value)

    # Add a READ access - we should not be able to
    # change this read to write as there's already a WRITE access.
    accesses.add_access(AccessType.READ, Node(), component_indices=None)
    with pytest.raises(InternalError) as err_internal:
        accesses.change_read_to_write()
    assert ("Variable 'var_name' has a 'WRITE' access. change_read_to_write() "
            "expects only inquiry accesses and a single 'READ' access."
            in str(err_internal.value))
    # And make sure the variable is not read_only if a write is added
    accesses.add_access(AccessType.WRITE, Node(), component_indices=None)
    assert accesses.is_read_only() is False
    assert accesses.all_read_accesses == [accesses[2]]
    assert accesses.all_write_accesses == [accesses[1], accesses[3]]
    # Check that we catch a case where there are no accesses at all.
    accesses = AccessSequence(Signature("var_name"))
    with pytest.raises(InternalError) as err_internal:
        accesses.change_read_to_write()
    assert "but it does not have a 'READ' access" in str(err_internal.value)

    # Test handling if there is more than one read and it is supposed
    # to change read to write:
    accesses.add_access(AccessType.READ, Node(), component_indices=None)
    accesses.add_access(AccessType.READ, Node(), component_indices=None)
    with pytest.raises(InternalError) as err_internal:
        accesses.change_read_to_write()
    assert ("Trying to change variable 'var_name' to 'WRITE' but it has more "
            "than one 'READ' access." in str(err_internal.value))

    # Now do just a CALL, this will have no data accesses
    accesses = AccessSequence(Signature("var_name"))
    accesses.add_access(AccessType.CALL, Node(), component_indices=None)
    assert accesses.is_called()
    assert not accesses.is_read()
    assert not accesses.is_written()
    assert not accesses.has_data_access()


def test_variable_access_sequence_update() -> None:
    '''Test that the AccessSequence class updates as expected.

    '''
    access_seq1 = AccessSequence(Signature("var_name"))
    node1 = Node()
    access_seq1.add_access(AccessType.CALL, node1, component_indices=None)

    access_seq2 = AccessSequence(Signature("var_name"))
    node2 = Node()
    access_seq2.add_access(AccessType.CALL, node2, component_indices=None)
    access_seq1.update(access_seq2)

    assert len(access_seq1) == 2
    assert access_seq1[0].node is node1
    assert access_seq1[1].node is node2

    access_seq3 = AccessSequence(Signature("other_name"))
    access_seq3.add_access(AccessType.CALL, Node(), component_indices=None)

    with pytest.raises(ValueError) as err:
        access_seq1.update(access_seq3)

    assert ("Updating AccessSequence for 'var_name' with data for "
            "'other_name'." == str(err.value))


def test_variable_access_sequence_has_indices(
        fortran_reader: "FortranReader"
    ) -> None:
    '''Test that the AccessSequence class handles indices as expected.

    '''
    vam = AccessSequence(Signature("var_name"))
    # Add non array-like access:
    vam.add_access(AccessType.READ, Node(), component_indices=None)
    assert not vam.has_indices()
    # Add array access:
    vam.add_access(AccessType.READ, Node(), [[Node()]])
    assert vam.has_indices()

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

    vam = AccessSequence(Signature("b"))
    vam.add_access(AccessType.READ, rhs, ComponentIndices([ref_i]))

    # Check that the access to "b[i]" is considered an index
    # when testing for access using "i"
    assert vam.has_indices("i")
    # Check that the access to "b[i]" is not considered an index
    # when testing for access using "j"
    assert not vam.has_indices("j")


# -----------------------------------------------------------------------------
def test_variable_access_sequence_read_write() -> None:
    '''Test the handling of READWRITE accesses. A READWRITE indicates both
    a read and a write access, but if a variable has a READ and a WRITE
    access, this is not one READWRITE access. A READWRITE access is only
    used in subroutine calls (depending on kernel metadata)
    '''

    accesses = AccessSequence(Signature("var_name"))
    assert accesses.has_read_write() is False
    assert accesses.is_written_first() is False

    # Add a READ and WRITE access and make sure it is not reported as
    # READWRITE access
    node = Node()
    accesses.add_access(AccessType.READ, node, component_indices=None)
    assert accesses[0].node == node
    # Test a single read access:
    assert accesses.is_written_first() is False
    accesses.add_access(AccessType.WRITE, Node(), component_indices=None)
    assert accesses.has_read_write() is False
    # This tests a read-then-write access:
    assert accesses.is_written_first() is False

    accesses.add_access(AccessType.READWRITE, Node(), component_indices=None)
    assert accesses.has_read_write()

    # Create a new instance, and add only one READWRITE access:
    accesses = AccessSequence(Signature("var_name"))
    accesses.add_access(AccessType.READWRITE, Node(), component_indices=None)
    assert accesses.has_read_write()
    assert accesses.is_read()
    assert accesses.is_written()
    assert accesses.has_data_access()
    assert not accesses.is_called()
