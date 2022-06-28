# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2022, Science and Technology Facilities Council.
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

'''This module tests the various classes in core.access_info.'''

from __future__ import absolute_import
import pytest

from psyclone.core import AccessInfo, ComponentIndices, Signature, \
    SingleVariableAccessInfo, VariablesAccessInfo
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
    vai.change_read_to_write()
    assert not vai.is_read()
    assert vai.is_written()
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
def test_variable_access_info_is_array():
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


# -----------------------------------------------------------------------------
def test_variable_access_info_read_write():
    '''Test the handling of READWRITE accesses. A READWRITE indicates both
    a read and a write access, but if a variable has a READ and a WRITE
    access, this is not one READWRITE access. A READWRITE access is only
    used in subroutine calls (depending on kernel metadata)
    '''

    vai = SingleVariableAccessInfo(Signature("var_name"))
    assert vai.has_read_write() is False

    # Add a READ and WRITE access at the same location, and make sure it
    # is not reported as READWRITE access
    node = Node()
    vai.add_access_with_location(AccessType.READ, 2, node,
                                 component_indices=None)
    assert vai[0].node == node
    assert vai[0].location == 2
    vai.add_access_with_location(AccessType.WRITE, 2, Node(),
                                 component_indices=None)
    assert vai.has_read_write() is False

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
    var_accesses = VariablesAccessInfo()
    var_sig = Signature("a")
    node1 = Node()
    var_accesses.add_access(var_sig, AccessType.READ, node1)
    node2 = Node()
    var_accesses.add_access(var_sig, AccessType.WRITE, node2)
    node3 = Node()
    var_accesses.add_access(var_sig, AccessType.WRITE, node3)

    accesses = var_accesses[var_sig]
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
    var_accesses = VariablesAccessInfo()
    var_sig = Signature("a")
    node1 = Node()
    var_accesses.add_access(var_sig, AccessType.WRITE, node1)
    node2 = Node()
    var_accesses.add_access(var_sig, AccessType.READ, node2)
    node3 = Node()
    var_accesses.add_access(var_sig, AccessType.WRITE, node3)

    accesses = var_accesses[var_sig]
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
    var_accesses = VariablesAccessInfo()
    var_sig = Signature("a")
    node1 = Node()
    var_accesses.add_access(var_sig, AccessType.WRITE, node1)
    node2 = Node()
    var_accesses.add_access(var_sig, AccessType.READ, node2)

    accesses = var_accesses[var_sig]
    assert accesses.is_accessed_before(node1) is False
    assert accesses.is_accessed_before(node2) is True

    # Now test a read access before the specified node:
    var_accesses = VariablesAccessInfo()
    var_accesses.add_access(var_sig, AccessType.READ, node1)
    var_accesses.add_access(var_sig, AccessType.READ, node2)

    accesses = var_accesses[var_sig]
    assert accesses.is_accessed_before(node1) is False
    assert accesses.is_accessed_before(node2) is True

    with pytest.raises(ValueError) as err:
        accesses.is_accessed_before(Node())
    assert ("Reference not found in 'is_accessed_before' for variable 'a'."
            in str(err.value))


# -----------------------------------------------------------------------------
def test_variables_access_info():
    '''Test the implementation of VariablesAccessInfo, a class that manages
    a list of variables, each with a list of accesses.
    '''
    var_accesses = VariablesAccessInfo()
    node1 = Node()
    var_accesses.add_access(Signature("read"), AccessType.READ, node1)
    node2 = Node()
    var_accesses.add_access(Signature("written"), AccessType.WRITE, node2)
    assert str(var_accesses) == "read: READ, written: WRITE"

    var_accesses.next_location()
    node = Node()
    var_accesses.add_access(Signature("written"), AccessType.WRITE, node)
    var_accesses.next_location()
    var_accesses.add_access(Signature("read_written"), AccessType.WRITE, node)
    var_accesses.add_access(Signature("read_written"), AccessType.READ, node)
    assert str(var_accesses) == "read: READ, read_written: READ+WRITE, "\
                                "written: WRITE"
    assert set(var_accesses.all_signatures) == set([Signature("read"),
                                                    Signature("written"),
                                                    Signature("read_written")])
    all_accesses = var_accesses[Signature("read")].all_accesses
    assert all_accesses[0].node == node1
    written_accesses = var_accesses[Signature("written")].all_accesses
    assert written_accesses[0].location == 0
    assert written_accesses[1].location == 1
    # Check that the location pointer is pointing to the next statement:
    assert var_accesses.location == 2

    # Create a new instance
    var_accesses2 = VariablesAccessInfo()
    var_accesses2.add_access(Signature("new_var"), AccessType.READ, node)
    var_accesses2.add_access(Signature("written"), AccessType.READ, node)

    # Now merge the new instance with the previous instance:
    var_accesses.merge(var_accesses2)
    assert str(var_accesses) == "new_var: READ, read: READ, " \
                                "read_written: READ+WRITE, written: READ+WRITE"


# -----------------------------------------------------------------------------
def test_variables_access_info_errors():
    '''Tests if errors are handled correctly. '''
    var_accesses = VariablesAccessInfo()
    node = Node()
    var_accesses.add_access(Signature("read"), AccessType.READ, node)
    with pytest.raises(KeyError) as err:
        _ = var_accesses[Signature("does_not_exist")]
    with pytest.raises(KeyError):
        var_accesses.is_read(Signature("does_not_exist"))
    with pytest.raises(KeyError):
        var_accesses.is_written(Signature("does_not_exist"))

    assert "READWRITE" not in str(var_accesses)
    var_accesses.add_access(Signature("readwrite"), AccessType.READWRITE, node)
    assert "READWRITE" in str(var_accesses)

    with pytest.raises(InternalError) as err:
        var_accesses.add_access("no-signature", AccessType.READWRITE, node)

    assert "Got 'no-signature' of type 'str' but expected it to be of type " \
           "psyclone.core.Signature." in str(err.value)

    # Check for consistency between signature and component indices:
    with pytest.raises(InternalError) as err:
        var_accesses.add_access(Signature(("a", "b")), AccessType.READ, node,
                                ComponentIndices([]))
    assert "Cannot add '[[]]' with length 1 as indices for 'a%b' which "\
           "requires 2 elements." in str(err.value)


# -----------------------------------------------------------------------------
def test_component_indices_auto_extension():
    '''To make it more convenient for the user certain combinations of
    signature and component_indices in the add_location call will
    automatically add empty indices to the component_indices. For example.
    adding "ssh_fld%grid%tmask" with indices ["i", "j"] will automatically
    create component_indices like [[], [], ["i", "j"]].
    '''
    var_accesses = VariablesAccessInfo()
    node = Node()
    sig = Signature(("a", "b", "c"))
    # This should auto-extent the component indices,
    # since they are specified as a simple list:
    var_accesses.add_access(sig, AccessType.READ, node, ["i", "j"])
    assert (var_accesses[sig][0].component_indices.indices_lists ==
            [[], [], ["i", "j"]])

    # This must trigger an exception, since a list of lists is used, which
    # should not get any values added:
    with pytest.raises(InternalError) as err:
        var_accesses.add_access(sig, AccessType.READ, node, [["i", "j"]])
    assert ("Cannot add '[['i', 'j']]' with length 1 as indices for 'a%b%c' "
            "which requires 3 elements." in str(err.value))

    component_indices = ComponentIndices(["i", "j"])
    with pytest.raises(InternalError) as err:
        var_accesses.add_access(sig, AccessType.READ, node, component_indices)
    assert ("Cannot add '[['i', 'j']]' with length 1 as indices for 'a%b%c' "
            "which requires 3 elements." in str(err.value))


# -----------------------------------------------------------------------------
def test_variables_access_info_merge():
    '''Tests the merge operation of VariablesAccessInfo.
    '''
    # First create one instance representing for example:
    # a=b; c=d
    var_accesses1 = VariablesAccessInfo()
    node = Node()
    var_accesses1.add_access(Signature("b"), AccessType.READ, node)
    var_accesses1.add_access(Signature("a"), AccessType.WRITE, node)
    var_accesses1.next_location()
    var_accesses1.add_access(Signature("d"), AccessType.READ, node)
    var_accesses1.add_access(Signature("c"), AccessType.WRITE, node)
    c_accesses = var_accesses1[Signature("c")]
    assert len(c_accesses.all_accesses) == 1
    assert c_accesses[0].access_type == AccessType.WRITE

    # First create one instance representing for example:
    # e=f; g=h
    var_accesses2 = VariablesAccessInfo()
    var_accesses2.add_access(Signature("f"), AccessType.READ, node)
    var_accesses2.add_access(Signature("e"), AccessType.WRITE, node)
    var_accesses2.next_location()
    var_accesses2.add_access(Signature("h"), AccessType.READ, node)
    var_accesses2.add_access(Signature("g"), AccessType.WRITE, node)

    # Now merge the second instance into the first one
    var_accesses1.merge(var_accesses2)

    # The e=f access pattern should have the same location
    # as the c=d (since there is no next_location after
    # adding the b=a access):
    c_accesses = var_accesses1[Signature("c")]
    e_accesses = var_accesses1[Signature("e")]
    assert c_accesses[0].access_type == AccessType.WRITE
    assert e_accesses[0].access_type == AccessType.WRITE
    assert c_accesses[0].location == e_accesses[0].location

    # Test that the g=h part has a higher location than the
    # c=d data. This makes sure that merge() increases the
    # location number of accesses when merging.
    c_accesses = var_accesses1[Signature("c")]
    g_accesses = var_accesses1[Signature("g")]
    h_accesses = var_accesses1[Signature("h")]
    assert c_accesses[0].location < g_accesses[0].location
    assert g_accesses[0].location == h_accesses[0].location

    # Also make sure that the access location was properly increased
    # Originally we had locations 0,1. Then we merged accesses with
    # location 0,1 in - the one at 0 is merged with the current 1,
    # and the new location 1 increases the current location from
    # 1 to 2:
    assert var_accesses1.location == 2


# -----------------------------------------------------------------------------
def test_constructor(fortran_reader):
    '''Test the optional constructor parameter (single node and list
    of nodes).'''
    code = '''module test
        contains
        subroutine tmp()
          integer :: a,b,c
          a = b/c
          c = a*b
        end subroutine tmp
        end module test'''
    psyir = fortran_reader.psyir_from_source(code)
    schedule = psyir.children[0].children[0]
    node1 = schedule[0]
    node2 = schedule[1]
    vai1 = VariablesAccessInfo(node1)
    assert str(vai1) == "a: WRITE, b: READ, c: READ"
    vai2 = VariablesAccessInfo([node1, node2])
    assert str(vai2) == "a: READ+WRITE, b: READ, c: READ+WRITE"

    with pytest.raises(InternalError) as err:
        VariablesAccessInfo([node1, node2, 3])
    assert "One element in the node list is not a Node, but of type " in \
        str(err.value)
    # The error message is slightly different between python 2 and 3
    # so only test for the part that is the same in both:
    assert "'int'>" in str(err.value)

    with pytest.raises(InternalError) as err:
        VariablesAccessInfo(1)
    assert "Error in VariablesAccessInfo" in str(err.value)
    # The error message is slightly different between python 2 and 3
    # so only test for the part that is the same in both:
    assert "'int'>" in str(err.value)


# -----------------------------------------------------------------------------
def test_derived_type_scalar(fortran_reader):
    '''This function tests the handling of derived scalartypes.
    '''

    code = '''module test
        contains
        subroutine tmp()
          use my_mod
          !use my_mod, only: something
          !type(something) :: a, b, c
          integer :: i, j, k
          a%b = b%c/c%d%e
        end subroutine tmp
        end module test'''
    schedule = fortran_reader.psyir_from_source(code).children[0]
    node1 = schedule.children[0][0]
    vai1 = VariablesAccessInfo(node1)
    assert isinstance(node1, Assignment)
    assert str(vai1) == "a%b: WRITE, b%c: READ, c%d%e: READ"


# -----------------------------------------------------------------------------
def to_fortran(writer, index_expression):
    '''A small helper function that converts index information from an
    AccessInfo object to a list of list of strings. For example, an access
    like `a(i)%b%c(j,k)` will have an index expression of
    `[ [i], [], [j, k]]`, where `i`, `j`, and `k` are the PSyIR representation
    of the indices. This function will convert each PSyIR node to a string,
    returning in the example: `[ ["i"], [], ["j", "k"]]`

    :param writer: a FortranWriter object.
    :type writer: :py:class:`psyclone.psyir.backend.fortan.FortranWriter`
    :param expression: a Fortran PSyIR node with the index expression to \
        convert.
    :type index_expression: list of list of :py:class:`psyclone.psyir.node`s

    :return: list of list of corresponding Fortran code, each as string.
    :rtype: list of list of str
    '''

    result = []
    for indices in index_expression:
        result.append([writer(index) for index in indices])
    return result


# -----------------------------------------------------------------------------
@pytest.mark.parametrize("array, indices",
                         [("a%b%c", [[], [], []]),
                          ("a%b%c(i)", [[], [], ["i"]]),
                          ("a%b(j)%c", [[], ["j"], []]),
                          ("a%b(j)%c(i)", [[], ["j"], ["i"]]),
                          ("a(k)%b%c", [["k"], [], []]),
                          ("a(k)%b%c(i)", [["k"], [], ["i"]]),
                          ("a(k)%b(j)%c", [["k"], ["j"], []]),
                          ("a(k)%b(j)%c(i)", [["k"], ["j"], ["i"]])
                          ])
def test_derived_type_array(array, indices, fortran_writer, fortran_reader):
    '''This function tests the handling of derived array types.
    '''
    code = f'''module test
        contains
        subroutine tmp()
          use my_mod
          !use my_mod, only: something
          !type(something) :: a, b, c
          integer :: i, j, k
          c(i)%e(j,k) = {array}
        end subroutine tmp
        end module test'''

    schedule = fortran_reader.psyir_from_source(code).children[0]
    node1 = schedule.children[0][0]
    vai1 = VariablesAccessInfo(node1)
    assert isinstance(node1, Assignment)
    assert str(vai1) == "a%b%c: READ, c%e: WRITE, i: READ, j: READ, k: READ"

    # Verify that the index expression is correct. Convert the index
    # expression to a list of list of strings to make this easier:
    sig = Signature(("a", "b", "c"))
    access = vai1[sig][0]
    assert to_fortran(fortran_writer, access.component_indices) == indices


# -----------------------------------------------------------------------------
def test_symbol_array_detection(fortran_reader):
    '''Verifies the handling of arrays together with access information.
    '''

    code = '''program test_prog
              use some_mod
              real, dimension(5,5) :: b, c
              integer :: i
              a = b(i) + c
              end program test_prog'''
    psyir = fortran_reader.psyir_from_source(code)
    scalar_assignment = psyir.children[0]
    symbol_table = scalar_assignment.scope.symbol_table
    sym_a = symbol_table.lookup("a")
    with pytest.raises(InternalError) as error:
        sym_a.is_array_access(index_variable="j")
    assert "In Symbol.is_array_access: index variable 'j' specified, but " \
           "no access information given." in str(error.value)

    vai = VariablesAccessInfo()
    scalar_assignment.reference_accesses(vai)

    # For 'a' we don't have access information, nor symbol table information
    access_info_a = vai[Signature("a")]
    assert not sym_a.is_array_access(access_info=access_info_a)

    # For the access to 'b' we will find array access information:
    access_info_b = vai[Signature("b")]
    sym_b = symbol_table.lookup("b")
    b_is_array = sym_b.is_array_access(access_info=access_info_b)
    assert b_is_array

    # For the access to 'c' we don't have access information, but
    # have symbol table information.
    access_info_c = vai[Signature("c")]
    sym_c = symbol_table.lookup("c")
    c_is_array = sym_c.is_array_access(access_info=access_info_c)
    assert c_is_array

    # Test specifying the index variable. The access to 'b' is
    # considered an array access when ysing the index variable 'i'.
    access_info_b = vai[Signature("b")]
    sym_b = symbol_table.lookup("b")
    b_is_array = sym_b.is_array_access(access_info=access_info_b,
                                       index_variable="i")
    assert b_is_array

    # Verify that the access to 'b' is not considered to be an
    # array access regarding the loop variable 'j' (the access
    # is loop independent):
    b_is_array = sym_b.is_array_access(access_info=access_info_b,
                                       index_variable="j")
    assert not b_is_array


# -----------------------------------------------------------------------------
def test_access_info_clone():
    '''Test that clone() works as expected.'''
    var_info = VariablesAccessInfo()
    assert var_info.array_shape_accesses_are_read is False
    var_info_clone = var_info.clone()
    # Make sure the default gets cloned.
    assert var_info_clone.array_shape_accesses_are_read is False

    var_info = VariablesAccessInfo(array_shape_accesses_are_read=True)
    assert var_info.array_shape_accesses_are_read is True
    var_info_clone = var_info.clone()
    # Make sure the default gets cloned.
    assert var_info_clone.array_shape_accesses_are_read is True


# -----------------------------------------------------------------------------
@pytest.mark.parametrize("function", ["size", "lbound", "ubound"])
def test_shape_access_handling(function, fortran_reader):
    '''Verifies the handling of accesses to arrays using shape operations like
    size, lbound and ubound.
    '''

    code = f'''program test_prog
               real, dimension(5,5) :: b, c
               integer :: i, j
               do i=1, {function}(b, 1)
               enddo
               do i=1, {function}(b, j)
               enddo
               end program test_prog'''
    psyir = fortran_reader.psyir_from_source(code)
    loop1 = psyir.children[0].children[0]
    # When collecting accesses to the shape as read, b must be in the
    # list of accesses
    vai = VariablesAccessInfo(loop1, array_shape_accesses_are_read=True)
    assert str(vai) == "b: READ, i: READ+WRITE"
    # When disabling the collection of read accesses, b must not be in the
    # list of accesses
    vai = VariablesAccessInfo(loop1, array_shape_accesses_are_read=False)
    assert str(vai) == "i: READ+WRITE"

    # Now verify that the read to a variable as dimension parameter
    # will be reported:
    loop2 = psyir.children[0].children[1]
    vai = VariablesAccessInfo(loop2, array_shape_accesses_are_read=True)
    assert str(vai) == "b: READ, i: READ+WRITE, j: READ"
    vai = VariablesAccessInfo(loop2, array_shape_accesses_are_read=False)
    assert str(vai) == "i: READ+WRITE, j: READ"
