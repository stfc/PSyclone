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

'''This module tests the VariablesAccessMap class.'''

import pytest

from psyclone.core import ComponentIndices, Signature, VariablesAccessMap
from psyclone.core.access_type import AccessType
from psyclone.errors import InternalError
from psyclone.psyir.nodes import Assignment, Node
from psyclone.tests.utilities import get_invoke


# -----------------------------------------------------------------------------
def test_variables_access_map():
    '''Test the implementation of VariablesAccessMap, a class that manages
    a list of variables, each with a list of accesses.
    '''
    var_accesses = VariablesAccessMap()
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
    var_accesses2 = VariablesAccessMap()
    var_accesses2.add_access(Signature("new_var"), AccessType.READ, node)
    var_accesses2.add_access(Signature("written"), AccessType.READ, node)

    # Now merge the new instance with the previous instance:
    var_accesses.update(var_accesses2)
    assert str(var_accesses) == "new_var: READ, read: READ, " \
                                "read_written: READ+WRITE, written: READ+WRITE"


# -----------------------------------------------------------------------------
def test_variables_access_map_errors():
    '''Tests if errors are handled correctly. '''
    var_accesses = VariablesAccessMap()
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
    var_accesses = VariablesAccessMap()
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
def test_variables_access_map_update():
    '''Tests the merge operation of VariablesAccessMap.
    '''
    # First create one instance representing for example:
    # a=b; c=d
    var_accesses1 = VariablesAccessMap()
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
    var_accesses2 = VariablesAccessMap()
    var_accesses2.add_access(Signature("f"), AccessType.READ, node)
    var_accesses2.add_access(Signature("e"), AccessType.WRITE, node)
    var_accesses2.next_location()
    var_accesses2.add_access(Signature("h"), AccessType.READ, node)
    var_accesses2.add_access(Signature("g"), AccessType.WRITE, node)

    # Now merge the second instance into the first one
    var_accesses1.update(var_accesses2)

    # The e=f access pattern should have the same location
    # as the c=d (since there is no next_location after
    # adding the b=a access):
    c_accesses = var_accesses1[Signature("c")]
    e_accesses = var_accesses1[Signature("e")]
    assert c_accesses[0].access_type == AccessType.WRITE
    assert e_accesses[0].access_type == AccessType.WRITE
    assert c_accesses[0].location == e_accesses[0].location

    # Test that the g=h part has a higher location than the
    # c=d data. This makes sure that update() increases the
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
    assignments = psyir.walk(Assignment)
    node1 = assignments[0]
    node2 = assignments[1]
    vai1 = node1.reference_accesses()
    assert str(vai1) == "a: WRITE, b: READ, c: READ"
    vai1.update(node2.reference_accesses())
    assert str(vai1) == "a: READ+WRITE, b: READ, c: READ+WRITE"


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
    node1 = fortran_reader.psyir_from_source(code).walk(Assignment)[0]
    vai1 = node1.reference_accesses()
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

    node1 = fortran_reader.psyir_from_source(code).walk(Assignment)[0]
    vai1 = node1.reference_accesses()
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

    vam = scalar_assignment.reference_accesses()

    # For 'a' we don't have access information, nor symbol table information
    access_info_a = vam[Signature("a")]
    assert not sym_a.is_array_access(access_info=access_info_a)

    # For the access to 'b' we will find array access information:
    access_info_b = vam[Signature("b")]
    sym_b = symbol_table.lookup("b")
    b_is_array = sym_b.is_array_access(access_info=access_info_b)
    assert b_is_array

    # For the access to 'c' we don't have access information, but
    # have symbol table information.
    access_info_c = vam[Signature("c")]
    sym_c = symbol_table.lookup("c")
    c_is_array = sym_c.is_array_access(access_info=access_info_c)
    assert c_is_array

    # Test specifying the index variable. The access to 'b' is
    # considered an array access when using the index variable 'i'.
    access_info_b = vam[Signature("b")]
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
@pytest.mark.parametrize("function", ["size", "lbound", "ubound"])
def test_variables_access_map_shape_bounds(fortran_reader, function):
    '''Test that access to an array using shape, or lbound/ubound is marked
    as 'inquiry'.

    '''
    code = f'''module test
        contains
        subroutine tmp()
          real, dimension(:,:), allocatable:: a
          integer :: n
          n = {function}(a, 1)
        end subroutine tmp
        end module test'''
    psyir = fortran_reader.psyir_from_source(code)
    node1 = psyir.walk(Assignment)[0]

    # Array-shape accesses are 'inquiry'
    vam = node1.reference_accesses()
    assert str(vam) == "a: NO_DATA_ACCESS, n: WRITE"


# -----------------------------------------------------------------------------
def test_variables_access_map_domain_loop():
    '''Tests that LFRic domain loop (that do not have an actual loop
    structure, so especially the loop variable is not defined) work as
    expected.
    '''
    _, invoke = get_invoke("25.1_kern_two_domain.f90", "lfric", idx=0)
    vam = invoke.schedule.reference_accesses()
    assert str(vam) == (
        "a: READ, b: READ, f1_data: READWRITE, f2_data: "
        "READWRITE, field_type: NO_DATA_ACCESS, i_def: NO_DATA_ACCESS, "
        "map_w3: READ, ncell_2d_no_halos: "
        "READ, ndf_w3: READ, nlayers_f1: READ, nlayers_f2: READ, "
        "r_def: NO_DATA_ACCESS, undf_w3: READ")


# -----------------------------------------------------------------------------
def test_lfric_access_map():
    '''Test some LFRic specific potential bugs:
    '''

    psy, _ = get_invoke("int_real_literal_scalar.f90", "lfric",
                        dist_mem=False, idx=0)

    schedule = psy.invokes.invoke_list[0].schedule
    vam = schedule.reference_accesses()

    # Make sure literals (e.g. 1_i_def or 2.0_r_def in this example) are not
    # reported as variables in the access list (but that the associated
    # precisions are):
    assert (
        "basis_w1_qr: READ, basis_w3_qr: READ, cell: READ+WRITE, "
        "diff_basis_w2_qr: READ, diff_basis_w3_qr: READ, f1_data: "
        "READ+WRITE, f2_data: READ, field_type: NO_DATA_ACCESS, i_def: "
        "NO_DATA_ACCESS, m1_data: READ, "
        "m2_data: READ, map_w1: READ, map_w2: READ, map_w3: READ, ndf_w1: "
        "READ, ndf_w2: READ, ndf_w3: READ, nlayers_f1: READ, np_xy_qr: READ, "
        "np_z_qr: READ, quadrature_xyoz_type: NO_DATA_ACCESS, "
        "r_def: NO_DATA_ACCESS, undf_w1: READ, undf_w2: READ, "
        "undf_w3: READ, uninitialised_loop0_start: READ, "
        "uninitialised_loop0_stop: READ, "
        "weights_xy_qr: READ, weights_z_qr: READ" == str(vam))
