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
# Author J. Henrichs, Bureau of Meteorology
# Modifications: A. R. Porter, R. W. Ford and S.Siso, STFC Daresbury Lab

''' Module containing tests for the dependency tools.'''

import pytest

from psyclone.configuration import Config
from psyclone.core import Signature, VariablesAccessInfo
from psyclone.errors import InternalError
from psyclone.psyir.tools import DependencyTools, DTCode, ReadWriteInfo
from psyclone.tests.utilities import get_invoke


@pytest.fixture(scope="function", autouse=True)
def clear_config_instance():
    '''The tests in this file all assume that the Nemo API is used.'''
    Config.get().api = "nemo"


# -----------------------------------------------------------------------------
def test_messages():
    '''Tests the messaging system of the dependency tools.'''

    dep_tools = DependencyTools()
    assert dep_tools.get_all_messages() == []

    # There aren't currently any INFO messages so we invent one by simply
    # adding one to the minimum INFO error code.
    dep_tools._add_message("info-test", DTCode.INFO_MIN+1,
                           ["a", "b"])
    msg = dep_tools.get_all_messages()[0]
    assert str(msg) == "Info: info-test"
    assert msg.code == DTCode.INFO_MIN + 1
    assert msg.var_names == ["a", "b"]

    dep_tools._add_message("warning-test", DTCode.WARN_SCALAR_REDUCTION,
                           ["a"])
    msg = dep_tools.get_all_messages()[1]
    assert str(msg) == "Warning: warning-test"
    assert msg.code == DTCode.WARN_SCALAR_REDUCTION
    assert msg.var_names == ["a"]

    dep_tools._add_message("error-test", DTCode.ERROR_DEPENDENCY, [])
    msg = dep_tools.get_all_messages()[2]
    assert str(msg) == "Error: error-test"
    assert msg.code == DTCode.ERROR_DEPENDENCY
    assert msg.var_names == []

    with pytest.raises(InternalError) as err:
        dep_tools._add_message("INVALID CODE", -999)
    assert "Unknown message code -999." in str(err.value)

    dep_tools._clear_messages()
    assert dep_tools.get_all_messages() == []


# -----------------------------------------------------------------------------
def test_dep_tool_constructor_errors():
    '''Test that invalid loop types raise an error in the constructor.
    '''
    with pytest.raises(TypeError) as err:
        _ = DependencyTools(loop_types_to_parallelise=["lon", "invalid"])
    assert ("Invalid loop type 'invalid' specified in DependencyTools. Valid "
            "values for API 'nemo' are ['lat', 'levels', 'lon', 'tracers', "
            "'unknown']." in str(err.value))

    # Test that a a change to the API works as expected, i.e. does
    # not raise an exception with a valid loop type, but still raises
    # one with an invalid loop type
    Config.get().api = "dynamo0.3"
    _ = DependencyTools(loop_types_to_parallelise=["dof", "colours"])
    with pytest.raises(TypeError) as err:
        _ = DependencyTools(loop_types_to_parallelise=["invalid"])
    assert ("Invalid loop type 'invalid' specified in DependencyTools. Valid "
            "values for API 'dynamo0.3' are ['dof', 'colours', 'colour', '', "
            "'null']." in str(err.value))


# -----------------------------------------------------------------------------
def test_loop_parallelise_errors():
    '''Tests errors that should be raised from the can_loop_be_parallelised
    function.'''

    dep_tools = DependencyTools()
    with pytest.raises(TypeError) as err:
        # The loop object must be a Loop, not e.g. an int:
        loop = 1
        dep_tools.can_loop_be_parallelised(loop)
    assert "node must be an instance of class Loop but got" in str(err.value)


# -----------------------------------------------------------------------------
def test_arrays_parallelise(fortran_reader):
    '''Tests the array checks of can_loop_be_parallelised.
    '''
    source = '''program test
                integer ji, jj, jk
                integer, parameter :: jpi=5, jpj=10
                real, dimension(jpi,jpi) :: mask, umask
                do jj = 1, jpj   ! loop 0
                   do ji = 1, jpi
                      mask(jk, jk) = -1.0d0
                    end do
                end do
                do jj = 1, jpj   ! loop 1
                   do ji = 1, jpi
                      mask(ji, jj) = umask(jk, jk)
                    end do
                end do
                do jj = 1, jpj   ! loop 2
                   do ji = 1, jpi
                      mask(jj, jj) = umask(ji, jj)
                    end do
                end do
                do jj = 1, jpj   ! loop 3
                   do ji = 1, jpi
                      mask(ji, jj) = mask(ji, jj+1)
                    end do
                end do
                end program test'''

    psyir = fortran_reader.psyir_from_source(source)
    loops = psyir.children[0].children[0:4]
    dep_tools = DependencyTools()

    # Write to array that does not depend on the parallel loop variable
    # Test that right default variable name (outer loop jj) is used.
    parallel = dep_tools.can_loop_be_parallelised(loops[0])
    assert parallel is False
    msg = dep_tools.get_all_messages()[0]
    assert ("The write access to 'mask' in 'mask(jk,jk)' causes a write-write "
            "race condition" in str(msg))
    assert msg.code == DTCode.ERROR_WRITE_WRITE_RACE
    assert msg.var_names == ["mask"]

    # Write to array that does not depend on the parallel loop variable
    parallel = dep_tools.can_loop_be_parallelised(loops[1])
    assert parallel is True
    assert dep_tools.get_all_messages() == []

    # Use the parallel loop variable in more than one dimension
    parallel = dep_tools.can_loop_be_parallelised(loops[2])
    assert parallel is True

    # Use a stencil access (with write), which prevents parallelisation
    parallel = dep_tools.can_loop_be_parallelised(loops[3])
    assert parallel is False
    msg = dep_tools.get_all_messages()[0]
    assert ("The write access to 'mask(ji,jj)' and to 'mask(ji,jj + 1)' are "
            "dependent and cannot be parallelised."
            in str(msg))
    assert msg.code == DTCode.ERROR_DEPENDENCY
    assert msg.var_names == ["mask(ji,jj)", "mask(ji,jj + 1)"]


# -----------------------------------------------------------------------------
# This list contains the test cases and expected partition information.
# The first two entries of each 3-tuple are the LHS and RHS. The third
# element is the partition information, which is a list of partitions. Each
# individual partition contains first a set of loop variables used, and
# then the indices where the variables are used. Each index is a 2-tuple,
# used as a component index (see core/component_indices)
@pytest.mark.parametrize("lhs, rhs, partition",
                         [("a1(i+i+j)", "a1(i)", [({"i", "j"}, {(0, 0)})]),
                          # n is not a loop variable, it must be ignored:
                          ("a1(i+j+n)", "a1(i)", [({"i", "j"}, {(0, 0)})]),
                          ("a1(n)", "a1(n)", [(set(), {(0, 0)})]),
                          ("a1(i+j+3)", "a1(i)", [({"i", "j"}, {(0, 0)})]),
                          ("dv(i)%a(j)", "dv(i+1)%a(j+1)",
                           [({"i"}, {(0, 0)}),
                            ({"j"}, {(1, 0)})]),
                          ("dv(i)%a(n)", "dv(n)%a(n)",
                           [({"i"}, {(0, 0)}),
                            (set(), {(1, 0)})]),
                          ("dv(i)%a(i+j+3)%c(k)", "dv(i)%a(i)%c(k)",
                           [({"i", "j"}, {(0, 0), (1, 0)}),
                            ({"k"}, {(2, 0)})]),
                          ("a3(i,j,k)", "a3(i,j,k)", [({"i"}, {(0, 0)}),
                                                      ({"j"}, {(0, 1)}),
                                                      ({"k"}, {(0, 2)})]),
                          ("a3(i,j,k)", "a3(k,j,i)", [({"i", "k"}, {(0, 0),
                                                                    (0, 2)}),
                                                      ({"j"}, {(0, 1)})]),
                          ("a3(i,j,k)", "a3(j,k,i)", [({"i", "j", "k"},
                                                       {(0, 0), (0, 1),
                                                        (0, 2)})]),
                          ("a4(i,j,k,l)", "a4(j,i,l,k)", [({"i", "j"},
                                                           {(0, 0), (0, 1)}),
                                                          ({"k", "l"},
                                                           {(0, 2), (0, 3)})])
                          ])
def test_partition(lhs, rhs, partition, fortran_reader):
    '''Tests that partitioning accesses to array variables works.
    '''
    source = f'''program test
                 use my_mod, only: my_type
                 type(my_type) :: dv(10)
                 integer i, j, k, l
                 integer, parameter :: n=10
                 real, dimension(n) :: a1
                 real, dimension(n,n) :: a2
                 real, dimension(n,n,n) :: a3
                 real, dimension(n, n,n,n) :: a4
                 {lhs} = {rhs}
                 end program test'''
    psyir = fortran_reader.psyir_from_source(source)
    assign = psyir.children[0].children[0]

    # Get all access info for the expression
    access_info_lhs = VariablesAccessInfo(assign.lhs)
    access_info_rhs = VariablesAccessInfo(assign.rhs)

    # Find the access that is an array (and therefore not a loop variable)
    #  --> this must be the 'main' array variable we need to check for:
    sig = None
    for sig in access_info_lhs:
        if access_info_lhs[sig].is_array():
            break

    # Get all accesses to the array variable. It has only one
    # access on each side (left/right)
    access_lhs = access_info_lhs[sig][0]
    access_rhs = access_info_rhs[sig][0]
    partition_infos = \
        DependencyTools._partition(access_lhs.component_indices,
                                   access_rhs.component_indices,
                                   ["i", "j", "k", "l"])
    # The order of the results could be different if the code is changed,
    # so keep this test as flexible as possible:
    # First check that we have the same number of elements
    assert len(partition_infos) == len(partition)
    # Then check if each element returns is in the correct result.
    for i, part_info in enumerate(partition_infos):
        correct = partition[i]
        # Check that the variables in each partition are identical.
        # Note that the variables are stores as sets, so order does
        # not matter:
        assert correct[0] == part_info[0]

        # Then check that the partition indices are the same as well.
        # The partition function returns the indices as lists, so
        # convert them to sets to get an order independent comparison:
        assert correct[1] == set(part_info[1])


# -----------------------------------------------------------------------------
@pytest.mark.parametrize("lhs, rhs, is_dependent",
                         [("a1(n)", "a1(n)", False),
                          ("a1(1)", "a1(3)", True),
                          ("a1(n-1)", "a1(n-1)", False),
                          ("a1(n-1)", "a1(n-2)", True),
                          ("a1(n)", "a1(5)", False),
                          ("a1(n)", "a1(m)", False),
                          ])
def test_array_access_pairs_0_vars(lhs, rhs, is_dependent, fortran_reader):
    '''Tests that array indices that do not use a loop variable are
    handled correctly.
    '''
    source = f'''program test
                 integer, parameter :: n=10, m=11
                 real, dimension(n) :: a1
                 {lhs} = {rhs}
                 end program test'''
    psyir = fortran_reader.psyir_from_source(source)
    assign = psyir.children[0].children[0]

    sig = Signature("a1")
    # Get all access info for the expression to 'a1'
    access_info_lhs = VariablesAccessInfo(assign.lhs)[sig][0]
    access_info_rhs = VariablesAccessInfo(assign.rhs)[sig][0]
    index = (0, 0)
    lhs_index0 = access_info_lhs.component_indices[index]
    rhs_index0 = access_info_rhs.component_indices[index]

    result = DependencyTools._independent_0_var(lhs_index0, rhs_index0)
    assert result is is_dependent


# -----------------------------------------------------------------------------
@pytest.mark.parametrize("lhs, rhs, distance",
                         [("a1(i)", "a1(i)", 0),
                          # This expression does not depend on the variable
                          # 'i' at all:
                          ("a1(j)", "a1(j)", None),
                          ("a1(indx(i))", "a1(indx(i))", None),
                          ("a1(i)", "a1(2)", None),
                          ("a1(i+1)", "a1(i)", 1),
                          ("a1(i)", "a1(i+1)", -1),
                          ("a1(i-1)", "a1(i+2)", -3),
                          ("a1(i+1)", "a1(i-2)", 3),
                          ("a1(2*i)", "a1(i+1)", None),
                          ("a1(i*i)", "a1(i*i)", None),
                          ("a1(i-d_i)", "a1(i-d_i)", 0),
                          ("a1(2*i)", "a1(2*i+1)", None),
                          ("a1(i*i)", "a1(-1)", None),
                          ("a1(i-i+2)", "a1(2)", None),
                          # Test the handling of array ranges. This is not
                          # yet supported (TODO #2168), so it will always
                          # return None, indicating an overlap.
                          ("a1(:)", "a1(:)", None),
                          # TODO #2168 A more complex case of distance
                          # computation with a range - this needs to compare
                          # the lower value of the left expression (i) with
                          # the upper value of the right expression (i-1) etc
                          ("a1(i:i+2)", "a1(i-3:i-1)", None),
                          # The /= is parsed as valid Fortran,
                          # but not converted into valid SymPy.
                          # It will therefore raise an exception,
                          # which we want to test:
                          ("a1(:)", "a1(mt%x(:) /= 1)+1", None),
                          ])
def test_array_access_pairs_1_var(lhs, rhs, distance, fortran_reader):
    '''Tests the array checks of can_loop_be_parallelised.
    '''
    source = f'''program test
                 use my_type_mod
                 integer i, j, k, d_i
                 integer, parameter :: n=10, m=10
                 integer, dimension(10, 10) :: indx
                 real, dimension(n) :: a1
                 real, dimension(n, m) :: a2
                 real, dimension(n, m, n) :: a3
                 type(my_type) :: mt
                 {lhs} = {rhs}
                 end program test'''
    psyir = fortran_reader.psyir_from_source(source)
    assign = psyir.children[0].children[0]

    sig = Signature("a1")
    # Get all access info for the expression to 'a1'
    access_info_lhs = VariablesAccessInfo(assign.lhs)[sig][0]
    access_info_rhs = VariablesAccessInfo(assign.rhs)[sig][0]
    subscript_lhs = access_info_lhs.component_indices[(0, 0)]
    subscript_rhs = access_info_rhs.component_indices[(0, 0)]

    result = DependencyTools._get_dependency_distance("i", subscript_lhs,
                                                      subscript_rhs)
    assert result == distance


# -----------------------------------------------------------------------------
@pytest.mark.parametrize("lhs, rhs, independent",
                         [("a3(i, j, 1)", "a3(i, i, 1)", True),
                          ("a2(i, i)", "a2(i+j, i)", True),
                          ("a2(i, j)", "a2(i+j, j)", False),
                          ("a2(i, j)", "a2(j, i)", False),
                          ("a3(i, j, indx(i,j))", "a3(i, i, indx(i,j))", True),
                          ])
def test_array_access_pairs_multi_var(lhs, rhs, independent, fortran_reader):
    '''Tests the array checks of can_loop_be_parallelised.
    '''
    source = f'''program test
                 integer i, j, k, d_i
                 integer, parameter :: n=10, m=10
                 integer, dimension(10, 10) :: indx
                 real, dimension(n) :: a1
                 real, dimension(n, m) :: a2
                 real, dimension(n, m, n) :: a3
                 {lhs} = {rhs}
                 end program test'''
    psyir = fortran_reader.psyir_from_source(source)
    assign = psyir.children[0].children[0]

    access_info_lhs = VariablesAccessInfo(assign.lhs)
    access_info_rhs = VariablesAccessInfo(assign.rhs)

    # Find the access that is not to i,j, or k --> this must be
    # the 'main' array variable we need to check for:
    sig = None
    for sig in access_info_lhs:
        if access_info_lhs[sig].is_array():
            break

    # Get all accesses to the array variable. It has only one
    # access on each side (left/right)
    access_lhs = access_info_lhs[sig][0]
    access_rhs = access_info_rhs[sig][0]
    partition = \
        DependencyTools._partition(access_lhs.component_indices,
                                   access_rhs.component_indices,
                                   ["i", "j", "k", "l"])

    # Get all access info for the expression to 'a1'
    access_info_lhs = VariablesAccessInfo(assign.lhs)[sig][0]
    access_info_rhs = VariablesAccessInfo(assign.rhs)[sig][0]
    # The variable partition contains a list, each element being a pair of
    # a variable set (element 0) and subscripts indices (element 1).
    # So partition[0][1] takes the subscript indices ([1]) of the first
    # partition ([0])
    result = DependencyTools.\
        _independent_multi_subscript("i", access_info_lhs, access_info_rhs,
                                     partition[0][1])
    assert result == independent


# -----------------------------------------------------------------------------
@pytest.mark.parametrize("lhs, rhs, is_parallelisable",
                         [("a2(k, n)", "a2(k, n)", True),
                          ("a2(k*k-k+n, 1)", "a2(k+n, 2)", False),
                          ("a2(k+d_k, 1)", "a2(k+d_k, 2)", True),
                          ("a2(k, 1)", "a2(k, 2)", True),
                          ("a2(k, n-1)", "a2(k, n-1)", True),
                          ("a2(k, n)", "a2(k, n-1)", True),
                          # J is an inside loop, and so will take several
                          # values, which will result in a race condition
                          ("a2(k+j, n)", "a2(k+j, n-1)", False),
                          # 'n' is not a loop variable, and as such is a
                          # constant in the loop and will not cause a
                          # race condition if parallelised
                          ("a2(k+n, n)", "a2(k+n, n-1)", True),
                          ("a1(n)", "a1(m)", False),
                          ])
def test_improved_dependency_analysis(lhs, rhs, is_parallelisable,
                                      fortran_reader):
    '''Tests the array checks of can_loop_be_parallelised.
    '''
    source = f'''program test
                 integer j, k, d_k
                 integer, parameter :: n=10, m=10
                 integer, dimension(10, 10) :: indx
                 real, dimension(n) :: a1
                 real, dimension(n, m) :: a2
                 real, dimension(n, m, n) :: a3
                 do k = 1, n
                    do j = 1, m
                       {lhs} = {rhs}
                    end do
                 end do
                 end program test'''
    psyir = fortran_reader.psyir_from_source(source)
    loop = psyir.children[0].children[0]
    dep_tools = DependencyTools()
    result = dep_tools.can_loop_be_parallelised(loop)
    assert result is is_parallelisable


# -----------------------------------------------------------------------------
@pytest.mark.parametrize("declaration, variable",
                         [("integer :: a", "a"),
                          ("type(my_type) :: a", "a%scalar"),
                          ("type(my_type) :: a", "a%sub%scalar")])
def test_scalar_parallelise(declaration, variable, fortran_reader):
    '''Tests that scalar variables are correctly handled. It tests for
    'normal' scalar variables ('b') as well as scalar variables in derived
    types ('b%b').

    :param str declaration: the declaration of the variable (e.g. \
        'integer :: b', or 'type(my_type) :: b'')
    :param str variable: the variable (e.g. 'b', or 'b%b')

    '''
    source = f'''program test
                 implicit none
                 type :: my_sub_type
                    integer :: scalar
                 end type my_sub_type
                 type :: my_type
                    integer :: scalar
                    type(my_sub_type) :: sub
                 end type my_type
                 {declaration}  ! Declaration of variable here
                 integer :: ji, jj
                 integer, parameter :: jpi=7, jpj=9
                 integer, dimension(jpi,jpj) :: b, c
                 do jj = 1, jpj   ! loop 0
                    do ji = 1, jpi
                       b(ji, jj) = {variable}
                     end do
                 end do
                 do jj = 1, jpj   ! loop 1
                    do ji = 1, jpi
                       {variable} = b(ji, jj)
                     end do
                 end do
                 do jj = 1, jpj   ! loop 2
                    do ji = 1, jpi
                       {variable} = b(ji, jj)
                       c(ji, jj) = {variable}*{variable}
                     end do
                 end do
                 do jj = 1, jpj   ! loop 3
                    do ji = 1, jpi
                       {variable} = {variable} + b(ji, jj)
                     end do
                 end do
                 end program test'''
    psyir = fortran_reader.psyir_from_source(source)
    loops = psyir.children[0].children
    dep_tools = DependencyTools()

    jj_symbol = psyir.children[0].scope.symbol_table.lookup("jj")

    # Read only scalar variable: a(ji, jj) = b
    parallel = dep_tools.can_loop_be_parallelised(loops[0])
    assert parallel is True

    # Write only scalar variable: a(ji, jj) = b
    parallel = dep_tools.can_loop_be_parallelised(loops[1])
    assert parallel is False
    msg = dep_tools.get_all_messages()[0]
    assert (f"Scalar variable '{variable}' is only written once"
            in str(msg))
    assert msg.code == DTCode.WARN_SCALAR_WRITTEN_ONCE
    assert msg.var_names == [f"{variable}"]

    # Write to scalar variable happens first
    parallel = dep_tools.can_loop_be_parallelised(loops[2], jj_symbol)
    assert parallel is True

    # Reduction operation on scalar variable
    parallel = dep_tools.can_loop_be_parallelised(loops[3], jj_symbol)
    msg = dep_tools.get_all_messages()[0]
    assert parallel is False
    assert (f"Variable '{variable}' is read first, which indicates a "
            f"reduction." in str(msg))
    assert msg.code == DTCode.WARN_SCALAR_REDUCTION
    assert msg.var_names == [f"{variable}"]


# -----------------------------------------------------------------------------
def test_derived_type(fortran_reader):
    ''' Tests assignment to derived type variables. '''
    source = '''program test
                use my_mod, only: my_type
                type(my_type) :: a, b
                integer :: ji, jj, jpi, jpj
                do jj = 1, jpj   ! loop 0
                   do ji = 1, jpi
                      a%b(ji, jj) = a%b(ji, jj-1)+1
                    end do
                end do
                do jj = 1, jpj   ! loop 0
                   do ji = 1, jpi
                      a%b(ji, jj) = a%b(ji, jj-1)+1
                      b%b(ji, jj) = b%b(ji, jj-1)+1
                    end do
                end do
                end program test'''
    psyir = fortran_reader.psyir_from_source(source)
    loops = psyir.children[0].children

    dep_tools = DependencyTools()

    parallel = dep_tools.can_loop_be_parallelised(loops[0])
    assert parallel is False

    # Test that testing is stopped with the first unparallelisable statement
    parallel = dep_tools.can_loop_be_parallelised(loops[1])
    assert parallel is False
    # Test that only one message is stored, i.e. no message for the
    # next assignment to a derived type.
    assert len(dep_tools.get_all_messages()) == 1
    msg = dep_tools.get_all_messages()[0]
    assert ("The write access to 'a%b(ji,jj)' and to 'a%b(ji,jj - 1)' are "
            "dependent and cannot be parallelised." in str(msg))
    assert msg.code == DTCode.ERROR_DEPENDENCY
    assert msg.var_names == ["a%b(ji,jj)", "a%b(ji,jj - 1)"]

    parallel = dep_tools.can_loop_be_parallelised(loops[1],
                                                  test_all_variables=True)
    assert parallel is False
    # Now we must have two messages, one for each of the two assignments
    assert len(dep_tools.get_all_messages()) == 2
    msg = dep_tools.get_all_messages()[0]
    assert ("The write access to 'a%b(ji,jj)' and to 'a%b(ji,jj - 1)' are "
            "dependent and cannot be parallelised." in str(msg))
    assert msg.var_names == ["a%b(ji,jj)", "a%b(ji,jj - 1)"]
    msg = dep_tools.get_all_messages()[1]
    assert ("The write access to 'b%b(ji,jj)' and to 'b%b(ji,jj - 1)' are "
            "dependent and cannot be parallelised." in str(msg))
    assert msg.code == DTCode.ERROR_DEPENDENCY
    assert msg.var_names == ["b%b(ji,jj)", "b%b(ji,jj - 1)"]

    # Test that variables are ignored as expected.
    parallel = dep_tools.\
        can_loop_be_parallelised(loops[1],
                                 signatures_to_ignore=[Signature(("a", "b"))])
    assert parallel is False
    assert len(dep_tools.get_all_messages()) == 1
    msg = dep_tools.get_all_messages()[0]
    assert ("he write access to 'b%b(ji,jj)' and to 'b%b(ji,jj - 1)' are "
            "dependent and cannot be parallelised" in str(msg))
    assert msg.code == DTCode.ERROR_DEPENDENCY
    assert msg.var_names == ["b%b(ji,jj)", "b%b(ji,jj - 1)"]

    # If both derived types are ignored, the loop should be marked
    # to be parallelisable
    parallel = dep_tools.\
        can_loop_be_parallelised(loops[1],
                                 signatures_to_ignore=[Signature(("a", "b")),
                                                       Signature(("b", "b"))])
    assert dep_tools.get_all_messages() == []
    assert parallel is True


# -----------------------------------------------------------------------------
def test_inout_parameters_nemo(fortran_reader):
    '''Test detection of input and output parameters in NEMO.
    '''
    source = '''program test
                integer :: ji, jj, jpj
                real :: a(5,5), c(5,5), b, dummy(5,5)
                do jj = lbound(dummy,1), jpj   ! loop 0
                   do ji = lbound(dummy,2), ubound(dummy,2)
                      a(ji, jj) = b+c(ji, jj)
                    end do
                end do
                end program test'''
    psyir = fortran_reader.psyir_from_source(source)
    loops = psyir.children[0].children

    dep_tools = DependencyTools()
    read_write_info_read = ReadWriteInfo()
    dep_tools.get_input_parameters(read_write_info_read, loops)
    # Use set to be order independent
    input_set = set(read_write_info_read.signatures_read)
    # Note that by default the read access to `dummy` in lbound etc should
    # not be reported, since it does not really read the array values.
    assert input_set == set([Signature("b"), Signature("c"),
                             Signature("jpj")])

    read_write_info_write = ReadWriteInfo()
    dep_tools.get_output_parameters(read_write_info_write, loops)
    # Use set to be order independent
    output_set = set(read_write_info_write.signatures_written)
    assert output_set == set([Signature("jj"), Signature("ji"),
                              Signature("a")])

    read_write_info_all = dep_tools.get_in_out_parameters(loops)

    assert read_write_info_read.read_list == read_write_info_all.read_list
    assert read_write_info_write.write_list == read_write_info_all.write_list

    # Check that we can also request to get the access to 'dummy'
    # inside the ubound/lbound function calls.
    read_write_info = ReadWriteInfo()
    dep_tools.get_input_parameters(read_write_info, loops,
                                   options={'COLLECT-ARRAY-SHAPE-READS': True})
    input_set = set(sig for _, sig in read_write_info.set_of_all_used_vars)
    assert input_set == set([Signature("b"), Signature("c"),
                             Signature("jpj"), Signature("dummy")])

    read_write_info = dep_tools.\
        get_in_out_parameters(loops,
                              options={'COLLECT-ARRAY-SHAPE-READS': True})
    output_set = set(read_write_info.signatures_read)
    assert output_set == set([Signature("b"), Signature("c"),
                              Signature("jpj"), Signature("dummy")])


# -----------------------------------------------------------------------------
def test_const_argument():
    '''Check that using a const scalar as parameter works, i.e. is not
    listed as input variable.'''
    _, invoke = get_invoke("test00.1_invoke_kernel_using_const_scalar.f90",
                           api="gocean1.0", idx=0)
    dep_tools = DependencyTools()
    read_write_info = ReadWriteInfo()
    dep_tools.get_input_parameters(read_write_info, invoke.schedule)
    # Make sure the constant '0' is not listed
    assert "0" not in read_write_info.signatures_read
    assert Signature("0") not in read_write_info.signatures_read


# -----------------------------------------------------------------------------
def test_da_array_expression(fortran_reader):
    '''Test that a mixture of using the same array with and without indices
    does not cause a crash and correctly disables parallelisation.
    '''
    source = '''program test
                integer :: ji, jj, jpi, jpj
                real :: a(5,5)
                do jj = 1, jpj   ! loop 0
                   do ji = 1, jpi
                      a(ji, jj) = a*a
                    end do
                end do
                end program test'''
    psyir = fortran_reader.psyir_from_source(source)
    loops = psyir.children[0].children

    dep_tools = DependencyTools()
    result = dep_tools.can_loop_be_parallelised(loops[0])
    assert result is False


# -----------------------------------------------------------------------------
def test_reserved_words(fortran_reader):
    '''Tests that using a reserved Python word ('lambda' here') as a loop
    variable, which will be renamed when converting to SymPy, works as
    expected. Also make sure that name clashes are handled as expected by
    declaring local symbols lambda_1 (which will clash with the renamed
    lambda) and lambda_1_1 (which will clash with the renamed lambda_1).
    Otherwise this test is identical to test_arrays_parallelise.
    '''
    source = '''program test
                integer ji, lambda, jk
                integer, parameter :: jpi=5, jpj=10
                real, dimension(jpi,jpi) :: mask, umask, lambda_1, lambda_1_1
                do lambda = 1, jpj   ! loop 0
                   do ji = 1, jpi
                      mask(jk, jk) = -1.0d0
                    end do
                end do
                do lambda = 1, jpj   ! loop 1
                   do ji = 1, jpi
                      mask(ji, lambda+lambda_1+lambda_1_1) = umask(jk, jk)
                    end do
                end do
                do lambda = 1, jpj   ! loop 2
                   do ji = 1, jpi
                      mask(lambda, lambda) = umask(ji, lambda)
                    end do
                end do
                do lambda = 1, jpj   ! loop 3
                   do ji = 1, jpi
                      mask(ji, lambda) = mask(ji, lambda+1)
                    end do
                end do
                end program test'''

    psyir = fortran_reader.psyir_from_source(source)
    loops = psyir.children[0].children[0:4]
    dep_tools = DependencyTools()

    # Write to array that does not depend on the parallel loop variable
    # Test that right default variable name (outer loop lambda) is used.
    parallel = dep_tools.can_loop_be_parallelised(loops[0])
    assert parallel is False
    msg = dep_tools.get_all_messages()[0]
    assert ("The write access to 'mask' in 'mask(jk,jk)' causes a write-write "
            "race condition" in str(msg))
    assert msg.code == DTCode.ERROR_WRITE_WRITE_RACE
    assert msg.var_names == ["mask"]

    # Write to array that does not depend on the parallel loop variable
    parallel = dep_tools.can_loop_be_parallelised(loops[1])
    assert parallel is True
    assert dep_tools.get_all_messages() == []

    # Use the parallel loop variable in more than one dimension
    parallel = dep_tools.can_loop_be_parallelised(loops[2])
    assert parallel is True

    # Use a stencil access (with write), which prevents parallelisation
    parallel = dep_tools.can_loop_be_parallelised(loops[3])
    assert parallel is False
    msg = dep_tools.get_all_messages()[0]
    assert ("The write access to 'mask(ji,lambda)' and to "
            "'mask(ji,lambda + 1)' are dependent and cannot be parallelised."
            in str(msg))
    assert msg.code == DTCode.ERROR_DEPENDENCY
    assert msg.var_names == ["mask(ji,lambda)", "mask(ji,lambda + 1)"]
