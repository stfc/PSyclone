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
# Author J. Henrichs, Bureau of Meteorology
# Modifications: A. R. Porter and R. W. Ford, STFC Daresbury Lab

''' Module containing tests for the dependency tools.'''

from __future__ import absolute_import
import pytest

from fparser.common.readfortran import FortranStringReader

from psyclone.configuration import Config
from psyclone.core import Signature, VariablesAccessInfo
from psyclone.errors import InternalError
from psyclone.psyGen import PSyFactory
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.tools.dependency_tools import DependencyTools
from psyclone.tests.utilities import get_invoke


@pytest.fixture(scope="function", autouse=True)
def clear_config_instance():
    '''The tests in this file all assume that the Nemo API is used.'''
    Config.get().api = "nemo"


# -----------------------------------------------------------------------------
def test_messages():
    '''Tests the messaging system of the dependency tools.'''

    dep_tools = DependencyTools()
    # pylint: disable=use-implicit-booleaness-not-comparison
    assert dep_tools.get_all_messages() == []
    dep_tools._add_info("info-test")
    assert dep_tools.get_all_messages()[0] == "Info: info-test"
    dep_tools._add_warning("warning-test")
    assert dep_tools.get_all_messages()[1] == "Warning: warning-test"
    dep_tools._add_error("error-test")
    assert dep_tools.get_all_messages()[2] == "Error: error-test"

    dep_tools._clear_messages()
    # pylint: disable=use-implicit-booleaness-not-comparison
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
        dep_tools.can_loop_be_parallelised(loop, "i")
    assert "node must be an instance of class Loop but got" in str(err.value)


# -----------------------------------------------------------------------------
def test_nested_loop_detection(parser):
    '''Tests if nested loop are handled correctly.
    '''
    reader = FortranStringReader('''program test
                                 integer :: ji, jk
                                 integer, parameter :: jpi=10, jpk=10
                                 real, dimension(jpi,jpi,jpk) :: umask, xmask
                                 do jk = 1, jpk   ! loop 0
                                   umask(1,1,jk) = -1.0d0
                                 end do
                                 do ji = 1, jpi   ! loop 1
                                   xmask(ji,1,1) = -1.0d0
                                 end do
                                 end program test''')
    prog = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(prog)
    loops = psy.invokes.get("test").schedule
    jk_symbol = loops.scope.symbol_table.lookup("jk")
    dep_tools = DependencyTools(["levels", "lat"])

    # Not a nested loop
    parallel = dep_tools.can_loop_be_parallelised(loops[0], jk_symbol)
    assert parallel is False
    assert "Not a nested loop" in dep_tools.get_all_messages()[0]

    # Now disable the test for nested loops:
    parallel = dep_tools.can_loop_be_parallelised(loops[0], jk_symbol, False)
    assert parallel is True
    # Make sure can_loop_be_parallelised clears old messages automatically
    # pylint: disable=use-implicit-booleaness-not-comparison
    assert dep_tools.get_all_messages() == []


# -----------------------------------------------------------------------------
def test_loop_type(parser):
    '''Tests general functionality of can_loop_be_parallelised.
    '''
    reader = FortranStringReader('''program test
                                 integer ji
                                 integer, parameter :: jpi=10
                                 real, dimension(jpi,1,1) :: xmask
                                 do ji = 1, jpi
                                   xmask(ji,1,1) = -1.0d0
                                 end do
                                 end program test''')
    prog = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(prog)
    loop = psy.invokes.get("test").schedule[0]
    dep_tools = DependencyTools(["levels", "lat"])

    # Check a loop that has the wrong loop type
    parallel = dep_tools.can_loop_be_parallelised(loop, "ji", False)
    assert parallel is False
    assert "wrong loop type 'lon'" in dep_tools.get_all_messages()[0]


# -----------------------------------------------------------------------------
def test_arrays_parallelise(parser):
    '''Tests the array checks of can_loop_be_parallelised.
    '''
    reader = FortranStringReader('''program test
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
                                 end program test''')
    prog = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(prog)
    loops = psy.invokes.get("test").schedule
    dep_tools = DependencyTools(["levels", "lat"])

    # Write to array that does not depend on parallel loop variable
    # Test that right default variable name (outer loop jj) is used.
    parallel = dep_tools.can_loop_be_parallelised(loops[0])
    assert parallel is False
    assert ("Variable 'mask' is written to, and does not depend on the loop "
            "variable 'jj'" in dep_tools.get_all_messages()[0])

    jj_symbol = loops.scope.symbol_table.lookup("jj")
    # Write to array that does not depend on parallel loop variable
    parallel = dep_tools.can_loop_be_parallelised(loops[1], jj_symbol)
    assert parallel is True
    # pylint: disable=use-implicit-booleaness-not-comparison
    assert dep_tools.get_all_messages() == []

    # Use parallel loop variable in more than one dimension
    parallel = dep_tools.can_loop_be_parallelised(loops[2], jj_symbol)
    assert parallel is False
    assert ("Variable 'mask' is written to and the loop variable 'jj' is "
            "used in different index locations: mask(jj,jj) and mask(jj,jj)"
            in dep_tools.get_all_messages()[0])

    # Use a stencil access (with write), which prevents parallelisation
    parallel = dep_tools.can_loop_be_parallelised(loops[3], jj_symbol)
    assert parallel is False
    assert ("Variable 'mask' is written and is accessed using indices "
            "'jj + 1' and 'jj' and can therefore not be parallelised"
            in dep_tools.get_all_messages()[0])


# -----------------------------------------------------------------------------
def test_array_access_consistent(parser):
    '''Tests the array checks of can_loop_be_parallelised.
    '''
    # pylint: disable=too-many-locals
    reader = FortranStringReader('''program test
                                 integer ji, jj, jk
                                 integer, parameter :: jpi=5, jpj=10
                                 real, dimension(jpi,jpi) :: a, b, c
                                 do jj = 1, jpj   ! loop 0
                                    do ji = 1, jpi
                                       a(ji, jj) = -1.0d0
                                     end do
                                 end do
                                 do jj = 1, jpj   ! loop 1
                                    do ji = 1, jpi
                                       a(ji, jj) = b(ji, jj)
                                       c(ji, jj) = a(ji, jj)
                                     end do
                                 end do
                                 do jj = 1, jpj   ! loop 2
                                    do ji = 1, jpi
                                       a(jj, ji) = b(ji, jj)
                                     end do
                                 end do
                                 end program test''')
    prog = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(prog)
    loops = psy.invokes.get("test").schedule
    dep_tools = DependencyTools(["levels", "lat"])

    var_info0 = VariablesAccessInfo(loops[0])
    var_info1 = VariablesAccessInfo(loops[1])
    jj_symbol = loops.scope.symbol_table.lookup("jj")

    # Test 1: provide access pattern of different variables:
    sig_a = Signature("a")
    sig_b = Signature("b")
    sig_c = Signature("c")
    with pytest.raises(InternalError) as err:
        _ = dep_tools.array_accesses_consistent(jj_symbol,
                                                [var_info0[sig_a],
                                                 var_info1[sig_b],
                                                 var_info1[sig_c]])
    assert ("Inconsistent signature provided in 'array_accesses_consistent'. "
            "Expected all accesses to be for 'a', but also got 'b,c'."
            in str(err.value))

    # Test 2: provide a consistent list of accesses.
    # Check number of messages and returned access array for correctness.
    a_access_1st_loop = var_info0[sig_a]
    a_access_2nd_loop = var_info1[sig_a]
    all_ind = []
    consistent = dep_tools.array_accesses_consistent(jj_symbol,
                                                     [a_access_1st_loop,
                                                      a_access_2nd_loop],
                                                     all_ind)
    assert consistent is True
    # pylint: disable=use-implicit-booleaness-not-comparison
    assert dep_tools.get_all_messages() == []
    assert len(all_ind) == 3
    assert all_ind[0] == a_access_1st_loop[0].component_indices[(0, 1)]
    assert all_ind[1] == a_access_2nd_loop[0].component_indices[(0, 1)]
    assert all_ind[2] == a_access_2nd_loop[1].component_indices[(0, 1)]

    # Test 3: provide a single instance (not a list).
    all_ind = []
    consistent = dep_tools.array_accesses_consistent(jj_symbol,
                                                     a_access_1st_loop,
                                                     all_ind)
    assert consistent is True
    assert dep_tools.get_all_messages() == []
    assert all_ind == [a_access_1st_loop[0].component_indices[(0, 1)]]

    # Test 4: trigger an error.
    var_info2 = VariablesAccessInfo(loops[2])
    a_access_3rd_loop = var_info2[sig_a]
    all_ind = []
    consistent = dep_tools.array_accesses_consistent(jj_symbol,
                                                     [a_access_1st_loop,
                                                      a_access_3rd_loop],
                                                     all_ind)
    assert consistent is False
    assert len(dep_tools.get_all_messages()) == 1
    assert ("Variable 'a' is written to and the loop variable 'jj' is used "
            "in different index locations: a(ji,jj) and a(jj,ji)."
            in dep_tools.get_all_messages()[0])


# -----------------------------------------------------------------------------
@pytest.mark.parametrize("expression, correct",
                         [("a1(i+i+j)", [set(("i", "j"))]),
                          ("a1(1)", [set()]),
                          ("a2(i+j,2*j+k+1)", [set(("i", "j")),
                                               set(("j", "k"))]),
                          ("a3(i,j,i)", [set("i"), set("j"), set("i")]),
                          ("dv(i)%a(j)%b(k)", [set("i"), set("j"),
                                               set("k")])])
def test_get_indices(expression, correct, parser):
    '''Tests that getting the indices of an array expressions
    works as expected.
    '''
    reader = FortranStringReader(f'''program test
                                 use my_mod, only: my_type
                                 type(my_type) :: dv(10)
                                 integer i, j, k
                                 integer, parameter :: n=10
                                 real, dimension(n) :: a1
                                 real, dimension(n,n) :: a2
                                 real, dimension(n,n,n) :: a3
                                 {expression} = 1
                                 end program test''')
    prog = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(prog)
    assign = psy.invokes.get("test").schedule

    # Get all access info for the expression
    access_info = VariablesAccessInfo(assign)

    # Find the access that is not to i,j, or k --> this must be
    # the 'main' array variable we need to check for:
    sig = None
    loop_vars = set(["i", "j", "k"])
    for sig in access_info:
        if str(sig) not in loop_vars:
            break
    # Get all accesses to the array variable. It has only one
    # access
    access = access_info[sig][0]
    result = DependencyTools.get_flat_indices(access.component_indices,
                                              loop_vars)
    assert result == correct


# -----------------------------------------------------------------------------
@pytest.mark.parametrize("lhs, rhs, partition",
                         [("a1(i+i+j)", "a1(i)", [({"i", "j"}, {0})]),
                          # n is not a loop variable, it must be ignored:
                          ("a1(i+j+n)", "a1(i)", [({"i", "j"}, {0})]),
                          ("a1(i+j+3)", "a1(i)", [({"i", "j"}, {0})]),
                          ("a3(i,j,k)", "a3(i,j,k)", [({"i"}, {0}),
                                                      ({"j"}, {1}),
                                                      ({"k"}, {2})]),
                          ("a3(i,j,k)", "a3(k,j,i)", [({"i", "k"}, {0, 2}),
                                                      ({"j"}, {1})]),
                          ("a3(i,j,k)", "a3(j,k,i)", [({"i", "j", "k"},
                                                       {0, 1, 2})]),
                          ("a4(i,j,k,l)", "a4(j,i,l,k)", [({"i", "j"},
                                                           {0, 1}),
                                                          ({"k", "l"},
                                                           {2, 3})])
                          ])
def test_partition(lhs, rhs, partition, parser):
    '''Tests that getting the indices of an array expressions
    works as expected.
    '''
    reader = FortranStringReader(f'''program test
                                 use my_mod, only: my_type
                                 type(my_type) :: dv(10)
                                 integer i, j, k, l
                                 integer, parameter :: n=10
                                 real, dimension(n) :: a1
                                 real, dimension(n,n) :: a2
                                 real, dimension(n,n,n) :: a3
                                 real, dimension(n, n,n,n) :: a4
                                 {lhs} = {rhs}
                                 end program test''')
    prog = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(prog)
    assign = psy.invokes.get("test").schedule[0]

    # Get all access info for the expression
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
    partition_infos = \
        DependencyTools.partition(access_lhs.component_indices,
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
@pytest.mark.parametrize("declaration, variable",
                         [("integer :: a", "a"),
                          ("type(my_type) :: a", "a%scalar"),
                          ("type(my_type) :: a", "a%sub%scalar")])
def test_scalar_parallelise(declaration, variable, parser):
    '''Tests that scalar variables are correctly handled. It tests for
    'normal' scalar variables ('b') as well as scalar variables in derived
    types ('b%b').

    :param str declaration: the declaration of the variable (e.g. \
        'integer :: b', or 'type(my_type) :: b'')
    :param str variable: the variable (e.g. 'b', or 'b%b')

    '''
    reader = FortranStringReader(f'''program test
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
                                 end program test''')
    prog = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(prog)
    loops = psy.invokes.get("test").schedule
    dep_tools = DependencyTools(["levels", "lat"],
                                language_writer=FortranWriter())

    jj_symbol = loops.scope.symbol_table.lookup("jj")

    # Read only scalar variable: a(ji, jj) = b
    parallel = dep_tools.can_loop_be_parallelised(loops[0], jj_symbol)
    assert parallel is True

    # Write only scalar variable: a(ji, jj) = b
    parallel = dep_tools.can_loop_be_parallelised(loops[1], jj_symbol)
    assert parallel is False
    assert (f"Scalar variable '{variable}' is only written once"
            in dep_tools.get_all_messages()[0])

    # Write to scalar variable happens first
    parallel = dep_tools.can_loop_be_parallelised(loops[2], jj_symbol)
    assert parallel is True

    # Reduction operation on scalar variable
    parallel = dep_tools.can_loop_be_parallelised(loops[3], jj_symbol)
    assert parallel is False
    assert (f"Variable '{variable}' is read first, which indicates a "
            f"reduction." in dep_tools.get_all_messages()[0])


# -----------------------------------------------------------------------------
def test_derived_type(parser):
    ''' Tests assignment to derived type variables. '''
    reader = FortranStringReader('''program test
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
                                 end program test''')
    prog = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(prog)
    loops = psy.invokes.get("test").schedule
    dep_tools = DependencyTools(["levels", "lat"])

    parallel = dep_tools.can_loop_be_parallelised(loops[0])
    assert parallel is False

    # Test that testing is stopped with the first unparallelisable statement
    parallel = dep_tools.can_loop_be_parallelised(loops[1])
    assert parallel is False
    # Test that only one message is stored, i.e. no message for the
    # next assignment to a derived type.
    assert len(dep_tools.get_all_messages()) == 1
    assert ("Variable 'a%b' is written and is accessed using indices 'jj - 1' "
            "and 'jj' and can therefore not be parallelised."
            in dep_tools.get_all_messages()[0])

    parallel = dep_tools.can_loop_be_parallelised(loops[1],
                                                  test_all_variables=True)
    assert parallel is False
    # Now we must have two messages, one for each of the two assignments
    assert len(dep_tools.get_all_messages()) == 2
    assert ("Variable 'a%b' is written and is accessed using indices 'jj - 1' "
            "and 'jj' and can therefore not be parallelised." in
            dep_tools.get_all_messages()[0])
    assert ("Variable 'b%b' is written and is accessed using indices 'jj - 1' "
            "and 'jj' and can therefore not be parallelised." in
            dep_tools.get_all_messages()[1])

    # Test that variables are ignored as expected.
    parallel = dep_tools.\
        can_loop_be_parallelised(loops[1],
                                 signatures_to_ignore=[Signature(("a", "b"))])
    assert parallel is False
    assert len(dep_tools.get_all_messages()) == 1
    assert ("Variable 'b%b' is written and is accessed using indices 'jj - 1' "
            "and 'jj' and can therefore not be parallelised." in
            dep_tools.get_all_messages()[0])

    # If both derived types are ignored, the loop should be marked
    # to be parallelisable
    parallel = dep_tools.\
        can_loop_be_parallelised(loops[1],
                                 signatures_to_ignore=[Signature(("a", "b")),
                                                       Signature(("b", "b"))])
    # pylint: disable=use-implicit-booleaness-not-comparison
    assert dep_tools.get_all_messages() == []
    assert parallel is True


# -----------------------------------------------------------------------------
def test_inout_parameters_nemo(parser):
    '''Test detection of input and output parameters in NEMO.
    '''
    reader = FortranStringReader('''program test
                         integer :: ji, jj, jpi, jpj
                         real :: a(5,5), c(5,5), b
                         do jj = 1, jpj   ! loop 0
                            do ji = 1, jpi
                               a(ji, jj) = b+c(ji, jj)
                             end do
                         end do
                         end program test''')
    prog = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(prog)
    loops = psy.invokes.get("test").schedule

    dep_tools = DependencyTools()
    input_list = dep_tools.get_input_parameters(loops)
    # Use set to be order independent
    input_set = set(input_list)
    assert input_set == set([Signature("b"), Signature("c"),
                             Signature("jpi"), Signature("jpj")])

    output_list = dep_tools.get_output_parameters(loops)
    # Use set to be order independent
    output_set = set(output_list)
    assert output_set == set([Signature("jj"), Signature("ji"),
                              Signature("a")])

    in_list1, out_list1 = dep_tools.get_in_out_parameters(loops)

    assert in_list1 == input_list
    assert out_list1 == output_list


# -----------------------------------------------------------------------------
def test_const_argument():
    '''Check that using a const scalar as parameter works, i.e. is not
    listed as input variable.'''
    _, invoke = get_invoke("test00.1_invoke_kernel_using_const_scalar.f90",
                           api="gocean1.0", idx=0)
    dep_tools = DependencyTools()
    input_list = dep_tools.get_input_parameters(invoke.schedule)
    # Make sure the constant '0' is not listed
    assert "0" not in input_list
    assert Signature("0") not in input_list
