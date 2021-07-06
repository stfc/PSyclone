# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2021, Science and Technology Facilities Council.
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
from psyclone.core import Signature, VariablesAccessInfo
from psyclone.errors import InternalError
from psyclone.psyGen import PSyFactory
from psyclone.psyir.tools.dependency_tools import DependencyTools
from psyclone.tests.utilities import get_invoke


# -----------------------------------------------------------------------------
def test_messages():
    '''Tests the messaging system of the dependency tools.'''

    dep_tools = DependencyTools()
    assert dep_tools.get_all_messages() == []
    dep_tools._add_info("info-test")
    assert dep_tools.get_all_messages()[0] == "Info: info-test"
    dep_tools._add_warning("warning-test")
    assert dep_tools.get_all_messages()[1] == "Warning: warning-test"
    dep_tools._add_error("error-test")
    assert dep_tools.get_all_messages()[2] == "Error: error-test"

    dep_tools._clear_messages()
    assert dep_tools.get_all_messages() == []


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
    assert not parallel
    assert "Not a nested loop" in dep_tools.get_all_messages()[0]

    # Now disable the test for nested loops:
    parallel = dep_tools.can_loop_be_parallelised(loops[0], jk_symbol, False)
    assert parallel
    # Make sure can_loop_be_parallelised clears old messages automatically
    assert not dep_tools.get_all_messages()


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
    assert not parallel
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
    assert not parallel
    assert "Variable 'mask' is written to, and does not depend on the loop "\
           "variable 'jj'" in dep_tools.get_all_messages()[0]

    jj_symbol = loops.scope.symbol_table.lookup("jj")
    # Write to array that does not depend on parallel loop variable
    parallel = dep_tools.can_loop_be_parallelised(loops[1], jj_symbol)
    assert parallel
    assert not dep_tools.get_all_messages()

    # Use parallel loop variable in more than one dimension
    parallel = dep_tools.can_loop_be_parallelised(loops[2], jj_symbol)
    assert not parallel
    assert "Variable 'mask' is written to and the loop variable 'jj' is " \
           "used differently: mask(jj,jj) and mask(jj,jj)" \
           in dep_tools.get_all_messages()[0]

    # Use a stencil access (with write), which prevents parallelisation
    parallel = dep_tools.can_loop_be_parallelised(loops[3], jj_symbol)
    assert not parallel
    assert "Variable mask is written and is accessed using indices jj + 1 "\
           "and jj and can therefore not be parallelised" \
           in dep_tools.get_all_messages()[0]


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
    assert "Inconsistent signature provided in 'array_accesses_consistent'. " \
           "Expected all access to be for 'a', but also got 'b,c'."\
           in str(err.value)

    # Test 2: provide a consistent list of accesses.
    # Check number of messages and returned access array for correctness.
    a_access_1st_loop = var_info0[sig_a]
    a_access_2nd_loop = var_info1[sig_a]
    all_ind = []
    consistent = dep_tools.array_accesses_consistent(jj_symbol,
                                                     [a_access_1st_loop,
                                                      a_access_2nd_loop],
                                                     all_ind)
    assert consistent
    assert len(dep_tools.get_all_messages()) == 0
    assert len(all_ind) == 3
    assert all_ind[0] == a_access_1st_loop[0].component_indices[(0, 1)]
    assert all_ind[1] == a_access_2nd_loop[0].component_indices[(0, 1)]
    assert all_ind[2] == a_access_2nd_loop[1].component_indices[(0, 1)]

    # Test 3: provide a single instance (not a list).
    all_ind = []
    consistent = dep_tools.array_accesses_consistent(jj_symbol,
                                                     a_access_1st_loop,
                                                     all_ind)
    assert consistent
    assert len(dep_tools.get_all_messages()) == 0
    assert all_ind == [a_access_1st_loop[0].component_indices[(0, 1)]]

    # Test 4: trigger an error.
    var_info2 = VariablesAccessInfo(loops[2])
    a_access_3rd_loop = var_info2[sig_a]
    all_ind = []
    consistent = dep_tools.array_accesses_consistent(jj_symbol,
                                                     [a_access_1st_loop,
                                                      a_access_3rd_loop],
                                                     all_ind)
    assert not consistent
    assert len(dep_tools.get_all_messages()) == 1
    assert "Variable 'a' is written to and the loop variable 'jj' is used " \
           "differently: a(ji,jj) and a(jj,ji)." \
           in dep_tools.get_all_messages()[0]


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
    reader = FortranStringReader('''program test
                                 implicit none
                                 type :: my_sub_type
                                    integer :: scalar
                                 end type my_sub_type
                                 type :: my_type
                                    integer :: scalar
                                    type(my_sub_type) :: sub
                                 end type my_type

                                 {0}  ! Declaration of variable here
                                 integer :: ji, jj
                                 integer, parameter :: jpi=7, jpj=9
                                 integer, dimension(jpi,jpj) :: b, c
                                 do jj = 1, jpj   ! loop 0
                                    do ji = 1, jpi
                                       b(ji, jj) = {1}
                                     end do
                                 end do
                                 do jj = 1, jpj   ! loop 1
                                    do ji = 1, jpi
                                       {1} = b(ji, jj)
                                     end do
                                 end do
                                 do jj = 1, jpj   ! loop 2
                                    do ji = 1, jpi
                                       {1} = b(ji, jj)
                                       c(ji, jj) = {1}*{1}
                                     end do
                                 end do
                                 do jj = 1, jpj   ! loop 3
                                    do ji = 1, jpi
                                       {1} = {1} + b(ji, jj)
                                     end do
                                 end do
                                 end program test'''.format(declaration,
                                                            variable))
    prog = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(prog)
    loops = psy.invokes.get("test").schedule
    dep_tools = DependencyTools(["levels", "lat"])

    # Test if supplying the loop name as symbol also works:
    jj_symbol = loops.scope.symbol_table.lookup("jj")

    # Read only scalar variable: a(ji, jj) = b
    parallel = dep_tools.can_loop_be_parallelised(loops[0], jj_symbol)
    assert parallel

    # Write only scalar variable: a(ji, jj) = b
    parallel = dep_tools.can_loop_be_parallelised(loops[1], jj_symbol)
    assert not parallel
    assert "Scalar variable '{0}' is only written once".format(variable) \
        in dep_tools.get_all_messages()[0]

    # Write to scalar variable happens first
    parallel = dep_tools.can_loop_be_parallelised(loops[2], jj_symbol)
    assert parallel

    # Reduction operation on scalar variable
    parallel = dep_tools.can_loop_be_parallelised(loops[3], jj_symbol)
    assert not parallel
    assert "Variable '{0}' is read first, which indicates a reduction."\
        .format(variable) in dep_tools.get_all_messages()[0]


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
    assert not parallel

    # Test that testing is stopped with the first unparallelisable statement
    parallel = dep_tools.can_loop_be_parallelised(loops[1])
    assert not parallel
    # Test that only one message is stored, i.e. no message for the
    # next assignment to a derived type.
    assert len(dep_tools.get_all_messages()) == 1

    parallel = dep_tools.can_loop_be_parallelised(loops[1],
                                                  test_all_variables=True)
    assert not parallel
    # Now we must have two messages, one for each of the two assignments
    assert len(dep_tools.get_all_messages()) == 2

    # Test that variables are ignored as expected.
    parallel = dep_tools.\
        can_loop_be_parallelised(loops[1],
                                 signatures_to_ignore=[Signature(("a", "b"))])
    assert not parallel
    assert len(dep_tools.get_all_messages()) == 1

    # If both derived types are ignored, the loop should be marked
    # to be parallelisable
    parallel = dep_tools.\
        can_loop_be_parallelised(loops[1],
                                 signatures_to_ignore=[Signature(("a", "b")),
                                                       Signature(("b", "b"))])
    assert len(dep_tools.get_all_messages()) == 0
    assert parallel


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
    assert input_list == [Signature('p_fld'),
                          Signature(('p_fld', 'grid', 'subdomain',
                                     'internal', 'xstop')),
                          Signature(('p_fld', 'grid', 'tmask'))]
