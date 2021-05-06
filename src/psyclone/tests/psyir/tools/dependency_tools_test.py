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
from psyclone.core import Signature
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
    dep_tools = DependencyTools(["levels", "lat"])

    # Not a nested loop
    parallel = dep_tools.can_loop_be_parallelised(loops[0], "jk")
    assert not parallel
    assert "Not a nested loop" in dep_tools.get_all_messages()[0]

    # Now disable the test for nested loops:
    parallel = dep_tools.can_loop_be_parallelised(loops[0], "jk", False)
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

    # Write to array that does not depend on parallel loop variable
    parallel = dep_tools.can_loop_be_parallelised(loops[1], "jj")
    assert parallel
    assert not dep_tools.get_all_messages()

    # Use parallel loop variable in more than one dimension
    parallel = dep_tools.can_loop_be_parallelised(loops[2], "jj")
    assert not parallel
    assert "Variable 'mask' is using loop variable 'jj' in index '(0, 0)'' " \
        "and '(0, 1)'" in dep_tools.get_all_messages()[0]

    # Use a stencil access (with write), which prevents parallelisation
    parallel = dep_tools.can_loop_be_parallelised(loops[3], "jj")
    assert not parallel
    assert "Variable mask is written and is accessed using indices jj + 1 "\
           "and jj and can therefore not be parallelised" \
           in dep_tools.get_all_messages()[0]


# -----------------------------------------------------------------------------
def test_scalar_parallelise(parser):
    '''Tests the scalar checks of can_loop_be_parallelised.
    '''
    reader = FortranStringReader('''program test
                                 integer :: ji, jj, b
                                 integer, parameter :: jpi=7, jpj=9
                                 integer, dimension(jpi,jpj) :: a, c
                                 do jj = 1, jpj   ! loop 0
                                    do ji = 1, jpi
                                       a(ji, jj) = b
                                     end do
                                 end do
                                 do jj = 1, jpj   ! loop 1
                                    do ji = 1, jpi
                                       b = a(ji, jj)
                                     end do
                                 end do
                                 do jj = 1, jpj   ! loop 2
                                    do ji = 1, jpi
                                       b = a(ji, jj)
                                       c(ji, jj) = b*b
                                     end do
                                 end do
                                 do jj = 1, jpj   ! loop 3
                                    do ji = 1, jpi
                                       b = b + a(ji, jj)
                                     end do
                                 end do
                                 end program test''')
    prog = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(prog)
    loops = psy.invokes.get("test").schedule
    dep_tools = DependencyTools(["levels", "lat"])

    # Read only scalar variable: a(ji, jj) = b
    parallel = dep_tools.can_loop_be_parallelised(loops[0], "jj")
    assert parallel

    # Write only scalar variable: a(ji, jj) = b
    parallel = dep_tools.can_loop_be_parallelised(loops[1], "jj")
    assert not parallel
    assert "Scalar variable 'b' is only written once" \
        in dep_tools.get_all_messages()[0]

    # Write to scalar variable happens first
    parallel = dep_tools.can_loop_be_parallelised(loops[2], "jj")
    assert parallel

    # Reduction operation on scalar variable
    parallel = dep_tools.can_loop_be_parallelised(loops[3], "jj")
    assert not parallel
    assert "Variable 'b' is read first, which indicates a reduction."\
        in dep_tools.get_all_messages()[0]


# -----------------------------------------------------------------------------
def test_derived_type(parser):
    ''' Tests assignment to derived type variables. '''
    reader = FortranStringReader('''program test
                                 use my_mod, only: my_type
                                 type(my_type) :: a, b
                                 integer :: ji, jj, jpi, jpj
                                 do jj = 1, jpj   ! loop 0
                                    do ji = 1, jpi
                                       a%b(ji, jj) = 0
                                     end do
                                 end do
                                 do jj = 1, jpj   ! loop 0
                                    do ji = 1, jpi
                                       a%b(ji, jj) = 0
                                       b%b(ji, jj) = 0
                                     end do
                                 end do
                                 end program test''')
    prog = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(prog)
    loops = psy.invokes.get("test").schedule
    dep_tools = DependencyTools(["levels", "lat"])

    parallel = dep_tools.can_loop_be_parallelised(loops[0], "jj")
    assert not parallel
    # TODO #1028 - arrays not yet working

    # Test that testing is stopped with the first unparallelisable statement
    parallel = dep_tools.can_loop_be_parallelised(loops[1], "jj")
    assert not parallel
    # TODO #1028 arrays not yet working
    # Test that only one message is stored, i.e. no message for the
    # next assignment to a derived type.
    assert len(dep_tools.get_all_messages()) == 1

    parallel = dep_tools.can_loop_be_parallelised(loops[1], "jj",
                                                  test_all_variables=True)
    assert not parallel
    # Now we must have two messages, one for each of the two assignments
    assert len(dep_tools.get_all_messages()) == 2

    # Test that variables are ignored as expected.
    parallel = dep_tools.\
        can_loop_be_parallelised(loops[1], "jj",
                                 signatures_to_ignore=[Signature(("a", "b"))])
    assert not parallel
    assert len(dep_tools.get_all_messages()) == 1

    # If both derived types are ignored, the loop should be marked
    # to be parallelisable
    parallel = dep_tools.\
        can_loop_be_parallelised(loops[1], "jj",
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
    assert input_set == set(["b", "c", "jpi", "jpj"])

    output_list = dep_tools.get_output_parameters(loops)
    # Use set to be order independent
    output_set = set(output_list)
    assert output_set == set(["jj", "ji", "a"])

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
    assert input_list == ['p_fld', 'p_fld%grid%subdomain%internal%xstop',
                          'p_fld%grid%tmask']
