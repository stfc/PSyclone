# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''A module to perform pytest tests on the code in the adjoint_visitor
file within the psyad directory.

'''
# from __future__ import print_function, absolute_import
import logging

import pytest

from psyclone.psyad import AdjointVisitor
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import FileContainer, Assignment, Loop, Schedule
from psyclone.tests.utilities import Compile


def check_adjoint(tl_fortran, active_variable_names, expected_ad_fortran,
                  tmpdir):
    '''Utility routine that takes tangent linear fortran code as input in
    the argument 'tl_fortran', transforms this code into its adjoint
    using the active variables specified in the
    'active_variable_names' argument and tests whether the result is
    the same as the expected result in the 'expected_ad_fortran'
    argument.

    To help keep the test code short this routine also adds the
    subroutine / end subroutine lines to the incoming code.

    :param str tl_fortran: tangent linear code.
    :param list of str active_variable_names: a list of active \
        variable names.
    :param str tl_fortran: the expected adjoint code to be produced.
    :param str tmpdir: temporary directory created by pytest in which
    to perform compilation.

    '''
    # Add "subroutine / end subroutine" lines to the incoming code.
    input_code = ("subroutine test()\n{0}end subroutine test\n"
                  "".format(tl_fortran))
    expected_output_code = ("subroutine test()\n{0}end subroutine test\n"
                            "".format(expected_ad_fortran))

    # Translate the tangent linear code to PSyIR.
    reader = FortranReader()
    psyir = reader.psyir_from_source(input_code)

    # Find the schedule in the PSyIR.
    schedule = psyir.children[0]
    assert isinstance(schedule, Schedule)

    # Create the visitor
    adj_visitor = AdjointVisitor(active_variable_names)

    # Apply the tangent linear to adjoint transformation.
    ad_psyir = adj_visitor(schedule)

    # Translate the adjoint code to Fortran.
    writer = FortranWriter()
    ad_fortran = writer(ad_psyir)

    # Check that the code produced is the same as the expected code
    # provided.
    assert ad_fortran == expected_output_code

    # Check that the code produced will compile.
    assert Compile(tmpdir).string_compiles(ad_fortran)


# TODO function active
# TODO function passive
# TODO function negate

# class adjoint visitor

# visitor init
def test_adjoint_visitor_init():
    '''Check an AdjointVisitor instance can be created and that it raises
    the expected exception if an incorrect argument is supplied.

    '''
    with pytest.raises(TypeError) as info:
        _ = AdjointVisitor([])
    assert ("There should be at least one active variable supplied."
            in str(info.value))

    adj_visitor = AdjointVisitor(["a"])
    assert isinstance(adj_visitor, AdjointVisitor)
    assert adj_visitor._active_variable_names == ["a"]
    assert adj_visitor._active_variables is None
    print (type(adj_visitor._logger))
    assert isinstance(adj_visitor._logger, logging.Logger)


# visitor filecontainer
def test_filecontainer(tmpdir):
    '''Check a FileContainer node that is part of a valid PSyIR
    representation of a tangent linear code results in a new
    FileContainer containing the PSyIR representation of the
    adjoint of the tangent linear code.

    '''
    tl_fortran = (
        "program test\n"
        "real :: a,b,x\n"
        "a = x*b\n"
        "end program test\n")
    expected_ad_fortran = (
        "program test\n"
        "  real :: a\n"
        "  real :: b\n"
        "  real :: x\n\n"
        "  b = b + x * a\n"
        "  a = 0.0\n\n"
        "end program test\n")
    reader = FortranReader()
    psyir = reader.psyir_from_source(tl_fortran)
    assert isinstance(psyir, FileContainer)
    adj_visitor = AdjointVisitor(["a", "b"])
    adjoint = adj_visitor._visit(psyir)
    assert isinstance(adjoint, FileContainer)
    # Should be a new FileContainer node
    assert adjoint is not psyir
    writer = FortranWriter()
    ad_fortran = writer(adjoint)
    assert ad_fortran == expected_ad_fortran
    assert Compile(tmpdir).string_compiles(ad_fortran)


def test_filecontainer_logging(caplog):
    '''Check the filecontainer method outputs the expected debug
    information.

    '''
    tl_fortran = (
        "program test\n"
        "real :: a,b,x\n"
        "a = x*b\n"
        "end program test\n")
    reader = FortranReader()
    psyir = reader.psyir_from_source(tl_fortran)
    adj_visitor = AdjointVisitor(["a", "b"])
    with caplog.at_level(logging.INFO):
        _ = adj_visitor._visit(psyir)
    assert caplog.text == ""
    with caplog.at_level(logging.DEBUG):
        _ = adj_visitor._visit(psyir)
    assert "Copying FileContainer" in caplog.text


# Visitor schedule
def test_schedule_active(tmpdir):
    '''Test the validate schedule_node method works when there are
    multiple active assignments in the schedule.

    '''
    tl_fortran = (
        "  real a, b, c, d\n"
        "  real w, x, y, z\n"
        "  a = w*a+x*b+y*c+d*z\n"
        "  b = b+x*d\n"
        "  c = y*a\n")
    active_variables = ["a", "b", "c", "d"]
    ad_fortran = (
        "  real :: a\n  real :: b\n"
        "  real :: c\n  real :: d\n"
        "  real :: w\n  real :: x\n"
        "  real :: y\n  real :: z\n\n"
        ""
        "  a = a + y * c\n"
        "  c = 0.0\n"
        ""
        "  d = d + x * b\n"
        ""
        "  b = b + x * a\n"
        "  c = c + y * a\n"
        "  d = d + a * z\n"
        "  a = w * a\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


def test_schedule_inactive(tmpdir):
    '''Test the visitor schedule_node method works when there are multiple
    inactive assignments in the schedule.

    '''
    tl_fortran = (
        "  real a, b, c, d\n"
        "  real w, x, y, z\n"
        "  w = x*y*z\n"
        "  x = y\n"
        "  z = z/w\n")
    active_variables = ["a", "b", "c", "d"]
    ad_fortran = (
        "  real :: a\n  real :: b\n"
        "  real :: c\n  real :: d\n"
        "  real :: w\n  real :: x\n"
        "  real :: y\n  real :: z\n\n"
        "  w = x * y * z\n"
        "  x = y\n"
        "  z = z / w\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


def test_schedule_mixed(tmpdir):
    '''Test the visitor schedule_node method works when there is a mixture
   of active and inactive assignments in the schedule.

    '''
    tl_fortran = (
        "  real a, b, c, d\n"
        "  real w, x, y, z\n"
        "  c = y*a\n"
        "  x = y*z\n"
        "  b = b+x*d\n"
        "  z = x+y\n"
        "  a = w*a+x*b+y*c+d*z\n")
    active_variables = ["a", "b", "c", "d"]
    ad_fortran = (
        "  real :: a\n  real :: b\n"
        "  real :: c\n  real :: d\n"
        "  real :: w\n  real :: x\n"
        "  real :: y\n  real :: z\n\n"
        ""
        "  x = y * z\n"
        "  z = x + y\n"
        ""
        "  b = b + x * a\n"
        "  c = c + y * a\n"
        "  d = d + a * z\n"
        "  a = w * a\n"
        ""
        "  d = d + x * b\n"
        ""
        "  a = a + y * c\n"
        "  c = 0.0\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


@pytest.mark.xfail(reason="Incorrect code is output if the variable in an inactive "
                   "assignment is read by an earlier statement.")
def test_schedule_dependent_active(tmpdir):
    '''Test the validate schedule_node method works when there is a
   mixture of active and inactive assignments in the schedule, the
   inactive variables are updated and have both forward and backward
   dependencies (i.e. the updated inactive variable is read both
   before and after it is updated).

    '''
    tl_fortran = (
        "  real a, b, c\n"
        "  real y\n"
        "  y = 2\n"
        "  a = a + y*b\n"
        "  y = 3\n"
        "  b = b + y*c\n")
    active_variables = ["a", "b", "c"]
    ad_fortran = (
        "  real :: a\n  real :: b\n"
        "  real :: c\n  real :: y\n\n"
        "  y = 3\n"
        "  c = c + y * b\n"
        "  y = 2\n"
        "  b = b + y * a\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir)


# TODO schedule loops and recursion
# TODO schedule logging


# visitor assignment
def test_assignment():
    '''Check an assignment node that is part of a PSyIR representation of
    a tangent linear code results in a list of assignment nodes
    containing the PSyIR representation of the adjoint of the tangent
    linear assinment.

    '''
    tl_fortran = (
        "program test\n"
        "real :: a,b,x\n"
        "a = x*b\n"
        "end program test\n")
    expected_ad_fortran = (
        "  b = b + x * a\n"
        "  a = 0.0\n")
    reader = FortranReader()
    psyir = reader.psyir_from_source(tl_fortran)
    assignment = psyir.children[0].children[0]
    assert isinstance(assignment, Assignment)
    adj_visitor = AdjointVisitor(["a", "b"])
    with pytest.raises(VisitorError) as info:
        _ = adj_visitor._visit(assignment)
    assert ("An assignment node should not be called without a schedule being "
            "called beforehand as the latter sets up the active variables."
            in str(info.value))
    # Make sure the parent schedule has been called
    _ = adj_visitor._visit(psyir)
    adjoint_list = adj_visitor._visit(assignment)
    assert isinstance(adjoint_list, list)
    assert len(adjoint_list) == 2
    writer = FortranWriter()
    for assignment in adjoint_list:
        ad_fortran = writer(assignment)
        assert ad_fortran in expected_ad_fortran


def test_assignment_logging(caplog):
    '''Check the assignment method outputs the expected debug
    information.

    '''
    tl_fortran = (
        "program test\n"
        "real :: a,b,x\n"
        "a = x*b\n"
        "end program test\n")
    reader = FortranReader()
    psyir = reader.psyir_from_source(tl_fortran)
    assignment = psyir.children[0].children[0]
    adj_visitor = AdjointVisitor(["a", "b"])
    # Make sure the parent schedule has been called
    _ = adj_visitor._visit(psyir)
    with caplog.at_level(logging.INFO):
        _ = adj_visitor._visit(assignment)
    assert caplog.text == ""
    with caplog.at_level(logging.DEBUG):
        _ = adj_visitor._visit(assignment)
    assert "Transforming active assignment" in caplog.text


# visitor loop
def test_loop():
    '''Check that the loop_node method takes a PSyIR loop node and returns
    its adjoint version (the loop with the original loop order
    reversed). Also check that an exception is raised if a schedule
    node has not already been called and if the loops contents do not
    contain any active variables.

    '''
    tl_fortran = (
        "program test\n"
        "real :: a,x\n"
        "integer :: i,n\n"
        "  do i = 1, n\n"
        "    x = 0.0\n"
        "  end do\n"
        "end program test\n")
    expected_ad_fortran = (
        "do i = n, 1, -1\n"
        "  x = 0.0\n"
        "enddo\n")
    reader = FortranReader()
    psyir = reader.psyir_from_source(tl_fortran)
    loop = psyir.children[0].children[0]
    assert isinstance(loop, Loop)
    adj_visitor = AdjointVisitor(["a"])

    # Schedule node not called, so active variables not set up.
    with pytest.raises(VisitorError) as info:
        _ = adj_visitor._visit(loop)
    assert ("A loop node should not be called without a schedule being called "
            "beforehand as the latter sets up the active variables."
            in str(info.value))
    _ = adj_visitor(psyir)

    # Loop contains no active variables.
    with pytest.raises(VisitorError) as info:
        _ = adj_visitor._visit(loop)
    assert ("Visitor loop_node tangent-linear to adjoint method called with "
            "a loop that contains no active variables." in str(info.value))

    # Check transformed output.
    adj_visitor = AdjointVisitor(["x"])
    _ = adj_visitor._visit(psyir)
    adjoint = adj_visitor._visit(loop)
    assert isinstance(adjoint, Loop)
    assert adjoint is not loop
    writer = FortranWriter()
    ad_fortran = writer(adjoint)
    assert ad_fortran in expected_ad_fortran


# Visitor loop recursion
def test_loop_recurse():
    '''Check that the loop_node method takes a PSyIR loop node that
    contains other loops and recurses correctly to reverse the order
    of all loops.

    '''
    tl_fortran = (
        "program test\n"
        "real :: a,x\n"
        "integer :: i,j,n\n"
        "do i = 1, n\n"
        "  do j = n, 1, -1\n"
        "    x = 0.0\n"
        "  end do\n"
        "end do\n"
        "end program test\n")
    expected_ad_fortran = (
        "do i = n, 1, -1\n"
        "  do j = 1, n, 1\n"
        "    x = 0.0\n"
        "  enddo\n"
        "enddo\n")
    reader = FortranReader()
    psyir = reader.psyir_from_source(tl_fortran)
    loop = psyir.children[0].children[0]
    assert isinstance(loop, Loop)

    # Check transformed output.
    adj_visitor = AdjointVisitor(["x"])
    _ = adj_visitor._visit(psyir)
    adjoint = adj_visitor._visit(loop)
    assert isinstance(adjoint, Loop)
    assert adjoint is not loop
    writer = FortranWriter()
    ad_fortran = writer(adjoint)
    assert ad_fortran in expected_ad_fortran


# Visitor loop logging
def test_loop_logging(caplog):
    '''Check the loop method outputs the expected debug
    information.

    '''
    tl_fortran = (
        "program test\n"
        "real :: a\n"
        "integer :: i,n\n"
        "  do i = 1, n\n"
        "    a = 0.0\n"
        "  end do\n"
        "end program test\n")
    reader = FortranReader()
    psyir = reader.psyir_from_source(tl_fortran)
    loop = psyir.children[0].children[0]
    adj_visitor = AdjointVisitor(["a"])
    # Make sure the parent schedule has been called
    _ = adj_visitor._visit(psyir)
    with caplog.at_level(logging.INFO):
        _ = adj_visitor._visit(loop)
    assert caplog.text == ""
    with caplog.at_level(logging.DEBUG):
        _ = adj_visitor._visit(loop)
    assert "Transforming active loop" in caplog.text
