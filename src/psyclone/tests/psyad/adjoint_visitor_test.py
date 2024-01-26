# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter, N. Nobre and S. Siso, STFC Daresbury Lab

'''A module to perform pytest tests on the code in the
adjoint_visitor.py file within the psyad directory.

'''
from __future__ import absolute_import
import logging
import pytest

from psyclone.psyad import AdjointVisitor
from psyclone.psyir.backend.visitor import PSyIRVisitor, VisitorError
from psyclone.psyir.backend.c import CWriter
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import (FileContainer, Schedule, Assignment, Loop,
                                  IfBlock)
from psyclone.psyir.symbols import Symbol, ArgumentInterface
from psyclone.tests.utilities import Compile

TL_CODE = (
    "program test\n"
    "real :: a, b, c\n"
    "a = b + c\n"
    "end program test\n")
EXPECTED_ADJ_CODE = (
    "program test\n"
    "  real :: a\n  real :: b\n  real :: c\n\n"
    "  a = 0.0\n"
    "  b = 0.0\n"
    "  c = 0.0\n"
    "  b = b + a\n"
    "  c = c + a\n"
    "  a = 0.0\n\n"
    "end program test\n"
)
TL_LOOP_CODE = (
    "subroutine test(lo, hi, step)\n"
    "  integer, intent(in) :: lo, hi, step\n"
    "  real :: a(10), b(10), c(10)\n"
    "  real :: d, e\n"
    "  integer :: i\n"
    "  do i=lo,hi,step\n"
    "    a(i) = b(i) + c(i)\n"
    "  end do\n"
    "  d = d - e\n"
    "end subroutine test\n"
)
TL_IF_CODE = (
    "subroutine test(a,b,c,d,e,f,g)\n"
    "  real, intent(in) :: a, b, c\n"
    "  real, intent(out) :: d, e, f, g\n"
    "  f=0.0\n"
    "  if (a+b<c) then\n"
    "    d=e\n"
    "  else\n"
    "    g=0.0\n"
    "  endif\n"
    "end subroutine test\n"
)


def check_adjoint(tl_fortran, active_variable_names, expected_ad_fortran,
                  tmpdir, fortran_writer):
    '''Utility routine that takes tangent-linear Fortran code as input in
    the argument 'tl_fortran', transforms this code into its adjoint
    using the active variables specified in the
    'active_variable_names' argument, tests whether the result is the
    same as the expected result in the 'expected_ad_fortran' argument
    and checks that it will compile.

    To help keep the test code short, this routine also adds the
    subroutine / end subroutine lines to the incoming code.

    :param str tl_fortran: tangent-linear code.
    :param list of str active_variable_names: a list of active \
        variable names.
    :param str tl_fortran: the expected adjoint code to be produced.
    :param str tmpdir: temporary directory created by pytest in which \
        to perform compilation.
    :param fortran_writer: writer to convert Fortran from PSyIR.
    :type fortran_writer: :py:class:`psyclone.psyir.backend.FortranWriter`

    '''
    # Add "subroutine / end subroutine" lines to the incoming code.
    input_code = f"subroutine test()\n{tl_fortran}end subroutine test\n"
    expected_output_code = (f"subroutine test()\n{expected_ad_fortran}"
                            f"end subroutine test\n")

    # Translate the tangent-linear code to PSyIR.
    reader = FortranReader()
    psyir = reader.psyir_from_source(input_code)

    # Find the schedule in the PSyIR.
    schedule = psyir.children[0]
    assert isinstance(schedule, Schedule)

    # Create the visitor
    adj_visitor = AdjointVisitor(active_variable_names)

    # Apply the tangent-linear to adjoint transformation.
    ad_psyir = adj_visitor(schedule)

    # Translate the adjoint code to Fortran.
    ad_fortran = fortran_writer(ad_psyir)

    # Check that the code produced is the same as the expected code
    # provided.
    assert ad_fortran == expected_output_code

    # Check that the code produced will compile.
    assert Compile(tmpdir).string_compiles(ad_fortran)


# AdjointVisitor.__init__()

def test_create():
    '''Test that an AdjointVisitor can be created correctly.'''

    adj_visitor = AdjointVisitor(["dummy"])
    assert isinstance(adj_visitor, AdjointVisitor)
    assert issubclass(AdjointVisitor, PSyIRVisitor)
    assert adj_visitor._active_variable_names == ["dummy"]
    assert adj_visitor._active_variables is None
    assert isinstance(adj_visitor._logger, logging.Logger)


def test_create_error_active():
    '''Test that an AdjointVisitor raises an exception if no active
    variables are provided.

    '''
    with pytest.raises(ValueError) as info:
        _ = AdjointVisitor([])
    assert ("There should be at least one active variable supplied to an "
            "AdjointVisitor." in str(info.value))


# AdjointVisitor.container_node()

@pytest.mark.xfail(reason="issue #1235: caplog returns an empty string in "
                   "github actions.", strict=False)
def test_create_container_logger(caplog):
    '''Test that the logger writes the expected output if the
    container_node method is called.

    '''
    tangent_linear = FileContainer("blah")
    adj_visitor = AdjointVisitor(["dummy"])
    with caplog.at_level(logging.INFO):
        _ = adj_visitor.container_node(tangent_linear)
    assert caplog.text == ""
    with caplog.at_level(logging.DEBUG):
        _ = adj_visitor._visit(tangent_linear)
    assert "Copying Container" in caplog.text


def test_create_container_node():
    '''Test that a copy of container node is returned.'''
    tangent_linear = FileContainer("blah")
    adj_visitor = AdjointVisitor(["dummy"])
    adjoint = adj_visitor.container_node(tangent_linear)
    assert isinstance(adjoint, FileContainer)
    assert adjoint is not tangent_linear
    assert adjoint.name == tangent_linear.name


def test_create_container_node_children(fortran_reader, fortran_writer):
    '''Test that the children of a container node (if any exist) are also
    processed by the visitor.

    '''
    tl_psyir = fortran_reader.psyir_from_source(TL_CODE)
    adj_visitor = AdjointVisitor(["a", "b", "c"])
    adjoint = adj_visitor.container_node(tl_psyir)
    # It is easier to test the generated Fortran code than to check
    # the PSyIR nodes.
    adjoint_fortran_code = fortran_writer(adjoint)
    assert adjoint_fortran_code == EXPECTED_ADJ_CODE


# AdjointVisitor.schedule_node()

@pytest.mark.xfail(reason="issue #1235: caplog returns an empty string in "
                   "github actions.", strict=False)
def test_create_schedule_logger(caplog, fortran_reader):
    '''Test that the logger writes the expected output if the
    schedule_node method is called.

    '''
    # A schedule can't be created in isolation as PSyAD expects it to
    # have a child assignment node. Therefore create from Fortran code
    # as it is simpler.
    tl_psyir = fortran_reader.psyir_from_source(TL_CODE)
    tl_schedule = tl_psyir.children[0]
    assert isinstance(tl_schedule, Schedule)
    adj_visitor = AdjointVisitor(["a", "b", "c"])
    with caplog.at_level(logging.INFO):
        _ = adj_visitor.schedule_node(tl_schedule)
    assert caplog.text == ""
    with caplog.at_level(logging.DEBUG):
        _ = adj_visitor._visit(tl_schedule)
    assert "Transforming Schedule" in caplog.text
    assert "Zero-ing any local active variables" in caplog.text
    assert "Adding passive code into new schedule" in caplog.text
    assert "Reversing order of active code" in caplog.text
    assert ("Processing active code and adding results into new schedule"
            in caplog.text)
    assert "Transforming active assignment" in caplog.text


def test_create_schedule_active_variables(fortran_reader):
    '''Test that any active variables specified by the adjoint visitor are
    found in the symbol table and stored in the _active_variables
    list. Also test that any unknown symbol names raise the expected
    exception.

    '''
    tl_psyir = fortran_reader.psyir_from_source(TL_CODE)
    tl_schedule = tl_psyir.children[0]
    assert isinstance(tl_schedule, Schedule)
    adj_visitor = AdjointVisitor(["a", "b", "c"])
    assert not adj_visitor._active_variables
    _ = adj_visitor.schedule_node(tl_schedule)
    assert len(adj_visitor._active_variables) == 3
    for symbol in adj_visitor._active_variables:
        assert isinstance(symbol, Symbol)
    assert adj_visitor._active_variables[0].name == "a"
    assert adj_visitor._active_variables[1].name == "b"
    assert adj_visitor._active_variables[2].name == "c"

    adj_visitor = AdjointVisitor(["non-existant"])
    with pytest.raises(KeyError) as info:
        _ = adj_visitor.schedule_node(tl_schedule)
    assert ("Could not find 'non-existant' in the Symbol Table."
            in str(info.value))


def test_schedule_active_assign(tmpdir, fortran_writer):
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
        "  a = 0.0\n  b = 0.0\n  c = 0.0\n  d = 0.0\n"
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
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir,
                  fortran_writer)


def test_schedule_inactive_assign(tmpdir, fortran_writer):
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
        ""
        "  a = 0.0\n  b = 0.0\n  c = 0.0\n  d = 0.0\n"
        ""
        "  w = x * y * z\n"
        "  x = y\n"
        "  z = z / w\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir,
                  fortran_writer)


def test_schedule_mixed(tmpdir, fortran_writer):
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
        "  a = 0.0\n  b = 0.0\n  c = 0.0\n  d = 0.0\n"
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
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir,
                  fortran_writer)


@pytest.mark.parametrize("datatype,precision,value",
                         [("real", "r_def", "0.0"), ("integer", "i_def", "0")])
def test_schedule_zero(tmpdir, fortran_writer, datatype, precision, value):
    '''Test the visitor schedule_node method sets active variables with
    the real and integer datatypes to zero. Tests for both scalars and
    arrays, with and without specified precision.

    '''
    tl_fortran = (
        f"  integer, parameter :: n=10\n"
        f"  integer, parameter :: {precision}=8\n"
        f"  {datatype} :: a\n"
        f"  {datatype} :: b(n)\n"
        f"  {datatype}(kind={precision}) :: c\n"
        f"  {datatype}(kind={precision}) :: d(n)\n")
    active_variables = ["a", "b", "c", "d"]
    ad_fortran = (
        f"  integer, parameter :: n = 10\n"
        f"  integer, parameter :: {precision} = 8\n"
        f"  {datatype} :: a\n"
        f"  {datatype}, dimension(n) :: b\n"
        f"  {datatype}(kind={precision}) :: c\n"
        f"  {datatype}(kind={precision}), dimension(n) :: d\n\n"
        f"  a = {value}\n"
        f"  b = {value}\n"
        f"  c = {value}_{precision}\n"
        f"  d = {value}_{precision}\n\n")
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir,
                  fortran_writer)


def test_schedule_zero_global(fortran_reader, fortran_writer, tmpdir):
    '''Test the visitor schedule_node does not zero non-local data.'''

    tl_fortran = (
        "subroutine global_test(a)\n"
        "  real, intent(in) :: a\n"
        "  real :: b\n"
        "end subroutine global_test\n")
    tl_psyir = fortran_reader.psyir_from_source(tl_fortran)
    tl_schedule = tl_psyir.children[0]
    adj_visitor = AdjointVisitor(["a", "b"])
    ad_psyir = adj_visitor(tl_schedule)
    ad_fortran = fortran_writer(ad_psyir)
    assert ad_fortran == (
        "subroutine global_test(a)\n"
        "  real, intent(out) :: a\n"
        "  real :: b\n\n"
        "  b = 0.0\n\n"
        "end subroutine global_test\n")
    assert Compile(tmpdir).string_compiles(ad_fortran)


def test_schedule_zero_datatype_error1(fortran_reader):
    '''Test the visitor schedule_node method raises the expected exception
    if an unsupported datatype (a structure in this test) is found for
    a local active variable. Supported types are scalars and arrays.

    '''
    tl_fortran = (
        "program structure_test\n"
        "  type :: field_type\n"
        "    real :: data(10)\n"
        "  end type\n"
        "  type(field_type) :: a\n"
        "end program structure_test\n")
    tl_psyir = fortran_reader.psyir_from_source(tl_fortran)
    tl_schedule = tl_psyir.children[0]
    adj_visitor = AdjointVisitor(["a"])
    with pytest.raises(NotImplementedError) as info:
        _ = adj_visitor.schedule_node(tl_schedule)
    assert ("Active local variables can only be scalars and arrays, but "
            "found 'a: DataSymbol<field_type: DataTypeSymbol, Automatic>'."
            in str(info.value))


def test_schedule_zero_datatype_error2(fortran_reader):
    '''Test the visitor schedule_node method raises the expected exception
    if an unsupported intrinsic datatype (logical in this test) is
    found for a local active variable.

    '''
    tl_fortran = (
        "program logical_test\n"
        "  logical :: l\n"
        "end program logical_test\n")
    tl_psyir = fortran_reader.psyir_from_source(tl_fortran)
    tl_schedule = tl_psyir.children[0]
    adj_visitor = AdjointVisitor(["l"])
    with pytest.raises(NotImplementedError) as info:
        _ = adj_visitor.schedule_node(tl_schedule)
    assert ("Datatype 'BOOLEAN' is not supported (for active local variable "
            "'l'). Supported types are 'REAL' and 'INTEGER'."
            in str(info.value))


@pytest.mark.xfail(reason="Incorrect code is output if the variable in an "
                   "inactive assignment is read by an earlier statement, "
                   "issue #1458.")
def test_schedule_dependent_active(tmpdir, fortran_writer):
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
    check_adjoint(tl_fortran, active_variables, ad_fortran, tmpdir,
                  fortran_writer)


def test_subroutine_schedule_access(fortran_reader):
    ''' Test that the accesses of any active variables that are subroutine
    arguments are updated correctly. '''
    tl_fortran_lines = [
        "subroutine kern(a, b, c, d)",
        "  real, intent(inout) :: a, b",
        "  real, intent(out) :: d",
        "  real, intent(in) :: c",
        "  real y",
        "  y = 2",
        "  a = a + y*b",
        "  b = b + y*c",
        "  d = 0",
        "end subroutine kern"]
    tl_fortran = "\n".join(tl_fortran_lines)
    psyir = fortran_reader.psyir_from_source(tl_fortran)
    schedule = psyir.children[0]
    adj_visitor = AdjointVisitor(["a", "b", "c", "d"])
    ad_psyir = adj_visitor(schedule)
    a_hat = ad_psyir.symbol_table.lookup("a")
    assert a_hat.interface.access == ArgumentInterface.Access.READ
    c_hat = ad_psyir.symbol_table.lookup("c")
    assert c_hat.interface.access == ArgumentInterface.Access.READWRITE
    d_hat = ad_psyir.symbol_table.lookup("d")
    assert d_hat.interface.access == ArgumentInterface.Access.WRITE


# AdjointVisitor.assignment_node()

@pytest.mark.xfail(reason="issue #1235: caplog returns an empty string in "
                   "github actions.", strict=False)
def test_assignment_node_logger(caplog, fortran_reader):
    '''Test that the logger writes the expected output if the
    assignment_node method is called.

    '''
    tl_psyir = fortran_reader.psyir_from_source(TL_CODE)
    assignment = tl_psyir.walk(Assignment)[0]
    assert isinstance(assignment, Assignment)
    adj_visitor = AdjointVisitor(["a", "b", "c"])
    # set up self._active_variables
    _ = adj_visitor._visit(tl_psyir)
    with caplog.at_level(logging.INFO):
        _ = adj_visitor.assignment_node(assignment)
    assert caplog.text == ""
    with caplog.at_level(logging.DEBUG):
        _ = adj_visitor.assignment_node(assignment)
    assert "Transforming active assignment" in caplog.text


def test_assignment_node_error(fortran_reader):
    '''Test that the assignment_node method raises the expected exception
    if no active variables are found.

    '''
    tl_psyir = fortran_reader.psyir_from_source(TL_CODE)
    assignment = tl_psyir.walk(Assignment)[0]
    assert isinstance(assignment, Assignment)
    adj_visitor = AdjointVisitor(["a", "b", "c"])
    with pytest.raises(VisitorError) as info:
        _ = adj_visitor.assignment_node(assignment)
    assert ("An assignment node should not be visited before a schedule, as "
            "the latter sets up the active variables." in str(info.value))


def test_assignment_node(fortran_reader, fortran_writer):
    '''Test that assignment_node transforms a tangent-linear assignment
    into its adjoint by calling the AdjointTrans transformation.

    '''
    tl_psyir = fortran_reader.psyir_from_source(TL_CODE)
    assignment = tl_psyir.walk(Assignment)[0]
    assert isinstance(assignment, Assignment)
    adj_visitor = AdjointVisitor(["a", "b", "c"])
    # set up self._active_variables
    _ = adj_visitor._visit(tl_psyir)
    adj_assignment_psyir_nodes = adj_visitor.assignment_node(assignment)
    assert isinstance(adj_assignment_psyir_nodes, list)
    for node in adj_assignment_psyir_nodes:
        assert isinstance(node, Assignment)
    assert fortran_writer(adj_assignment_psyir_nodes[0]) == "b = b + a\n"
    assert fortran_writer(adj_assignment_psyir_nodes[1]) == "c = c + a\n"
    assert fortran_writer(adj_assignment_psyir_nodes[2]) == "a = 0.0\n"


# AdjointVisitor.loop_node()

def test_loop_node_active_error(fortran_reader):
    '''Test that the loop_node method raises the expected exception
    if no active variables are specified.

    '''
    tl_psyir = fortran_reader.psyir_from_source(TL_LOOP_CODE)
    loop = tl_psyir.walk(Loop)[0]
    adj_visitor = AdjointVisitor(["a", "b", "c"])
    with pytest.raises(VisitorError) as info:
        _ = adj_visitor.loop_node(loop)
    assert ("A loop node should not be visited before a schedule, as "
            "the latter sets up the active variables." in str(info.value))


def test_loop_node_bounds_error(fortran_reader):
    '''Test that the loop_node method raises the expected exception if an
    active variable is found in a loop bound (lower, upper or step) or
    the iterator is an active variable.

    '''
    tl_psyir = fortran_reader.psyir_from_source(TL_LOOP_CODE)
    # lower bound
    adj_visitor = AdjointVisitor(["a", "b", "c", "lo"])
    with pytest.raises(VisitorError) as info:
        _ = adj_visitor(tl_psyir)
    assert ("The lower bound of a loop should not contain active variables, "
            "but found 'lo' in 'lo'" in str(info.value))
    # upper bound
    adj_visitor = AdjointVisitor(["a", "b", "c", "hi"])
    with pytest.raises(VisitorError) as info:
        _ = adj_visitor(tl_psyir)
    assert ("The upper bound of a loop should not contain active variables, "
            "but found 'hi' in 'hi'" in str(info.value))
    # step
    adj_visitor = AdjointVisitor(["a", "b", "c", "step"])
    with pytest.raises(VisitorError) as info:
        _ = adj_visitor(tl_psyir)
    assert ("The step of a loop should not contain active variables, "
            "but found 'step' in 'step'" in str(info.value))
    # loop variable
    adj_visitor = AdjointVisitor(["a", "b", "c", "i"])
    with pytest.raises(VisitorError) as info:
        _ = adj_visitor(tl_psyir)
    assert ("The loop iterator 'i' should not be an active variable."
            in str(info.value))


def test_loop_node_bounds_intrinsic(fortran_reader, fortran_writer, tmpdir):
    '''Test that the loop_node method does not raise an exception if an
    active variable is found in a loop bound but is the first argument
    to one of the LBOUND or UBOUND intrinsic functions as these
    determine the size of arrays rather than modifying their
    values. Also test that it does raise an exception if an active
    variable is found in a loop bound if it is part of the second
    argument to one of the LBOUND and UBOUND intrinsic functions.

    '''
    # active variables within 1st arg of lbound or ubound are OK
    tl_code = (
        "program test\n"
        "  real :: a(10), b(2)\n"
        "  integer :: i\n"
        "  do i = lbound(a,1), ubound(a,1), 2*ubound(b,1)\n"
        "    a(i) = 0.0\n"
        "  end do\n"
        "end program test\n")
    expected_ad_code = (
        "program test\n"
        "  real, dimension(10) :: a\n"
        "  real, dimension(2) :: b\n"
        "  integer :: i\n\n"
        "  a = 0.0\n"
        "  b = 0.0\n"
        "  do i = UBOUND(a, 1) - MOD(UBOUND(a, 1) - LBOUND(a, 1), "
        "2 * UBOUND(b, 1)), LBOUND(a, 1), -1 * (2 * UBOUND(b, 1))\n"
        "    a(i) = 0.0\n"
        "  enddo\n\n"
        "end program test\n")
    tl_psyir = fortran_reader.psyir_from_source(tl_code)
    adj_visitor = AdjointVisitor(["a", "b"])
    ad_psyir = adj_visitor(tl_psyir)
    result = fortran_writer(ad_psyir)
    assert result == expected_ad_code
    assert Compile(tmpdir).string_compiles(result)

    # active variables within 2nd arg of lbound or ubound are not OK
    tl_code = (
        "program test\n"
        "  real :: a(10), b\n"
        "  integer :: i\n"
        "  do i = 1,ubound(a,b)\n"
        "    a(i) = b\n"
        "  end do\n"
        "end program test\n")
    tl_psyir = fortran_reader.psyir_from_source(tl_code)
    adj_visitor = AdjointVisitor(["a", "b"])
    with pytest.raises(VisitorError) as error:
        _ = adj_visitor(tl_psyir)
    assert ("The upper bound of a loop should not contain active variables, "
            "but found 'b' in 'UBOUND(a, b)'." in str(error.value))


def test_loop_node_passive(fortran_reader):
    '''Test that the loop_node method raises an exception if there are no
    active variables within the supplied loop node. This is because
    the schedule node should have already dealt with this case.

    '''
    tl_psyir = fortran_reader.psyir_from_source(TL_LOOP_CODE)
    tl_loop = tl_psyir.walk(Loop)[0]
    adj_visitor = AdjointVisitor(["d", "e"])
    _ = adj_visitor._visit(tl_psyir)

    with pytest.raises(VisitorError) as info:
        _ = adj_visitor.loop_node(tl_loop)
    assert ("A passive loop node should not be processed by the loop_node() "
            "method within the AdjointVisitor() class, as it should have been "
            "dealt with by the schedule_node() method." in str(info.value))


@pytest.mark.parametrize("in_bounds,out_bounds", [
    ("lo,hi", "hi, lo, -1"),
    ("lo,hi,1", "hi, lo, -1"),
    ("lo,hi,-1", "hi - MOD(hi - lo, - 1), lo, 1"),
    ("lo,hi,step", "hi - MOD(hi - lo, step), lo, -1 * step")])
def test_loop_node_active(fortran_reader, fortran_writer, in_bounds,
                          out_bounds):
    '''Test that a loop_node containing active variables returns with its
    loop order reversed and its loop body processed by the adjoint
    visitor. Checks that appropriate offset code is generated when the
    loop step is not, or might not be, 1 or -1. Note that in the
    PSyIR, -1 can be represented as a unitary minus containing a
    literal with value 1 and that, in such a case, an offset will be
    computed (see the 3rd parameterised case where this occurs).

    '''
    code = TL_LOOP_CODE.replace("lo,hi,step", in_bounds)
    tl_psyir = fortran_reader.psyir_from_source(code)
    adj_visitor = AdjointVisitor(["a", "b", "c"])
    ad_psyir = adj_visitor(tl_psyir)
    ad_loop = ad_psyir.walk(Loop)[0]
    result = fortran_writer(ad_loop)
    expected_result = (
        f"do i = {out_bounds}\n"
        f"  b(i) = b(i) + a(i)\n"
        f"  c(i) = c(i) + a(i)\n"
        f"  a(i) = 0.0\n"
        f"enddo\n")
    assert result == expected_result


@pytest.mark.xfail(reason="issue #1235: caplog returns an empty string in "
                   "github actions.", strict=False)
def test_loop_logger(fortran_reader, caplog):
    '''Test that the logger writes the expected output if the loop_node
    method is called with an inactive node and an active node.

    '''
    tl_psyir = fortran_reader.psyir_from_source(TL_LOOP_CODE)
    tl_loop = tl_psyir.walk(Loop)[0]

    adj_visitor = AdjointVisitor(["a", "b", "c"])

    # Need to use _visit() here rather than adj_visitor(tl_psyir) as
    # the latter takes a copy of the tree in case any lowering needs
    # to be done and that causes the stored symbols for active
    # variables to be different which means they do not match in
    # subsequent calls. This is only a problem for tests as we don't
    # normally call loop_node() or similar, directly.

    # The _visit() method is called so that the active variables
    # symbols are set up when calling the loop_node() method directly.
    _ = adj_visitor._visit(tl_psyir)

    # active loop
    with caplog.at_level(logging.INFO):
        _ = adj_visitor.loop_node(tl_loop)
    assert caplog.text == ""
    with caplog.at_level(logging.DEBUG):
        _ = adj_visitor.loop_node(tl_loop)
    assert "Transforming active loop" in caplog.text

    # Remove content for subsequent inactive loop code
    caplog.clear()

    # inactive loop
    adj_visitor = AdjointVisitor(["d", "e"])

    # The visitor is called so that the active variables symbols are
    # set up when calling the loop_node() method directly.
    _ = adj_visitor(tl_psyir)

    with caplog.at_level(logging.INFO):
        _ = adj_visitor.loop_node(tl_loop)
    assert caplog.text == ""
    with caplog.at_level(logging.DEBUG):
        _ = adj_visitor.loop_node(tl_loop)
    assert ("Returning a copy of the original loop and its descendants as it "
            "contains no active variables" in caplog.text)


# AdjointVisitor.ifblock_node()

def test_ifblock_node_active_error(fortran_reader):
    '''Test that the ifblock_node method raises the expected exception
    if no active variables are specified.

    '''
    tl_psyir = fortran_reader.psyir_from_source(TL_IF_CODE)
    ifblock = tl_psyir.walk(IfBlock)[0]
    adj_visitor = AdjointVisitor(["a", "b", "c"])
    with pytest.raises(VisitorError) as info:
        _ = adj_visitor.ifblock_node(ifblock)
    assert ("An ifblock node should not be visited before a schedule, as "
            "the latter sets up the active variables." in str(info.value))


@pytest.mark.parametrize("active_variables", [["a"], ["b"], ["c"],
                                              ["a", "b", "c"]])
def test_ifblock_node_active_condition_error(fortran_reader, active_variables):
    '''Test that the ifblock_node method raises the expected exception if
    an active variable is found in the condition of an ifblock node.

    '''
    tl_psyir = fortran_reader.psyir_from_source(TL_IF_CODE)
    adj_visitor = AdjointVisitor(active_variables)
    with pytest.raises(VisitorError) as info:
        _ = adj_visitor._visit(tl_psyir)
    assert (f"The if condition 'a + b < c' of an ifblock node should "
            f"not contain an active variable (one or more of "
            f"{active_variables})." in str(info.value))


def test_ifblock_node_passive(fortran_reader):
    '''Test that the ifblock_node method raises an exception if there are no
    active variables within the supplied ifblock node. This is because
    the schedule node should have already dealt with this case.

    '''
    tl_psyir = fortran_reader.psyir_from_source(TL_IF_CODE)
    tl_ifblock = tl_psyir.walk(IfBlock)[0]
    adj_visitor = AdjointVisitor(["f"])
    _ = adj_visitor._visit(tl_psyir)

    with pytest.raises(VisitorError) as info:
        _ = adj_visitor.ifblock_node(tl_ifblock)
    assert ("A passive ifblock node should not be processed by the "
            "ifblock_node() method within the AdjointVisitor() class, as it "
            "should have been dealt with by the schedule_node() method."
            in str(info.value))


@pytest.mark.xfail(reason="issue #1235: caplog returns an empty string in "
                   "github actions.", strict=False)
def test_ifblock_logger(fortran_reader, caplog):
    '''Test that the logger writes the expected output if it transforms
    the ifblock.

    '''
    tl_psyir = fortran_reader.psyir_from_source(TL_IF_CODE)

    adj_visitor = AdjointVisitor(["d", "e"])
    with caplog.at_level(logging.INFO):
        _ = adj_visitor._visit(tl_psyir)
    assert "Transforming active ifblock" not in caplog.text
    with caplog.at_level(logging.DEBUG):
        _ = adj_visitor._visit(tl_psyir)
    assert "Transforming active ifblock" in caplog.text


def test_ifblock_active(tmpdir, fortran_writer):
    '''Test the visitor ifblock_node method works when there is an active
    then part.

    '''
    tl_fortran = (
        "  real :: a, b, c, d, e, f, g, h\n"
        "  f=0.0\n"
        "  if (a+b<c) then\n"
        "    d=e\n"
        "  else\n"
        "    g=h\n"
        "  endif\n")
    # active then
    ad_fortran = (
        "  real :: a\n  real :: b\n  real :: c\n  real :: d\n  real :: e\n"
        "  real :: f\n  real :: g\n  real :: h\n\n"
        "  d = 0.0\n"
        "  e = 0.0\n"
        "  f = 0.0\n"
        "  if (a + b < c) then\n"
        "    e = e + d\n"
        "    d = 0.0\n"
        "  else\n"
        "    g = h\n"
        "  end if\n\n")
    check_adjoint(tl_fortran, ["d", "e"], ad_fortran, tmpdir, fortran_writer)
    # active else
    ad_fortran = (
        "  real :: a\n  real :: b\n  real :: c\n  real :: d\n  real :: e\n"
        "  real :: f\n  real :: g\n  real :: h\n\n"
        "  g = 0.0\n"
        "  h = 0.0\n"
        "  f = 0.0\n"
        "  if (a + b < c) then\n"
        "    d = e\n"
        "  else\n"
        "    h = h + g\n"
        "    g = 0.0\n"
        "  end if\n\n")
    check_adjoint(tl_fortran, ["g", "h"], ad_fortran, tmpdir, fortran_writer)
    # active then/else
    ad_fortran = (
        "  real :: a\n  real :: b\n  real :: c\n  real :: d\n  real :: e\n"
        "  real :: f\n  real :: g\n  real :: h\n\n"
        "  d = 0.0\n"
        "  e = 0.0\n"
        "  g = 0.0\n"
        "  h = 0.0\n"
        "  f = 0.0\n"
        "  if (a + b < c) then\n"
        "    e = e + d\n"
        "    d = 0.0\n"
        "  else\n"
        "    h = h + g\n"
        "    g = 0.0\n"
        "  end if\n\n")
    check_adjoint(tl_fortran, ["d", "e", "g", "h"], ad_fortran, tmpdir,
                  fortran_writer)


# AdjointVisitor._copy_and_process()

def test_copy_and_process(fortran_reader, fortran_writer):
    '''Test that the _copy_and_process utility method works as
    expected.

    '''
    tl_psyir = fortran_reader.psyir_from_source(TL_CODE)
    tl_schedule = tl_psyir.children[0]
    assignment = tl_psyir.walk(Assignment)[0]
    assert isinstance(assignment, Assignment)
    adj_visitor = AdjointVisitor(["a", "b", "c"])
    # set up self._active_variables
    _ = adj_visitor._visit(tl_psyir)
    result = adj_visitor._copy_and_process(tl_schedule)
    # Node copy
    assert isinstance(result, Schedule)
    assert result is not tl_schedule
    # Processed child (an Assignment) has returned a list of nodes
    for node in result.children:
        assert isinstance(node, Assignment)
    assert fortran_writer(result.children[0]) == "b = b + a\n"
    assert fortran_writer(result.children[1]) == "c = c + a\n"
    assert fortran_writer(result.children[2]) == "a = 0.0\n"
    # Processed children which return a node (child of FileContainer
    # is a schedule)
    result = adj_visitor._copy_and_process(tl_psyir)
    assert len(result.children) == 1
    assert isinstance(result.children[0], Schedule)
    assert result.children[0] is not tl_psyir.children[0]
    assert result.children[0].name == tl_psyir.children[0].name
