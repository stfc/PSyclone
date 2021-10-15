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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''A module to perform pytest tests on the code in the
adjoint_visitor.py file within the psyad directory.

'''
from __future__ import absolute_import
import logging
import pytest

from psyclone.psyad import AdjointVisitor
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.backend.visitor import PSyIRVisitor, VisitorError
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import FileContainer, Schedule, Assignment
from psyclone.psyir.symbols import Symbol
from psyclone.tests.utilities import Compile

TL_CODE = (
    "program test\n"
    "real :: a, b, c\n"
    "a = b + c\n"
    "end program test\n")
EXPECTED_ADJ_CODE = (
    "program test\n"
    "  real :: a\n  real :: b\n  real :: c\n\n"
    "  b = b + a\n"
    "  c = c + a\n"
    "  a = 0.0\n\n"
    "end program test\n"
)


def check_adjoint(tl_fortran, active_variable_names, expected_ad_fortran,
                  tmpdir):
    '''Utility routine that takes tangent-linear Fortran code as input in
    the argument 'tl_fortran', transforms this code into its adjoint
    using the active variables specified in the
    'active_variable_names' argument, tests whether the result is the
    same as the expected result in the 'expected_ad_fortran' argument
    and checks that it will compile.

    To help keep the test code short, this routine also adds the
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


# AdjointVisitor.__init__()

def test_create():
    '''Test that an AdjointVisitor can be created correctly.'''

    adj_visitor = AdjointVisitor(["dummy"])
    assert isinstance(adj_visitor, AdjointVisitor)
    assert issubclass(AdjointVisitor, PSyIRVisitor)
    assert adj_visitor._active_variable_names == ["dummy"]
    assert adj_visitor._active_variables is None
    assert isinstance(adj_visitor._logger, logging.Logger)


def test_create_error():
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
    assert "Adding passive code into new schedule" in caplog.text
    assert "Reversing order of active code" in caplog.text
    assert ("Processing active code and adding results into new schedule"
            in caplog.text)


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


def test_schedule_active_assign(tmpdir):
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


def test_schedule_inactive_assign(tmpdir):
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


@pytest.mark.xfail(reason="Incorrect code is output if the variable in an "
                   "inactive assignment is read by an earlier statement, "
                   "issue #1458.")
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


# AdjointVisitor.assignment_node()

@pytest.mark.xfail(reason="issue #1235: caplog returns an empty string in "
                   "github actions.", strict=False)
def test_assignment_node_logger(caplog, fortran_reader):
    '''Test that the logger writes the expected output if the
    assignment_node method is called.

    '''
    tl_psyir = fortran_reader.psyir_from_source(TL_CODE)
    tl_schedule = tl_psyir.children[0]
    assignment = tl_schedule.children[0]
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
    tl_schedule = tl_psyir.children[0]
    assignment = tl_schedule.children[0]
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
    tl_schedule = tl_psyir.children[0]
    assignment = tl_schedule.children[0]
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


# AdjointVisitor._copy_and_process()

def test_copy_and_process(fortran_reader, fortran_writer):
    '''Test that the _copy_and_process utility method works as
    expected.

    '''
    tl_psyir = fortran_reader.psyir_from_source(TL_CODE)
    tl_schedule = tl_psyir.children[0]
    assignment = tl_schedule.children[0]
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
