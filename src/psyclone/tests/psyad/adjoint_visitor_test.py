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
from psyclone.psyir.backend.visitor import PSyIRVisitor, VisitorError
from psyclone.psyir.backend.c import CWriter
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import FileContainer, Schedule, Assignment, Loop, \
    Node
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
    input_code = ("subroutine test()\n{0}end subroutine test\n"
                  "".format(tl_fortran))
    expected_output_code = ("subroutine test()\n{0}end subroutine test\n"
                            "".format(expected_ad_fortran))

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
    assert isinstance(adj_visitor._writer, FortranWriter)
    # Optional writer argument
    c_writer = CWriter()
    adj_visitor = AdjointVisitor(["dummy"], writer=c_writer)
    assert adj_visitor._writer == c_writer


def test_create_error_active():
    '''Test that an AdjointVisitor raises an exception if no active
    variables are provided.

    '''
    with pytest.raises(ValueError) as info:
        _ = AdjointVisitor([])
    assert ("There should be at least one active variable supplied to an "
            "AdjointVisitor." in str(info.value))


def test_create_error_writer():
    '''Test that an AdjointVisitor raises an exception if an invalid
    writer argument is supplied.

    '''
    with pytest.raises(TypeError) as info:
        _ = AdjointVisitor(["dummy"], writer=None)
    assert ("The writer argument should be a subclass of LanguageWriter but "
            "found 'NoneType'." in str(info.value))


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
    if no active variables are found.

    '''
    tl_psyir = fortran_reader.psyir_from_source(TL_LOOP_CODE)
    loop = tl_psyir.walk(Loop)[0]
    assert isinstance(loop, Loop)
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
            "but found 'lo'" in str(info.value))
    # upper bound
    adj_visitor = AdjointVisitor(["a", "b", "c", "hi"])
    with pytest.raises(VisitorError) as info:
        _ = adj_visitor(tl_psyir)
    assert ("The upper bound of a loop should not contain active variables, "
            "but found 'hi'" in str(info.value))
    # step
    adj_visitor = AdjointVisitor(["a", "b", "c", "step"])
    with pytest.raises(VisitorError) as info:
        _ = adj_visitor(tl_psyir)
    assert ("The step of a loop should not contain active variables, "
            "but found 'step'" in str(info.value))
    # loop variable
    adj_visitor = AdjointVisitor(["a", "b", "c", "i"])
    with pytest.raises(VisitorError) as info:
        _ = adj_visitor(tl_psyir)
    assert ("The loop iterator 'i' should not be an active variable."
            in str(info.value))


def test_loop_node_inactive(fortran_reader, fortran_writer):
    '''Test that the loop_node method returns an unchanged copy of the
    loop and the loop body when there are no active variables within
    the loop.

    '''
    tl_psyir = fortran_reader.psyir_from_source(TL_LOOP_CODE)
    tl_loop = tl_psyir.walk(Loop)[0]
    assert isinstance(tl_loop, Loop)
    adj_visitor = AdjointVisitor(["d", "e"])
    ad_psyir = adj_visitor(tl_psyir)
    ad_loop = ad_psyir.walk(Loop)[0]
    assert isinstance(ad_loop, Loop)
    # Check that tl_loop and ad_loop are equal but not identical
    assert fortran_writer(ad_loop) == fortran_writer(tl_loop)
    tl_nodes = tl_loop.walk(Node)
    ad_nodes = ad_loop.walk(Node)
    assert len(tl_nodes) == len(ad_nodes)
    for idx, tl_node in enumerate(tl_nodes):
        assert tl_node is not ad_nodes[idx]


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
    loop step is not, or might not be, 1.

    '''
    code = TL_LOOP_CODE.replace("lo,hi,step", in_bounds)
    tl_psyir = fortran_reader.psyir_from_source(code)
    tl_loop = tl_psyir.walk(Loop)[0]
    assert isinstance(tl_loop, Loop)
    adj_visitor = AdjointVisitor(["a", "b", "c"])
    ad_psyir = adj_visitor(tl_psyir)
    ad_loop = ad_psyir.walk(Loop)[0]
    assert isinstance(ad_loop, Loop)
    result = fortran_writer(ad_loop)
    expected_result = (
        "do i = {0}\n"
        "  b(i) = b(i) + a(i)\n"
        "  c(i) = c(i) + a(i)\n"
        "  a(i) = 0.0\n"
        "enddo\n".format(out_bounds))
    assert result == expected_result


@pytest.mark.xfail(reason="issue #1235: caplog returns an empty string in "
                   "github actions.", strict=False)
def test_loop_logger(fortran_reader, caplog):
    '''Test that the logger writes the expected output if the loop_node
    method is called with an inactive node and an active node.

    '''
    tl_psyir = fortran_reader.psyir_from_source(TL_LOOP_CODE)
    tl_loop = tl_psyir.walk(Loop)[0]
    assert isinstance(tl_loop, Loop)

    adj_visitor = AdjointVisitor(["a", "b", "c"])
    # Need to use _visit() here rather than adj_visitor(tl_psyir) as
    # the latter takes a copy of the tree in case any lowering needs
    # to be done and that causes the stored symbols for active
    # variables to be different which means they do not match in
    # subsequent calls. This only a problem for tests as we don't
    # normally call loop_node() or similar, directly.
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
    _ = adj_visitor(tl_psyir)
    with caplog.at_level(logging.INFO):
        _ = adj_visitor.loop_node(tl_loop)
    assert caplog.text == ""
    with caplog.at_level(logging.DEBUG):
        _ = adj_visitor.loop_node(tl_loop)
    assert ("Returning a copy of the original loop and its descendants as it "
            "contains no active variables" in caplog.text)


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
