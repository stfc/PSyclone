import logging

import pytest

from psyclone.psyad import AdjointVisitor
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import FileContainer, Assignment
from psyclone.tests.utilities import Compile

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


# TODO visitor schedule


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
            "called beforehand as the latter sets up the active variables.")
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


# TODO visitor loop
