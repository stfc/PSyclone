# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council.
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
# Author R. W. Ford, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''Performs pytest tests on the psyclone.psyir.backend.sir module'''

import pytest
from psyclone.psyir.backend.sir import gen_stencil, SIRWriter
from psyclone.psyir.backend.base import VisitorError
from psyclone.psyGen import Node, PSyFactory
from fparser.common.readfortran import FortranStringReader


def create_schedule(code):
    '''Utility function that returns a PSyIR tree from Fortran
    code using fparser2 and NemoFparser2ASTProcessor.

    :param str code: Fortran code.

    :returns: PSyIR tree representing the Fortran code.
    :rtype: Subclass of :py:class:`psyclone.psyGen.Node`

    '''
    from psyclone.nemo import NemoFparser2ASTProcessor
    from fortran_test import create_schedule as f_create_schedule
    return f_create_schedule(code, ast_processor=NemoFparser2ASTProcessor)


# (1/3) function gen_stencil
def test_gen_stencil_1():
    '''Check the gen_stencil function produces the expected dimension
    strings.

    '''
    for form, expected in [("i,j,k,l,m", "[0,0,0,0,0]"),
                           ("i+1,j-1", "[1,-1]"),
                           ("m+7", "[7]"),
                           (" i + 1 , j , k - 1 ", "[1,0,-1]"),
                           ("i+1,j-2,k+3,l-4", "[1,-2,3,-4]"),
                           ("i+(1), j-(2)", "[1,-2]")]:
        code = (
            "module test\n"
            "contains\n"
            "  subroutine tmp()\n"
            "    real :: a(1,1,1)\n"
            "    integer :: i,j,k,l,m\n"
            "    a({0})=1.0\n"
            "  end subroutine tmp\n"
            "end module test\n".format(form))
        schedule = create_schedule(code)
        assignment = schedule.children[0]
        array_reference = assignment.children[0]
        result = gen_stencil(array_reference)
        assert result == expected


# (2/3) function gen_stencil
def test_gen_stencil_2():
    '''Check the gen_stencil function raises an exception when
    a node of the wrong type is provided.

    '''
    code = (
        "module test\n"
        "contains\n"
        "  subroutine tmp()\n"
        "  end subroutine tmp\n"
        "end module test\n")
    schedule = create_schedule(code)
    with pytest.raises(VisitorError) as excinfo:
        _ = gen_stencil(schedule)
    assert "gen_stencil expected an Array as input" in str(excinfo.value)


# (3/3) function gen_stencil
def test_gen_stencil_3():
    '''Check the gen_stencil function raises an exception when an
    unsupported form of indexing is found. Currently only "var +/-
    int" is supported.

    '''
    for form in ["1", "1+i", "-1+i", "i+j", "i+1+1", "i+(1+1)", "i*2"]:
        code = (
            "module test\n"
            "contains\n"
            "  subroutine tmp()\n"
            "    real :: a(1,1)\n"
            "    integer :: i,j\n"
            "    a({0})=1.0\n"
            "  end subroutine tmp\n"
            "end module test\n".format(form))
        schedule = create_schedule(code)
        assignment = schedule.children[0]
        array_reference = assignment.children[0]
        with pytest.raises(VisitorError) as excinfo:
            _ = gen_stencil(array_reference)
        if form in ["1"]:
            error = "unsupported (non-stencil) index found"
        elif form in ["i*2"]:
            error = "unsupported stencil operator found"
        else:
            error = "unsupported stencil index found"
        assert error in str(excinfo.value)


# Class SIRWriter start


# (1/2) Method __init__
def test_sirwriter_init_1():
    '''Check the __init__ function of the SIRWriter class sets default and
    initial values as expected.

    '''
    sir_writer = SIRWriter()
    # pylint: disable=protected-access
    assert sir_writer._field_names == set()
    assert not sir_writer._skip_nodes
    assert sir_writer._indent == "  "
    assert sir_writer._depth == 0
    # pylint: enable=protected-access


# (2/2) Method __init__
def test_sirwriter_init_2():
    '''Check the __init__ function of the SIRWriter class can change
    default values as expected.

    '''
    sir_writer = SIRWriter(skip_nodes=True, indent_string="[ooaah]",
                           initial_indent_depth=3)
    # pylint: disable=protected-access
    assert sir_writer._skip_nodes
    assert sir_writer._indent == "[ooaah]"
    assert sir_writer._depth == 3
    # pylint: enable=protected-access


# (1/1) Method node_node
def test_sirwriter_node_1():
    '''Check the node_node method of the SIRWriter class is called when an
    unsupported node is found and that it raises the appropriate
    exception if skip_nodes is false and continues (outputting
    information about the unsupported node) if skip_nodes is
    True. Also check for SIR indentation.

    '''
    code = (
        "module test\n"
        "contains\n"
        "  subroutine tmp()\n"
        "    real :: a(1)\n"
        "    integer :: i\n"
        "    a(i) = 1.0\n"
        "  end subroutine tmp\n"
        "end module test\n")
    schedule = create_schedule(code)

    # pylint: disable=abstract-method
    # modify the reference to b to be something unsupported
    class Unsupported(Node):
        '''A PSyIR node that will not be supported by the SIR writer.'''
    # pylint: enable=abstract-method

    unsupported = Unsupported()

    # Add the unsupported node as the root of the tree
    unsupported.children = [schedule]

    sir_writer = SIRWriter(skip_nodes=False)
    with pytest.raises(VisitorError) as excinfo:
        sir_writer(unsupported)
    assert "unsupported node found" in str(excinfo.value)

    sir_writer = SIRWriter(skip_nodes=True)
    result = sir_writer(unsupported)
    assert "[ Unsupported start ]" in result
    assert "[ Unsupported end ]" in result
    # Check indentation works.
    assert "    makeAssignmentStmt(" in result


def get_schedule(parser, code):
    ''' Utility function ... '''
    reader = FortranStringReader(code)
    prog = parser(reader)
    psy = PSyFactory(api="nemo").create(prog)
    return psy.invokes.invoke_list[0].schedule


def get_kernel(parser, code):
    ''' Utility function '''
    from psyclone.nemo import NemoKern
    schedule = get_schedule(parser, code)
    loop1 = schedule.children[0]
    loop2 = loop1.loop_body.children[0]
    loop3 = loop2.loop_body.children[0]
    kernel = loop3.loop_body.children[0]
    assert isinstance(kernel, NemoKern)
    return kernel


def get_assignment(parser, code):
    ''' Utility function ... '''
    from psyclone.psyGen import Assignment
    kernel = get_kernel(parser, code)
    kernel_schedule = kernel.get_kernel_schedule()
    assignment = kernel_schedule.children[0]
    assert isinstance(assignment, Assignment)
    return assignment


def get_lhs(parser, code):
    ''' Utility function ... '''
    assignment = get_assignment(parser, code)
    return assignment.lhs

    
def get_rhs(parser, code):
    ''' Utility function ... '''
    assignment = get_assignment(parser, code)
    return assignment.rhs


# (1/6) Method nemoloop_node
def test_sirwriter_nemoloop_node_1(parser):
    '''Check the nemoloop_node method of the SIRWriter class outputs the
    expected SIR code with two triply nested loops. Also test that it
    supports sir indentation.

    '''
    code = (
        "subroutine tmp(b,n)\n"
        "  integer,intent(in) :: n\n"
        "  real :: a(n,n,n), b(n,n,n)\n"
        "  integer :: i,j,k\n"
        "  do i=1,n\n"
        "    do j=1,n\n"
        "      do k=1,n\n"
        "        a(i,j,k) = 1.0\n"
        "      end do\n"
        "    end do\n"
        "  end do\n"
        "  do i=1,n\n"
        "    do j=1,n\n"
        "      do k=1,n\n"
        "        a(i,j,k) = a(i,j,k) + b(i,j,k)\n"
        "      end do\n"
        "    end do\n"
        "  end do\n"
        "end subroutine tmp\n")
    schedule = get_schedule(parser, code)
    sir_writer = SIRWriter()
    result = sir_writer(schedule)
    assert result.count(
        "interval = makeInterval(Interval.Start, Interval.End, 0, 0)\n"
        "bodyAST = makeAST([\n") == 2
    assert result.count(
        "])\n"
        "verticalRegionFns.append(makeVerticalRegionDeclStmt(bodyAST, "
        "interval, VerticalRegion.Forward))\n") == 2
    # Check for indentation.
    assert result.count("  makeAssignmentStmt(\n") == 2


# (2/6) Method nemoloop_node
def test_sirwriter_nemoloop_node_2(parser):
    '''Check the nemoloop_node method of the SIRWriter class raises an
    exception if the first child of a loop is not a loop.
    '''
    code = (
        "module test\n"
        "  contains\n"
        "  subroutine tmp(b,n)\n"
        "    integer,intent(in) :: n\n"
        "    real :: a(n,n,n)\n"
        "    integer :: i,j,k\n"
        "    do i=1,n\n"
        "      b(i,1,1) = 2.0\n"
        "      do j=1,n\n"
        "      end do\n"
        "    end do\n"
        "  end subroutine tmp\n"
        "end module test\n")
    schedule = get_schedule(parser, code)
    sir_writer = SIRWriter()
    with pytest.raises(VisitorError) as excinfo:
        _ = sir_writer(schedule)
    assert "Child of loop should be a single loop" in str(excinfo.value)


# (3/6) Method nemoloop_node
def test_sirwriter_nemoloop_node_3(parser):
    '''Check the nemoloop_node method of the SIRWriter class raises an
    exception if a loop has more than one child.

    '''
    code = (
        "module test\n"
        "  contains\n"
        "  subroutine tmp(b,n)\n"
        "    integer,intent(in) :: n\n"
        "    real :: a(n,n,n)\n"
        "    integer :: i,j,k\n"
        "    do i=1,n\n"
        "      do j=1,n\n"
        "      end do\n"
        "      do j=1,n\n"
        "      end do\n"
        "    end do\n"
        "  end subroutine tmp\n"
        "end module test\n")
    schedule = get_schedule(parser, code)
    sir_writer = SIRWriter()
    with pytest.raises(VisitorError) as excinfo:
        _ = sir_writer(schedule)
    assert "Child of loop should be a single loop" in str(excinfo.value)


# (4/6) Method nemoloop_node
def test_sirwriter_nemoloop_node_4(parser):
    '''Check the nemoloop_node method of the SIRWriter class raises an
    exception if the first child of the child of a loop is not a loop
    (i.e. not triply nested).

    '''
    code = (
        "module test\n"
        "  contains\n"
        "  subroutine tmp(b,n)\n"
        "    integer,intent(in) :: n\n"
        "    real :: a(n,n,n)\n"
        "    integer :: i,j,k\n"
        "    do i=1,n\n"
        "      do j=1,n\n"
        "        b(i,j,1) = 2.0\n"
        "        do k=1,n\n"
        "        end do\n"
        "      end do\n"
        "    end do\n"
        "  end subroutine tmp\n"
        "end module test\n")
    schedule = get_schedule(parser, code)
    sir_writer = SIRWriter()
    with pytest.raises(VisitorError) as excinfo:
        _ = sir_writer(schedule)
    assert ("Child of child of loop should be a single loop"
            in str(excinfo.value))


# (5/6) Method nemoloop_node
def test_sirwriter_nemoloop_node_5(parser):
    '''Check the nemoloop_node method of the SIRWriter class raises an
    exception if the child of a loop has more than one child (i.e. not
    triply nested).

    '''
    code = (
        "module test\n"
        "  contains\n"
        "  subroutine tmp(b,n)\n"
        "    integer,intent(in) :: n\n"
        "    real :: a(n,n,n)\n"
        "    integer :: i,j,k\n"
        "    do i=1,n\n"
        "      do j=1,n\n"
        "        do k=1,n\n"
        "        end do\n"
        "        do k=1,n\n"
        "        end do\n"
        "      end do\n"
        "    end do\n"
        "  end subroutine tmp\n"
        "end module test\n")
    schedule = get_schedule(parser, code)
    sir_writer = SIRWriter()
    with pytest.raises(VisitorError) as excinfo:
        _ = sir_writer(schedule)
    assert ("Child of child of loop should be a single loop"
            in str(excinfo.value))


# (6/6) Method nemoloop_node
def test_sirwriter_nemoloop_node_6(parser):
    '''Check the nemoloop_node method of the SIRWriter class raises an
    exception if the content of the triply nested loop is not a
    NemoKern.

    '''
    code = (
        "module test\n"
        "  contains\n"
        "  subroutine tmp(b,n)\n"
        "    integer,intent(in) :: n\n"
        "    real :: a(n,n,n,3)\n"
        "    integer :: i,j,k,l\n"
        "    do i=1,n\n"
        "      do j=1,n\n"
        "        do k=1,n\n"
        "          do l=1,3\n"
        "          end do\n"
        "        end do\n"
        "      end do\n"
        "    end do\n"
        "  end subroutine tmp\n"
        "end module test\n")
    schedule = get_schedule(parser, code)
    sir_writer = SIRWriter()
    with pytest.raises(VisitorError) as excinfo:
        _ = sir_writer(schedule)
    assert ("Child of child of child of loop should be a NemoKern."
            in str(excinfo.value))


CODE = (
    "module test\n"
    "  contains\n"
    "  subroutine tmp(n)\n"
    "    integer,intent(in) :: n\n"
    "    real :: a(n,n,n)\n"
    "    integer :: i,j,k\n"
    "    do i=1,n\n"
    "      do j=1,n\n"
    "        do k=1,n\n"
    "          a(i,j,k) = 1.0\n"
    "        end do\n"
    "      end do\n"
    "    end do\n"
    "  end subroutine tmp\n"
    "end module test\n")


# (1/1) Method nemokern_node
def test_sirwriter_nemokern_node(parser):
    '''Check the nemokern_node method of the SIRWriter class correctly
    calls the children of the schedule associated with the supplied
    kernel.

    '''
    kernel = get_kernel(parser, CODE)
    sir_writer = SIRWriter()
    result = sir_writer.nemokern_node(kernel)
    assert (
        "makeAssignmentStmt(\n"
        "  makeFieldAccessExpr(\"a\",[0,0,0]),\n"
        "  makeLiteralAccessExpr(\"1.0\", BuiltinType.Float),\n"
        "  \"=\")," in result)


# (1/1) Method nemoinvokeschedule_node
def test_sirwriter_nemoinvokeschedule_node_1(parser):
    '''Check the nemoinvokeschedule_node method of the SIRWriter class
    outputs the expected SIR code.

    '''
    schedule = get_schedule(parser, CODE)
    sir_writer = SIRWriter()
    result = sir_writer(schedule)
    assert (
        "# PSyclone autogenerated SIR Python\n"
        "verticalRegionFns = []\n"
        "stencilname = \"psyclone\"\n" in result)
    # Make sure that child nodes are also visited.
    assert (
        "interval = makeInterval(Interval.Start, Interval.End, 0, 0)\n"
        in result)
    assert (
        "hir = makeSIR(stencilname+\".cpp\", [\n"
        "  makeStencil(\n"
        "    stencilname,\n"
        "    makeAST(verticalRegionFns),\n"
        "    [makeField(\"a\")]\n"
        "  )\n"
        "])\n" in result)


# (1/1) Method assignment_node
def test_sirwriter_assignment_node(parser):
    '''Check the assignment_node method of the SIRWriter class
    outputs the expected SIR code.

    '''
    assignment = get_assignment(parser, CODE)
    sir_writer = SIRWriter()
    result = sir_writer.assignment_node(assignment)
    assert (
        "makeAssignmentStmt(\n"
        "  makeFieldAccessExpr(\"a\",[0,0,0]),\n"
        "  makeLiteralAccessExpr(\"1.0\", BuiltinType.Float),\n"
        in result)


# (1/2) Method binaryoperation_node
def test_sirwriter_binaryoperation_node_1(parser):
    '''Check the binaryoperation_node method of the SIRWriter class
    outputs the expected SIR code. Check all supported mappings.

    '''
    for oper in ["+", "-", "*", "/"]:
        code = CODE.replace("a(i,j,k) = 1.0", "a(i,j,k) = b {0} c".format(oper))
        rhs = get_rhs(parser, code)
        sir_writer = SIRWriter()
        result = sir_writer.binaryoperation_node(rhs)
        assert (
            "makeBinaryOperator(\n"
            "  makeVarAccessExpr(\"b\"),\n"
            "  \"{0}\",\n"
            "  makeVarAccessExpr(\"c\")\n"
            "  )\n".format(oper) in result)


# (2/2) Method binaryoperation_node
def test_sirwriter_binaryoperation_node_2(parser):
    '''Check the binaryoperation_node method of the SIRWriter class raises
    the expected exception if an unsupported binary operator is found.

    '''
    # Choose the power function (**) as there are no examples of its
    # use in the SIR so no mapping is currently provided.
    oper = "**"
    code = CODE.replace("a(i,j,k) = 1.0", "a(i,j,k) = b {0} c".format(oper))
    rhs = get_rhs(parser, code)
    sir_writer = SIRWriter()
    with pytest.raises(VisitorError) as excinfo:
        _ = sir_writer.binaryoperation_node(rhs)
    assert "unsupported operator 'Operator.POW' found" in str(excinfo.value)


# (1/2) Method reference_node
def test_sirwriter_reference_node_1(parser):
    '''Check the reference_node method of the SIRWriter class outputs the
    expected SIR when given a PSyIR Reference node.

    '''
    lhs = get_lhs(parser, CODE)
    sir_writer = SIRWriter()
    assert (sir_writer.reference_node(lhs.children[0]) ==
            "makeVarAccessExpr(\"i\")")
    assert (sir_writer.reference_node(lhs.children[1]) ==
            "makeVarAccessExpr(\"j\")")
    assert (sir_writer.reference_node(lhs.children[2]) ==
            "makeVarAccessExpr(\"k\")")


# (2/2) Method reference_node
def test_sirwriter_reference_node_2(parser):
    '''Check the reference_node method of the SIRWriter class raises an
    exception if the PSyIR Reference node has children.

    '''
    schedule = get_schedule(parser, CODE)
    sir_writer = SIRWriter()
    with pytest.raises(VisitorError) as excinfo:
        # Use a node which has children to raise the exception.
        _ = sir_writer.reference_node(schedule)
    assert ("SIR Reference node is not expected to have any children"
            in str(excinfo.value))


# (1/1) Method array_node
def test_sirwriter_array_node(parser):
    '''Check the array_node method of the SIRWriter class outputs the
    expected SIR when given a PSyIR Array node.

    '''
    lhs = get_lhs(parser, CODE)
    sir_writer = SIRWriter()
    assert (sir_writer.array_node(lhs) ==
            "makeFieldAccessExpr(\"a\",[0,0,0])")


# (1/2) Method literal_node
def test_sirwriter_literal_node_1(parser):
    '''Check the array_node method of the SIRWriter class outputs the
    expected SIR when given a PSyIR Literal node with a 'real' value.

    '''
    rhs = get_rhs(parser, CODE)
    sir_writer = SIRWriter()
    assert (sir_writer.literal_node(rhs) ==
            "makeLiteralAccessExpr(\"1.0\", BuiltinType.Float)")


# (2/2) Method literal_node
@pytest.mark.xfail(reason="#468 PSyIR does not capture the type of literals")
def test_sirwriter_literal_node_2(parser):
    '''Check the array_node method of the SIRWriter class outputs the
    expected SIR when given a PSyIR Literal node with an 'integer'
    value.

    '''
    code = CODE.replace("1.0", "1")
    rhs = get_rhs(parser, code)
    sir_writer = SIRWriter()
    assert (sir_writer.literal_node(rhs) ==
            "makeLiteralAccessExpr(\"1\", BuiltinType.Integer)")


# (1/4) Method unaryoperation_node
def test_sirwriter_unaryoperation_node_1(parser):
    '''Check the unaryoperation_node method of the SIRWriter class outputs
    the expected SIR code. Check all supported mappings - currently
    there is only one.

    '''
    for oper in ["-"]:  # Currently only one supported mapping
        code = CODE.replace("1.0", "{0}1.0".format(oper))
        rhs = get_rhs(parser, code)
        sir_writer = SIRWriter()
        result = sir_writer.unaryoperation_node(rhs)
        assert "makeLiteralAccessExpr(\"-1.0\", BuiltinType.Float)" in result


# (2/4) Method unaryoperation_node
def test_sirwriter_unary_node_2(parser):
    '''Check the unaryoperation_node method of the SIRWriter class raises
    the expected exception if an unsupported unary operator is found.

    '''
    # Choose the sin function as there are no examples of its
    # use in the SIR so no mapping is currently provided.
    code = CODE.replace("1.0", "sin(1.0)")
    rhs = get_rhs(parser, code)
    sir_writer = SIRWriter()
    with pytest.raises(VisitorError) as excinfo:
        _ = sir_writer.unaryoperation_node(rhs)
    assert "unsupported operator 'Operator.SIN' found" in str(excinfo.value)


# (3/4) Method unaryoperation_node
def test_sirwriter_unary_node_3(parser):
    '''Check the unaryoperation_node method of the SIRWriter class raises
    the expected exception if the subject of the unary operator is not
    a literal value (as currently only '-' is supported and it is only
    supported for literal values).

    '''
    code = CODE.replace("1.0", "-a(i,j,k)")
    rhs = get_rhs(parser, code)
    sir_writer = SIRWriter()
    with pytest.raises(VisitorError) as excinfo:
        _ = sir_writer.unaryoperation_node(rhs)
    assert ("Child of unary operator should be a literal."
            in str(excinfo.value))


# (4/4) Method unaryoperation_node
@pytest.mark.xfail(reason="#468 PSyIR does not capture the type of literals")
def test_sirwriter_unary_node_4(parser):
    '''Check the unaryoperation_node method of the SIRWriter class outputs
    the expected SIR when the subject of the unary operator is an
    integer literal.

    '''
    code = CODE.replace("1.0", "1")
    rhs = get_rhs(parser, code)
    sir_writer = SIRWriter()
    result = sir_writer.unaryoperation_node(rhs)
    assert "makeLiteralAccessExpr(\"-1\", BuiltinType.Integer)" in result

# Class SIRWriter end
