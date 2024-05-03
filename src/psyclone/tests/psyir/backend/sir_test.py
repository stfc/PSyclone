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
# Author: R. W. Ford, STFC Daresbury Lab
# Modified by: A. R. Porter and S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''Performs pytest tests on the psyclone.psyir.backend.sir module'''

import pytest

from fparser.common.readfortran import FortranStringReader

from psyclone.psyGen import PSyFactory
from psyclone.psyir.backend.sir import gen_stencil, SIRWriter
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.nodes import (
    Assignment, BinaryOperation, IfBlock, Literal, Loop,
    Node, Schedule, UnaryOperation)
from psyclone.psyir.symbols import INTEGER_TYPE


# pylint: disable=redefined-outer-name
@pytest.fixture(scope="function")
def sir_writer():
    '''Create and return an sir SIRWriter object with default settings.'''
    return SIRWriter()


# Sample code for use in tests.
CODE = (
    "module test\n"
    "  contains\n"
    "  subroutine tmp(n)\n"
    "    integer,intent(in) :: n\n"
    "    real :: a(n,n,n)\n"
    "    integer :: i,j,k,l,m\n"
    "    do i=1,n\n"
    "      do j=1,n\n"
    "        do k=1,n\n"
    "          a(i,j,k) = 1.0\n"
    "        end do\n"
    "      end do\n"
    "    end do\n"
    "  end subroutine tmp\n"
    "end module test\n")


def get_schedule(parser, code):
    '''Utility function that returns the first schedule for a code with
    the NEMO api.

    :param parser: the parser class.
    :type parser: :py:class:`fparser.two.Fortran2003.Program`
    :param str code: the code as a string.

    :returns: the first schedule in the supplied code.
    :rtype: :py:class:`psyclone.nemo.NemoInvokeSchedule`

    '''
    reader = FortranStringReader(code)
    prog = parser(reader)
    psy = PSyFactory(api="nemo").create(prog)
    return psy.invokes.invoke_list[0].schedule


def get_assignment(parser, code):
    '''Utility function that returns the assignment (x=y) in code similar
    to that specified in the CODE string.

    :param parser: the parser class.
    :type parser: :py:class:`fparser.two.Fortran2003.Program`
    :param str code: the code as a string.

    :returns: an assignment node from the supplied code.
    :rtype: :py:class:`psyclone.psyir.nodes.Assignment`

    '''
    schedule = get_schedule(parser, code)
    assignment = schedule.walk(Assignment)[0]
    assert isinstance(assignment, Assignment)
    return assignment


def get_lhs(parser, code):
    '''Utility function that returns the left hand side of an assignment
    (x=y) in code similar to that specified in the CODE string.

    :param parser: the parser class.
    :type parser: :py:class:`fparser.two.Fortran2003.Program`
    :param str code: the code as a string.

    :returns: an array node from the supplied code.
    :rtype: subclass of :py:class:`psyclone.psyir.nodes.Node`

    '''
    assignment = get_assignment(parser, code)
    return assignment.lhs


def get_rhs(parser, code):
    '''Utility function that returns the left hand side of an assignment
    (x=y) in code similar to that specified in the CODE string.

    :param parser: the parser class.
    :type parser: :py:class:`fparser.two.Fortran2003.Program`
    :param str code: the code as a string.

    :returns: the right hand side of an assignment from the supplied \
    code.
    :rtype: subclass of :py:class:`psyclone.psyir.nodes.Node`

    '''
    assignment = get_assignment(parser, code)
    return assignment.rhs


# (1/3) function gen_stencil
def test_gen_stencil_1(parser):
    '''Check the gen_stencil function produces the expected dimension
    strings.

    '''
    for form, expected in [("i,j,k,l,m", "[0, 0, 0, 0, 0]"),
                           ("i+1,j-1", "[1, -1]"),
                           ("m+7", "[7]"),
                           (" i + 1 , j , k - 1 ", "[1, 0, -1]"),
                           ("i+1,j-2,k+3,l-4", "[1, -2, 3, -4]"),
                           ("i+(1), j-(2)", "[1, -2]")]:
        code = CODE.replace("a(i,j,k)", f"a({form})")
        lhs = get_lhs(parser, code)
        result = gen_stencil(lhs)
        assert result == expected


# (2/3) function gen_stencil
def test_gen_stencil_2(parser):
    '''Check the gen_stencil function raises an exception when
    a node of the wrong type is provided.

    '''
    schedule = get_schedule(parser, CODE)
    with pytest.raises(VisitorError) as excinfo:
        _ = gen_stencil(schedule)
    assert ("gen_stencil expected an ArrayReference as input" in
            str(excinfo.value))


# (3/3) function gen_stencil
def test_gen_stencil_3(parser):
    '''Check the gen_stencil function raises an exception when an
    unsupported form of indexing is found. Currently only "var +/-
    int" is supported.

    '''
    for form in ["1", "1+i", "-1+i", "i+j", "i+1+1", "i+(1+1)", "i*2"]:
        code = CODE.replace("a(i,j,k)", f"a({form},j,k)")
        lhs = get_lhs(parser, code)
        with pytest.raises(VisitorError) as excinfo:
            _ = gen_stencil(lhs)
        if form in ["1"]:
            error = "unsupported (non-stencil) index found"
        elif form in ["i*2"]:
            error = "unsupported stencil operator found"
        else:
            error = "unsupported stencil index found"
        assert error in str(excinfo.value)


# Class SIRWriter start


# (1/2) Method __init__
def test_sirwriter_init_1(sir_writer):
    '''Check the __init__ function of the SIRWriter class sets default and
    initial values as expected.

    '''
    assert sir_writer._field_names == set()
    assert sir_writer._scalar_names == set()
    assert not sir_writer._skip_nodes
    assert sir_writer._indent == "  "
    assert sir_writer._depth == 0


# (2/2) Method __init__
def test_sirwriter_init_2():
    '''Check the __init__ function of the SIRWriter class can change
    default values as expected.

    '''
    sir_writer = SIRWriter(skip_nodes=True, indent_string="[ooaah]",
                           initial_indent_depth=3)
    assert sir_writer._skip_nodes
    assert sir_writer._indent == "[ooaah]"
    assert sir_writer._depth == 3


# (1/1) Method node_node
def test_sirwriter_node_1(parser):
    '''Check the node_node method of the SIRWriter class is called when an
    unsupported node is found and that it raises the appropriate
    exception if skip_nodes is false and continues (outputting
    information about the unsupported node) if skip_nodes is
    True. Also check for SIR indentation.

    '''
    schedule = get_schedule(parser, CODE)

    class Unsupported(Node):
        '''A PSyIR node that will not be supported by the SIR writer but
        accepts any children inside.'''
        @staticmethod
        def _validate_child(_1, _2):
            return True
    # pylint: enable=abstract-method

    unsupported = Unsupported()

    # Add the unsupported node as the root of the tree
    unsupported.children = [schedule.detach()]

    sir_writer = SIRWriter(skip_nodes=False)
    with pytest.raises(VisitorError) as excinfo:
        sir_writer(unsupported)
    assert "unsupported node found" in str(excinfo.value)

    sir_writer = SIRWriter(skip_nodes=True)
    result = sir_writer(unsupported)
    assert "[ Unsupported start ]" in result
    assert "[ Unsupported end ]" in result
    # Check indentation works.
    assert "    make_assignment_stmt(" in result


# (1/6) Method nemoloop_node
def test_sirwriter_nemoloop_node_1(parser, sir_writer):
    '''Check the nemoloop_node method of the SIRWriter class outputs the
    expected SIR code with two triply nested loops. Also test that it
    supports sir indentation.

    '''
    code = CODE.replace(
        "  end subroutine tmp\n",
        "  do i=1,n\n"
        "    do j=1,n\n"
        "      do k=1,n\n"
        "        a(i,j,k) = a(i,j,k) + b(i,j,k)\n"
        "      end do\n"
        "    end do\n"
        "  end do\n"
        "end subroutine tmp\n")
    code = code.replace(
        "    real :: a(n,n,n)\n",
        "    real :: a(n,n,n), b(n,n,n)\n")
    schedule = get_schedule(parser, code)
    result = sir_writer(schedule)
    assert result.count(
        "interval = make_interval(Interval.Start, Interval.End, 0, 0)\n"
        "body_ast = make_ast([\n") == 2
    assert result.count(
        "])\n"
        "vertical_region_fns.append(make_vertical_region_decl_stmt(body_ast, "
        "interval, VerticalRegion.Forward))\n") == 2
    # Check for indentation.
    assert result.count("  make_assignment_stmt(\n") == 2


# (2/6) Method nemoloop_node
def test_sirwriter_nemoloop_node_2(parser, sir_writer):
    '''Check the nemoloop_node method of the SIRWriter class raises an
    exception if the first child of a loop is not a loop.
    '''
    code = CODE.replace(
        "      do j=1,n\n",
        "      a(i,1,1) = 1.0\n"
        "      do j=1,n\n")
    schedule = get_schedule(parser, code)
    with pytest.raises(VisitorError) as excinfo:
        _ = sir_writer(schedule)
    assert "Child of loop should be a single loop" in str(excinfo.value)


# (3/6) Method nemoloop_node
def test_sirwriter_nemoloop_node_3(parser, sir_writer):
    '''Check the nemoloop_node method of the SIRWriter class raises an
    exception if a loop has more than one child.

    '''
    code = CODE.replace(
        "      do j=1,n\n"
        "        do k=1,n\n"
        "          a(i,j,k) = 1.0\n"
        "        end do\n"
        "      end do\n",
        "      do j=1,n\n"
        "      end do\n"
        "      do j=1,n\n"
        "      end do\n")
    schedule = get_schedule(parser, code)
    with pytest.raises(VisitorError) as excinfo:
        _ = sir_writer(schedule)
    assert "Child of loop should be a single loop" in str(excinfo.value)


# (4/6) Method nemoloop_node
def test_sirwriter_nemoloop_node_4(parser, sir_writer):
    '''Check the nemoloop_node method of the SIRWriter class raises an
    exception if the first child of the child of a loop is not a loop
    (i.e. not triply nested).

    '''
    code = CODE.replace(
        "        do k=1,n\n"
        "          a(i,j,k) = 1.0\n"
        "        end do\n",
        "        a(i,j,1) = 1.0\n"
        "        do k=1,n\n"
        "        end do\n")
    schedule = get_schedule(parser, code)
    with pytest.raises(VisitorError) as excinfo:
        _ = sir_writer(schedule)
    assert ("Child of child of loop should be a single loop"
            in str(excinfo.value))


# (5/6) Method nemoloop_node
def test_sirwriter_nemoloop_node_5(parser, sir_writer):
    '''Check the nemoloop_node method of the SIRWriter class raises an
    exception if the child of a loop has more than one child (i.e. not
    triply nested).

    '''
    code = CODE.replace(
        "        do k=1,n\n"
        "          a(i,j,k) = 1.0\n"
        "        end do\n",
        "        do k=1,n\n"
        "        end do\n"
        "        do k=1,n\n"
        "        end do\n")
    schedule = get_schedule(parser, code)
    with pytest.raises(VisitorError) as excinfo:
        _ = sir_writer(schedule)
    assert "Only triply-nested loops are supported" in str(excinfo.value)


# (6/6) Method nemoloop_node
def test_sirwriter_nemoloop_node_6(parser, sir_writer):
    '''Check the nemoloop_node method of the SIRWriter class raises an
    exception if the content of the triply nested loop is another loop.

    '''
    code = CODE.replace("          a(i,j,k) = 1.0\n",
                        "          do l=1,3\n"
                        "            a(i,j,k,l) = 1.0\n"
                        "          end do\n")
    code = code.replace("real :: a(n,n,n)", "real :: a(n,n,n,3)")
    schedule = get_schedule(parser, code)
    with pytest.raises(VisitorError) as excinfo:
        _ = sir_writer(schedule)
    assert ("Only triply-nested loops are supported."
            in str(excinfo.value))


def test_sirwriter_nemoloop_node_not_compute(parser, sir_writer):
    '''Check the nemoloop_node method of the SIRWriter class raises an
    exception if the content of the triply nested loop is not computation.

    '''
    code = CODE.replace("          a(i,j,k) = 1.0\n",
                        "          write(*,*) a(i,j,k)\n")
    schedule = get_schedule(parser, code)
    with pytest.raises(VisitorError) as excinfo:
        _ = sir_writer(schedule)
    assert ("A loop nest containing a CodeBlock cannot be translated to SIR"
            in str(excinfo.value))


# (1/2) Method nemoinvokeschedule_node
def test_sirwriter_nemoinvokeschedule_node_1(parser, sir_writer):
    '''Check the nemoinvokeschedule_node method of the SIRWriter class
    outputs the expected SIR code.

    '''
    schedule = get_schedule(parser, CODE)
    result = sir_writer(schedule)
    assert (
        "# PSyclone autogenerated SIR Python\n"
        "vertical_region_fns = []\n"
        "stencil_name = \"psyclone\"\n" in result)
    # Make sure that child nodes are also visited.
    assert (
        "interval = make_interval(Interval.Start, Interval.End, 0, 0)\n"
        in result)
    assert (
        "hir = make_sir(stencil_name+\".cpp\", "
        "AST.GridType.Value(\"Cartesian\"), [\n"
        "  make_stencil(\n"
        "    stencil_name,\n"
        "    make_ast(vertical_region_fns),\n"
        "    [make_field(\"a\", make_field_dimensions_cartesian())]\n"
        "  )\n"
        "])\n" in result)


# (2/2) Method nemoinvokeschedule_node
def test_sirwriter_nemoinvokeschedule_node_2(parser, sir_writer,
                                             monkeypatch):
    '''Check the nemoinvokeschedule_node method of the SIRWriter class
    outputs the expected SIR code when there is a scalar variable.

    '''
    code = CODE.replace("\n    integer ::", "\n    real :: b\n    integer ::")
    code = code.replace("a(i,j,k) = 1.0", "b = a(i,j,k)")
    schedule = get_schedule(parser, code)
    loops = schedule.walk(Loop)
    # Writing to a shared scalar is not parallel so monkeypatch the check to
    # allow it through.
    monkeypatch.setattr(loops[2], "independent_iterations", lambda: True)
    result = sir_writer(schedule)
    assert (
        "# PSyclone autogenerated SIR Python\n"
        "vertical_region_fns = []\n"
        "stencil_name = \"psyclone\"\n" in result)
    # Make sure that child nodes are also visited.
    assert (
        "interval = make_interval(Interval.Start, Interval.End, 0, 0)\n"
        in result)
    assert (
        "hir = make_sir(stencil_name+\".cpp\", "
        "AST.GridType.Value(\"Cartesian\"), [\n"
        "  make_stencil(\n"
        "    stencil_name,\n"
        "    make_ast(vertical_region_fns),\n"
        "    [make_field(\"a\", make_field_dimensions_cartesian()), "
        "make_field(\"b\", make_field_dimensions_cartesian(), "
        "is_temporary=True)]\n"
        "  )\n"
        "])\n" in result)


# (1/1) Method assignment_node
def test_sirwriter_assignment_node(parser, sir_writer):
    '''Check the assignment_node method of the SIRWriter class
    outputs the expected SIR code.

    '''
    assignment = get_assignment(parser, CODE)
    result = sir_writer.assignment_node(assignment)
    assert (
        "make_assignment_stmt(\n"
        "  make_field_access_expr(\"a\", [0, 0, 0]),\n"
        "  make_literal_access_expr(\"1.0\", BuiltinType.Float),\n"
        in result)


# (1/4) Method binaryoperation_node
@pytest.mark.parametrize("oper", ["+", "-", "*", "/", "**"])
def test_sirwriter_binaryoperation_node_1(parser, sir_writer, oper):
    '''Check the binaryoperation_node method of the SIRWriter class
    outputs the expected SIR code. Check all supported computation
    mappings.

    '''
    code = CODE.replace("\n    integer ::",
                        "\n    real :: b, c\n    integer ::")
    code = code.replace(
        "a(i,j,k) = 1.0", f"a(i,j,k) = b {oper} c")
    rhs = get_rhs(parser, code)
    result = sir_writer.binaryoperation_node(rhs)
    assert (
        f"make_binary_operator(\n"
        f"  make_field_access_expr(\"b\"),\n"
        f"  \"{oper}\",\n"
        f"  make_field_access_expr(\"c\")\n"
        f"  )\n" in result)


# (2/4) Method binaryoperation_node
@pytest.mark.parametrize(
    "foper,soper",
    [(".eq.", "=="), ("/=", "!="), (".le.", "<="), (".lt.", "<"),
     (".ge.", ">="), (".gt.", ">"), (".and.", "&&"), (".or.", "||")])
def test_sirwriter_binaryoperation_node_2(parser, sir_writer, foper, soper):
    '''Check the binaryoperation_node method of the SIRWriter class
    outputs the expected SIR code. Check all supported comparator
    mappings.

    '''
    code = CODE.replace("\n    integer ::",
                        "\n    real :: b, c\n    integer ::")
    code = code.replace(
        "a(i,j,k) = 1.0", f"if (b {foper} c) then\na(i,j,k) = 1.0\nend if")
    sched = get_schedule(parser, code)
    if_statement = sched.walk(IfBlock)[0]
    if_condition = if_statement.condition
    result = sir_writer.binaryoperation_node(if_condition)
    assert (
        f"make_binary_operator(\n"
        f"  make_field_access_expr(\"b\"),\n"
        f"  \"{soper}\",\n"
        f"  make_field_access_expr(\"c\")\n"
        f"  )\n" in result)


# (3/4) Method binaryoperation_node
def test_sirwriter_binaryoperation_node_3(parser, sir_writer):
    '''Check the binaryoperation_node method of the SIRWriter class
    outputs the expected SIR code when there are are a series of
    binary operations. The reason for this test is that, for
    formatting purposes the number of carriage returns needs to be
    managed in this case due to the SIR makeBinaryOperator functions
    being nested.

    '''
    code = CODE.replace("\n    integer ::",
                        "\n    real :: b, c, d\n    integer ::")
    code = code.replace("a(i,j,k) = 1.0", "a(i,j,k) = b*c+d")
    rhs = get_rhs(parser, code)
    result = sir_writer.binaryoperation_node(rhs)
    assert (
        "make_binary_operator(\n"
        "  make_binary_operator(\n"
        "    make_field_access_expr(\"b\"),\n"
        "    \"*\",\n"
        "    make_field_access_expr(\"c\")\n"
        "    ),\n"
        "  \"+\",\n"
        "  make_field_access_expr(\"d\")\n"
        "  )" in result)


def test_sirwriter_binaryoperator_not_supported(sir_writer):
    ''' Check that unsupported BinaryOperators produce a relevant error. '''
    operation = BinaryOperation.create(
        BinaryOperation.Operator.REM,
        Literal("1", INTEGER_TYPE),
        Literal("2", INTEGER_TYPE))
    with pytest.raises(VisitorError) as excinfo:
        sir_writer.binaryoperation_node(operation)
    assert ("Method binaryoperation_node in class SIRWriter, unsupported "
            "operator 'Operator.REM' found." in str(excinfo.value))


def test_sirwriter_intrinsiccall_node(parser, sir_writer):
    '''Check the intrinsiccall_node method of the SIRWriter class raises
    the expected exception if an unsupported intrinsic is found.

    '''
    code = CODE.replace("\n    integer ::",
                        "\n    real :: b, c\n    integer ::")
    # Choose the matmul function as there is no direct support for it in
    # in the SIR and no mapping is currently provided.
    code = code.replace("a(i,j,k) = 1.0", "a(i,j,k) = matmul(b, c)")
    rhs = get_rhs(parser, code)
    with pytest.raises(VisitorError) as excinfo:
        _ = sir_writer.intrinsiccall_node(rhs)
    assert "unsupported intrinsic 'MATMUL' found" in str(excinfo.value)


# (1/2) Method reference_node
def test_sirwriter_reference_node_1(parser, sir_writer):
    '''Check the reference_node method of the SIRWriter class outputs the
    expected SIR when given a PSyIR Reference node.

    '''
    lhs = get_lhs(parser, CODE)
    assert (sir_writer.reference_node(lhs.children[0]) ==
            "make_field_access_expr(\"i\")")
    assert (sir_writer.reference_node(lhs.children[1]) ==
            "make_field_access_expr(\"j\")")
    assert (sir_writer.reference_node(lhs.children[2]) ==
            "make_field_access_expr(\"k\")")


# (2/2) Method reference_node
def test_sirwriter_reference_node_2(parser, sir_writer):
    '''Check the reference_node method of the SIRWriter class raises an
    exception if the PSyIR Reference node has children.

    '''
    schedule = get_schedule(parser, CODE)
    with pytest.raises(VisitorError) as excinfo:
        # Use a node which has children to raise the exception.
        _ = sir_writer.reference_node(schedule)
    assert ("SIR Reference node is not expected to have any children"
            in str(excinfo.value))


# (1/1) Method array_node
def test_sirwriter_array_node(parser, sir_writer):
    '''Check the array_node method of the SIRWriter class outputs the
    expected SIR when given a PSyIR ArrayReference node.

    '''
    lhs = get_lhs(parser, CODE)
    assert (sir_writer.arrayreference_node(lhs) ==
            "make_field_access_expr(\"a\", [0, 0, 0])")


# (1/3) Method literal_node
def test_sirwriter_literal_node_1(parser, sir_writer):
    '''Check the arrayreference_node method of the SIRWriter class outputs
    the expected SIR when given a PSyIR Literal node with a 'real' value.

    '''
    rhs = get_rhs(parser, CODE)
    assert (sir_writer.literal_node(rhs) ==
            "make_literal_access_expr(\"1.0\", BuiltinType.Float)")


# (2/3) Method literal_node
def test_sirwriter_literal_node_2(parser, sir_writer):
    '''Check the arrayreference_node method of the SIRWriter class outputs the
    expected SIR when given a PSyIR Literal node with an 'integer'
    value.

    '''
    code = CODE.replace("1.0", "1")
    rhs = get_rhs(parser, code)
    assert (sir_writer.literal_node(rhs) ==
            "make_literal_access_expr(\"1\", BuiltinType.Integer)")


# (3/3) Method literal_node
@pytest.mark.parametrize("value,datatype", [(".true.", "BOOLEAN"),
                                            ("'hello'", "CHARACTER")])
def test_sirwriter_literal_node_error(parser, sir_writer, value, datatype):
    '''Check the arrayreference_node method of the SIRWriter class raises the
    expected exception when given a PSyIR Literal node with an
    unsupported value.

    '''
    code = CODE.replace("1.0", value)
    rhs = get_rhs(parser, code)
    with pytest.raises(VisitorError) as excinfo:
        sir_writer.literal_node(rhs)
    assert (
        f"PSyIR type 'Scalar<{datatype}, UNDEFINED>' has no representation in "
        f"the SIR backend." in str(excinfo.value))


# (1/5) Method unaryoperation_node
def test_sirwriter_unaryoperation_node_1(parser, sir_writer):
    '''Check the unaryoperation_node method of the SIRWriter class outputs
    the expected SIR code. Check all supported mappings - currently
    there is only one.

    '''
    for oper in ["-"]:  # Currently only one supported mapping
        code = CODE.replace("1.0", f"{oper}1.0")
        rhs = get_rhs(parser, code)
        result = sir_writer.unaryoperation_node(rhs)
        assert ("make_literal_access_expr(\"-1.0\", BuiltinType.Float)"
                in result)


# (3/5) Method unaryoperation_node
@pytest.mark.parametrize(
    "value, datatype", [("-1", "Integer"), ("-1.0", "Float")])
def test_sirwriter_unary_node_3(parser, sir_writer, value, datatype):
    '''Check the unaryoperation_node method of the SIRWriter class outputs
    the expected SIR when the subject of the unary operator is a
    literal (tests for both integer and real).

    '''
    code = CODE.replace("1.0", value)
    rhs = get_rhs(parser, code)
    result = sir_writer.unaryoperation_node(rhs)
    assert (f"make_literal_access_expr(\"{value}\", BuiltinType.{datatype})"
            in result)


# (4/5) Method unaryoperation_node
def test_sirwriter_unary_node_4(parser, sir_writer):
    '''Check the unaryoperation_node method of the SIRWriter class raises
    the expected Exception when the subject of the '-' unary operator
    is a literal but is not of type REAL or INTEGER.

    '''
    code = CODE.replace("1.0", "-.false.")
    rhs = get_rhs(parser, code)
    with pytest.raises(VisitorError) as excinfo:
        _ = sir_writer.unaryoperation_node(rhs)
    assert ("PSyIR type 'Scalar<BOOLEAN, UNDEFINED>' does not work "
            "with the '-' operator." in str(excinfo.value))


# (5/5) Method unaryoperation_node
def test_sirwriter_unary_node_5(parser, sir_writer):
    '''Check the unaryoperation_node method of the SIRWriter class outputs
    the expected SIR when the subject of the unary operator is not a
    literal.

    '''
    code = CODE.replace("1.0", "-(a(i,j,k)-b(i,j,k))")
    code = code.replace(
        "    real :: a(n,n,n)\n",
        "    real :: a(n,n,n), b(n,n,n)\n")
    rhs = get_rhs(parser, code)
    result = sir_writer.unaryoperation_node(rhs)
    assert (
        result ==
        "make_binary_operator(\n"
        "  make_literal_access_expr(\"-1.0\", BuiltinType.Float),\n"
        "  \"*\",\n"
        "  make_binary_operator(\n"
        "    make_field_access_expr(\"a\", [0, 0, 0]),\n"
        "    \"-\",\n"
        "    make_field_access_expr(\"b\", [0, 0, 0])\n"
        "    )\n"
        ")\n")


def test_sirwriter_unaryoperator_not_supported(sir_writer):
    ''' Check that unsupported UnaryOperators produce a relevant error. '''
    operation = UnaryOperation.create(
        UnaryOperation.Operator.NOT,
        Literal("1", INTEGER_TYPE))
    with pytest.raises(VisitorError) as excinfo:
        sir_writer.unaryoperation_node(operation)
    assert ("Method unaryoperation_node in class SIRWriter, unsupported "
            "operator 'Operator.NOT' found." in str(excinfo.value))


# (1/4) Method ifblock_node
def test_sirwriter_ifblock_node_1(parser, sir_writer):
    '''Check the ifblock_node method of the SIRWriter class
    creates the expected code when there is an if statement with no
    else clause.

    '''
    code = CODE.replace("\n    integer ::",
                        "\n    integer :: b, c\n    integer ::")
    code = code.replace(
        "a(i,j,k) = 1.0", "if (b .eq. c) then\na(i,j,k) = 1.0\nend if")
    sched = get_schedule(parser, code)
    if_statement = sched.walk(IfBlock)[0]
    result = sir_writer.ifblock_node(if_statement)
    assert (
        "make_if_stmt(make_expr_stmt(make_binary_operator(\n"
        "  make_field_access_expr(\"b\"),\n"
        "  \"==\",\n"
        "  make_field_access_expr(\"c\")\n"
        "  )), make_block_stmt([make_assignment_stmt(\n"
        "  make_field_access_expr(\"a\", [0, 0, 0]),\n"
        "  make_literal_access_expr(\"1.0\", BuiltinType.Float),\n"
        "  \"=\")]), None),\n" in result)


# (2/4) Method ifblock_node
def test_sirwriter_ifblock_node_2(parser, sir_writer):
    '''Check the ifblock_node method of the SIRWriter class creates the
    expected code when there is an if statement with an else clause.

    '''
    code = CODE.replace("\n    integer ::",
                        "\n    integer :: b, c\n    integer ::")
    code = code.replace(
        "a(i,j,k) = 1.0", "if (b .eq. c) then\na(i,j,k) = 1.0\nelse\n"
        "a(i,j,k) = 0.0\nend if")
    sched = get_schedule(parser, code)
    if_statement = sched.walk(IfBlock)[0]
    result = sir_writer.ifblock_node(if_statement)
    assert (
        "make_if_stmt(make_expr_stmt(make_binary_operator(\n"
        "  make_field_access_expr(\"b\"),\n"
        "  \"==\",\n"
        "  make_field_access_expr(\"c\")\n"
        "  )), make_block_stmt([make_assignment_stmt(\n"
        "  make_field_access_expr(\"a\", [0, 0, 0]),\n"
        "  make_literal_access_expr(\"1.0\", BuiltinType.Float),\n"
        "  \"=\")]), make_block_stmt([make_assignment_stmt(\n"
        "  make_field_access_expr(\"a\", [0, 0, 0]),\n"
        "  make_literal_access_expr(\"0.0\", BuiltinType.Float),\n"
        "  \"=\")])),\n" in result)


# (3/4) Method ifblock_node
def test_sirwriter_ifblock_node_3(parser, sir_writer):
    '''Check the ifblock_node method of the SIRWriter class creates the
    expected code when there is more than one if statement in the code.

    '''
    code = CODE.replace("\n    integer ::",
                        "\n    integer :: b, c\n    integer ::")
    code = code.replace(
        "a(i,j,k) = 1.0", "if (b .eq. c) then\na(i,j,k) = 1.0\nend if\n"
        "if (c .ge. 0.5) then\na(i,j,k) = -1.0\nend if\n")
    sched = get_schedule(parser, code)
    if_stmts = sched.walk(IfBlock)
    if_statement_0 = if_stmts[0]
    result_0 = sir_writer.ifblock_node(if_statement_0)
    if_statement_1 = if_stmts[1]
    result_1 = sir_writer.ifblock_node(if_statement_1)
    assert (
        "make_if_stmt(make_expr_stmt(make_binary_operator(\n"
        "  make_field_access_expr(\"b\"),\n"
        "  \"==\",\n"
        "  make_field_access_expr(\"c\")\n"
        "  )), make_block_stmt([make_assignment_stmt(\n"
        "  make_field_access_expr(\"a\", [0, 0, 0]),\n"
        "  make_literal_access_expr(\"1.0\", BuiltinType.Float),\n"
        "  \"=\")]), None),\n" in result_0)
    assert (
        "make_if_stmt(make_expr_stmt(make_binary_operator(\n"
        "  make_field_access_expr(\"c\"),\n"
        "  \">=\",\n"
        "  make_literal_access_expr(\"0.5\", BuiltinType.Float)\n"
        "  )), make_block_stmt([make_assignment_stmt(\n"
        "  make_field_access_expr(\"a\", [0, 0, 0]),\n"
        "  make_literal_access_expr(\"-1.0\", BuiltinType.Float),\n"
        "  \"=\")]), None),\n" in result_1)


# (4/4) Method ifblock_node
def test_sirwriter_ifblock_node_4(parser, sir_writer):
    '''Check the ifblock_node method of the SIRWriter class creates the
    expected code when ifs are nested within each other.

    '''
    code = CODE.replace("\n    integer ::",
                        "\n    integer :: b, c\n    integer ::")
    code = code.replace(
        "a(i,j,k) = 1.0",
        "if (b .eq. c) then\n"
        "  if (b. gt. 0.5) then\n"
        "    a(i,j,k) = 1.0\n"
        "  end if\n"
        "else\n"
        "  if (c .lt. 0.5) then\n"
        "    a(i,j,k) = 0.0\n"
        "  else\n"
        "    a(i,j,k) = -1.0\n"
        "  end if\n"
        "end if")
    sched = get_schedule(parser, code)
    if_statement = sched.walk(IfBlock)[0]
    result = sir_writer.ifblock_node(if_statement)
    assert (
        "make_if_stmt(make_expr_stmt(make_binary_operator(\n"
        "  make_field_access_expr(\"b\"),\n"
        "  \"==\",\n"
        "  make_field_access_expr(\"c\")\n"
        "  )), make_block_stmt([make_if_stmt(make_expr_stmt("
        "make_binary_operator(\n"
        "  make_field_access_expr(\"b\"),\n"
        "  \">\",\n"
        "  make_literal_access_expr(\"0.5\", BuiltinType.Float)\n"
        "  )), make_block_stmt([make_assignment_stmt(\n"
        "  make_field_access_expr(\"a\", [0, 0, 0]),\n"
        "  make_literal_access_expr(\"1.0\", BuiltinType.Float),\n"
        "  \"=\")]), None)]), make_block_stmt([make_if_stmt(make_expr_stmt("
        "make_binary_operator(\n"
        "  make_field_access_expr(\"c\"),\n"
        "  \"<\",\n"
        "  make_literal_access_expr(\"0.5\", BuiltinType.Float)\n"
        "  )), make_block_stmt([make_assignment_stmt(\n"
        "  make_field_access_expr(\"a\", [0, 0, 0]),\n"
        "  make_literal_access_expr(\"0.0\", BuiltinType.Float),\n"
        "  \"=\")]), make_block_stmt([make_assignment_stmt(\n"
        "  make_field_access_expr(\"a\", [0, 0, 0]),\n"
        "  make_literal_access_expr(\"-1.0\", BuiltinType.Float),\n"
        "  \"=\")]))])),\n" in result)


# (1/1) Method schedule_node
def test_sirwriter_schedule_node_1(parser, sir_writer):
    '''Check the schedule method of the SIRWriter class
    creates the expected code by calling its children.

    '''
    code = CODE.replace("\n    integer ::",
                        "\n    integer :: b, c\n    integer ::")
    code = code.replace(
        "a(i,j,k) = 1.0", "if (b .eq. c) then\na(i,j,k) = 1.0\nend if")
    sched = get_schedule(parser, code)
    if_statement = sched.walk(IfBlock)[0]
    schedule = if_statement.if_body
    assert isinstance(schedule, Schedule)
    schedule_result = sir_writer.schedule_node(schedule)
    content = schedule.children[0]
    content_result = sir_writer.assignment_node(content)
    assert schedule_result == content_result
    assert (
        "make_assignment_stmt(\n"
        "  make_field_access_expr(\"a\", [0, 0, 0]),\n"
        "  make_literal_access_expr(\"1.0\", BuiltinType.Float),\n"
        "  \"=\")," in schedule_result)


def test_sirwriter_intrinsiccall_node_2(parser, sir_writer):
    '''Check the intrinsiccall_node method of the SIRWriter class
    outputs the expected SIR code for a supported intrinsic with
    1 argument.

    '''
    code = CODE.replace("1.0", "abs(1.0)")
    rhs = get_rhs(parser, code)
    result = sir_writer.intrinsiccall_node(rhs)
    assert ("make_fun_call_expr(\"math::fabs\", [make_literal_access_expr("
            "\"1.0\", BuiltinType.Float)])" in result)


@pytest.mark.parametrize("intrinsic", ["min", "max"])
def test_sirwriter_intrinsiccall_node_3(parser, sir_writer, intrinsic):
    '''Check the intrinsiccall_node method of the SIRWriter class
    outputs the expected SIR code for a supported intrinsic with 2
    arguments.

    '''
    code = CODE.replace("1.0", f"{intrinsic}(1.0, 2.0)")
    rhs = get_rhs(parser, code)
    result = sir_writer.intrinsiccall_node(rhs)
    assert (f"make_fun_call_expr(\"math::{intrinsic}\", ["
            f"make_literal_access_expr(\"1.0\", BuiltinType.Float)], "
            f"[make_literal_access_expr(\"2.0\", BuiltinType.Float)])"
            in result)


def test_sirwriter_intrinsiccall_sign_node(parser, sir_writer):
    '''Check the intrinsiccall_node method of the SIRWriter class
    outputs the expected SIR code for the sign intrinsic.
    This is a special case as the sign intrinsic is
    implemented differently in the PSyIR (Fortran implementation) and
    SIR (C implementation).

    '''
    code = CODE.replace("1.0", "sign(1.0, 2.0)")
    rhs = get_rhs(parser, code)
    result = sir_writer.intrinsiccall_node(rhs)
    assert ("make_binary_operator(make_fun_call_expr(\"math::fabs\", "
            "[make_literal_access_expr(\"1.0\", BuiltinType.Float)]), "
            "\"*\", make_fun_call_expr(\"math::sign\", "
            "[make_literal_access_expr(\"2.0\", BuiltinType.Float)]))"
            in result)


# Class SIRWriter end
