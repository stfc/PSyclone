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
from psyclone.psyGen import Node


def create_schedule(code):
    ''' xxx '''
    from psyclone.nemo import NemoFparser2ASTProcessor
    from fortran_test import create_schedule as create_base_schedule
    return create_base_schedule(code, ASTProcessor=NemoFparser2ASTProcessor)


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
            result = gen_stencil(array_reference)
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
    assert sir_writer._field_names == set()
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


# (1/1) Method nemoloop_node
def test_sirwriter_nemoloop_node_1():
    '''Check the nemoloop_node method of the SIRWriter class is called for
    nemo loops. Raise the appropriate exception loops are not triply
    nested. Also check for SIR indentation.

    '''
    code = (
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
    schedule = create_schedule(code)
    exit(1)


# Class SIRWriter end
