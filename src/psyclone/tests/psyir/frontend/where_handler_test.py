# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council
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
# Author A. R. Porter, STFC Daresbury Laboratory

''' Module containing pytest tests for the handling of the WHERE
construct in the PSyIR. '''

from __future__ import absolute_import

import pytest
from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from psyclone.psyGen import Schedule, CodeBlock
from psyclone.psyir.frontend.fparser2 import Fparser2Reader


def test_where_broken_tree(parser):
    ''' Check that we raise the expected exceptions if the fparser2 parse
    tree does not have the correct structure. '''
    from psyclone.psyGen import InternalError

    fake_parent = Schedule()
    processor = Fparser2Reader()
    reader = FortranStringReader("WHERE (ptsu(:, :, :) /= 0._wp)\n"
                                 "  z1_st(:, :, :) = 1._wp / ptsu(:, :, :)\n"
                                 "END WHERE\n")
    fparser2spec = Fortran2003.Where_Construct(reader)
    # Break the parse tree by removing the end-where statement
    del fparser2spec.content[-1]
    with pytest.raises(InternalError) as err:
        processor.process_nodes(fake_parent, [fparser2spec], None)
    assert "Failed to find closing end where statement" in str(err.value)
    # Now remove the opening where statement
    del fparser2spec.content[0]
    with pytest.raises(InternalError) as err:
        processor.process_nodes(fake_parent, [fparser2spec], None)
    assert "Failed to find opening where construct " in str(err.value)


def test_missing_array_notation_expr(parser):
    ''' Check that we get a code block if the WHERE does not use explicit
    array syntax in the logical expression. '''
    fake_parent = Schedule()
    processor = Fparser2Reader()
    reader = FortranStringReader("WHERE (ptsu /= 0._wp)\n"
                                 "  z1_st(:, :, :) = 1._wp / ptsu(:, :, :)\n"
                                 "END WHERE\n")
    fparser2spec = Fortran2003.Where_Construct(reader)
    processor.process_nodes(fake_parent, [fparser2spec], None)
    assert isinstance(fake_parent.children[0], CodeBlock)


def test_missing_array_notation_lhs(parser):
    ''' Check that we get a code block if the WHERE does not use explicit
    array syntax on the LHS of an assignment within the body. '''
    fake_parent = Schedule()
    processor = Fparser2Reader()
    reader = FortranStringReader("WHERE (ptsu(:,:,:) /= 0._wp)\n"
                                 "  z1_st = 1._wp / ptsu(:, :, :)\n"
                                 "END WHERE\n")
    fparser2spec = Fortran2003.Where_Construct(reader)
    processor.process_nodes(fake_parent, [fparser2spec], None)
    assert isinstance(fake_parent.children[0], CodeBlock)


def test_where_array_notation_rank(parser):
    ''' Test that the _array_notation_rank() utility raises the expected
    errors when passed an unsupported Array object. '''
    from psyclone.psyGen import Array, Literal
    my_array = Array("my_array", None)
    processor = Fparser2Reader()
    with pytest.raises(NotImplementedError) as err:
        processor._array_notation_rank(my_array)
    assert ("Array reference in the PSyIR must have at least one child but "
            "'my_array'" in str(err.value))
    # Give the Array one child that is not a CodeBlock
    my_array.addchild(Literal("2", my_array))
    with pytest.raises(NotImplementedError) as err:
        processor._array_notation_rank(my_array)
    assert ("that uses Fortran array notation is assumed to have at least "
            "one CodeBlock as its child " in str(err.value))
    my_array._children = []
    my_array.addchild(CodeBlock([Fortran2003.Literal_Constant("1")],
                                CodeBlock.Structure.EXPRESSION, my_array))
    with pytest.raises(NotImplementedError) as err:
        processor._array_notation_rank(my_array)
    assert ("Only array notation of the form my_array(:, :, ...) is supported"
            in str(err.value))


def test_different_ranks_error(parser):
    ''' Check that a WHERE construct containing array references of different
    ranks results in the creation of a CodeBlock. '''
    fake_parent = Schedule()
    processor = Fparser2Reader()
    # Invalid Fortran
    reader = FortranStringReader("WHERE (dry(:, :, :))\n"
                                 "  z1_st(:, :) = depth / ptsu(:, :, :)\n"
                                 "END WHERE\n")
    fparser2spec = Fortran2003.Where_Construct(reader)
    # We should end up with a CodeBlock in the PSyIR
    processor.process_nodes(fake_parent, [fparser2spec], None)
    assert isinstance(fake_parent.children[0], CodeBlock)


def test_array_notation_rank(parser):
    ''' Check that our _array_notation_rank() utility handles various examples
    of array notation. '''
    from psyclone.psyGen import Assignment, Loop
    fake_parent = Schedule()
    processor = Fparser2Reader()
    reader = FortranStringReader("  z1_st(:, 2, :) = ptsu(:, :, 3)")
    fparser2spec = Fortran2003.Assignment_Stmt(reader)
    processor.process_nodes(fake_parent, [fparser2spec], None)
    assert processor._array_notation_rank(fake_parent[0].lhs) == 2
    reader = FortranStringReader("  z1_st(:, :, 2, :) = ptsu(:, :, :, 3)")
    fparser2spec = Fortran2003.Assignment_Stmt(reader)
    processor.process_nodes(fake_parent, [fparser2spec], None)
    assert processor._array_notation_rank(fake_parent[1].lhs) == 3
    # We don't support bounds on slices
    reader = FortranStringReader("  z1_st(:, 1:n, 2, :) = ptsu(:, :, :, 3)")
    fparser2spec = Fortran2003.Assignment_Stmt(reader)
    processor.process_nodes(fake_parent, [fparser2spec], None)
    with pytest.raises(NotImplementedError) as err:
        processor._array_notation_rank(fake_parent[2].lhs)
    assert "Bounds on array slices are not supported" in str(err.value)


def test_basic_where(parser):
    ''' Check that a basic WHERE using a logical array as a mask is correctly
    translated into the PSyIR. '''
    from psyclone.psyGen import Loop, Literal, BinaryOperation, IfBlock
    fake_parent = Schedule()
    processor = Fparser2Reader()
    reader = FortranStringReader("WHERE (dry(:, :, :))\n"
                                 "  z1_st(:, :, :) = depth / ptsu(:, :, :)\n"
                                 "END WHERE\n")
    fparser2spec = Fortran2003.Where_Construct(reader)
    processor.process_nodes(fake_parent, [fparser2spec], None)
    # We should have a triply-nested loop with an IfBlock inside
    loops = fake_parent.walk(Loop)
    assert len(loops) == 3
    for loop in loops:
        assert "was_where" in loop.annotations
        assert isinstance(loop.ast, Fortran2003.Where_Construct)

    assert isinstance(loops[0].children[0], Literal)
    assert isinstance(loops[0].children[1], BinaryOperation)
    assert str(loops[0].children[1].children[0]) == "Reference[name:'dry']"

    ifblock = loops[2].loop_body[0]
    assert isinstance(ifblock, IfBlock)
    assert "was_where" in ifblock.annotations
    assert ("ArrayReference[name:'dry']\n"
            "Reference[name:'widx1']\n" in str(ifblock.condition))


def test_elsewhere(parser):
    ''' Check that a WHERE construct with an ELSEWHERE clause is correctly
    translated into a canonical form in the PSyIR. '''
    from psyclone.psyGen import Loop, IfBlock, Assignment, Array, \
        BinaryOperation
    fake_parent = Schedule()
    processor = Fparser2Reader()
    reader = FortranStringReader("WHERE (ptsu(:, :, :) /= 0._wp)\n"
                                 "  z1_st(:, :, :) = 1._wp / ptsu(:, :, :)\n"
                                 "ELSEWHERE\n"
                                 "  z1_st(:, :, :) = 0._wp\n"
                                 "END WHERE\n")
    fparser2spec = Fortran2003.Where_Construct(reader)
    processor.process_nodes(fake_parent, [fparser2spec], None)
    assert len(fake_parent.children) == 1
    # Check that we have a triply-nested loop
    loop = fake_parent.children[0]
    assert isinstance(loop.ast, Fortran2003.Where_Construct)
    assert isinstance(loop, Loop)
    assert isinstance(loop.loop_body[0], Loop)
    assert isinstance(loop.loop_body[0].loop_body[0], Loop)
    # Check that we have an IF block within the innermost loop
    ifblock = loop.loop_body[0].loop_body[0].loop_body[0]
    assert isinstance(ifblock, IfBlock)
    assert isinstance(ifblock.ast, Fortran2003.Where_Construct)
    assert isinstance(ifblock.condition, BinaryOperation)
    assert ifblock.condition.operator == BinaryOperation.Operator.NE
    assert ("ArrayReference[name:'ptsu']\n"
            "Reference[name:'widx1']\n" in str(ifblock.condition.children[0]))
    assert "Literal[value:'0." in str(ifblock.condition.children[1])
    # Check that this IF block has an else body
    assert ifblock.else_body is not None
    assert isinstance(ifblock.else_body[0], Assignment)
    assert isinstance(ifblock.else_body[0].lhs, Array)
    assert ifblock.else_body[0].lhs.name == "z1_st"
