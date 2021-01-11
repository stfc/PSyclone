# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council.
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
# Author A. R. Porter, STFC Daresbury Lab
# Modified by R. W. Ford and S. Siso, STFC Daresbury Lab

''' Module containing pytest tests for the handling of the WHERE
construct in the PSyIR. '''

from __future__ import absolute_import

import pytest
from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from psyclone.psyir.nodes import Schedule, CodeBlock, Loop, ArrayReference, \
    Assignment, Literal, Reference, BinaryOperation, IfBlock
from psyclone.errors import InternalError
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.symbols import DataSymbol, ArrayType, ScalarType, \
    REAL_TYPE, INTEGER_TYPE


def process_where(code, fparser_cls):
    '''
    Utility routine to process the supplied Fortran code and return the
    PSyIR and fparser2 parse trees.

    :param str code: Fortran code to process.
    :param type fparser_cls: the fparser2 class to instantiate to \
                             represent the supplied Fortran.

    :returns: 2-tuple of a parent PSyIR Schedule and the created instance of \
              the requested fparser2 class.
    :rtype: (:py:class:`psyclone.psyir.nodes.Schedule`, \
             :py:class:`fparser.two.utils.Base`)
    '''
    sched = Schedule()
    processor = Fparser2Reader()
    reader = FortranStringReader(code)
    fparser2spec = fparser_cls(reader)

    if fparser_cls is Fortran2003.Execution_Part:
        processor.process_nodes(sched, fparser2spec.content)
    else:
        processor.process_nodes(sched, [fparser2spec])
    return sched, fparser2spec


@pytest.mark.usefixtures("parser", "disable_declaration_check")
def test_where_broken_tree():
    ''' Check that we raise the expected exceptions if the fparser2 parse
    tree does not have the correct structure.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.

    '''
    fake_parent, fparser2spec = process_where(
        "WHERE (ptsu(:, :, :) /= 0._wp)\n"
        "  z1_st(:, :, :) = 1._wp / ptsu(:, :, :)\n"
        "END WHERE\n", Fortran2003.Where_Construct)
    processor = Fparser2Reader()
    # Break the parse tree by removing the end-where statement
    del fparser2spec.content[-1]
    with pytest.raises(InternalError) as err:
        processor.process_nodes(fake_parent, [fparser2spec])
    assert "Failed to find closing end where statement" in str(err.value)
    # Now remove the opening where statement
    del fparser2spec.content[0]
    with pytest.raises(InternalError) as err:
        processor.process_nodes(fake_parent, [fparser2spec])
    assert "Failed to find opening where construct " in str(err.value)


@pytest.mark.usefixtures("parser", "disable_declaration_check")
def test_missing_array_notation_expr():
    ''' Check that we get a code block if the WHERE does not use explicit
    array syntax in the logical expression.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    fake_parent, _ = process_where("WHERE (ptsu /= 0._wp)\n"
                                   "  z1_st(:, :, :) = 1._wp / ptsu(:, :, :)\n"
                                   "END WHERE\n", Fortran2003.Where_Construct)
    assert isinstance(fake_parent.children[0], CodeBlock)


@pytest.mark.usefixtures("parser", "disable_declaration_check")
def test_missing_array_notation_lhs():
    ''' Check that we get a code block if the WHERE does not use explicit
    array syntax on the LHS of an assignment within the body.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    fake_parent, _ = process_where("WHERE (ptsu(:,:,:) /= 0._wp)\n"
                                   "  z1_st = 1._wp / ptsu(:, :, :)\n"
                                   "END WHERE\n", Fortran2003.Where_Construct)
    assert isinstance(fake_parent.children[0], CodeBlock)


@pytest.mark.usefixtures("parser")
def test_where_array_notation_rank():
    ''' Test that the _array_notation_rank() utility raises the expected
    errors when passed an unsupported Array object.
    '''
    array_type = ArrayType(REAL_TYPE, [10])
    symbol = DataSymbol("my_array", array_type)
    my_array = ArrayReference(symbol)
    processor = Fparser2Reader()
    with pytest.raises(NotImplementedError) as err:
        processor._array_notation_rank(my_array)
    assert ("Array reference in the PSyIR must have at least one child but "
            "'my_array'" in str(err.value))
    from psyclone.psyir.nodes import Range
    array_type = ArrayType(REAL_TYPE, [10])
    my_array = ArrayReference.create(
        DataSymbol("my_array", array_type),
        [Range.create(Literal("1", INTEGER_TYPE),
                      Literal("10", INTEGER_TYPE))])
    with pytest.raises(NotImplementedError) as err:
        processor._array_notation_rank(my_array)
    assert ("Only array notation of the form my_array(:, :, ...) is "
            "supported." in str(err.value))


@pytest.mark.usefixtures("parser", "disable_declaration_check")
def test_different_ranks_error():
    ''' Check that a WHERE construct containing array references of different
    ranks results in the creation of a CodeBlock.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    fake_parent, _ = process_where("WHERE (dry(:, :, :))\n"
                                   "  z1_st(:, :) = depth / ptsu(:, :, :)\n"
                                   "END WHERE\n", Fortran2003.Where_Construct)
    assert isinstance(fake_parent.children[0], CodeBlock)


@pytest.mark.usefixtures("parser", "disable_declaration_check")
def test_array_notation_rank():
    ''' Check that the _array_notation_rank() utility handles various examples
    of array notation.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    fake_parent = Schedule()
    processor = Fparser2Reader()
    reader = FortranStringReader("  z1_st(:, 2, :) = ptsu(:, :, 3)")
    fparser2spec = Fortran2003.Assignment_Stmt(reader)
    processor.process_nodes(fake_parent, [fparser2spec])
    assert processor._array_notation_rank(fake_parent[0].lhs) == 2
    reader = FortranStringReader("  z1_st(:, :, 2, :) = ptsu(:, :, :, 3)")
    fparser2spec = Fortran2003.Assignment_Stmt(reader)
    processor.process_nodes(fake_parent, [fparser2spec])
    assert processor._array_notation_rank(fake_parent[1].lhs) == 3
    # We don't support bounds on slices
    reader = FortranStringReader("  z1_st(:, 1:n, 2, :) = ptsu(:, :, :, 3)")
    fparser2spec = Fortran2003.Assignment_Stmt(reader)
    processor.process_nodes(fake_parent, [fparser2spec])
    with pytest.raises(NotImplementedError) as err:
        processor._array_notation_rank(fake_parent[2].lhs)
    assert ("Only array notation of the form my_array(:, :, ...) is "
            "supported." in str(err.value))


def test_where_symbol_clash(parser):
    ''' Check that we handle the case where the code we are processing
    already contains a symbol with the same name as one of the loop variables
    we want to introduce.

    '''
    reader = FortranStringReader("SUBROUTINE widx_array()\n"
                                 "LOGICAL :: widx1(3,3,3)\n"
                                 "REAL :: z1_st(3,3,3), ptsu(3,3,3), depth\n"
                                 "WHERE (widx1(:, :, :))\n"
                                 "  z1_st(:, :, :) = depth / ptsu(:, :, :)\n"
                                 "END WHERE\n"
                                 "END SUBROUTINE widx_array\n")
    fparser2spec = parser(reader)
    processor = Fparser2Reader()
    sched = processor.generate_schedule("widx_array", fparser2spec)
    var = sched.symbol_table.lookup("widx1")
    assert isinstance(var, DataSymbol)
    assert var.datatype.intrinsic == ScalarType.Intrinsic.BOOLEAN
    # Check that we have a new symbol for the loop variable
    loop_var = sched.symbol_table.lookup("widx1_1")
    assert loop_var.datatype.intrinsic == ScalarType.Intrinsic.INTEGER


@pytest.mark.usefixtures("parser", "disable_declaration_check")
def test_basic_where():
    ''' Check that a basic WHERE using a logical array as a mask is correctly
    translated into the PSyIR.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    fake_parent, _ = process_where("WHERE (dry(:, :, :))\n"
                                   "  z1_st(:, :, :) = depth / ptsu(:, :, :)\n"
                                   "END WHERE\n", Fortran2003.Where_Construct)
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
            "Reference[name:'widx1']\n"
            "Reference[name:'widx2']\n"
            "Reference[name:'widx3']\n"
            in str(ifblock.condition))


@pytest.mark.usefixtures("parser", "disable_declaration_check")
def test_where_array_subsections():
    ''' Check that we handle a WHERE construct with non-contiguous array
    subsections.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    fake_parent, _ = process_where("WHERE (dry(1, :, :))\n"
                                   "  z1_st(:, 2, :) = depth / ptsu(:, :, 3)\n"
                                   "END WHERE\n", Fortran2003.Where_Construct)
    # We should have a doubly-nested loop with an IfBlock inside
    loops = fake_parent.walk(Loop)
    assert len(loops) == 2
    for loop in loops:
        assert "was_where" in loop.annotations
        assert isinstance(loop.ast, Fortran2003.Where_Construct)

    ifblock = loops[1].loop_body[0]
    assert isinstance(ifblock, IfBlock)
    # Check that the array reference is indexed correctly
    assign = ifblock.if_body[0]
    assert isinstance(assign, Assignment)
    assert isinstance(assign.lhs.children[0], Reference)
    assert assign.lhs.children[0].name == "widx1"
    assert assign.lhs.children[2].name == "widx2"


@pytest.mark.usefixtures("parser", "disable_declaration_check")
def test_elsewhere():
    ''' Check that a WHERE construct with an ELSEWHERE clause is correctly
    translated into a canonical form in the PSyIR.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    fake_parent, _ = process_where("WHERE (ptsu(:, :, :) /= 0._wp)\n"
                                   "  z1_st(:, :, :) = 1._wp / ptsu(:, :, :)\n"
                                   "ELSEWHERE\n"
                                   "  z1_st(:, :, :) = 0._wp\n"
                                   "END WHERE\n", Fortran2003.Where_Construct)
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
    assert isinstance(ifblock.else_body[0].lhs, ArrayReference)
    assert ifblock.else_body[0].lhs.name == "z1_st"


@pytest.mark.usefixtures("parser", "disable_declaration_check")
def test_where_stmt_validity():
    ''' Check that the correct exceptions are raised when the parse tree
    for a WHERE statement has an unexpected structure.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    fake_parent, fparser2spec = process_where(
        "WHERE( at_i(:,:) > rn_amax_2d(:,:) )   "
        "a_i(:,:,jl) = a_i(:,:,jl) * rn_amax_2d(:,:) / at_i(:,:)",
        Fortran2003.Where_Stmt)
    # Break the parse tree
    fparser2spec.items = (fparser2spec.items[0], "a string")
    processor = Fparser2Reader()
    with pytest.raises(InternalError) as err:
        processor.process_nodes(fake_parent, [fparser2spec])
    assert "items tuple to be an Assignment_Stmt but found" in str(err.value)
    # Break it so that the tuple only contains one item
    fparser2spec.items = (fparser2spec.items[0], )
    with pytest.raises(InternalError) as err:
        processor.process_nodes(fake_parent, [fparser2spec])
    assert ("Where_Stmt to have exactly two entries in 'items' but found 1"
            in str(err.value))


@pytest.mark.usefixtures("parser", "disable_declaration_check")
def test_where_stmt():
    ''' Basic check that we handle a WHERE statement correctly.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    fake_parent, _ = process_where(
        "WHERE( at_i(:,:) > rn_amax_2d(:,:) )   "
        "a_i(:,:,jl) = a_i(:,:,jl) * rn_amax_2d(:,:) / at_i(:,:)",
        Fortran2003.Where_Stmt)
    assert len(fake_parent.children) == 1
    assert isinstance(fake_parent[0], Loop)


def test_where_ordering(parser):
    ''' Check that the generated schedule has the correct ordering when
    a WHERE construct is processed.

    '''
    reader = FortranStringReader(
        "    subroutine test(zdiv, pbef, paft, zmsk, tmask)\n"
        "      integer, parameter :: wp=1\n"
        "      real :: zsml\n"
        "      integer :: ji, jj, jpjm1, jpim1\n"
        "      real :: zdiv(:,:), pbef(:,:), paft(:,:), zmsk(:,:), "
        "tmask(:,:)\n"
        "      zsml = 1.e-15_wp\n"
        "      DO jj = 2, jpjm1\n"
        "         DO ji = 2, jpim1\n"
        "            zdiv(ji,jj) =  1.0_wp\n"
        "         END DO\n"
        "      END DO\n"
        "      CALL lbc_lnk( zdiv, 'T', 1. )\n"
        "      WHERE( pbef(:,:) == 0._wp .AND. paft(:,:) == 0._wp .AND. "
        "zdiv(:,:) == 0._wp )   ;   zmsk(:,:) = 0._wp\n"
        "      ELSEWHERE;   zmsk(:,:) = 1._wp * tmask(:,:,1)\n"
        "      END WHERE\n"
        "    end subroutine test\n")
    fparser2_tree = parser(reader)
    processor = Fparser2Reader()
    result = processor.generate_schedule("test", fparser2_tree)
    assert isinstance(result[0], Assignment)
    assert isinstance(result[1], Loop)
    assert isinstance(result[2], CodeBlock)
    assert isinstance(result[3], Loop)
