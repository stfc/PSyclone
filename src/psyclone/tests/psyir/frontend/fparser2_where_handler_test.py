# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2026, Science and Technology Facilities Council.
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

import pytest
from typing import Optional

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003, utils

from psyclone.errors import InternalError
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import (
    ArrayMember, ArrayReference, Assignment, BinaryOperation,
    Call, CodeBlock, Container, IfBlock, IntrinsicCall, Literal, Loop, Range,
    Reference, Routine, Schedule, UnaryOperation)
from psyclone.psyir.symbols import (
    DataSymbol, ScalarType, INTEGER_TYPE)


def process_where(
    code: str,
    fparser_cls: type,
    symbols: Optional[list[str]] = None,
    scalars: Optional[list[str]] = None
) -> tuple[Schedule, utils.Base]:
    '''
    Utility routine to process the supplied Fortran code and return the
    PSyIR and fparser2 parse trees.

    :param code: Fortran code to process.
    :param fparser_cls: the fparser2 class to instantiate to represent the
        supplied Fortran.
    :param symbols: list of symbol names that must be added to the symbol
                    table before constructing the PSyIR.
    :param scalars: list of symbol names that must be added to the symbol
                    table with an integer, scalar datatype.

    :returns: 2-tuple of a parent PSyIR Schedule and the created instance of
              the requested fparser2 class.
    '''
    sched = Schedule()
    # Always add the 'wp' kind parameter as this must have specific properties.
    sched.symbol_table.new_symbol("wp", symbol_type=DataSymbol,
                                  datatype=INTEGER_TYPE,
                                  initial_value=Literal("8", INTEGER_TYPE),
                                  is_constant=True)
    if symbols:
        for sym_name in symbols:
            sched.symbol_table.new_symbol(sym_name)
    if scalars:
        for sym_name in scalars:
            sched.symbol_table.new_symbol(sym_name, symbol_type=DataSymbol,
                                          datatype=INTEGER_TYPE)
    processor = Fparser2Reader()
    reader = FortranStringReader(code)
    fparser2spec = fparser_cls(reader)

    processor.process_nodes(sched, [fparser2spec])
    return sched, fparser2spec


@pytest.mark.usefixtures("parser")
def test_where_broken_tree():
    ''' Check that we raise the expected exceptions if the fparser2 parse
    tree does not have the correct structure.

    '''
    fake_parent, fparser2spec = process_where(
        "WHERE (ptsu(:, :, :) /= 0._wp)\n"
        "  z1_st(:, :, :) = 1._wp / ptsu(:, :, :)\n"
        "END WHERE\n", Fortran2003.Where_Construct, ["ptsu", "z1_st"])
    processor = Fparser2Reader()
    # Test with unexpected clause by adding an extra end-where statement
    assert isinstance(fparser2spec.content[-1], Fortran2003.End_Where_Stmt)
    fparser2spec.content.insert(-1, fparser2spec.content[-1])
    with pytest.raises(InternalError) as err:
        processor.process_nodes(fake_parent, [fparser2spec])
    assert ("Expected either Fortran2003.Masked_Elsewhere_Stmt or "
            "Fortran2003.Elsewhere_Stmt but found 'End_Where_Stmt'" in
            str(err.value))
    del fparser2spec.content[-2]
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


@pytest.mark.usefixtures("parser")
def test_where_unknown_selector_type():
    ''' Check that we create the expected CodeBlock if we can't resolve the
    resulting shape of an array expression.

    '''
    fake_parent, fparser2spec = process_where(
        "WHERE (ptsu(myfunc(), :, :) /= 0._wp)\n"
        "  z1_st(myfunc(), :, :) = 1._wp / ptsu(myfunc(), :, :)\n"
        "END WHERE\n", Fortran2003.Where_Construct, ["ptsu", "z1_st"])
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2spec])
    assert isinstance(fake_parent.children[0], CodeBlock)
    assert ("We can not get the resulting shape of the expression: "
            "ptsu(myfunc(),:,:)" in fake_parent.children[0].preceding_comment)

    fake_parent, fparser2spec = process_where(
        "WHERE (ptsu(:, :, :) /= 0._wp)\n"
        "  z1_st(myfunc(), :, :) = 1._wp / ptsu(myfunc(), :, :)\n"
        "END WHERE\n", Fortran2003.Where_Construct, ["ptsu", "z1_st"])
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2spec])
    assert isinstance(fake_parent.children[0], CodeBlock)
    assert ("We can not get the resulting shape of the expression: "
            "z1_st(myfunc(),:,:)" in fake_parent.children[0].preceding_comment)


@pytest.mark.usefixtures("parser")
def test_elsewhere_broken_tree():
    ''' Check that we raise the expected exceptions if the fparser2 parse
    tree containing an ELSEWHERE does not have the correct structure.

    '''
    fake_parent, fparser2spec = process_where(
        "WHERE (ptsu(:, :, :) /= 0._wp)\n"
        "  z1_st(:, :, :) = 1._wp / ptsu(:, :, :)\n"
        "ELSE WHERE\n"
        "  z1_st(:, :, :) = 0._wp\n"
        "END WHERE\n", Fortran2003.Where_Construct, ["ptsu", "z1_st"])
    processor = Fparser2Reader()
    # Insert an additional Elsewhere_Stmt
    assert isinstance(fparser2spec.content[-3], Fortran2003.Elsewhere_Stmt)
    fparser2spec.content.insert(-1, fparser2spec.content[-3])
    with pytest.raises(InternalError) as err:
        processor.process_nodes(fake_parent, [fparser2spec])
    assert ("Elsewhere_Stmt should only be found next to last clause, but "
            "found" in str(err.value))


@pytest.mark.usefixtures("parser")
@pytest.mark.parametrize("mask", ["ptsu", "ptsu(1,1)"])
def test_missing_array_notation_expr(mask):
    ''' Check that we get a code block if the WHERE does not use explicit
    array syntax (with range(s)) in the logical expression.

    '''
    fake_parent, _ = process_where(f"WHERE ({mask} /= 0._wp)\n"
                                   f"z1_st(:, :, :) = 1._wp / ptsu(:, :, :)\n"
                                   f"END WHERE\n", Fortran2003.Where_Construct,
                                   ["ptsu", "z1_st"])
    assert isinstance(fake_parent.children[0], CodeBlock)


@pytest.mark.usefixtures("f2008_parser")
def test_labelled_where():
    ''' Check that we get a code block if the WHERE statement has a label.

    '''
    fake_parent, _ = process_where("100 WHERE (ptsu /= 0._wp)\n"
                                   "  z1_st(:, :, :) = 1._wp / ptsu(:, :, :)\n"
                                   "END WHERE\n", Fortran2003.Where_Construct,
                                   ["ptsu", "z1_st"])
    assert isinstance(fake_parent.children[0], CodeBlock)


@pytest.mark.usefixtures("parser")
def test_different_ranks_error():
    ''' Check that a WHERE construct containing array references of different
    ranks results in the creation of a CodeBlock.

    '''
    fake_parent, _ = process_where("WHERE (dry(:, :, :))\n"
                                   "  z1_st(:, :) = depth / ptsu(:, :, :)\n"
                                   "END WHERE\n", Fortran2003.Where_Construct,
                                   ["dry", "z1_st", "ptsu"], ["depth"])
    assert isinstance(fake_parent.children[0], CodeBlock)


def test_where_symbol_clash(fortran_reader):
    ''' Check that we handle the case where the code we are processing
    already contains a symbol with the same name as one of the loop variables
    we want to introduce.

    '''
    code = ("MODULE MY_MOD\n"
            "CONTAINS\n"
            "SUBROUTINE widx_array()\n"
            "LOGICAL :: widx1(3,3,3)\n"
            "REAL :: z1_st(3,3,3), ptsu(3,3,3), depth\n"
            "WHERE (widx1(:, :, :))\n"
            "  z1_st(:, :, :) = depth / ptsu(:, :, :)\n"
            "END WHERE\n"
            "END SUBROUTINE widx_array\n"
            "END MODULE MY_MOD\n")
    psyir = fortran_reader.psyir_from_source(code)
    sched = psyir.walk(Routine)[0]
    var = sched.symbol_table.lookup("widx1")
    assert isinstance(var, DataSymbol)
    assert var.datatype.intrinsic == ScalarType.Intrinsic.BOOLEAN
    # Check that we have a new symbol for the loop variable
    loop_var = sched.symbol_table.lookup("widx1_1")
    assert loop_var.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    # Check that it's in the expected symbol table
    assert loop_var.name in sched.symbol_table


def test_where_within_loop(fortran_reader):
    ''' Test for correct operation (Codeblock) when we have a WHERE within an
    existing loop and the referenced arrays are brought in from a module. '''
    code = ("module my_mod\n"
            " use some_mod\n"
            "contains\n"
            "subroutine my_sub()\n"
            "  integer :: jl\n"
            "  do jl = 1, 10\n"
            "  where (var(:) > epsi20)\n"
            "    var2(:, jl) = 2.0\n"
            "  end where\n"
            "  end do\n"
            "end subroutine my_sub\n"
            "end module my_mod\n")
    psyir = fortran_reader.psyir_from_source(code)

    # Check that we have symbols for the two arrays
    mymod = psyir.children[0]
    assert isinstance(mymod, Container)
    sub = mymod.children[0]
    assert isinstance(sub, Routine)
    assert isinstance(sub[0].loop_body.children[0], CodeBlock)


@pytest.mark.usefixtures("parser")
def test_basic_where():
    ''' Check that a basic WHERE using a logical array as a mask is correctly
    translated into the PSyIR.

    '''
    fake_parent, _ = process_where("WHERE (dry(:, :, :))\n"
                                   "  z1_st(:, :, :) = depth / ptsu(:, :, :)\n"
                                   "END WHERE\n", Fortran2003.Where_Construct,
                                   ["dry", "z1_st", "ptsu"], ["depth"])
    # We should have a triply-nested loop with an IfBlock inside
    loops = fake_parent.walk(Loop)
    assert len(loops) == 3
    for loop in loops:
        assert "was_where" in loop.annotations
        assert isinstance(loop.ast, Fortran2003.Where_Construct)

    assert isinstance(loops[0].start_expr, Literal)
    assert loops[0].stop_expr.debug_string() == "SIZE(dry, dim=3)"

    ifblock = loops[2].loop_body[0]
    assert isinstance(ifblock, IfBlock)
    assert "was_where" in ifblock.annotations
    assert (ifblock.condition.debug_string() ==
            "dry(LBOUND(dry, dim=1) + widx1 - 1,"
            "LBOUND(dry, dim=2) + widx2 - 1,"
            "LBOUND(dry, dim=3) + widx3 - 1)")


@pytest.mark.usefixtures("parser")
def test_where_array_subsections():
    ''' Check that we handle a WHERE construct with non-contiguous array
    subsections.

    '''
    fake_parent, _ = process_where("WHERE (dry(1, :, :))\n"
                                   "  z1_st(:, 2, :) = depth / ptsu(:, :, 3)\n"
                                   "END WHERE\n", Fortran2003.Where_Construct,
                                   ["dry", "z1_st", "ptsu"], ["depth"])
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
    assert isinstance(assign.lhs.children[0], BinaryOperation)
    assert (assign.lhs.children[0].debug_string() ==
            "LBOUND(z1_st, dim=1) + widx1 - 1")
    assert (assign.lhs.children[2].debug_string() ==
            "LBOUND(z1_st, dim=3) + widx2 - 1")


def test_where_mask_starting_value(fortran_reader, fortran_writer):
    '''
    Check handling of a case where the mask array is indexed from values other
    than unity.

    '''
    code = '''\
    program my_sub
      use some_mod
      type :: thing
        real, dimension(100000, 100000, 1) :: z3
      end type
      integer, parameter :: jpl = 123435
      integer, parameter :: jpr_ievp = 123
      type(thing), dimension(10000) :: frcv
      real, dimension(-5:5,-5:5) :: picefr
      real, DIMENSION(11,11,jpl) :: zevap_ice
      real, dimension(-2:8,jpl,-3:7) :: snow
      real, dimension(-22:0,jpl,-32:0) :: slush
      integer, dimension(jpl) :: map

      WHERE( picefr(:,:) > 1.e-10 )
        zevap_ice(:,:,1) = snow(:,3,:) * frcv(jpr_ievp)%z3(:,:,1) / picefr(:,:)
      ELSEWHERE
        zevap_ice(:,:,1) = snow(:,map(jpl),:) + slush(-22:-11,jpl,-32:-21)
      END WHERE
    end program my_sub
'''
    psyir = fortran_reader.psyir_from_source(code)
    output = fortran_writer(psyir)
    expected = '''\
  do widx2 = 1, 5 - (-5) + 1, 1
    do widx1 = 1, 5 - (-5) + 1, 1
      if (picefr(-5 + widx1 - 1,-5 + widx2 - 1) > 1.e-10) then
        zevap_ice(widx1,widx2,1) = snow(-2 + widx1 - 1,3,-3 + widx2 - 1) * \
frcv(jpr_ievp)%z3(widx1,widx2,1) / \
picefr(-5 + widx1 - 1,-5 + widx2 - 1)
      else
        zevap_ice(widx1,widx2,1) = snow(-2 + widx1 - 1,map(jpl),\
-3 + widx2 - 1) + slush(-22 + widx1 - 1,jpl,-32 + widx2 - 1)
'''
    assert expected in output


def test_where_mask_is_slice(fortran_reader, fortran_writer):
    '''
    Check that the correct loop bounds and index expressions are created
    when the mask expression uses a slice with specified bounds.
    '''
    code = '''\
    program my_sub
      type :: thing
        real, dimension(100000, 100000, 1) :: z3
      end type
      type(thing), dimension(10000) :: frcv
      real, dimension(-5:5,-5:5) :: picefr
      real, DIMENSION(11,11,jpl) :: zevap_ice
      integer :: jstart, jstop, jpl, jpr_ievp

      WHERE( picefr(2:4,jstart:jstop) > 1.e-10 )
        zevap_ice(:,:,1) = frcv(jpr_ievp)%z3(:,:,1) / picefr(:,:)
      ELSEWHERE ( picefr(1:3,jstart:jstop) > 4.e-10)
        zevap_ice(:,:,1) = 0.0
      END WHERE
    end program my_sub
'''
    psyir = fortran_reader.psyir_from_source(code)
    out = fortran_writer(psyir)
    # Check that created loops have the correct number of iterations
    assert "do widx2 = 1, jstop - jstart + 1, 1" in out
    assert "do widx1 = 1, 4 - 2 + 1, 1" in out
    assert "if (picefr(2 + widx1 - 1,jstart + widx2 - 1) > 1.e-10)" in out
    assert ("zevap_ice(widx1,widx2,1) = 0.0" in out)
    # If the lower bound of the slice is unity then we can use the loop
    # index directly.
    assert "if (picefr(widx1,jstart + widx2 - 1) > 4.e-10)" in out


def test_where_mask_is_slice_lower_limit(fortran_reader, fortran_writer):
    '''
    Check that the correct loop bounds and index expressions are created
    when the mask expression uses a slice and the array itself is a formal
    argument with a specified lower bound and no explicit upper bound.
    '''
    code = '''\
    subroutine my_sub(picefr)
      real, dimension(3:,2:) :: picefr
      type :: thing
        real, dimension(100000, 100000, 1) :: z3
      end type
      type(thing), dimension(10000) :: frcv
      real, DIMENSION(11,11,jpl) :: zevap_ice
      integer :: jstart, jstop, jpl, jpr_ievp
      WHERE( picefr(:4,jstart:) > 1.e-10 )
        zevap_ice(:,:,1) = frcv(jpr_ievp)%z3(:,:,1) / picefr(:,:)
      ELSEWHERE ( picefr(4:5,jstart:) > 4.e-10)
        zevap_ice(:,:,1) = 0.0
      END WHERE
    end subroutine my_sub
'''
    psyir = fortran_reader.psyir_from_source(code)
    out = fortran_writer(psyir)
    # Check that created loops have the correct number of iterations
    assert "do widx2 = 1, UBOUND(picefr, dim=2) - jstart + 1, 1" in out
    assert "do widx1 = 1, 4 - LBOUND(picefr, dim=1) + 1, 1" in out
    # Check that the indexing into the mask expression uses the lower bounds
    # specified in the original slice.
    assert ("if (picefr(LBOUND(picefr, dim=1) + widx1 - 1,"
            "jstart + widx2 - 1) > 1.e-10)" in out)
    assert ("zevap_ice(widx1,widx2,1) = 0.0" in out)
    assert "if (picefr(4 + widx1 - 1,jstart + widx2 - 1) > 4.e-10)" in out


def test_where_body_containing_sum_with_dim(fortran_reader, fortran_writer):
    '''
    Since a SUM(x, dim=y) performs a reduction but produces an array, we need
    to replace it with a temporary in order to translate the WHERE into
    canonical form. We can't do that without being able to declare a suitable
    temporary and that requires TODO #1799.
    '''
    code = '''\
    module my_mod
    contains
    subroutine my_sub(picefr)
      use some_mod
      REAL(wp), INTENT(in), DIMENSION(:,:) ::   picefr
      REAL(wp), DIMENSION(jpi,jpj,jpl) :: zevap_ice
      REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: a_i_last_couple
      WHERE( picefr(:,:) > 1.e-10 )
        zevap_ice(:,:,1) = frcv(jpr_ievp)%z3(:,:,1) * &
                           SUM( a_i_last_couple, dim=3 ) / picefr(:,:)
      ELSEWHERE
        zevap_ice(:,:,1) = 0.0
      END WHERE
    end subroutine my_sub
    end module my_mod
'''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    assert isinstance(routine[0], CodeBlock)
    output = fortran_writer(psyir)
    assert "WHERE (picefr" in output
    assert "SUM(a_i_last_couple, dim = 3) / picefr(:, :)" in output


def test_where_containing_sum_no_dim(fortran_reader, fortran_writer):
    '''
    Since a SUM without a dim argument always produces a scalar we can
    translate a WHERE containing it into canonical form.
    '''
    code = '''\
    module my_mod
    contains
    subroutine my_sub(picefr)
      use some_mod
      REAL(wp), INTENT(in), DIMENSION(:,:) ::   picefr
      REAL(wp), DIMENSION(jpi,jpj,jpl) :: zevap_ice
      REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: a_i_last_couple
      type :: thing
        real, dimension(100000, 100000, 1) :: z3
      end type
      integer :: jpr_ievp
      type(thing), dimension(10000) :: frcv
      WHERE( picefr(:,:) > SUM(picefr) )
        zevap_ice(:,:,1) = frcv(jpr_ievp)%z3(:,:,1) * &
                           SUM( a_i_last_couple ) / picefr(:,:)
      ELSEWHERE
        zevap_ice(:,:,1) = 0.0
      END WHERE
    end subroutine my_sub
    end module my_mod
'''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    assert isinstance(routine[0], Loop)
    output = fortran_writer(psyir)
    assert ("SUM(a_i_last_couple) / picefr(LBOUND(picefr, dim=1) "
            "+ widx1 - 1,LBOUND(picefr, dim=2) + widx2 - 1)" in output)


def test_where_mask_containing_sum_with_dim(fortran_reader):
    '''Since a SUM(x, dim=y) appearing in a mask expression performs
    a reduction but produces an array, we need to replace it with a
    temporary in order to translate the WHERE into canonical form
    (since the mask expression determines the number of nested loops
    required). We can't do that without being able to declare a
    suitable temporary and that requires TODO #1799.

    '''
    code = '''\
    subroutine my_sub(v2)
      REAL(wp), INTENT(in), DIMENSION(:,:) :: v2
      REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:) :: v3
      where(sum(v2(:,:), dim=2) > 0.0)
        v3(:) = 1.0
      end where
    end subroutine my_sub
'''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    assert isinstance(routine[0], CodeBlock)


@pytest.mark.usefixtures("parser")
def test_where_with_scalar_assignment(fortran_reader, fortran_writer):
    ''' Test that a WHERE containing a scalar assignment is handled correctly.
    '''
    code = '''
    subroutine sub()
        integer, dimension(100,100,100) :: dry, z1_st, ptsu
        integer :: var1, depth

        where(dry(1, :, :))
            var1 = depth
            z1_st(:,2,:) = var1 / ptsu(:, :, 3)
        end where
    end subroutine sub
    '''
    fake_parent = fortran_reader.psyir_from_source(code)
    # We should have a doubly-nested loop with an IfBlock inside
    loops = fake_parent.walk(Loop)
    assert len(loops) == 2
    for loop in loops:
        assert "was_where" in loop.annotations
    assert isinstance(loops[1].loop_body[0], IfBlock)
    out = fortran_writer(fake_parent)
    correct = '''subroutine sub()
  integer, dimension(100,100,100) :: dry
  integer, dimension(100,100,100) :: z1_st
  integer, dimension(100,100,100) :: ptsu
  integer :: var1
  integer :: depth
  integer :: widx2
  integer :: widx1

  do widx2 = 1, SIZE(dry, dim=3), 1
    do widx1 = 1, SIZE(dry, dim=2), 1
      if (dry(1,widx1,widx2)) then
        var1 = depth
        z1_st(widx1,2,widx2) = var1 / ptsu(widx1,widx2,3)
      end if
    enddo
  enddo

end subroutine sub
'''
    assert out == correct


@pytest.mark.usefixtures("parser")
def test_where_with_array_reduction_intrinsic():
    ''' Test that a WHERE containing an array-reduction intrinsic is handled
    correctly. Currently it is not supported. This will be fixed in #1960.
    '''
    fake_parent, _ = process_where(
        "WHERE (dry(1, :, :))\n"
        "  var1 = maxval(depth(:))\n"
        "  z1_st(:, 2, :) = var1 / ptsu(:, :, 3)\n"
        "END WHERE\n", Fortran2003.Where_Construct,
        ["dry", "z1_st", "ptsu", "depth"], ["var1"])
    # We should have a doubly-nested loop with an IfBlock inside
    loops = fake_parent.walk(Loop)
    assert len(loops) == 2
    for loop in loops:
        assert "was_where" in loop.annotations
    assert isinstance(loops[1].loop_body[0], IfBlock)


@pytest.mark.usefixtures("parser")
def test_elsewhere():
    ''' Check that a WHERE construct with two ELSEWHERE clauses is correctly
    translated into a canonical form in the PSyIR.

    '''
    fake_parent, _ = process_where("WHERE (ptsu(:, :, :) > 10._wp)\n"
                                   "  z1_st(:, :, :) = 1._wp / ptsu(:, :, :)\n"
                                   "ELSEWHERE (ptsu(:, :, :) < 0.0_wp)\n"
                                   "  z1_st(:, :, :) = -1._wp\n"
                                   "ELSEWHERE\n"
                                   "  z1_st(:, :, :) = 0._wp\n"
                                   "END WHERE\n", Fortran2003.Where_Construct,
                                   ["ptsu", "z1_st"])
    # This should become:
    #
    # if ptsu(ji,jj,jk) > 10._wp)then
    #   zval = ...
    #   z1_st(ji,jj,jk) = ...
    # else
    #   if ptsu(ji,jj,jk) < 0.0_wp)then
    #     z1_st(ji,jj,jk) = -1...
    #   else
    #     z1_st(ji,jj,jk) = 0._wp
    # end if
    assert len(fake_parent.children) == 1
    # We should have no CodeBlock nodes
    assert fake_parent.walk(CodeBlock) == []
    # Check that we have a triply-nested loop
    loop = fake_parent.children[0]
    assert isinstance(loop.ast, Fortran2003.Where_Construct)
    assert isinstance(loop, Loop)
    assert isinstance(loop.loop_body[0], Loop)
    assert isinstance(loop.loop_body[0].loop_body[0], Loop)
    # Check that we have an IF block within the innermost loop
    ifblock = loop.loop_body[0].loop_body[0].loop_body[0]
    assert isinstance(ifblock, IfBlock)
    assert "was_where" in ifblock.annotations
    assert isinstance(ifblock.ast, Fortran2003.Where_Construct)
    assert isinstance(ifblock.condition, BinaryOperation)
    assert ifblock.condition.operator == BinaryOperation.Operator.GT
    assert (ifblock.condition.debug_string() ==
            "ptsu(LBOUND(ptsu, dim=1) + widx1 - 1,"
            "LBOUND(ptsu, dim=2) + widx2 - 1,"
            "LBOUND(ptsu, dim=3) + widx3 - 1) > 10._wp")
    # Check that this IF block has an else body which contains another IF
    assert ifblock.else_body is not None
    ifblock2 = ifblock.else_body[0]
    assert "was_where" in ifblock2.annotations
    assert isinstance(ifblock2, IfBlock)
    assert isinstance(ifblock2.condition, BinaryOperation)
    assert ifblock2.condition.operator == BinaryOperation.Operator.LT
    assert (ifblock2.condition.debug_string() ==
            "ptsu(LBOUND(ptsu, dim=1) + widx1 - 1,"
            "LBOUND(ptsu, dim=2) + widx2 - 1,"
            "LBOUND(ptsu, dim=3) + widx3 - 1) < 0.0_wp")
    # Check that this IF block too has an else body
    assert isinstance(ifblock2.else_body[0], Assignment)
    # Check that we have three assignments of the correct form and with the
    # correct parents
    assigns = ifblock.walk(Assignment)
    assert len(assigns) == 3
    for assign in assigns:
        assert isinstance(assign.lhs, ArrayReference)
        assert (assign.lhs.debug_string() ==
                "z1_st(LBOUND(z1_st, dim=1) + widx1 - 1,"
                "LBOUND(z1_st, dim=2) + widx2 - 1,"
                "LBOUND(z1_st, dim=3) + widx3 - 1)")
        assert isinstance(assign.parent.parent, IfBlock)

    assert isinstance(assigns[0].rhs, BinaryOperation)
    assert assigns[0].rhs.operator == BinaryOperation.Operator.DIV
    assert isinstance(assigns[1].rhs, UnaryOperation)
    assert assigns[1].rhs.operator == UnaryOperation.Operator.MINUS
    assert isinstance(assigns[2].rhs, Literal)
    assert "0." in assigns[2].rhs.value


@pytest.mark.usefixtures("parser")
def test_where_stmt_validity():
    ''' Check that the correct exceptions are raised when the parse tree
    for a WHERE statement has an unexpected structure.

    '''
    fake_parent, fparser2spec = process_where(
        "WHERE( at_i(:,:) > rn_amax_2d(:,:) )   "
        "a_i(:,:,jl) = a_i(:,:,jl) * rn_amax_2d(:,:) / at_i(:,:)",
        Fortran2003.Where_Stmt, ["at_i", "rn_amax_2d", "a_i", "jl"])
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


@pytest.mark.usefixtures("parser")
def test_where_stmt(fortran_reader):
    ''' Basic check that we handle a WHERE statement correctly.

    '''
    code = '''\
    program where_test
      implicit none
      integer :: jl
      real, dimension(:,:), allocatable :: at_i, rn_amax_2d
      real, dimension(:,:,:), allocatable :: a_i
      WHERE( at_i(:,:) > rn_amax_2d(:,:) ) &
            a_i(:,:,jl) = a_i(:,:,jl) * rn_amax_2d(:,:) / at_i(:,:)
    end program where_test
    '''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    assert len(routine.children) == 1
    assert isinstance(routine[0], Loop)


def test_where_stmt_no_reduction(fortran_reader, fortran_writer):
    '''
    Test that a WHERE statement containing an intrinsic reduction is put
    into a CodeBlock.
    '''
    code = '''\
    program my_prog
    integer, dimension(10,10,5) :: a3d
    integer, dimension(10,10) :: a_i, at_i, rn_amax_2d
    WHERE( at_i(:,:) > rn_amax_2d(:,:) ) a_i(:,:,jl) = a_i(:,:,jl) * \
rn_amax_2d(:,:) / sum(a3d, dim=3)
    end program my_prog'''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    assert isinstance(routine[0], CodeBlock)
    output = fortran_writer(psyir)
    assert ("WHERE (at_i(:, :) > rn_amax_2d(:, :)) a_i(:, :, jl) = "
            "a_i(:, :, jl) * rn_amax_2d(:, :) / SUM(a3d, dim = 3)" in output)


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
    sched = processor.generate_psyir(fparser2_tree)
    result = sched.walk(Routine)[0]
    assert isinstance(result[0], Assignment)
    assert isinstance(result[1], Loop)
    assert isinstance(result[2], Call)
    assert isinstance(result[3], Loop)


@pytest.mark.parametrize(
    "code, size_arg",
    [("where (my_type%var(:) > epsi20)\n"
      "my_type%array(:,jl) = 3.0\n", "my_type%var"),
     ("where (my_type%var(:) > epsi20)\n"
      "my_type2(jl)%array2(:,jl) = 3.0\n", "my_type%var"),
     ("where (my_type%block(jl)%var(:) > epsi20)\n"
      "my_type%block(jl)%array(:,jl) = 3.0\n", "my_type%block(jl)%var"),
     ("where (my_type%block(jl)%var(:) > epsi20)\n"
      "my_type%block(jl)%array(:,jl) = 3.0\n", "my_type%block(jl)%var"),
     ("where (my_type2(:)%var(jl) > epsi20)\n"
      "my_type2(:)%var(jl) = 3.0\n", "my_type2")])
def test_where_derived_type(fortran_reader, code, size_arg):
    ''' Test that we handle the case where array members of a derived type
    are accessed within a WHERE. '''
    code = (f"module my_mod\n"
            f" use some_mod\n"
            f"contains\n"
            f"subroutine my_sub()\n"
            f"  type :: subtype\n"
            f"    real, dimension(:) :: var\n"
            f"    real, dimension(:,:) :: array\n"
            f" end type\n"
            f"  type :: sometype\n"
            f"    real, dimension(:) :: var\n"
            f"    real, dimension(:) :: array\n"
            f"    real, dimension(:,:) :: array2\n"
            f"    type(subtype), dimension(:) :: block\n"
            f"  end type\n"
            f"  type(sometype) :: my_type\n"
            f"  type(sometype), dimension(:) :: my_type2\n"
            f"  integer :: jl\n"
            f"  real :: epsi20\n"
            f"  do jl = 1, 10\n"
            f"{code}"
            f"    end where\n"
            f"  end do\n"
            f"end subroutine my_sub\n"
            f"end module my_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    loops = psyir.walk(Loop)
    assert len(loops) == 2
    assert isinstance(loops[1].stop_expr, IntrinsicCall)
    assert (loops[1].stop_expr.debug_string() ==
            f"SIZE({size_arg}, dim=1)")
    assert isinstance(loops[1].loop_body[0], IfBlock)
    # All Range nodes should have been replaced
    assert not loops[0].walk(Range)
    # All ArrayMember accesses other than 'var' should now use the `widx1`
    # loop variable
    array_members = loops[0].walk(ArrayMember)
    for member in array_members:
        if member.name != "var":
            assert "+ widx1 - 1" in member.indices[0].debug_string()


@pytest.mark.parametrize(
    "code",
    [("where (my_type%var > epsi20)\n"
      "my_type%array(:,jl) = 3.0\n"),
     ("where (my_type%var > epsi20)\n"
      "my_type(jl)%array(:,jl) = 3.0\n"),
     ("where (my_type%block(jl)%var > epsi20)\n"
      "my_type(jl%array(:,jl) = 3.0\n"),
     ("where (my_type%block(jl)%var > epsi20)\n"
      "my_type%block(jl)%var = 3.0\n")])
@pytest.mark.xfail(reason="#1960 Can't handle WHERE constructs without "
                          "explicit array notation inside derived types.")
def test_where_noarray_syntax_derived_types(fortran_reader, code):
    '''Xfailing test for when a derived type access in a where condition
    doesn't use range syntax.'''
    code = (f"module my_mod\n"
            f" use some_mod\n"
            f"contains\n"
            f"subroutine my_sub()\n"
            f"  integer :: jl\n"
            f"  do jl = 1, 10\n"
            f"{code}"
            f"    end where\n"
            f"  end do\n"
            f"end subroutine my_sub\n"
            f"end module my_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    cbs = psyir.walk(CodeBlock)
    assert len(cbs) == 0


@pytest.mark.parametrize(
    "code",
    [("where (var(:) > 1.0)\n"
        "var(:) = 3.0\n"),
     ("where (var(:) > var2(1:20))\n"
      "var(:) = 3.0\n"),
     ("where (var > 1.0)\n"
      "var = 3.0\n"),
     ("where (var > var2)\n"
      "var = 3.0\n"),
     ("where (var(:) > var2)\n"
      "var(:) = 3.0\n"),
     ("where (var > var2(1:20))\n"
      "var = 3.0\n")])
def test_where_scalar_var(fortran_reader, code):
    '''Test where we have a scalar variable in a WHERE clause with no
    array index clause.'''
    code = (
        f"program where_test\n"
        f"implicit none\n"
        f"integer, parameter :: nc=20\n"
        f"integer :: ii\n"
        f"real :: var(nc)\n"
        f"real:: var2(nc)\n"
        f"var = 1.5\n"
        f"var2 = 1.6\n"
        f"{code}"
        f"end where\n"
        f"end program where_test")
    psyir = fortran_reader.psyir_from_source(code)
    loops = psyir.walk(Loop)
    assert len(loops) == 1
    assert isinstance(loops[0].stop_expr, Reference)
    assert loops[0].stop_expr.debug_string() == "nc"
    assert isinstance(loops[0].loop_body[0], IfBlock)
    # All Range nodes should have been replaced
    assert not loops[0].walk(Range)
    array_refs = loops[0].walk(ArrayReference)
    for member in array_refs:
        assert "widx1" in member.indices[0].debug_string()

    # All References to var or var2 should be ArrayReferences
    refs = loops[0].walk(Reference)
    for ref in refs:
        if "var" in ref.symbol.name:
            assert isinstance(ref, ArrayReference)


def test_import_in_where_clause(fortran_reader):
    '''
    Test that imported symbols inside a where clause produce a code block
    as we don't know what is scalar vs an array.
    '''
    code = '''
    program where_test
    implicit none
    use some_module, only: a, b, c, d

    where(a(:) + b > 1)
       b = c + d
    end where
    end program
    '''
    psyir = fortran_reader.psyir_from_source(code)
    assert isinstance(psyir.children[0].children[0], CodeBlock)

    code2 = '''
    program where_test
    implicit none
    use some_module, only: c, d
    integer, dimension(100) :: a, b

    where(a(:) + b > 1)
       b = c + d
    end where
    end program
    '''
    psyir = fortran_reader.psyir_from_source(code2)
    assert isinstance(psyir.children[0].children[0], CodeBlock)


def test_non_array_reduction_intrinsic(fortran_reader, fortran_writer):
    '''
    Test that a non-array reduction intrinsic (such as SIN) produces
    the correct PSyIR.
    '''
    code = '''
    program where_test
    implicit none
    integer, dimension(100) :: a, b, c

    where(a < b)
       c = sin(a)
    end where
    end program
    '''
    psyir = fortran_reader.psyir_from_source(code)
    assert isinstance(psyir.children[0].children[0], Loop)
    out = fortran_writer(psyir)
    correct = '''program where_test
  integer, dimension(100) :: a
  integer, dimension(100) :: b
  integer, dimension(100) :: c
  integer :: widx1

  do widx1 = 1, 100, 1
    if (a(widx1) < b(widx1)) then
      c(widx1) = SIN(a(widx1))
    end if
  enddo

end program where_test'''
    assert correct in out


def test_non_elemental_intrinsic(fortran_reader):
    '''
    Test that a non-elemental reduction intrinsic (such as DOT_PRODUCT)
    produces the correct PSyIR.
    '''
    code = '''
    program where_test
    implicit none
    integer, dimension(100) :: a, b, c

    where(a < b)
       c = DOT_PRODUCT(a, b)
    end where
    end program
    '''
    psyir = fortran_reader.psyir_from_source(code)
    # This should be a Loop I suppose, this is valid Fortran
    # as long as a and b aren't expanded
    assert isinstance(psyir.children[0].children[0], Loop)
    intrinsic = psyir.walk(IntrinsicCall)[0]
    assert intrinsic.intrinsic.name == "DOT_PRODUCT"
    assert intrinsic.children[1].name == "a"
    assert not isinstance(intrinsic.children[1], ArrayReference)
    assert intrinsic.children[2].name == "b"
    assert not isinstance(intrinsic.children[2], ArrayReference)


def test_non_array_ref_intrinsic_transformation_error(fortran_reader):
    '''
    Test for coverage for the try/except in the intrinsic_ancestor section
    of _array_syntax_to_indexed sub function.'''

    code = '''
    program where_test
    implicit none
    integer, dimension(100) :: a, b, c
    real :: d

    where(a < b)
       c = sin(d)
    end where
    end program
    '''
    psyir = fortran_reader.psyir_from_source(code)
    references = psyir.walk(Reference)
    # The d should not have been transformed into an array.
    assert not isinstance(references[7], ArrayReference)


def test_elemental_intrinsic_to_loop(fortran_reader, fortran_writer):
    '''
    Tests that if we have an elemental intrinsic (like ABS) that we
    expand the where into a loop.
    '''
    code = '''
    program where_test
    implicit none
    real, dimension(100) :: a, b

    where(abs(a) < 2)
        b = a
    end where
    end program
    '''
    psyir = fortran_reader.psyir_from_source(code)
    correct = '''program where_test
  real, dimension(100) :: a
  real, dimension(100) :: b
  integer :: widx1

  do widx1 = 1, 100, 1
    if (ABS(a(widx1)) < 2) then
      b(widx1) = a(widx1)
    end if
  enddo

end program where_test'''
    out = fortran_writer(psyir)
    assert correct in out

    # Test a case with both an intrinsic and non-intrinsic
    code = '''
    program where_test
    implicit none
    real, dimension(100) :: a, b

    where(dot_product(a,a(:)) + abs(a) < 2)
        b = a
    end where
    end program
    '''
    psyir = fortran_reader.psyir_from_source(code)
    out = fortran_writer(psyir)
    assert isinstance(psyir.children[0].children[0], Loop)
    correct = '''do widx1 = 1, 100, 1
    if (DOT_PRODUCT(a, a(:)) + ABS(a(widx1)) < 2) then
      b(widx1) = a(widx1)
    end if
  enddo'''
    assert correct in out


def test_elemental_function_to_loop(fortran_reader, fortran_writer):
    '''
    Tests that if we have an elemental function that we expand the where
    into a loop.
    '''
    code = '''
    module mymod
    contains
    real elemental function x(i)
       real :: i
       x = i + 1.5
    end function
    subroutine where_test
    implicit none
    real, dimension(100) :: a, b

    where(x(a) < 2)
        b = a
    end where
    end subroutine
    end module'''
    psyir = fortran_reader.psyir_from_source(code)
    correct = '''  subroutine where_test()
    real, dimension(100) :: a
    real, dimension(100) :: b
    integer :: widx1

    do widx1 = 1, 100, 1
      if (x(a(widx1)) < 2) then
        b(widx1) = a(widx1)
      end if
    enddo

  end subroutine where_test'''
    out = fortran_writer(psyir)
    assert correct in out

    # Test the behaviour if we have a non-elemental function.
    code = '''
    module mymod
    contains
    real function x(i)
       real, dimension(*) :: i
       x = sum(i) + 1.5
    end function
    subroutine where_test
    implicit none
    real, dimension(100) :: a, b

    where(x(a(:)) + abs(a) < 2)
        b = a
    end where
    end subroutine
    end module'''
    psyir = fortran_reader.psyir_from_source(code)
    assert isinstance(psyir.children[0].children[1].children[0], Loop)
    correct = '''do widx1 = 1, 100, 1
      if (x(a(:)) + ABS(a(widx1)) < 2) then
        b(widx1) = a(widx1)
      end if
    enddo'''
    out = fortran_writer(psyir)
    assert correct in out

    # Test the behaviour if we have a non-elemental and elemental
    # functions in the where.
    code = '''
    module mymod
    contains
    real function x(i)
       real, dimension(*) :: i
       x = sum(i) + 1.5
    end function
    real elemental function y(i)
       real :: i
       y = i + 1.5
    end function
    subroutine where_test
    implicit none
    real, dimension(100) :: a, b

    where(x(a(:)) + y(a) < 2)
        b = a
    end where
    end subroutine
    end module'''
    psyir = fortran_reader.psyir_from_source(code)
    assert isinstance(psyir.children[0].children[2].children[0], Loop)
    correct = '''do widx1 = 1, 100, 1
      if (x(a(:)) + y(a(widx1)) < 2) then
        b(widx1) = a(widx1)
      end if
    enddo'''
    out = fortran_writer(psyir)
    assert correct in out

    # Imported function has unknown elemental status.
    code = '''
    subroutine test
    use mod, only: somefunc
    real, dimension(100) :: a, b
    where(somefunc(a) < 2)
        b = a
    end where
    end subroutine'''
    psyir = fortran_reader.psyir_from_source(code)
    assert isinstance(psyir.children[0].children[0], CodeBlock)
    correct = '''! PSyclone CodeBlock (unsupported code) reason:
  !  - WHERE not supported because 'a' cannot be converted to an array \
due to: Transformation Error: The supplied node is passed as an argument to \
a Call that may or may not be elemental: 'somefunc(a)'. Consider \
adding the function's filename to RESOLVE_IMPORTS.
  WHERE (somefunc(a) < 2)
    b = a
  END WHERE'''
    out = fortran_writer(psyir)
    assert correct in out


def test_array_syntax_to_indexed_unknown_elemental(fortran_reader):
    # Check we get a codeblock if we have an function of unknown elemental
    # status.
    code = '''
    module mymod
    contains
    real function x(i)
       real, dimension(*) :: i
       x = sum(i) + 1.5
    end function
    subroutine where_test
    implicit none
    real, dimension(100) :: a, b

    if(x(a(:)) + abs(a) < 2) then
        b = a
    end if
    end subroutine
    end module'''
    psyir = fortran_reader.psyir_from_source(code)
    calls = psyir.walk(Call)
    # Override the call to x symbol to be None elemental type
    calls[1].routine.symbol.is_elemental = None
    ifblock = psyir.walk(IfBlock)[0]
    parser = fortran_reader._processor
    with pytest.raises(NotImplementedError) as excinfo:
        parser._array_syntax_to_indexed(ifblock, ["i"])
    assert ("WHERE not supported because 'a' cannot be converted to an "
            "array due to: Transformation Error: The supplied node is "
            "passed as an argument to a Call that may or may not be elemental"
            ": 'x(a(:))'. Consider adding the function's "
            "filename to RESOLVE_IMPORTS." in str(excinfo.value))


def test_nested_where(fortran_reader, fortran_writer):
    ''' Test that we handle nested WHERE statements. '''

    # If the types are not known it creates a codeblock with the associated
    # reason explained
    code = ("module my_mod\n"
            "  use some_mod\n"
            "contains\n"
            "subroutine my_sub()\n"
            "  WHERE ( z_lenp4(:,:) <= 0.0_wp )\n"
            "    p_dal%D12(:,:,1) = 0.0_wp\n"
            "    z_lenp2(:,:) = SQRT( p_dal%D11(:,:,1) * p_dal%D22(:,:,1) )\n"
            "    WHERE ( z_lenp2(:,:) == 0.0_wp )\n"
            "      p_dal%D11(:,:,1) = p_ens%D11_min(:,:)\n"
            "      p_dal%D22(:,:,1) = p_ens%D22_min(:,:)\n"
            "      z_lenp2(:,:) = z_lenp2_min(:,:)\n"
            "    END WHERE\n"
            "  END WHERE\n"
            "end subroutine my_sub\n"
            "end module my_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    code = fortran_writer(psyir)
    assert ("WHERE not supported because 'p_dal' cannot be converted to an "
            "array due to: Transformation Error: The supplied node should be "
            "a Reference to a symbol of known type, but 'p_dal%D12(:,:,1)' is"
            " 'UnresolvedType'. Consider adding the name of the file "
            "containing the declaration of this quantity to RESOLVE_IMPORTS.")

    # If some information is provided, one of the WHEREs can be resolved, but
    # the other still is a CodeBlock
    code = ("module my_mod\n"
            "  use some_mod\n"
            "contains\n"
            "subroutine my_sub()\n"
            "  type :: mytype\n"
            "    real, dimension(10,10,10) :: D11\n"
            "    real, dimension(10,10,10) :: D12\n"
            "    real, dimension(10,10,10) :: D22\n"
            "  end type\n"
            "  type(mytype) :: p_dal\n"
            "  WHERE ( z_lenp4(:,:) <= 0.0_wp )\n"
            "    p_dal%D12(:,:,1) = 0.0_wp\n"
            "    z_lenp2(:,:) = SQRT( p_dal%D11(:,:,1) * p_dal%D22(:,:,1) )\n"
            "    WHERE ( z_lenp2(:,:) == 0.0_wp )\n"
            "      p_dal%D11(:,:,1) = p_ens%D11_min(:,:)\n"
            "      p_dal%D22(:,:,1) = p_ens%D22_min(:,:)\n"
            "      z_lenp2(:,:) = z_lenp2_min(:,:)\n"
            "    END WHERE\n"
            "  END WHERE\n"
            "end subroutine my_sub\n"
            "end module my_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    code = fortran_writer(psyir)
    assert """
    do widx2 = 1, SIZE(z_lenp4, dim=2), 1
      do widx1 = 1, SIZE(z_lenp4, dim=1), 1
        if (z_lenp4(LBOUND(z_lenp4, dim=1) + widx1 - 1,LBOUND(z_lenp4, dim=2) \
+ widx2 - 1) <= 0.0_wp) then
          p_dal%D12(widx1,widx2,1) = 0.0_wp
          z_lenp2(LBOUND(z_lenp2, dim=1) + widx1 - 1,LBOUND(z_lenp2, dim=2) + \
widx2 - 1) = SQRT(p_dal%D11(widx1,widx2,1) * p_dal%D22(widx1,widx2,1))

          ! PSyclone CodeBlock (unsupported code) reason:
          !  - WHERE not supported because 'p_ens' cannot be converted to an \
array due to: Transformation Error: The supplied node should be a Reference \
to a symbol of known type, but 'p_ens%D11_min(:,:)' is 'UnresolvedType'. \
Consider adding the name of the file containing the declaration of this \
quantity to RESOLVE_IMPORTS.
          WHERE (z_lenp2(:, :) == 0.0_wp)
            p_dal % D11(:, :, 1) = p_ens % D11_min(:, :)
            p_dal % D22(:, :, 1) = p_ens % D22_min(:, :)
            z_lenp2(:, :) = z_lenp2_min(:, :)
          END WHERE
        end if
      enddo
    enddo""" in code

    # If enough information is provided, both WHEREs are resolved and nested
    code = ("module my_mod\n"
            "  use some_mod\n"
            "contains\n"
            "subroutine my_sub()\n"
            "  type :: mytype\n"
            "    real, dimension(10,10,10) :: D11\n"
            "    real, dimension(10,10,10) :: D12\n"
            "    real, dimension(10,10,10) :: D22\n"
            "  end type\n"
            "  type :: mytype2\n"
            "    real, dimension(10,10) :: D11_min\n"
            "    real, dimension(10,10) :: D22_min\n"
            "  end type\n"
            "  type(mytype) :: p_dal\n"
            "  type(mytype2) :: p_ens\n"
            "  WHERE ( z_lenp4(:,:) <= 0.0_wp )\n"
            "    p_dal%D12(:,:,1) = 0.0_wp\n"
            "    z_lenp2(:,:) = SQRT( p_dal%D11(:,:,1) * p_dal%D22(:,:,1) )\n"
            "    WHERE ( z_lenp2(:,:) == 0.0_wp )\n"
            "      p_dal%D11(:,:,1) = p_ens%D11_min(:,:)\n"
            "      p_dal%D22(:,:,1) = p_ens%D22_min(:,:)\n"
            "      z_lenp2(:,:) = z_lenp2_min(:,:)\n"
            "    END WHERE\n"
            "  END WHERE\n"
            "end subroutine my_sub\n"
            "end module my_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    code = fortran_writer(psyir)
    assert """
    do widx2 = 1, SIZE(z_lenp4, dim=2), 1
      do widx1 = 1, SIZE(z_lenp4, dim=1), 1
        if (z_lenp4(LBOUND(z_lenp4, dim=1) + widx1 - 1,LBOUND(z_lenp4, dim=2) \
+ widx2 - 1) <= 0.0_wp) then
          p_dal%D12(widx1,widx2,1) = 0.0_wp
          z_lenp2(LBOUND(z_lenp2, dim=1) + widx1 - 1,LBOUND(z_lenp2, dim=2) + \
widx2 - 1) = SQRT(p_dal%D11(widx1,widx2,1) * p_dal%D22(widx1,widx2,1))
          do widx2_1 = 1, SIZE(z_lenp2, dim=2), 1
            do widx1_1 = 1, SIZE(z_lenp2, dim=1), 1
              if (z_lenp2(LBOUND(z_lenp2, dim=1) + widx1_1 - 1,\
LBOUND(z_lenp2, dim=2) + widx2_1 - 1) == 0.0_wp) then
                p_dal%D11(widx1_1,widx2_1,1) = p_ens%D11_min(widx1_1,widx2_1)
                p_dal%D22(widx1_1,widx2_1,1) = p_ens%D22_min(widx1_1,widx2_1)
                z_lenp2(LBOUND(z_lenp2, dim=1) + widx1_1 - 1,\
LBOUND(z_lenp2, dim=2) + widx2_1 - 1) = \
z_lenp2_min(LBOUND(z_lenp2_min, dim=1) + widx1_1 - 1,\
LBOUND(z_lenp2_min, dim=2) + widx2_1 - 1)
              end if
            enddo
          enddo
        end if
      enddo
    enddo""" in code
