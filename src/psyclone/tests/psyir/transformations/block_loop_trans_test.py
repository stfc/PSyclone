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
# Authors A. B. G. Chalk STFC Daresbury Lab
# -----------------------------------------------------------------------------
'''This module contains the unit tests for the BlockLoopTrans module'''
from __future__ import absolute_import, print_function
import os
import pytest

from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Literal, Loop, Reference, Schedule, \
    Routine, BinaryOperation, Assignment
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, \
    ScalarType, SymbolTable
from psyclone.psyir.transformations import TransformationError, BlockLoopTrans
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory

GOCEAN_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                os.pardir, os.pardir, "test_files",
                                "gocean1p0")


def test_blockloop_trans():
    '''Test the base functions of BlockLoopTrans'''
    trans = BlockLoopTrans()
    assert str(trans) == "Split a loop into a blocked loop pair"


def test_blockloop_trans_validate1():
    '''Test the validate function of BlockLoopTrans for non constant
    increment'''
    blocktrans = BlockLoopTrans()
    # Construct a Loop with a non-constant increment
    routine = Routine("test_routine")
    lvar = routine.symbol_table.symbol_from_tag(
            "lvar", symbol_type=DataSymbol, datatype=ScalarType(
                    ScalarType.Intrinsic.INTEGER,
                    ScalarType.Precision.SINGLE))
    incvar = routine.symbol_table.symbol_from_tag(
            "incvar", symbol_type=DataSymbol, datatype=ScalarType(
                    ScalarType.Intrinsic.INTEGER,
                    ScalarType.Precision.SINGLE))
    parent = Loop()
    parent.addchild(Reference(lvar))
    parent.addchild(Literal("10", INTEGER_TYPE))
    parent.addchild(Reference(incvar))
    parent.addchild(Schedule())
    with pytest.raises(TransformationError) as excinfo:
        blocktrans.validate(parent)
    assert ("Cannot apply a BlockLoopTrans to a loop with a non-constant step"
            " size") in str(excinfo.value)


def test_blockloop_trans_validate2():
    '''Test the validate function of BlockLoopTrans for bad step sizes'''
    blocktrans = BlockLoopTrans()
    # Construct a Loop with too large a step-size
    parent = Loop()
    parent.addchild(Literal("1", INTEGER_TYPE))
    parent.addchild(Literal("2560", INTEGER_TYPE))
    parent.addchild(Literal("128", INTEGER_TYPE))
    parent.addchild(Schedule())
    with pytest.raises(TransformationError) as excinfo:
        blocktrans.validate(parent, {"blocksize": 16})
    assert ("Cannot apply a BlockLoopTrans to a loop with larger step size "
            "than the chosen block size") in str(excinfo.value)

    # Construct a Loop with step-size of 0
    parent = Loop()
    parent.addchild(Literal("1", INTEGER_TYPE))
    parent.addchild(Literal("2560", INTEGER_TYPE))
    parent.addchild(Literal("0", INTEGER_TYPE))
    parent.addchild(Schedule())
    with pytest.raises(TransformationError) as excinfo:
        blocktrans.validate(parent)
    assert ("Cannot apply a BlockLoopTrans to a loop with a step size of 0"
            in str(excinfo.value))


def test_blockloop_trans_validate3():
    '''Test the validate function of BlockLoopTrans fails when applying it
    to a loop which has already been blocked'''
    blocktrans = BlockLoopTrans()
    # Construct a Loop and apply a BlockLoopTrans to it, then revalidate the
    # parent loop (can't apply a block loop trans to a block loop trans
    symbol_table = SymbolTable()
    parent = Loop()
    parent.addchild(Literal("1", INTEGER_TYPE))
    parent.addchild(Literal("512", INTEGER_TYPE))
    parent.addchild(Literal("1", INTEGER_TYPE))
    parent.addchild(Schedule())
    routine = Routine.create("test_routine", symbol_table, [parent])
    lvar = routine.symbol_table.symbol_from_tag(
            "lvar", symbol_type=DataSymbol, datatype=ScalarType(
                    ScalarType.Intrinsic.INTEGER,
                    ScalarType.Precision.SINGLE))
    parent._variable = lvar
    blocktrans.apply(parent)
    with pytest.raises(TransformationError) as excinfo:
        blocktrans.validate(parent.ancestor(Loop))
    assert "Cannot apply a BlockLoopTrans to an already blocked loop." \
           in str(excinfo.value)
    with pytest.raises(TransformationError) as excinfo:
        blocktrans.validate(parent)
    assert "Cannot apply a BlockLoopTrans to an already blocked loop." \
           in str(excinfo.value)


def test_blockloop_trans_validate4():
    '''Test the validate function of BlockLoopTrans fails when applying it
    to a loop that writes to the loop variables'''
    blocktrans = BlockLoopTrans()
    # Construct a Loop that writes to the Loop variable inside its body
    symbol_table = SymbolTable()
    parent = Loop()
    parent.addchild(Literal("1", INTEGER_TYPE))
    parent.addchild(Literal("512", INTEGER_TYPE))
    parent.addchild(Literal("1", INTEGER_TYPE))
    sched = Schedule()
    parent.addchild(sched)
    routine = Routine.create("test_routine", symbol_table, [parent])
    lvar = routine.symbol_table.symbol_from_tag(
            "lvar", symbol_type=DataSymbol, datatype=ScalarType(
                    ScalarType.Intrinsic.INTEGER,
                    ScalarType.Precision.SINGLE))
    binop = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                   Reference(lvar), Literal("1", INTEGER_TYPE))
    assign = Assignment.create(Reference(lvar), binop)
    sched.addchild(assign)
    parent._variable = lvar
    with pytest.raises(TransformationError) as excinfo:
        blocktrans.validate(parent)
    assert("Cannot apply a BlockedLoopTrans to a loop where loop variables are"
           " written to inside the loop body.") in str(excinfo.value)

    # Construct a loop that writes to the variable used for the initial value
    symbol_table = SymbolTable()
    parent = Loop()
    parent.addchild(Literal("1", INTEGER_TYPE))
    parent.addchild(Literal("512", INTEGER_TYPE))
    parent.addchild(Literal("1", INTEGER_TYPE))
    sched = Schedule()
    parent.addchild(sched)
    routine = Routine.create("test_routine", symbol_table, [parent])
    lvar = routine.symbol_table.symbol_from_tag(
            "lvar", symbol_type=DataSymbol, datatype=ScalarType(
                    ScalarType.Intrinsic.INTEGER,
                    ScalarType.Precision.SINGLE))
    ivar = routine.symbol_table.symbol_from_tag(
            "ivar", symbol_type=DataSymbol, datatype=ScalarType(
                    ScalarType.Intrinsic.INTEGER,
                    ScalarType.Precision.SINGLE))
    binop = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                   Reference(lvar), Literal("1", INTEGER_TYPE))
    assign = Assignment.create(Reference(ivar), binop)
    sched.addchild(assign)
    parent.children[0].replace_with(Reference(ivar))
    parent._variable = lvar
    with pytest.raises(TransformationError) as excinfo:
        blocktrans.validate(parent)
    assert("Cannot apply a BlockedLoopTrans to a loop where loop variables are"
           " written to inside the loop body.") in str(excinfo.value)

    # Construct a loop that writes to the variable used for the final value
    symbol_table = SymbolTable()
    parent = Loop()
    parent.addchild(Literal("1", INTEGER_TYPE))
    parent.addchild(Literal("512", INTEGER_TYPE))
    parent.addchild(Literal("1", INTEGER_TYPE))
    sched = Schedule()
    parent.addchild(sched)
    routine = Routine.create("test_routine", symbol_table, [parent])
    lvar = routine.symbol_table.symbol_from_tag(
            "lvar", symbol_type=DataSymbol, datatype=ScalarType(
                    ScalarType.Intrinsic.INTEGER,
                    ScalarType.Precision.SINGLE))
    ivar = routine.symbol_table.symbol_from_tag(
            "ivar", symbol_type=DataSymbol, datatype=ScalarType(
                    ScalarType.Intrinsic.INTEGER,
                    ScalarType.Precision.SINGLE))
    binop = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                   Reference(ivar), Literal("1", INTEGER_TYPE))
    assign = Assignment.create(Reference(ivar), binop)
    sched.addchild(assign)
    parent.children[1].replace_with(Reference(ivar))
    parent._variable = lvar
    with pytest.raises(TransformationError) as excinfo:
        blocktrans.validate(parent)
    assert("Cannot apply a BlockedLoopTrans to a loop where loop variables are"
           " written to inside the loop body.") in str(excinfo.value)


def test_blockloop_trans_validate5():
    '''Test the validate function of BlockLoopTrans passes when applying it
    to a loop that reads from the loop variable'''
    blocktrans = BlockLoopTrans()
    # Construct a loop that reads from the loop variable (this is allowed)
    symbol_table = SymbolTable()
    parent = Loop()
    parent.addchild(Literal("1", INTEGER_TYPE))
    parent.addchild(Literal("512", INTEGER_TYPE))
    parent.addchild(Literal("1", INTEGER_TYPE))
    sched = Schedule()
    parent.addchild(sched)
    routine = Routine.create("test_routine", symbol_table, [parent])
    lvar = routine.symbol_table.symbol_from_tag(
            "lvar", symbol_type=DataSymbol, datatype=ScalarType(
                    ScalarType.Intrinsic.INTEGER,
                    ScalarType.Precision.SINGLE))
    ivar = routine.symbol_table.symbol_from_tag(
            "ivar", symbol_type=DataSymbol, datatype=ScalarType(
                    ScalarType.Intrinsic.INTEGER,
                    ScalarType.Precision.SINGLE))
    binop = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                   Reference(lvar), Literal("1", INTEGER_TYPE))
    assign = Assignment.create(Reference(ivar), binop)
    sched.addchild(assign)
    parent._variable = lvar
    blocktrans.validate(parent)


def test_blockloop_trans_apply_pos():
    '''Test the apply function of BlockLoopTrans'''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    blocktrans = BlockLoopTrans()
    blocktrans.apply(schedule.children[0])
    code = str(psy.gen)
    print(code)
    correct = \
        '''DO j_out_var = cu_fld%internal%ystart, cu_fld%internal%ystop, 32
        j_el_inner = MIN(j_out_var + 32, cu_fld%internal%ystop)
        DO j = j_out_var, j_el_inner, 1
          DO i = cu_fld%internal%xstart, cu_fld%internal%xstop, 1
    '''
    assert correct in code
    correct = '''END DO
        END DO
      END DO'''
    assert correct in code


def test_blockloop_trans_apply_neg():
    '''Test the apply function of BlockLoopTrans for a negative step'''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    schedule.children[0].children[2].replace_with(Literal("-1", INTEGER_TYPE))
    blocktrans = BlockLoopTrans()
    blocktrans.apply(schedule.children[0])
    code = str(psy.gen)
    correct = \
        '''DO j_out_var = cu_fld%internal%ystart, cu_fld%internal%ystop, -32
        j_el_inner = MAX(j_out_var - 32, cu_fld%internal%ystop)
        DO j = j_out_var, j_el_inner, -1
          DO i = cu_fld%internal%xstart, cu_fld%internal%xstop, 1
    '''
    assert correct in code
    correct = '''END DO
        END DO
      END DO'''
    assert correct in code


def test_blockloop_trans_apply_double_block():
    '''Test the apply function of BlockLoopTrans for multiple
    blocks of 2 nested loops'''
    code = \
        '''Program test
    integer :: i, j, end
    integer, dimension(1:end, 1:end) :: ai, aj
    do i=1, end
        do j=1, end
            ai(i, j) = 1
        end do
    end do

    do i=1, end, 2
      do j = 1, end, 2
        aj(i, j) = 1
      end do
    end do
    End Program test'''
    reader = FortranReader()
    psyir = reader.psyir_from_source(code)
    blocktrans = BlockLoopTrans()
    loops = psyir.walk(Loop)
    for loop in loops:
        blocktrans.apply(loop)
    writer = FortranWriter()
    result = writer(psyir)
    correct_vars = \
        '''integer :: i_el_inner
  integer :: i_out_var
  integer :: j_el_inner
  integer :: j_out_var'''
    assert correct_vars in result

    correct = \
        '''do i_out_var = 1, end, 32
    i_el_inner = MIN(i_out_var + 32, end)
    do i = i_out_var, i_el_inner, 1
      do j_out_var = 1, end, 32
        j_el_inner = MIN(j_out_var + 32, end)
        do j = j_out_var, j_el_inner, 1
          ai(i,j) = 1
        enddo
      enddo
    enddo
  enddo
  do i_out_var = 1, end, 32
    i_el_inner = MIN(i_out_var + 32, end)
    do i = i_out_var, i_el_inner, 2
      do j_out_var = 1, end, 32
        j_el_inner = MIN(j_out_var + 32, end)
        do j = j_out_var, j_el_inner, 2
          aj(i,j) = 1
        enddo
      enddo
    enddo
  enddo'''
    assert correct in result
