# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
# ----------------------------------------------------------------------------
# Author: S. Siso, STFC Daresbury Lab

'''This module tests the hoist loop bound expressions transformation.
'''

import pytest

from psyclone.psyir.nodes import Literal, Loop, Routine
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE
from psyclone.psyir.transformations import HoistLoopBoundExprTrans, \
    TransformationError
from psyclone.transformations import OMPParallelLoopTrans


def test_str():
    '''Test the transformation's str method return the expected string.

    '''
    trans = HoistLoopBoundExprTrans()
    assert str(trans) == ("Hoist complex loop bound expressions outside the "
                          "loop construct")


def test_apply(fortran_reader, fortran_writer):
    '''Test the apply method moves the complex loop bounds out of
    the loop construct and places them immediately before the loop.

    '''
    psyir = fortran_reader.psyir_from_source('''
        module test_mod
            use mymod, only: mytype
            contains
            subroutine test(A)
                real, dimension(:), intent(inout) :: A
                integer :: i
                do i=1, UBOUND(a,1), mytype%step
                    A(i) = 1
                enddo
            end subroutine test
        end module test_mod
    ''')
    loop = psyir.walk(Loop)[0]
    trans = HoistLoopBoundExprTrans()
    trans.apply(loop)
    # Start expression is not hoisted because it is a literal
    expected = """
    loop_step = mytype%step
    loop_stop = UBOUND(a, 1)
    do i = 1, loop_stop, loop_step
      a(i) = 1
    enddo\n"""
    assert expected in fortran_writer(psyir)


def test_apply_nested(fortran_reader, fortran_writer):
    '''Test the apply method moves the complex loop bounds out of
    the loop construct and places them immediately before the loop,
    but the symbols are always at the routine scope.

    '''
    psyir = fortran_reader.psyir_from_source('''
        module test_mod
            use mymod, only: mytype
            contains
            subroutine test(A)
                real, dimension(:,:), intent(inout) :: A
                integer :: start = 1
                integer :: i,j
                do i=start, UBOUND(a,2)
                    do j=LBOUND(a,1), UBOUND(a,1)
                        A(j, i) = 1
                    enddo
                enddo
            end subroutine test
        end module test_mod
    ''')
    trans = HoistLoopBoundExprTrans()
    for loop in psyir.walk(Loop):
        trans.apply(loop)
    # Start expression is not hoisted because it is a simple scalar reference
    expected = """
    loop_stop = UBOUND(a, 2)
    do i = start, loop_stop, 1
      loop_stop_1 = UBOUND(a, 1)
      loop_start = LBOUND(a, 1)
      do j = loop_start, loop_stop_1, 1
        a(j,i) = 1
      enddo
    enddo\n"""
    assert expected in fortran_writer(psyir)
    routine_symtab = psyir.walk(Routine)[0].symbol_table
    # Check that all the new symbols are in the routine scope
    assert "loop_start" in routine_symtab
    assert "loop_stop" in routine_symtab
    assert "loop_stop_1" in routine_symtab


def test_validate_loop_with_directive(fortran_reader):
    '''Test that the validate method rejects bound expression hoisting when
    the loop construct has a parent schedule belonging to a directive.

    TODO #1817: This behaviour could change if loop directives get rid
    of the schedule.

    '''
    psyir = fortran_reader.psyir_from_source('''
        module test_mod
            contains
            subroutine test(A)
                real, dimension(:), intent(inout) :: A
                integer :: i
                do i=LBOUND(a,1), UBOUND(a,1)
                    A(i) = 1
                enddo
            end subroutine test
        end module test_mod
    ''')
    loop = psyir.walk(Loop)[0]
    # Currently OMPLoops cannot be applied to generic Loops, we bypass this
    # limitation by giving it a loop_type attribute.
    loop.loop_type = None
    omplooptrans = OMPParallelLoopTrans()
    hoist_trans = HoistLoopBoundExprTrans()
    omplooptrans.apply(loop)
    with pytest.raises(TransformationError) as err:
        hoist_trans.validate(loop)
    assert ("The loop provided to HoistLoopBoundExprTrans must not be directly"
            " inside a Directive as its Schedule does not support multiple "
            "statements, but found 'OMPParallelDoDirective'."
            in str(err.value))


def test_validate():
    '''Test the apply method call the validation and that this checks that the
    hoist is applied to a loop which has an ancestor Routine node.

    '''
    trans = HoistLoopBoundExprTrans()
    with pytest.raises(TransformationError) as err:
        trans.apply(Literal("3", INTEGER_TYPE))
    assert ("Target of HoistLoopBoundExprTrans transformation must be a "
            "sub-class of Loop but got 'Literal'" in str(err.value))
    with pytest.raises(TransformationError) as err:
        trans.apply(Loop.create(
            DataSymbol("i", INTEGER_TYPE), Literal("1", INTEGER_TYPE),
            Literal("10", INTEGER_TYPE), Literal("1", INTEGER_TYPE), []))
    assert ("The loop provided to HoistLoopBoundExprTrans must belong to a "
            "Routine into which the hoisted expressions can be placed."
            in str(err.value))
