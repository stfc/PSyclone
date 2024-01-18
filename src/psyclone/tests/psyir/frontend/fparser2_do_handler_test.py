# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Authors: A. R. Porter and N. Nobre, STFC Daresbury Lab
# Modified A. B. G. Chalk, STFC Daresbury Lab
# Modified R. W. Ford, STFC Daresbury Lab
# Modified S. Siso, STFC Daresbury Lab

''' Module containing pytest tests for the handling of the DO
construct in the PSyIR fparser2 frontend. '''


from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.utils import walk

from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import (
    Assignment, BinaryOperation, CodeBlock, Literal, Loop, Routine, Schedule,
    WhileLoop, Reference)
from psyclone.psyir.symbols import DataSymbol, ScalarType, INTEGER_TYPE
from psyclone.tests.utilities import Compile


def test_handling_end_do_stmt(fortran_reader):
    ''' Test that the fparser2 End_Do_Stmt is ignored.'''
    code = '''
      subroutine test()
        integer :: i, a
        do i=1,10
            a=a+1
        end do
      end subroutine test
        '''
    psyir = fortran_reader.psyir_from_source(code)
    sched = psyir.walk(Schedule)[0]
    assert len(sched.children) == 1  # Just the loop (no end statement)


def test_do_construct(fortran_reader):
    ''' Check that do loop constructs are converted to the expected
    PSyIR node. '''
    code = '''
      subroutine test()
        integer :: i, sum
        do i = 1, 10 , 2
            sum = sum + i
        end do
      end subroutine test
      '''
    psyir = fortran_reader.psyir_from_source(code)
    result = psyir.walk(Routine)[0]
    assert result.children[0]
    new_loop = result.children[0]
    assert isinstance(new_loop, Loop)
    assert new_loop.variable.name == "i"
    assert new_loop.start_expr.value == "1"
    assert new_loop.stop_expr.value == "10"
    assert new_loop.step_expr.value == "2"
    assert len(new_loop.loop_body.children) == 1
    assert isinstance(new_loop.loop_body[0], Assignment)


def test_do_construct_while(fortran_reader):
    ''' Check that do while and general, uncoditioned loop constructs are
    converted to WhileLoop PSyIR nodes. '''
    code = '''
      subroutine test()
        integer :: a, b, c
        do while (a .gt. b)
          c = c + 1
        end do
        do
          c = c + 1
        end do
      end subroutine test
      '''
    psyir = fortran_reader.psyir_from_source(code)
    result = psyir.walk(Routine)[0]
    while_loop = result.children[0]
    assert isinstance(while_loop, WhileLoop)
    assert isinstance(while_loop.condition, BinaryOperation)
    assert len(while_loop.loop_body.children) == 1
    assert isinstance(while_loop.loop_body[0], Assignment)
    no_condition_loop = result.children[1]
    assert isinstance(no_condition_loop, WhileLoop)
    assert isinstance(no_condition_loop.condition, Literal)
    assert len(no_condition_loop.loop_body.children) == 1
    assert isinstance(no_condition_loop.loop_body[0], Assignment)
    assert no_condition_loop.annotations == ['was_unconditional']


def test_handled_named_do_without_exit(fortran_reader):
    ''' Check that a named DO results in a Loop if it does not contain
    any statements that refer to the construct-name (such as an EXIT). '''
    code = '''PROGRAM my_test
integer :: i
real, dimension(10) :: a
outer_do: DO i = 1, 10
  a(i) = 1.0
END DO outer_do
END PROGRAM my_test'''
    psyir = fortran_reader.psyir_from_source(code)
    prog = psyir.walk(Routine)[0]
    assert len(prog.children) == 1
    assert isinstance(prog.children[0], Loop)


def test_unhandled_named_do(fortran_reader):
    ''' Check that a named DO results in a CodeBlock when it contains a
    reference to the construct-name. '''
    code = '''PROGRAM my_test
integer :: i
real, dimension(10) :: a
outer_do: DO i = 1, 10
  a(i) = 1.0
  EXIT outer_do
END DO outer_do
END PROGRAM my_test'''
    psyir = fortran_reader.psyir_from_source(code)
    prog = psyir.walk(Routine)[0]
    assert len(prog.children) == 1
    assert isinstance(prog.children[0], CodeBlock)
    assert isinstance(prog.children[0].ast,
                      Fortran2003.Block_Nonlabel_Do_Construct)


def test_unhandled_labelled_do(fortran_reader):
    ''' Check that a labelled DO results in a CodeBlock. '''
    code = '''PROGRAM my_test
integer :: i
real, dimension(10) :: a
111 DO i = 1, 10
  a(i) = 1.0
END DO
GOTO 111
END PROGRAM my_test'''
    psyir = fortran_reader.psyir_from_source(code)
    prog = psyir.walk(Routine)[0]
    assert len(prog.children) == 1
    assert isinstance(prog.children[0], CodeBlock)
    assert isinstance(prog.children[0].ast,
                      Fortran2003.Block_Nonlabel_Do_Construct)


def test_undeclared_loop_var(fortran_reader):
    '''Check that the do handler defaults the loop var to an integer if
    the declaration of the loop variable can't be found. This may be
    because it is declared in another module or in some code that
    can't be parsed by the parser. One result of this is that a loop
    variable that really is not declared will be accepted by PSyclone
    and determined to be an integer.

    '''
    code = '''
      subroutine test()
        do i=1,10
        end do
      end subroutine test
    '''
    result = fortran_reader.psyir_from_source(code)
    i_var = result.children[0].symbol_table.lookup("i")
    assert isinstance(i_var, DataSymbol)
    assert isinstance(i_var.datatype, ScalarType)
    assert i_var.datatype.intrinsic is ScalarType.Intrinsic.INTEGER
    assert i_var.datatype.precision is ScalarType.Precision.UNDEFINED


def test_do_inside_while(fortran_reader, fortran_writer, tmpdir):
    '''Check that the loop handler correctly identifies a while loop
    containing a do loop as a separate while loop.'''
    code = '''subroutine test_subroutine
    integer :: j, iu_stdout, range_bands, i

    i = 0
    DO
      WRITE(iu_stdout, '(A)') &
        'Enter units followed by lower and upper limits and increment:'
      DO
        EXIT
      END DO
      range_bands = 3
      if (range_bands + i > 3 .and. range_bands + i < 15) then
        CYCLE
      end if
      do j = 1, range_bands
        i = i + 1
      end do
      if (i > 15) then
        EXIT
      end if
  end do

  end subroutine'''

    psyir = fortran_reader.psyir_from_source(code)
    loops = psyir.walk((WhileLoop, Loop))
    assert isinstance(loops[0], WhileLoop)
    assert isinstance(loops[1], WhileLoop)
    assert isinstance(loops[2], Loop)

    output = fortran_writer(psyir)
    correct = '''subroutine test_subroutine()
  integer :: j
  integer :: iu_stdout
  integer :: range_bands
  integer :: i

  i = 0
  do while (.true.)
    WRITE(iu_stdout, '(A)') 'Enter units followed by lower and upper \
limits and increment:'
    do while (.true.)
      EXIT
    end do
    range_bands = 3
    if (range_bands + i > 3 .AND. range_bands + i < 15) then
      CYCLE
    end if
    do j = 1, range_bands, 1
      i = i + 1
    enddo
    if (i > 15) then
      EXIT
    end if
  end do

end subroutine test_subroutine
'''
    assert correct == output
    assert Compile(tmpdir).string_compiles(output)


def test_do_concurrent(fortran_reader, fortran_writer, tmpdir):
    '''Check that the loop handler correctly identifies do concurrent loops '''

    # Single do concurrent loop
    code = '''
    subroutine test
      integer, parameter :: N =10, M=20
      integer, dimension(N,M) :: array
      integer i

      do concurrent(i=1:N)
        array(i,5) = 1
      end do
    end subroutine test
    '''
    psyir = fortran_reader.psyir_from_source(code)
    loops = psyir.walk(Loop)
    assert len(loops) == 1
    assert loops[0].variable.name == "i"
    assert loops[0].start_expr == Literal("1", INTEGER_TYPE)
    assert isinstance(loops[0].stop_expr, Reference)
    assert loops[0].stop_expr.symbol.name == "n"
    assert loops[0].step_expr == Literal("1", INTEGER_TYPE)

    # Do concurrent with nested loops and step values
    code = '''
    subroutine test
      integer, parameter :: N =10, M=20
      integer, dimension(N,M) :: array
      integer i, j

      do concurrent(i=1:N:3, j=1:M:2)
        array(i,j) = 1
      end do
    end subroutine test
    '''
    psyir = fortran_reader.psyir_from_source(code)
    loops = psyir.walk(Loop)
    assert len(loops) == 2
    output = fortran_writer(psyir)
    assert """
  do i = 1, n, 3
    do j = 1, m, 2
      array(i,j) = 1
    enddo
  enddo""" in output
    assert Compile(tmpdir).string_compiles(output)

    # Do concurrent mixed with regular do and dependencies between them
    code = '''
    subroutine test
      integer, parameter :: N =10, M=20
      integer, dimension(N,M,N,M) :: array
      integer i, j, k, f

      do concurrent(i=1:N:3, j=1:M:2)
        do k = 1, i
          do concurrent(f=1:i:k)
            array(i,j,k,f) = 1
          end do
        end do
      end do
    end subroutine test
    '''
    psyir = fortran_reader.psyir_from_source(code)
    loops = psyir.walk(Loop)
    assert len(loops) == 4
    output = fortran_writer(psyir)
    assert """
  do i = 1, n, 3
    do j = 1, m, 2
      do k = 1, i, 1
        do f = 1, i, k
          array(i,j,k,f) = 1
        enddo
      enddo
    enddo
  enddo""" in output
    assert Compile(tmpdir).string_compiles(output)


def test_unsupported_loop(parser):
    '''Test that loops with unsupported LoopCtrl are put in a CodeBlock'''
    code = ("program test\n"
            "do i = 1, 10\n"
            "  a(i) = i\n"
            "enddo\n"
            "end program test\n")
    reader = FortranStringReader(code)
    ast = parser(reader)
    ctrl = walk(ast, Fortran2003.Loop_Control)
    ctrl[0].items = (None, None, None, None)  # Make an unsupported Loop

    processor = Fparser2Reader()
    program = ast.children[0]
    psyir = processor._main_program_handler(program, None)
    assert not psyir.walk(Loop)
    assert psyir.walk(CodeBlock)
