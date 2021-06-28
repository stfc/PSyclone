# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council.
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

''' Module containing pytest tests for the handling of the DO
construct in the PSyIR fparser2 frontend. '''

from __future__ import absolute_import

import pytest
from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from psyclone.psyir.nodes import Schedule, CodeBlock, Loop, Assignment
from psyclone.psyir.frontend.fparser2 import Fparser2Reader


def test_handling_end_do_stmt(parser):
    ''' Test that the fparser2 End_Do_Stmt is ignored.'''
    reader = FortranStringReader('''
      subroutine test()
        integer :: i, a
        do i=1,10
            a=a+1
        end do
      end subroutine test
        ''')
    fparser2_tree = parser(reader)
    processor = Fparser2Reader()
    result = processor.generate_schedule("test", fparser2_tree)
    assert len(result.children) == 1  # Just the loop (no end statement)


def test_do_construct(parser):
    ''' Check that do loop constructs are converted to the expected
    PSyIR node. '''
    reader = FortranStringReader('''
      subroutine test()
        integer :: i, sum
        do i = 1, 10 , 2
            sum = sum + i
        end do
      end subroutine test
      ''')
    fparser2_tree = parser(reader)
    processor = Fparser2Reader()
    result = processor.generate_schedule("test", fparser2_tree)
    assert result.children[0]
    new_loop = result.children[0]
    assert isinstance(new_loop, Loop)
    assert new_loop.variable.name == "i"
    assert new_loop.start_expr.value == "1"
    assert new_loop.stop_expr.value == "10"
    assert new_loop.step_expr.value == "2"
    assert len(new_loop.loop_body.children) == 1
    assert isinstance(new_loop.loop_body[0], Assignment)


@pytest.mark.usefixtures("parser")
def test_do_construct_while():
    ''' Check that do while constructs are placed in Codeblocks. '''
    reader = FortranStringReader('''
        do while(a .gt. b)\n
            c = c + 1\n
        end do\n
        ''')
    fparser2while = Fortran2003.Execution_Part.match(reader)[0][0]
    processor = Fparser2Reader()
    fake_parent = Schedule()
    processor.process_nodes(fake_parent, [fparser2while])
    assert isinstance(fake_parent.children[0], CodeBlock)


def test_unhandled_do(parser):
    ''' Test that a DO without any control logic results in a CodeBlock. '''
    lines = ["SUBROUTINE a_loop()",
             "  integer :: niter_atgen, jp_maxniter_atgen",
             "  real :: zh, zh_prev",
             "  DO",
             "    IF (niter_atgen >= jp_maxniter_atgen) THEN",
             "      zh = - 1._wp",
             "      EXIT",
             "    END IF",
             "    zh_prev = zh",
             "    niter_atgen = niter_atgen + 1",
             "  END DO",
             "END SUBROUTINE a_loop"]
    reader = FortranStringReader("\n".join(lines))
    fp2spec = parser(reader)
    processor = Fparser2Reader()
    sched = processor.generate_schedule("a_loop", fp2spec)
    assert isinstance(sched[0], CodeBlock)


def test_unhandled_named_do(fortran_reader):
    ''' Check that a named DO results in a CodeBlock. '''
    code = '''PROGRAM my_test
integer :: i
real, dimension(10) :: a
outer_do: DO i = 1, 10
  a(i) = 1.0
END DO outer_do
END PROGRAM my_test'''
    psyir = fortran_reader.psyir_from_source(code)
    assert psyir.walk(Loop) == []
    cblocks = psyir.walk(CodeBlock)
    assert len(cblocks) == 1
