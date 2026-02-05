# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2026, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
# Modified I. Kavcic, Met Office
# -----------------------------------------------------------------------------

'''
Performs py.test tests on the binary-operation handler of the fparser2
PSyIR front-end.

'''

import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from psyclone.psyir.frontend.fparser2 import (
    Fparser2Reader)
from psyclone.psyir.nodes import (
    Schedule, CodeBlock, Assignment, UnaryOperation, BinaryOperation,
    Reference, IntrinsicCall)


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_binaryopbase():
    ''' Test that fparser2 BinaryOpBase is converted to the expected PSyIR
    tree structure.
    '''
    reader = FortranStringReader("x=1+4")
    fp2binaryop = Fortran2003.Execution_Part.match(reader)[0][0]

    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fp2binaryop])
    # Check a new node was generated and connected to parent
    assert len(fake_parent.children) == 1
    new_node = fake_parent[0].rhs
    assert isinstance(new_node, BinaryOperation)
    assert len(new_node.children) == 2
    assert new_node._operator == BinaryOperation.Operator.ADD

    # Test parsing all supported arithmetic binary operators.
    testlist = (('+', BinaryOperation.Operator.ADD),
                ('-', BinaryOperation.Operator.SUB),
                ('*', BinaryOperation.Operator.MUL),
                ('/', BinaryOperation.Operator.DIV),
                ('**', BinaryOperation.Operator.POW),
                ('==', BinaryOperation.Operator.EQ),
                ('.eq.', BinaryOperation.Operator.EQ),
                ('.EQ.', BinaryOperation.Operator.EQ),
                ('/=', BinaryOperation.Operator.NE),
                ('.ne.', BinaryOperation.Operator.NE),
                ('>', BinaryOperation.Operator.GT),
                ('.GT.', BinaryOperation.Operator.GT),
                ('<', BinaryOperation.Operator.LT),
                ('.lt.', BinaryOperation.Operator.LT),
                ('>=', BinaryOperation.Operator.GE),
                ('.ge.', BinaryOperation.Operator.GE),
                ('<=', BinaryOperation.Operator.LE),
                ('.LE.', BinaryOperation.Operator.LE))

    for opstring, expected in testlist:
        # Manipulate the fparser2 ParseTree so that it contains the operator
        # under test
        reader = FortranStringReader("x=1" + opstring + "4")
        fp2binaryop = Fortran2003.Execution_Part.match(reader)[0][0]
        # And then translate it to PSyIR again.
        fake_parent = Schedule()
        processor.process_nodes(fake_parent, [fp2binaryop])
        assert len(fake_parent.children) == 1
        assert isinstance(fake_parent[0].rhs, BinaryOperation), \
            "Fails when parsing '" + opstring + "'"
        assert fake_parent[0].rhs._operator == expected, \
            "Fails when parsing '" + opstring + "'"

    # Test parsing all supported logical binary operators.
    testlist = (('.and.', BinaryOperation.Operator.AND),
                ('.eqv.', BinaryOperation.Operator.EQV),
                ('.neqv.', BinaryOperation.Operator.NEQV),
                ('.or.', BinaryOperation.Operator.OR))
    for opstring, expected in testlist:
        # Manipulate the fparser2 ParseTree so that it contains the operator
        # under test
        reader = FortranStringReader("x=a" + opstring + ".true.")
        fp2binaryop = Fortran2003.Execution_Part.match(reader)[0][0]
        # And then translate it to PSyIR again.
        fake_parent = Schedule()
        processor.process_nodes(fake_parent, [fp2binaryop])
        assert len(fake_parent.children) == 1
        assert isinstance(fake_parent[0].rhs, BinaryOperation), \
            "Fails when parsing '" + opstring + "'"
        assert fake_parent[0].rhs._operator == expected, \
            "Fails when parsing '" + opstring + "'"

    # Test that an unsupported binary operator creates a CodeBlock
    fake_parent = Schedule()
    fp2binaryop.items = (fp2binaryop.items[0], fp2binaryop.items[1],
                         (fp2binaryop.items[2].items[0], 'unsupported',
                          fp2binaryop.items[2].items[2]))
    processor.process_nodes(fake_parent, [fp2binaryop])
    assert len(fake_parent.children) == 1
    assert isinstance(fake_parent[0].rhs, CodeBlock)


def test_binaryopbase_simplification(fortran_reader):
    '''Test the simplification of binary operations involving +/- integer 0.'''
    code = """\
    subroutine a_sub()
      integer :: a_var
      a_var = a_var + 0
      a_var = a_var - 0
      a_var = 0 + a_var
      a_var = 0 - MAX(a_var, 1)
      a_var = a_var + 0.0
    end subroutine a_sub"""
    sched = fortran_reader.psyir_from_source(code)
    assigns = sched.walk(Assignment)
    # a_var + 0 => a_var
    assert isinstance(assigns[0].rhs, Reference)
    # a_var - 0 => a_var
    assert isinstance(assigns[1].rhs, Reference)
    # 0 + a_var => a_var
    assert isinstance(assigns[2].rhs, Reference)
    # 0 - MAX(...) => - MAX(...)
    assert isinstance(assigns[3].rhs, UnaryOperation)
    assert isinstance(assigns[3].rhs.operand, IntrinsicCall)
    # Addition of floating point number left unchanged.
    assert isinstance(assigns[4].rhs, BinaryOperation)
