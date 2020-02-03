# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council
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

'''Module containing tests for the nemo abs transformation.'''

from __future__ import absolute_import
import pytest
from psyclone.psyir.transformations import NemoAbsTrans, TransformationError
from psyclone.psyir.symbols import SymbolTable, DataSymbol, DataType, \
    ArgumentInterface
from psyclone.psyir.nodes import Reference, UnaryOperation, Assignment, \
    BinaryOperation, Literal
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyGen import KernelSchedule
from psyclone.configuration import Config
from psyclone.tests.utilities import Compile


def test_initialise():
    '''Check that variables are set up as expected when an instance of the
    class is created and that the str and name methods work as expected.

    '''
    trans = NemoAbsTrans()
    assert trans._operator_name == "ABS"
    assert trans._classes == (UnaryOperation,)
    assert trans._operators == (UnaryOperation.Operator.ABS,)
    assert (str(trans) == "Convert the PSyIR ABS intrinsic to equivalent "
            "PSyIR code.")
    assert trans.name == "NemoAbsTrans"


def example_psyir(create_expression):
    '''Utility function that creates a PSyIR tree containing an ABS
    intrinsic operator and returns the operator.

    :param function create_expresssion: function used to create the \
        content of the ABS operator.

    :returns: PSyIR ABS operator instance.
    :rtype: :py:class:`psyclone.psyGen.UnaryOperation`

    '''
    symbol_table = SymbolTable()
    name1 = symbol_table.new_symbol_name("arg")
    arg1 = DataSymbol(name1, DataType.REAL, interface=ArgumentInterface(
        ArgumentInterface.Access.READWRITE))
    symbol_table.add(arg1)
    name2 = symbol_table.new_symbol_name()
    local = DataSymbol(name2, DataType.REAL)
    symbol_table.add(local)
    symbol_table.specify_argument_list([arg1])
    var1 = Reference(arg1)
    var2 = Reference(local)
    oper = UnaryOperation.Operator.ABS
    expression = create_expression(var1)
    operation = UnaryOperation.create(oper, expression)
    assign = Assignment.create(var2, operation)
    _ = KernelSchedule.create("abs_example", symbol_table, [assign])
    return operation


@pytest.mark.parametrize("func,output",
                         [(lambda arg: arg, "arg"),
                          (lambda arg: BinaryOperation.create(
                              BinaryOperation.Operator.MUL, arg,
                              Literal("3.14", DataType.REAL)), "arg * 3.14")])
def test_correct(func, output, tmpdir):
    '''Check that a valid example produces the expected output when the
    argument to ABS is a simple argument and when it is an
    expresssion.

    '''
    Config.get().api = "nemo"
    operation = example_psyir(func)
    writer = FortranWriter()
    result = writer(operation.root)
    assert (
        "subroutine abs_example(arg)\n"
        "  real, intent(inout) :: arg\n"
        "  real :: psyir_tmp\n\n"
        "  psyir_tmp=ABS({0})\n\n"
        "end subroutine abs_example\n".format(output)) in result
    trans = NemoAbsTrans()
    _, _ = trans.apply(operation, operation.root.symbol_table)
    result = writer(operation.root)
    assert (
        "subroutine abs_example(arg)\n"
        "  real, intent(inout) :: arg\n"
        "  real :: psyir_tmp\n"
        "  real :: res_abs\n"
        "  real :: tmp_abs\n\n"
        "  tmp_abs={0}\n"
        "  if (tmp_abs > 0.0) then\n"
        "    res_abs=tmp_abs\n"
        "  else\n"
        "    res_abs=tmp_abs * -1.0\n"
        "  end if\n"
        "  psyir_tmp=res_abs\n\n"
        "end subroutine abs_example\n".format(output)) in result
    assert Compile(tmpdir).string_compiles(result)
    # Remove the created config instance
    Config._instance = None


def test_correct_expr(tmpdir):
    '''Check that a valid example produces the expected output when ABS()
    is part of an expression.

    '''
    Config.get().api = "nemo"
    operation = example_psyir(
        lambda arg: BinaryOperation.create(
            BinaryOperation.Operator.MUL, arg,
            Literal("3.14", DataType.REAL)))
    assignment = operation.parent
    op1 = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                 Literal("1.0", DataType.REAL), operation)
    op2 = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                 op1, Literal("2.0", DataType.REAL))
    op2.parent = assignment
    assignment.children[1] = op2
    writer = FortranWriter()
    result = writer(operation.root)
    assert (
        "subroutine abs_example(arg)\n"
        "  real, intent(inout) :: arg\n"
        "  real :: psyir_tmp\n\n"
        "  psyir_tmp=1.0 + ABS(arg * 3.14) + 2.0\n\n"
        "end subroutine abs_example\n") in result
    trans = NemoAbsTrans()
    _, _ = trans.apply(operation, operation.root.symbol_table)
    result = writer(operation.root)
    assert (
        "subroutine abs_example(arg)\n"
        "  real, intent(inout) :: arg\n"
        "  real :: psyir_tmp\n"
        "  real :: res_abs\n"
        "  real :: tmp_abs\n\n"
        "  tmp_abs=arg * 3.14\n"
        "  if (tmp_abs > 0.0) then\n"
        "    res_abs=tmp_abs\n"
        "  else\n"
        "    res_abs=tmp_abs * -1.0\n"
        "  end if\n"
        "  psyir_tmp=1.0 + res_abs + 2.0\n\n"
        "end subroutine abs_example\n") in result
    assert Compile(tmpdir).string_compiles(result)
    # Remove the created config instance
    Config._instance = None


def test_correct_2abs(tmpdir):
    '''Check that a valid example produces the expected output when there
    is more than one ABS() in an expression.

    '''
    Config.get().api = "nemo"
    operation = example_psyir(
        lambda arg: BinaryOperation.create(
            BinaryOperation.Operator.MUL, arg,
            Literal("3.14", DataType.REAL)))
    assignment = operation.parent
    abs_op = UnaryOperation.create(UnaryOperation.Operator.ABS,
                                   Literal("1.0", DataType.REAL))
    op1 = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                 operation, abs_op)
    op1.parent = assignment
    assignment.children[1] = op1
    writer = FortranWriter()
    result = writer(operation.root)
    assert (
        "subroutine abs_example(arg)\n"
        "  real, intent(inout) :: arg\n"
        "  real :: psyir_tmp\n\n"
        "  psyir_tmp=ABS(arg * 3.14) + ABS(1.0)\n\n"
        "end subroutine abs_example\n") in result
    trans = NemoAbsTrans()
    _, _ = trans.apply(operation, operation.root.symbol_table)
    _, _ = trans.apply(abs_op, operation.root.symbol_table)
    result = writer(operation.root)
    assert (
        "subroutine abs_example(arg)\n"
        "  real, intent(inout) :: arg\n"
        "  real :: psyir_tmp\n"
        "  real :: res_abs\n"
        "  real :: tmp_abs\n"
        "  real :: res_abs_0\n"
        "  real :: tmp_abs_0\n\n"
        "  tmp_abs=arg * 3.14\n"
        "  if (tmp_abs > 0.0) then\n"
        "    res_abs=tmp_abs\n"
        "  else\n"
        "    res_abs=tmp_abs * -1.0\n"
        "  end if\n"
        "  tmp_abs_0=1.0\n"
        "  if (tmp_abs_0 > 0.0) then\n"
        "    res_abs_0=tmp_abs_0\n"
        "  else\n"
        "    res_abs_0=tmp_abs_0 * -1.0\n"
        "  end if\n"
        "  psyir_tmp=res_abs + res_abs_0\n\n"
        "end subroutine abs_example\n") in result
    assert Compile(tmpdir).string_compiles(result)
    # Remove the created config instance
    Config._instance = None


def test_invalid():
    '''Check that the validate tests are run when the apply method is
    called.'''
    Config.get().api = "dynamo0.3"
    operation = example_psyir(lambda arg: arg)
    trans = NemoAbsTrans()
    with pytest.raises(TransformationError) as excinfo:
        _, _ = trans.apply(operation, operation.root.symbol_table)
    assert (
        "Error in NemoAbsTrans transformation. This transformation only works "
        "for the nemo API, but found 'dynamo0.3'" in str(excinfo.value))
    # Remove the created config instance
    Config._instance = None
