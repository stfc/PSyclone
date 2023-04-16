# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2022, Science and Technology Facilities Council
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
# Authors: R. W. Ford, S. Siso and N. Nobre, STFC Daresbury Lab

'''Module containing tests for the abs2code transformation.'''

from __future__ import absolute_import
import pytest
from psyclone.psyir.transformations import Abs2CodeTrans, TransformationError
from psyclone.psyir.symbols import SymbolTable, DataSymbol, \
    ArgumentInterface, REAL_TYPE
from psyclone.psyir.nodes import Reference, UnaryOperation, Assignment, \
    BinaryOperation, Literal, KernelSchedule
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.configuration import Config
from psyclone.tests.utilities import Compile


def test_initialise():
    '''Check that variables are set up as expected when an instance of the
    class is created and that the str and name methods work as expected.

    '''
    trans = Abs2CodeTrans()
    assert trans._operator_name == "ABS"
    assert trans._classes == (UnaryOperation,)
    assert trans._operators == (UnaryOperation.Operator.ABS,)
    assert (str(trans) == "Convert the PSyIR ABS intrinsic to equivalent "
            "PSyIR code.")
    assert trans.name == "Abs2CodeTrans"


def example_psyir(create_expression):
    '''Utility function that creates a PSyIR tree containing an ABS
    intrinsic operator and returns the operator.

    :param function create_expression: function used to create the \
        content of the ABS operator.

    :returns: PSyIR ABS operator instance.
    :rtype: :py:class:`psyclone.psyir.nodes.UnaryOperation`

    '''
    symbol_table = SymbolTable()
    arg1 = symbol_table.new_symbol(
        "arg", symbol_type=DataSymbol, datatype=REAL_TYPE,
        interface=ArgumentInterface(ArgumentInterface.Access.READWRITE))
    local = symbol_table.new_symbol(symbol_type=DataSymbol, datatype=REAL_TYPE)
    symbol_table.specify_argument_list([arg1])
    var1 = Reference(arg1)
    var2 = Reference(local)
    oper = UnaryOperation.Operator.ABS
    operation = UnaryOperation.create(oper, create_expression(var1))
    assign = Assignment.create(var2, operation)
    _ = KernelSchedule.create("abs_example", symbol_table, [assign])
    return operation


@pytest.mark.parametrize("func,output",
                         [(lambda arg: arg, "arg"),
                          (lambda arg: BinaryOperation.create(
                              BinaryOperation.Operator.MUL, arg,
                              Literal("3.14", REAL_TYPE)), "arg * 3.14")])
def test_correct(func, output, tmpdir):
    '''Check that a valid example produces the expected output when the
    argument to ABS is a simple argument and when it is an expression.

    '''
    Config.get().api = "nemo"
    operation = example_psyir(func)
    root = operation.root
    writer = FortranWriter()
    result = writer(root)
    assert (
        f"subroutine abs_example(arg)\n"
        f"  real, intent(inout) :: arg\n"
        f"  real :: psyir_tmp\n\n"
        f"  psyir_tmp = ABS({output})\n\n"
        f"end subroutine abs_example\n") in result
    trans = Abs2CodeTrans()
    trans.apply(operation, root.symbol_table)
    result = writer(root)
    assert (
        f"subroutine abs_example(arg)\n"
        f"  real, intent(inout) :: arg\n"
        f"  real :: psyir_tmp\n"
        f"  real :: res_abs\n"
        f"  real :: tmp_abs\n\n"
        f"  tmp_abs = {output}\n"
        f"  if (tmp_abs > 0.0) then\n"
        f"    res_abs = tmp_abs\n"
        f"  else\n"
        f"    res_abs = tmp_abs * -1.0\n"
        f"  end if\n"
        f"  psyir_tmp = res_abs\n\n"
        f"end subroutine abs_example\n") in result
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
            Literal("3.14", REAL_TYPE)))
    root = operation.root
    assignment = operation.parent
    operation.detach()
    op1 = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                 Literal("1.0", REAL_TYPE), operation)
    op2 = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                 op1, Literal("2.0", REAL_TYPE))
    assignment.addchild(op2)
    writer = FortranWriter()
    result = writer(root)
    assert (
        "subroutine abs_example(arg)\n"
        "  real, intent(inout) :: arg\n"
        "  real :: psyir_tmp\n\n"
        "  psyir_tmp = 1.0 + ABS(arg * 3.14) + 2.0\n\n"
        "end subroutine abs_example\n") in result
    trans = Abs2CodeTrans()
    trans.apply(operation, root.symbol_table)
    result = writer(root)
    assert (
        "subroutine abs_example(arg)\n"
        "  real, intent(inout) :: arg\n"
        "  real :: psyir_tmp\n"
        "  real :: res_abs\n"
        "  real :: tmp_abs\n\n"
        "  tmp_abs = arg * 3.14\n"
        "  if (tmp_abs > 0.0) then\n"
        "    res_abs = tmp_abs\n"
        "  else\n"
        "    res_abs = tmp_abs * -1.0\n"
        "  end if\n"
        "  psyir_tmp = 1.0 + res_abs + 2.0\n\n"
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
            Literal("3.14", REAL_TYPE)))
    root = operation.root
    assignment = operation.parent
    abs_op = UnaryOperation.create(UnaryOperation.Operator.ABS,
                                   Literal("1.0", REAL_TYPE))
    operation.detach()
    op1 = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                 operation, abs_op)
    assignment.addchild(op1)
    writer = FortranWriter()
    result = writer(root)
    assert (
        "subroutine abs_example(arg)\n"
        "  real, intent(inout) :: arg\n"
        "  real :: psyir_tmp\n\n"
        "  psyir_tmp = ABS(arg * 3.14) + ABS(1.0)\n\n"
        "end subroutine abs_example\n") in result
    trans = Abs2CodeTrans()
    trans.apply(operation, root.symbol_table)
    trans.apply(abs_op, root.symbol_table)
    result = writer(root)
    assert (
        "subroutine abs_example(arg)\n"
        "  real, intent(inout) :: arg\n"
        "  real :: psyir_tmp\n"
        "  real :: res_abs\n"
        "  real :: tmp_abs\n"
        "  real :: res_abs_1\n"
        "  real :: tmp_abs_1\n\n"
        "  tmp_abs = arg * 3.14\n"
        "  if (tmp_abs > 0.0) then\n"
        "    res_abs = tmp_abs\n"
        "  else\n"
        "    res_abs = tmp_abs * -1.0\n"
        "  end if\n"
        "  tmp_abs_1 = 1.0\n"
        "  if (tmp_abs_1 > 0.0) then\n"
        "    res_abs_1 = tmp_abs_1\n"
        "  else\n"
        "    res_abs_1 = tmp_abs_1 * -1.0\n"
        "  end if\n"
        "  psyir_tmp = res_abs + res_abs_1\n\n"
        "end subroutine abs_example\n") in result
    assert Compile(tmpdir).string_compiles(result)
    # Remove the created config instance
    Config._instance = None


def test_invalid():
    '''Check that the validate tests are run when the apply method is
    called.'''
    trans = Abs2CodeTrans()
    with pytest.raises(TransformationError) as excinfo:
        trans.apply(None)
    assert (
        "Error in Abs2CodeTrans transformation. The supplied node argument "
        "is not a ABS operator, found 'NoneType'." in str(excinfo.value))
