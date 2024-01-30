# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council
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

'''Module containing tests for the MinOrMax2Code utility
transformation. This transformation is designed to be configured to
support either MIN or MAX transformations and should not be called
directly.

'''
import pytest

from psyclone.psyir.nodes import Reference, BinaryOperation, \
    Assignment, Literal, KernelSchedule, IntrinsicCall
from psyclone.psyir.symbols import SymbolTable, DataSymbol, \
    ArgumentInterface, REAL_TYPE
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.transformations.intrinsics.minormax2code_trans import \
    MinOrMax2CodeTrans
from psyclone.tests.utilities import Compile


def test_initialise():
    '''Check that variables are set up as expected when an instance of the
    class is created and that the str and name methods work as expected.

    '''
    trans = MinOrMax2CodeTrans()
    assert trans._compare_operator is None
    # from parent class
    assert trans._intrinsic is None


def example_psyir_binary(create_expression):
    '''Utility function that creates a PSyIR tree containing a MIN
    intrinsic and returns it.

    :param function create_expression: function used to create the \
        content of the first argument of the MIN intrinsic.

    :returns: PSyIR MIN intrinsic instance.
    :rtype: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

    '''
    symbol_table = SymbolTable()
    arg1 = symbol_table.new_symbol(
        "arg", symbol_type=DataSymbol, datatype=REAL_TYPE,
        interface=ArgumentInterface(ArgumentInterface.Access.READWRITE))
    arg2 = symbol_table.new_symbol(
        "arg", symbol_type=DataSymbol, datatype=REAL_TYPE,
        interface=ArgumentInterface(ArgumentInterface.Access.READWRITE))
    arg3 = symbol_table.new_symbol(symbol_type=DataSymbol, datatype=REAL_TYPE)
    symbol_table.specify_argument_list([arg1, arg2])
    var1 = Reference(arg1)
    var2 = Reference(arg2)
    var3 = Reference(arg3)
    intr = IntrinsicCall.Intrinsic.MIN
    intr_call = IntrinsicCall.create(intr, [create_expression(var1), var2])
    assign = Assignment.create(var3, intr_call)
    _ = KernelSchedule.create("min_example", symbol_table, [assign])
    return intr_call


def example_psyir_nary():
    '''Utility function that creates a PSyIR tree containing a MAX
    intrinsic and returns it.

    :returns: PSyIR MAX intrinsic instance.
    :rtype: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

    '''
    symbol_table = SymbolTable()
    arg1 = symbol_table.new_symbol(
        "arg", symbol_type=DataSymbol, datatype=REAL_TYPE,
        interface=ArgumentInterface(ArgumentInterface.Access.READWRITE))
    arg2 = symbol_table.new_symbol(
        "arg", symbol_type=DataSymbol, datatype=REAL_TYPE,
        interface=ArgumentInterface(ArgumentInterface.Access.READWRITE))
    arg3 = symbol_table.new_symbol(
        "arg", symbol_type=DataSymbol, datatype=REAL_TYPE,
        interface=ArgumentInterface(ArgumentInterface.Access.READWRITE))
    arg4 = symbol_table.new_symbol(symbol_type=DataSymbol, datatype=REAL_TYPE)
    symbol_table.specify_argument_list([arg1, arg2, arg3])
    var1 = Reference(arg1)
    var2 = Reference(arg2)
    var3 = Reference(arg3)
    var4 = Reference(arg4)
    intr = IntrinsicCall.Intrinsic.MAX
    intr_call = IntrinsicCall.create(intr, [var1, var2, var3])
    assign = Assignment.create(var4, intr_call)
    _ = KernelSchedule.create("max_example", symbol_table, [assign])
    return intr_call


@pytest.mark.parametrize("func,output",
                         [(lambda arg: arg, "arg"),
                          (lambda arg: BinaryOperation.create(
                              BinaryOperation.Operator.MUL, arg,
                              Literal("3.14", REAL_TYPE)), "arg * 3.14")])
def test_correct_binary(func, output, tmpdir, fortran_writer):
    '''Check that a valid example produces the expected output when the
    first argument to MIN is a simple argument and when it is an
    expression.

    '''
    intr_call = example_psyir_binary(func)
    root = intr_call.root
    result = fortran_writer(root)
    assert (
        f"subroutine min_example(arg, arg_1)\n"
        f"  real, intent(inout) :: arg\n"
        f"  real, intent(inout) :: arg_1\n"
        f"  real :: psyir_tmp\n\n"
        f"  psyir_tmp = MIN({output}, arg_1)\n\n"
        f"end subroutine min_example\n") in result
    trans = MinOrMax2CodeTrans()
    # Configure this transformation to use MIN
    trans._intrinsic = IntrinsicCall.Intrinsic.MIN
    trans._compare_operator = BinaryOperation.Operator.LT
    trans.apply(intr_call)
    result = fortran_writer(root)
    assert (
        f"subroutine min_example(arg, arg_1)\n"
        f"  real, intent(inout) :: arg\n"
        f"  real, intent(inout) :: arg_1\n"
        f"  real :: psyir_tmp\n"
        f"  real :: res_min\n"
        f"  real :: tmp_min\n\n"
        f"  res_min = {output}\n"
        f"  tmp_min = arg_1\n"
        f"  if (tmp_min < res_min) then\n"
        f"    res_min = tmp_min\n"
        f"  end if\n"
        f"  psyir_tmp = res_min\n\n"
        f"end subroutine min_example\n") in result
    assert Compile(tmpdir).string_compiles(result)


def test_correct_expr(tmpdir, fortran_writer):
    '''Check that a valid example produces the expected output when MIN()
    is part of an expression.

    '''
    intr_call = example_psyir_binary(lambda arg: arg)
    root = intr_call.root
    assignment = intr_call.parent
    intr_call.detach()
    op1 = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                 Literal("1.0", REAL_TYPE), intr_call)
    op2 = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                 op1, Literal("2.0", REAL_TYPE))
    assignment.addchild(op2)

    result = fortran_writer(root)
    assert (
        "subroutine min_example(arg, arg_1)\n"
        "  real, intent(inout) :: arg\n"
        "  real, intent(inout) :: arg_1\n"
        "  real :: psyir_tmp\n\n"
        "  psyir_tmp = 1.0 + MIN(arg, arg_1) + 2.0\n\n"
        "end subroutine min_example\n") in result
    trans = MinOrMax2CodeTrans()
    # Configure this transformation to use MIN
    trans._intrinsic = IntrinsicCall.Intrinsic.MIN
    trans._compare_operator = BinaryOperation.Operator.LT
    trans.apply(intr_call)
    result = fortran_writer(root)
    assert (
        "subroutine min_example(arg, arg_1)\n"
        "  real, intent(inout) :: arg\n"
        "  real, intent(inout) :: arg_1\n"
        "  real :: psyir_tmp\n"
        "  real :: res_min\n"
        "  real :: tmp_min\n\n"
        "  res_min = arg\n"
        "  tmp_min = arg_1\n"
        "  if (tmp_min < res_min) then\n"
        "    res_min = tmp_min\n"
        "  end if\n"
        "  psyir_tmp = 1.0 + res_min + 2.0\n\n"
        "end subroutine min_example\n") in result
    assert Compile(tmpdir).string_compiles(result)


def test_correct_2min(tmpdir, fortran_writer):
    '''Check that a valid example produces the expected output when there
    is more than one MIN() in an expression.

    '''
    intr_call = example_psyir_binary(lambda arg: arg)
    root = intr_call.root
    assignment = intr_call.parent
    intr_call.detach()
    intr_call2 = IntrinsicCall.create(IntrinsicCall.Intrinsic.MIN,
                                      [Literal("1.0", REAL_TYPE),
                                       Literal("2.0", REAL_TYPE)])
    op1 = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                 intr_call2, intr_call)
    assignment.addchild(op1)

    result = fortran_writer(root)
    assert (
        "subroutine min_example(arg, arg_1)\n"
        "  real, intent(inout) :: arg\n"
        "  real, intent(inout) :: arg_1\n"
        "  real :: psyir_tmp\n\n"
        "  psyir_tmp = MIN(1.0, 2.0) + MIN(arg, arg_1)\n\n"
        "end subroutine min_example\n") in result
    trans = MinOrMax2CodeTrans()
    # Configure this transformation to use MIN
    trans._intrinsic = IntrinsicCall.Intrinsic.MIN
    trans._compare_operator = BinaryOperation.Operator.LT
    trans.apply(intr_call)
    trans.apply(intr_call2)
    result = fortran_writer(root)
    assert (
        "subroutine min_example(arg, arg_1)\n"
        "  real, intent(inout) :: arg\n"
        "  real, intent(inout) :: arg_1\n"
        "  real :: psyir_tmp\n"
        "  real :: res_min\n"
        "  real :: tmp_min\n"
        "  real :: res_min_1\n"
        "  real :: tmp_min_1\n\n"
        "  res_min = arg\n"
        "  tmp_min = arg_1\n"
        "  if (tmp_min < res_min) then\n"
        "    res_min = tmp_min\n"
        "  end if\n"
        "  res_min_1 = 1.0\n"
        "  tmp_min_1 = 2.0\n"
        "  if (tmp_min_1 < res_min_1) then\n"
        "    res_min_1 = tmp_min_1\n"
        "  end if\n"
        "  psyir_tmp = res_min_1 + res_min\n\n"
        "end subroutine min_example\n") in result
    assert Compile(tmpdir).string_compiles(result)


def test_correct_nary(tmpdir, fortran_writer):
    '''Check that a valid example with an nary MAX produces the expected
    output.

    '''
    intr_call = example_psyir_nary()
    root = intr_call.root
    result = fortran_writer(root)
    assert (
        "subroutine max_example(arg, arg_1, arg_2)\n"
        "  real, intent(inout) :: arg\n"
        "  real, intent(inout) :: arg_1\n"
        "  real, intent(inout) :: arg_2\n"
        "  real :: psyir_tmp\n\n"
        "  psyir_tmp = MAX(arg, arg_1, arg_2)\n\n"
        "end subroutine max_example\n") in result
    trans = MinOrMax2CodeTrans()
    # Configure this transformation to use MAX
    trans._intrinsic = IntrinsicCall.Intrinsic.MAX
    trans._compare_operator = BinaryOperation.Operator.GT
    trans.apply(intr_call)
    result = fortran_writer(root)
    assert (
        "subroutine max_example(arg, arg_1, arg_2)\n"
        "  real, intent(inout) :: arg\n"
        "  real, intent(inout) :: arg_1\n"
        "  real, intent(inout) :: arg_2\n"
        "  real :: psyir_tmp\n"
        "  real :: res_max\n"
        "  real :: tmp_max\n\n"
        "  res_max = arg\n"
        "  tmp_max = arg_1\n"
        "  if (tmp_max > res_max) then\n"
        "    res_max = tmp_max\n"
        "  end if\n"
        "  tmp_max = arg_2\n"
        "  if (tmp_max > res_max) then\n"
        "    res_max = tmp_max\n"
        "  end if\n"
        "  psyir_tmp = res_max\n\n"
        "end subroutine max_example\n") in result
    assert Compile(tmpdir).string_compiles(result)


def test_invalid():
    '''Check that the validate tests are run when the apply method is
    called.'''
    trans = MinOrMax2CodeTrans()
    # Configure this transformation to use MAX
    trans._intrinsic = IntrinsicCall.Intrinsic.MAX
    trans._compare_operator = BinaryOperation.Operator.GT
    with pytest.raises(TransformationError) as excinfo:
        trans.apply(None)
    assert (
        "Error in MinOrMax2CodeTrans transformation. The supplied node must "
        "be an 'IntrinsicCall', but found 'NoneType'." in str(excinfo.value))
