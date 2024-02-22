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

'''Module containing tests for the sign2code transformation.'''

import pytest
from psyclone.psyir.transformations import Sign2CodeTrans, TransformationError
from psyclone.psyir.symbols import SymbolTable, DataSymbol, \
    ArgumentInterface, REAL_TYPE
from psyclone.psyir.nodes import Reference, BinaryOperation, Assignment, \
    Literal, KernelSchedule, IntrinsicCall
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import Compile


def test_initialise():
    '''Check that variables are set up as expected when an instance of the
    class is created and that the str and name methods work as expected.

    '''
    trans = Sign2CodeTrans()
    assert trans._intrinsic == IntrinsicCall.Intrinsic.SIGN
    assert (str(trans) == "Convert the PSyIR 'SIGN' intrinsic to equivalent "
            "PSyIR code.")
    assert trans.name == "Sign2CodeTrans"


def example_psyir(create_expression):
    '''Utility function that creates a PSyIR tree containing a SIGN
    intrinsic and returns it.

    :param function create_expression: function used to create the \
        content of the first argument of the SIGN intrinsic.

    :returns: PSyIR SIGN intrinsic instance.
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
    intr = IntrinsicCall.Intrinsic.SIGN
    intr_call = IntrinsicCall.create(intr, [create_expression(var1), var2])
    assign = Assignment.create(var3, intr_call)
    _ = KernelSchedule.create("sign_example", symbol_table, [assign])
    return intr_call


@pytest.mark.parametrize("func,output",
                         [(lambda arg: arg, "arg"),
                          (lambda arg: BinaryOperation.create(
                              BinaryOperation.Operator.MUL, arg,
                              Literal("3.14", REAL_TYPE)), "arg * 3.14")])
def test_correct(func, output, tmpdir):
    '''Check that a valid example produces the expected output when the
    first argument to SIGN is a simple argument and when it is an
    expression.

    '''
    intr_call = example_psyir(func)
    root = intr_call.root
    writer = FortranWriter()
    result = writer(root)
    assert (
        f"subroutine sign_example(arg, arg_1)\n"
        f"  real, intent(inout) :: arg\n"
        f"  real, intent(inout) :: arg_1\n"
        f"  real :: psyir_tmp\n\n"
        f"  psyir_tmp = SIGN({output}, arg_1)\n\n"
        f"end subroutine sign_example\n") in result
    trans = Sign2CodeTrans()
    trans.apply(intr_call, root.symbol_table)
    result = writer(root)
    assert (
        f"subroutine sign_example(arg, arg_1)\n"
        f"  real, intent(inout) :: arg\n"
        f"  real, intent(inout) :: arg_1\n"
        f"  real :: psyir_tmp\n"
        f"  real :: res_sign\n"
        f"  real :: tmp_sign\n"
        f"  real :: res_abs\n"
        f"  real :: tmp_abs\n\n"
        f"  tmp_abs = {output}\n"
        f"  if (tmp_abs > 0.0) then\n"
        f"    res_abs = tmp_abs\n"
        f"  else\n"
        f"    res_abs = tmp_abs * -1.0\n"
        f"  end if\n"
        f"  res_sign = res_abs\n"
        f"  tmp_sign = arg_1\n"
        f"  if (tmp_sign < 0.0) then\n"
        f"    res_sign = res_sign * -1.0\n"
        f"  end if\n"
        f"  psyir_tmp = res_sign\n\n"
        f"end subroutine sign_example\n") in result
    assert Compile(tmpdir).string_compiles(result)


def test_correct_expr(tmpdir):
    '''Check that a valid example produces the expected output when SIGN
    is part of an expression.

    '''
    intr_call = example_psyir(
        lambda arg: BinaryOperation.create(
            BinaryOperation.Operator.MUL, arg,
            Literal("3.14", REAL_TYPE)))
    root = intr_call.root
    assignment = intr_call.parent
    intr_call.detach()
    op1 = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                 Literal("1.0", REAL_TYPE), intr_call)
    op2 = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                 op1, Literal("2.0", REAL_TYPE))
    assignment.addchild(op2)
    writer = FortranWriter()
    result = writer(root)
    assert (
        "subroutine sign_example(arg, arg_1)\n"
        "  real, intent(inout) :: arg\n"
        "  real, intent(inout) :: arg_1\n"
        "  real :: psyir_tmp\n\n"
        "  psyir_tmp = 1.0 + SIGN(arg * 3.14, arg_1) + 2.0\n\n"
        "end subroutine sign_example\n") in result
    trans = Sign2CodeTrans()
    trans.apply(intr_call, root.symbol_table)
    result = writer(root)
    assert (
        "subroutine sign_example(arg, arg_1)\n"
        "  real, intent(inout) :: arg\n"
        "  real, intent(inout) :: arg_1\n"
        "  real :: psyir_tmp\n"
        "  real :: res_sign\n"
        "  real :: tmp_sign\n"
        "  real :: res_abs\n"
        "  real :: tmp_abs\n\n"
        "  tmp_abs = arg * 3.14\n"
        "  if (tmp_abs > 0.0) then\n"
        "    res_abs = tmp_abs\n"
        "  else\n"
        "    res_abs = tmp_abs * -1.0\n"
        "  end if\n"
        "  res_sign = res_abs\n"
        "  tmp_sign = arg_1\n"
        "  if (tmp_sign < 0.0) then\n"
        "    res_sign = res_sign * -1.0\n"
        "  end if\n"
        "  psyir_tmp = 1.0 + res_sign + 2.0\n\n"
        "end subroutine sign_example\n") in result
    assert Compile(tmpdir).string_compiles(result)


def test_correct_2sign(tmpdir):
    '''Check that a valid example produces the expected output when there
    is more than one SIGN in an expression.

    '''
    intr_call = example_psyir(
        lambda arg: BinaryOperation.create(
            BinaryOperation.Operator.MUL, arg,
            Literal("3.14", REAL_TYPE)))
    root = intr_call.root
    assignment = intr_call.parent
    intr_call.detach()
    intr_call2 = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.SIGN,
        [Literal("1.0", REAL_TYPE), Literal("1.0", REAL_TYPE)])
    op1 = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                 intr_call2, intr_call)
    assignment.addchild(op1)
    writer = FortranWriter()
    result = writer(root)
    assert (
        "subroutine sign_example(arg, arg_1)\n"
        "  real, intent(inout) :: arg\n"
        "  real, intent(inout) :: arg_1\n"
        "  real :: psyir_tmp\n\n"
        "  psyir_tmp = SIGN(1.0, 1.0) + SIGN(arg * 3.14, arg_1)\n\n"
        "end subroutine sign_example\n") in result
    trans = Sign2CodeTrans()
    trans.apply(intr_call, root.symbol_table)
    trans.apply(intr_call2, root.symbol_table)
    result = writer(root)
    assert (
        "subroutine sign_example(arg, arg_1)\n"
        "  real, intent(inout) :: arg\n"
        "  real, intent(inout) :: arg_1\n"
        "  real :: psyir_tmp\n"
        "  real :: res_sign\n"
        "  real :: tmp_sign\n"
        "  real :: res_abs\n"
        "  real :: tmp_abs\n"
        "  real :: res_sign_1\n"
        "  real :: tmp_sign_1\n"
        "  real :: res_abs_1\n"
        "  real :: tmp_abs_1\n\n"
        "  tmp_abs = arg * 3.14\n"
        "  if (tmp_abs > 0.0) then\n"
        "    res_abs = tmp_abs\n"
        "  else\n"
        "    res_abs = tmp_abs * -1.0\n"
        "  end if\n"
        "  res_sign = res_abs\n"
        "  tmp_sign = arg_1\n"
        "  if (tmp_sign < 0.0) then\n"
        "    res_sign = res_sign * -1.0\n"
        "  end if\n"
        "  tmp_abs_1 = 1.0\n"
        "  if (tmp_abs_1 > 0.0) then\n"
        "    res_abs_1 = tmp_abs_1\n"
        "  else\n"
        "    res_abs_1 = tmp_abs_1 * -1.0\n"
        "  end if\n"
        "  res_sign_1 = res_abs_1\n"
        "  tmp_sign_1 = 1.0\n"
        "  if (tmp_sign_1 < 0.0) then\n"
        "    res_sign_1 = res_sign_1 * -1.0\n"
        "  end if\n"
        "  psyir_tmp = res_sign_1 + res_sign\n\n"
        "end subroutine sign_example\n") in result
    assert Compile(tmpdir).string_compiles(result)


def test_invalid():
    '''Check that the validate tests are run when the apply method is
    called.'''
    trans = Sign2CodeTrans()
    with pytest.raises(TransformationError) as excinfo:
        trans.apply(None)
    assert (
        "Error in Sign2CodeTrans transformation. The supplied node must be "
        "an 'IntrinsicCall', but found 'NoneType'." in str(excinfo.value))
