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

'''Module containing tests for the nemo MIN transformation.'''

from psyclone.psyir.transformations import NemoMinTrans, TransformationError
from psyclone.psyir.symbols import SymbolTable, DataSymbol, DataType, \
    ArgumentInterface
from psyclone.psyGen import Reference, BinaryOperation, NaryOperation, \
    Assignment, KernelSchedule
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.configuration import Config
import pytest


def test_initialise():
    '''Check that variables are set up as expected when an instance of the
    class is created and that the str and name methods work as expected.

    '''
    trans = NemoMinTrans()
    assert trans._operator_name == "MIN"
    assert trans._classes == (BinaryOperation, NaryOperation)
    assert trans._operators == (BinaryOperation.Operator.MIN,
                                NaryOperation.Operator.MIN)


def test_str_name():
    '''Check that the str and name methods work as expected.'''
    
    trans = NemoMinTrans()
    assert (str(trans) == "Convert the PSyIR MIN intrinsic to equivalent "
            "PSyIR code.")
    assert trans.name == "NemoMinTrans"


def example_psyir_binary():
    '''Utility function that creates a PSyIR tree containing a binary MIN
    intrinsic operator and returns the operator.

    :returns: PSyIR MIN operator instance.
    :rtype: :py:class:`psyclone.psyGen.BinaryOperation`

    '''
    symbol_table = SymbolTable()
    name1 = symbol_table.new_symbol_name("arg")
    arg1 = DataSymbol(name1, DataType.REAL, interface=ArgumentInterface(
        ArgumentInterface.Access.READWRITE))
    symbol_table.add(arg1)
    name2 = symbol_table.new_symbol_name("arg")
    arg2 = DataSymbol(name2, DataType.REAL, interface=ArgumentInterface(
        ArgumentInterface.Access.READWRITE))
    symbol_table.add(arg2)
    name3 = symbol_table.new_symbol_name()
    symbol_table.add(DataSymbol(name3, DataType.REAL))
    symbol_table.specify_argument_list([arg1, arg2])
    var1 = Reference(name1)
    var2 = Reference(name2)
    var3 = Reference(name3)
    oper = BinaryOperation.Operator.MIN
    operation = BinaryOperation.create(oper, var1, var2)
    assign = Assignment.create(var3, operation)
    kernel_schedule = KernelSchedule.create("min_example", symbol_table, [assign])
    return operation


def example_psyir_nary():
    '''Utility function that creates a PSyIR tree containing a nary MIN
    intrinsic operator and returns the operator.

    :returns: PSyIR MIN operator instance.
    :rtype: :py:class:`psyclone.psyGen.NaryOperation`

    '''
    symbol_table = SymbolTable()
    name1 = symbol_table.new_symbol_name("arg")
    arg1 = DataSymbol(name1, DataType.REAL, interface=ArgumentInterface(
        ArgumentInterface.Access.READWRITE))
    symbol_table.add(arg1)
    name2 = symbol_table.new_symbol_name("arg")
    arg2 = DataSymbol(name2, DataType.REAL, interface=ArgumentInterface(
        ArgumentInterface.Access.READWRITE))
    symbol_table.add(arg2)
    name3 = symbol_table.new_symbol_name("arg")
    arg3 = DataSymbol(name3, DataType.REAL, interface=ArgumentInterface(
        ArgumentInterface.Access.READWRITE))
    symbol_table.add(arg3)
    name4 = symbol_table.new_symbol_name()
    symbol_table.add(DataSymbol(name4, DataType.REAL))
    symbol_table.specify_argument_list([arg1, arg2, arg3])
    var1 = Reference(name1)
    var2 = Reference(name2)
    var3 = Reference(name3)
    var4 = Reference(name4)
    oper = NaryOperation.Operator.MIN
    operation = NaryOperation.create(oper, [var1, var2, var3])
    assign = Assignment.create(var4, operation)
    kernel_schedule = KernelSchedule.create("min_example", symbol_table, [assign])
    return operation


def test_correct_binary():
    '''Check that a valid example with a binary MIN produces the expected
    output.

    '''
    Config.get().api = "nemo"
    operation = example_psyir_binary()
    writer = FortranWriter()
    result = writer(operation.root)
    assert (
        "subroutine min_example(arg,arg_0)\n"
        "  real, intent(inout) :: arg\n"
        "  real, intent(inout) :: arg_0\n"
        "  real :: psyir_tmp\n\n"
        "  psyir_tmp=MIN(arg, arg_0)\n\n"
        "end subroutine min_example\n") in result
    trans = NemoMinTrans()
    _, _ = trans.apply(operation, operation.root.symbol_table)
    result = writer(operation.root)
    assert (
        "subroutine min_example(arg,arg_0)\n"
        "  real, intent(inout) :: arg\n"
        "  real, intent(inout) :: arg_0\n"
        "  real :: psyir_tmp\n"
        "  real :: res_min\n"
        "  real :: tmp_min\n\n"
        "  res_min=arg\n"
        "  tmp_min=arg_0\n"
        "  if (tmp_min < res_min) then\n"
        "    res_min=tmp_min\n"
        "  end if\n"
        "  psyir_tmp=res_min\n\n"
        "end subroutine min_example\n") in result
    # Remove the created config instance
    Config._instance = None


def test_correct_nary():
    '''Check that a valid example with an nary MIN produces the expected
    output.

    '''
    Config.get().api = "nemo"
    operation = example_psyir_nary()
    writer = FortranWriter()
    result = writer(operation.root)
    assert (
        "subroutine min_example(arg,arg_0,arg_1)\n"
        "  real, intent(inout) :: arg\n"
        "  real, intent(inout) :: arg_0\n"
        "  real, intent(inout) :: arg_1\n"
        "  real :: psyir_tmp\n\n"
        "  psyir_tmp=MIN(arg, arg_0, arg_1)\n\n"
        "end subroutine min_example\n") in result
    trans = NemoMinTrans()
    _, _ = trans.apply(operation, operation.root.symbol_table)
    result = writer(operation.root)
    assert (
        "subroutine min_example(arg,arg_0,arg_1)\n"
        "  real, intent(inout) :: arg\n"
        "  real, intent(inout) :: arg_0\n"
        "  real, intent(inout) :: arg_1\n"
        "  real :: psyir_tmp\n"
        "  real :: res_min\n"
        "  real :: tmp_min\n"
        "  real :: tmp_min_0\n\n"
        "  res_min=arg\n"
        "  tmp_min=arg_0\n"
        "  if (tmp_min < res_min) then\n"
        "    res_min=tmp_min\n"
        "  end if\n"
        "  tmp_min_0=arg_1\n"
        "  if (tmp_min_0 < res_min) then\n"
        "    res_min=tmp_min_0\n"
        "  end if\n"
        "  psyir_tmp=res_min\n\n"
        "end subroutine min_example\n") in result
    # Remove the created config instance
    Config._instance = None


def test_invalid():
    '''Check that the validate tests are run when the apply method is
    called.'''
    Config.get().api = "dynamo0.3"
    operation = example_psyir_binary()
    trans = NemoMinTrans()
    with pytest.raises(TransformationError) as excinfo:
        _, _ = trans.apply(operation, operation.root.symbol_table)
    assert (
        "Error in NemoMinTrans transformation. This transformation only works "
        "for the nemo api, but found 'dynamo0.3'" in str(excinfo.value))
    # Remove the created config instance
    Config._instance = None

