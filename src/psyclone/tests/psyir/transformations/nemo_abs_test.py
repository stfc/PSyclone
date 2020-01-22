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

import pytest
from psyclone.psyir.transformations import NemoAbsTrans, TransformationError
from psyclone.psyir.symbols import SymbolTable, DataSymbol, DataType, \
    ArgumentInterface
from psyclone.psyir.nodes import Reference, UnaryOperation, Assignment
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyGen import KernelSchedule
from psyclone.configuration import Config


def test_initialise():
    '''Check that variables are set up as expected when an instance of the
    class is created and that the str and name methods work as expected.

    '''
    trans = NemoAbsTrans()
    # pylint: disable=protected-access
    assert trans._operator_name == "ABS"
    assert trans._classes == (UnaryOperation,)
    assert trans._operators == (UnaryOperation.Operator.ABS,)
    # pylint: enable=protected-access
    assert (str(trans) == "Convert the PSyIR ABS intrinsic to equivalent "
            "PSyIR code.")
    assert trans.name == "NemoAbsTrans"


def example_psyir():
    '''Utility function that creates a PSyIR tree containing an ABS
    intrinsic operator and returns the operator.

    :returns: PSyIR ABS operator instance.
    :rtype: :py:class:`psyclone.psyGen.UnaryOperation`

    '''
    symbol_table = SymbolTable()
    name1 = symbol_table.new_symbol_name("arg")
    arg1 = DataSymbol(name1, DataType.REAL, interface=ArgumentInterface(
        ArgumentInterface.Access.READWRITE))
    symbol_table.add(arg1)
    name2 = symbol_table.new_symbol_name()
    symbol_table.add(DataSymbol(name2, DataType.REAL))
    symbol_table.specify_argument_list([arg1])
    var1 = Reference(name1)
    var2 = Reference(name2)
    oper = UnaryOperation.Operator.ABS
    operation = UnaryOperation.create(oper, var1)
    assign = Assignment.create(var2, operation)
    _ = KernelSchedule.create("abs_example", symbol_table, [assign])
    return operation


def test_correct():
    '''Check that a valid example produces the expected output.'''

    Config.get().api = "nemo"
    operation = example_psyir()
    writer = FortranWriter()
    result = writer(operation.root)
    assert (
        "subroutine abs_example(arg)\n"
        "  real, intent(inout) :: arg\n"
        "  real :: psyir_tmp\n\n"
        "  psyir_tmp=ABS(arg)\n\n"
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
        "  tmp_abs=arg\n"
        "  if (tmp_abs > 0.0) then\n"
        "    res_abs=tmp_abs\n"
        "  else\n"
        "    res_abs=tmp_abs * -1.0\n"
        "  end if\n"
        "  psyir_tmp=res_abs\n\n"
        "end subroutine abs_example\n") in result
    # Remove the created config instance
    # pylint: disable=protected-access
    Config._instance = None
    # pylint: enable=protected-access


def test_invalid():
    '''Check that the validate tests are run when the apply method is
    called.'''
    Config.get().api = "dynamo0.3"
    operation = example_psyir()
    trans = NemoAbsTrans()
    with pytest.raises(TransformationError) as excinfo:
        _, _ = trans.apply(operation, operation.root.symbol_table)
    assert (
        "Error in NemoAbsTrans transformation. This transformation only works "
        "for the nemo API, but found 'dynamo0.3'" in str(excinfo.value))
    # Remove the created config instance
    # pylint: disable=protected-access
    Config._instance = None
    # pylint: enable=protected-access
