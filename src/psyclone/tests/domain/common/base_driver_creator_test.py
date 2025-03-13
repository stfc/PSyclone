# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2025, Science and Technology Facilities Council.
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
# Author: J. Henrichs, Bureau of Meteorology
# Modified: S. Siso, STFC Daresbury Lab,
#           I. Kavcic, Met Office

''' This module tests the driver creation for extracted kernels.'''

import pytest

from psyclone.domain.common import BaseDriverCreator
from psyclone.psyir.nodes import Literal, Routine
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, RoutineSymbol


def test_basic_driver_add_call(fortran_writer):
    '''Tests that adding a call detects errors and adds calls
    with and without parameters as expected.
    '''
    program = Routine.create("routine", is_program=True)
    program.symbol_table.find_or_create_tag("test")
    with pytest.raises(TypeError) as err:
        BaseDriverCreator.add_call(program, "test", [])
    assert ("Error creating call to 'test' - existing symbol is of type "
            "'Symbol', not a 'RoutineSymbol'" in str(err.value))

    # Clean up previous invalid test symbol
    del program.symbol_table._symbols['test']
    del program.symbol_table._tags['test']

    BaseDriverCreator.add_call(program, "my_sub", [])
    BaseDriverCreator.add_call(program, "my_sub_2",
                               [Literal("1", INTEGER_TYPE)])
    out = fortran_writer(program)
    assert "call my_sub()" in out
    assert "call my_sub_2(1)" in out


def test_lfric_driver_add_result_tests(fortran_writer):
    '''Tests adding tests that compare results.
    '''
    program = Routine.create("routine", is_program=True)
    program.symbol_table.find_or_create_tag("test", symbol_type=RoutineSymbol)
    a1 = program.symbol_table.find_or_create(
        "a1", symbol_type=DataSymbol, datatype=INTEGER_TYPE)
    a1_orig = program.symbol_table.find_or_create(
        "a1_orig", symbol_type=DataSymbol, datatype=INTEGER_TYPE)
    # This will add one test for the variable a1 with the
    # correct values a1_orig.
    BaseDriverCreator.add_result_tests(program, [(a1, a1_orig)])
    out = fortran_writer(program)
    expected = """  call compare_init(1)
  call compare('a1', a1, a1_orig)
  call compare_summary()"""
    assert expected in out
