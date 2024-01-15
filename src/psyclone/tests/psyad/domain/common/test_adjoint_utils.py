# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''
A module to perform pytest tests on the utilities in the
psyad/domain/common/adjoint_utils.py file.

'''
import pytest

from psyclone.errors import InternalError
from psyclone.psyad.domain.common import (
    create_adjoint_name, find_container, create_real_comparison,
    common_real_comparison)
from psyclone.psyad.domain.common.adjoint_utils import _common_write
from psyclone.psyir.nodes import Container, FileContainer, Return, Routine
from psyclone.psyir.symbols import SymbolTable, DataSymbol, REAL_TYPE


# create_adjoint_name function

def test_create_adjoint_name():
    '''Test that the create_adjoint_name() function works as expected.

    '''
    assert create_adjoint_name("name") == "adj_name"
    assert create_adjoint_name("NAME") == "adj_name"
    assert create_adjoint_name("tl_name") == "adj_name"
    assert create_adjoint_name("Tl_NaMe") == "adj_name"

#  create_real_comparison


def test_create_real_comparison(fortran_writer):
    '''Test for the create_real_comparison method.'''
    symbol_table = SymbolTable()
    var1_symbol = symbol_table.new_symbol(
        "var1", symbol_type=DataSymbol, datatype=REAL_TYPE)
    var2_symbol = symbol_table.new_symbol(
        "var2", symbol_type=DataSymbol, datatype=REAL_TYPE)
    routine = Routine.create("test", symbol_table, [])
    stmt_list = create_real_comparison(
        symbol_table, routine, var1_symbol, var2_symbol)
    routine.children = stmt_list
    result = fortran_writer(routine)
    expected = (
        "  real, parameter :: overall_tolerance = 1500.0\n"
        "  real :: var1\n"
        "  real :: var2\n"
        "  real :: MachineTol\n"
        "  real :: relative_diff\n\n"
        "  ! Test the inner-product values for equality, allowing for the "
        "precision of the active variables\n"
        "  MachineTol = SPACING(MAX(ABS(var1), ABS(var2)))\n"
        "  relative_diff = ABS(var1 - var2) / MachineTol\n"
        "  if (relative_diff < overall_tolerance) then\n"
        "    WRITE(*, *) 'Test of adjoint of ''test'' PASSED: ', var1, "
        "var2, relative_diff\n"
        "  else\n"
        "    WRITE(*, *) 'Test of adjoint of ''test'' FAILED: ', var1, "
        "var2, relative_diff\n"
        "  end if\n")
    assert expected in result

#  common_real_comparison


def test_common_real_comparison(fortran_writer):
    '''Test for the common_real_comparison method.'''
    symbol_table = SymbolTable()
    var1_symbol = symbol_table.new_symbol(
        "var1", symbol_type=DataSymbol, datatype=REAL_TYPE)
    var2_symbol = symbol_table.new_symbol(
        "var2", symbol_type=DataSymbol, datatype=REAL_TYPE)
    routine = Routine.create("test", symbol_table, [])
    stmt_list = common_real_comparison(
        symbol_table, var1_symbol, var2_symbol)
    routine.children = stmt_list
    result = fortran_writer(routine)
    expected = (
        "  real, parameter :: overall_tolerance = 1500.0\n"
        "  real :: var1\n"
        "  real :: var2\n"
        "  real :: MachineTol\n"
        "  real :: relative_diff\n\n"
        "  ! Test the inner-product values for equality, allowing for the "
        "precision of the active variables\n"
        "  MachineTol = SPACING(MAX(ABS(var1), ABS(var2)))\n"
        "  relative_diff = ABS(var1 - var2) / MachineTol\n")
    assert expected in result

#  _common_write


def test_common_write(fortran_writer):
    '''Test for the _common_write method.'''
    symbol_table = SymbolTable()
    var1_symbol = symbol_table.new_symbol(
        "var1", symbol_type=DataSymbol, datatype=REAL_TYPE)
    var2_symbol = symbol_table.new_symbol(
        "var2", symbol_type=DataSymbol, datatype=REAL_TYPE)
    # The tags of the following symbols are used (in _common_write),
    # not the symbols themselves.
    _ = symbol_table.new_symbol(
        "relative_diff", symbol_type=DataSymbol,
        datatype=var1_symbol.datatype, tag="relative_diff")
    _ = symbol_table.new_symbol(
        "overall_tolerance", tag="overall_tolerance",
        symbol_type=DataSymbol, datatype=var1_symbol.datatype,
        is_constant=True, initial_value=1500.0)
    routine = Routine.create("test", symbol_table, [])
    stmt_list = _common_write(
        symbol_table, routine, var1_symbol, var2_symbol)
    routine.children = stmt_list
    result = fortran_writer(routine)
    expected = (
        "  real, parameter :: overall_tolerance = 1500.0\n"
        "  real :: var1\n"
        "  real :: var2\n"
        "  real :: relative_diff\n\n"
        "  if (relative_diff < overall_tolerance) then\n"
        "    WRITE(*, *) 'Test of adjoint of ''test'' PASSED: ', var1, "
        "var2, relative_diff\n"
        "  else\n"
        "    WRITE(*, *) 'Test of adjoint of ''test'' FAILED: ', var1, "
        "var2, relative_diff\n"
        "  end if\n")
    assert expected in result

#  find_container function


def test_find_container():
    ''' Tests for the helper function find_container(). '''
    assert find_container(Return()) is None
    assert find_container(FileContainer("test")) is None
    cont = Container("my_mod")
    assert find_container(cont) is cont
    cont.addchild(FileContainer("test"))
    with pytest.raises(InternalError) as err:
        find_container(cont)
    assert ("The supplied PSyIR contains two Containers but the innermost is "
            "a FileContainer. This should not be possible" in str(err.value))
    cont = Container("my_mod")
    cont.addchild(Container("another_mod"))
    with pytest.raises(NotImplementedError) as err:
        find_container(cont)
    assert ("supplied PSyIR contains two Containers and the outermost one is "
            "not a FileContainer. This is not supported." in str(err.value))
    file_cont = FileContainer("test")
    cont = Container("my_mod")
    file_cont.addchild(cont)
    assert find_container(file_cont) is cont
    file_cont.addchild(cont.copy())
    with pytest.raises(NotImplementedError) as err:
        find_container(file_cont)
    assert ("The supplied PSyIR contains more than two Containers. This is "
            "not supported." in str(err.value))
