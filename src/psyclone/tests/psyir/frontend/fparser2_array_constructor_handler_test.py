# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2026, Science and Technology Facilities Council.
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
# Authors: M. Naylor, University of Cambridge


'''Performs py.test tests on the support for array constructors in the fparser2
PSyIR front-end '''

import pytest
from psyclone.psyir.nodes import (
    ArrayConstructor, Reference, Literal, BinaryOperation, CodeBlock)
from psyclone.psyir.symbols import ScalarType, ArrayType, DataTypeSymbol


def test_handling_array_constructor_assignment(fortran_reader):
    '''Check that the fparser2 frontend can handle simple
    array constructors, without type specs and implicit do loops.
    '''
    code = """
program my_prog
  implicit none
  integer :: arr(3)
  integer :: x
  x = 10
  arr(:) = [1, x, x+2, [x]]
end program my_prog
"""
    prog = fortran_reader.psyir_from_source(code)
    ctrs = prog.walk(ArrayConstructor)

    # Program has two array constructors, one with 4 elements one with 1
    assert len(ctrs) == 2
    assert len(ctrs[0].children) == 4
    assert len(ctrs[1].children) == 1

    # Look at the outer array constructor first
    ctr = ctrs[0]

    # The first element is the literal 1
    assert isinstance(ctr.children[0], Literal)
    assert ctr.children[0].value == "1"

    # The second element is the reference "x"
    assert isinstance(ctr.children[1], Reference)
    assert ctr.children[1].name == "x"

    # The third element is the binary operation "x+2"
    assert isinstance(ctr.children[2], BinaryOperation)
    assert ctr.children[2].operator == BinaryOperation.Operator.ADD
    assert isinstance(ctr.children[2].operands[0], Reference)
    assert ctr.children[2].operands[0].name == "x"
    assert isinstance(ctr.children[2].operands[1], Literal)
    assert ctr.children[2].operands[1].value == "2"

    # The fourth element is a one-element ArrayConstructor holding "x"
    assert isinstance(ctr.children[3], ArrayConstructor)
    assert len(ctr.children[3].children) == 1
    assert isinstance(ctr.children[3].children[0], Reference)
    assert ctr.children[3].children[0].name == "x"


def test_handling_array_constructor_arg(fortran_reader):
    '''Check that the fparser2 frontend can handle array
    constructors in arguments.
    '''
    code = """
program my_prog
  implicit none
  integer :: arr(4), arr2(2,2)
  arr(:) = [0, 1, 2, 3]
  arr2(:,:) = reshape(arr, [2,2])
end program my_prog
"""
    prog = fortran_reader.psyir_from_source(code)
    ctrs = prog.walk(ArrayConstructor)

    # Program has two array constructors
    assert len(ctrs) == 2

    # The first has four elements (0, 1, 2, and 3)
    assert len(ctrs[0].children) == 4
    for i in range(0, 4):
        assert isinstance(ctrs[0].children[i], Literal)
        assert ctrs[0].children[i].value == str(i)

    # The second has two elements (both 2)
    assert len(ctrs[1].children) == 2
    for i in range(0, 2):
        assert isinstance(ctrs[1].children[i], Literal)
        assert ctrs[1].children[i].value == "2"


@pytest.mark.parametrize("arr_type", ["real", "integer"])
def test_handling_array_constructor_datatype(fortran_reader, arr_type):
    '''Check that the datatype of a parsed ArrayConstructor is correct.
    '''
    if arr_type == "real":
        one = "1.0"
    else:
        one = "1"
    code = f"""
program my_prog
  implicit none
  {arr_type}, allocatable :: arr(:)
  {arr_type} :: arr2(2, 2)
  {arr_type} :: x
  x = {one}
  arr(:) = [x]
  arr(:) = [x+{one}]
  arr(:) = [[{one}]]
  arr2(:,:) = {one}
  arr(:) = [arr2]
  arr(:) = [{one}, arr2]
  arr(:) = [{arr_type} ::]            ! Currently parsed to CodeBlock
  arr(:) = [{arr_type} :: {one}, x]   ! Currently parsed to CodeBlock
end program my_prog
"""
    prog = fortran_reader.psyir_from_source(code)

    # Check that all ArrayConstructors are rank-1 arrays of scalars
    count = 0
    for ctr in prog.walk(ArrayConstructor):
        dt = ctr.datatype
        assert isinstance(dt, ArrayType)
        assert len(dt.shape) == 1
        assert isinstance(dt.elemental_type, ScalarType)
        if arr_type == "real":
            assert dt.elemental_type.intrinsic == ScalarType.Intrinsic.REAL
        else:
            assert dt.elemental_type.intrinsic == ScalarType.Intrinsic.INTEGER
        count += 1

    # Check number of ArrayConstructors
    assert count == 6

    # Check number of CodeBlocks
    assert len(prog.walk(CodeBlock)) == 2


def test_handling_array_constructor_derived_type(fortran_reader):
    '''Check that the datatype of a parsed ArrayConstructor is correct
    in the case of a derived type.
    '''
    code = """
module my_mod
  type :: my_type
    integer :: val
  end type
contains
  subroutine sub()
    implicit none
    type(my_type), allocatable :: arr(:)
    type(my_type) :: arr2(2, 2)
    type(my_type) :: x
    x%val = 1
    arr(:) = [x]
    arr(:) = [[x]]
    arr2(:,:) = x
    arr(:) = [arr2]
    arr(:) = [x, arr2]
    arr(:) = [my_type ::]       ! Currently parsed to CodeBlock
    arr(:) = [my_type :: x, x]  ! Currently parsed to CodeBlock
  end subroutine
end module
"""
    prog = fortran_reader.psyir_from_source(code)

    # Check that all ArrayConstructors are rank-1 arrays of derived type
    count = 0
    for ctr in prog.walk(ArrayConstructor):
        dt = ctr.datatype
        assert isinstance(dt, ArrayType)
        assert len(dt.shape) == 1
        assert isinstance(dt.elemental_type, DataTypeSymbol)
        assert dt.elemental_type.name == "my_type"
        count += 1

    # Check number of ArrayConstructors
    assert count == 5

    # Check number of CodeBlocks
    assert len(prog.walk(CodeBlock)) == 2
