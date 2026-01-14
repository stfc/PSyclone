# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2026, Science and Technology Facilities Council.
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
# Author: R. W. Ford, STFC Daresbury Lab
# Modified: S. Siso, STFC Daresbury Lab


'''Performs pytest tests on the support for intrinsic statements that
    become IntrinsicCall's or Operations in the fparser2 PSyIR
    front-end via the _intrinsic_handler method.

'''
import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two.Fortran2003 import Execution_Part

from psyclone.psyir.frontend.fparser2 import (
    Fparser2Reader)
from psyclone.psyir.nodes import (
    Schedule, Assignment, Reference, IntrinsicCall, Literal, CodeBlock)
from psyclone.psyir.symbols import (
    REAL_TYPE, DataSymbol, UnsupportedFortranType, INTEGER_TYPE, SymbolTable,
    ArrayType, RoutineSymbol, AutomaticInterface)


@pytest.fixture(scope="function", name="symbol_table")
def make_symbol_table():
    '''
    pytest fixture to create and populate a symbol table for the
    'test_handling_intrinsics' test below.

    '''
    symbol_table = SymbolTable()
    symbol_table.new_symbol("x", symbol_type=DataSymbol,
                            datatype=REAL_TYPE)
    symbol_table.new_symbol("a", symbol_type=DataSymbol,
                            datatype=REAL_TYPE)
    symbol_table.new_symbol("b", symbol_type=DataSymbol,
                            datatype=REAL_TYPE)
    symbol_table.new_symbol("c", symbol_type=DataSymbol,
                            datatype=REAL_TYPE)
    symbol_table.new_symbol("idx", symbol_type=DataSymbol,
                            datatype=INTEGER_TYPE)
    symbol_table.new_symbol("mask", symbol_type=DataSymbol,
                            datatype=ArrayType(INTEGER_TYPE, [10]))
    return symbol_table


@pytest.mark.parametrize("arguments", ["a, dim=d, mask=m", "a, d, m"])
@pytest.mark.parametrize("intrinsic_name", ["MINVAL", "MAXVAL", "SUM"])
def test_intrinsic_handler_intrinsiccall_mms(
        fortran_reader, fortran_writer, intrinsic_name, arguments):
    '''Check that the FParser2Reader class _intrinsic_handler method in
    fparser2.py works as expected for the minval, maxval and sum
    intrinsics, which result in an IntrinsicCall. Test with and
    without canonicalisation being required to make sure the
    canonicalise_* function is called.

    _intrinsic_handler needs a symbol table to be set up so it is difficult
    to use a snippet of code. Therefore use a full Fortran example.

    '''
    code = f'''subroutine intrinsic_test(a, d, m, result)
  real,    intent(in)  :: a(:)
  real,    intent(out) ::result
  integer, intent(in)  :: d
  logical, intent(in)  :: m
  result = {intrinsic_name}({arguments})
end subroutine
'''
    psyir = fortran_reader.psyir_from_source(code)
    intrinsic_call = psyir.children[0].children[0].children[1]
    assert isinstance(intrinsic_call, IntrinsicCall)
    result = fortran_writer(intrinsic_call)
    assert result == f"{intrinsic_name}(a, d, mask=m)"
    routine_symbol = intrinsic_call.routine.symbol

    assert isinstance(routine_symbol, RoutineSymbol)
    assert intrinsic_call.routine.name == intrinsic_name
    assert isinstance(routine_symbol.interface, AutomaticInterface)
    # TODO: issue #2102, intrinsics are not currently added to the
    # symbol table "assert routine_symbol is \
    #     intrinsic_call.scope.symbol_table.lookup(intrinsic_name)"
    assert (str(intrinsic_call)) == f"IntrinsicCall[name='{intrinsic_name}']"


@pytest.mark.parametrize("intrinsic_name", ["TINY", "HUGE"])
def test_intrinsic_handler_intrinsiccall_onearg(
        fortran_reader, fortran_writer, intrinsic_name):
    '''Check that the FParser2Reader class _intrinsic_handler method in
    fparser2.py works as expected for intrinsics that result in an
    IntrinsicCall and have a single argument (currently tiny and
    huge).

    _intrinsic_handler needs a symbol table to be set up so it is difficult
    to use a snippet of code. Therefore use a full Fortran example.

    '''
    code = f'''subroutine intrinsic_test(a, result)
  real,    intent(in)  :: a(:)
  real,    intent(out) ::result
  result = {intrinsic_name}(a)
end subroutine
'''
    psyir = fortran_reader.psyir_from_source(code)
    intrinsic_call = psyir.children[0].children[0].children[1]
    assert isinstance(intrinsic_call, IntrinsicCall)
    result = fortran_writer(intrinsic_call)
    assert result == f"{intrinsic_name}(a)"
    routine_symbol = intrinsic_call.routine.symbol
    assert isinstance(routine_symbol, RoutineSymbol)
    assert intrinsic_call.routine.name == intrinsic_name
    assert isinstance(routine_symbol.interface, AutomaticInterface)
    # TODO: issue #2102, intrinsics are not currently added to the
    # symbol table "assert routine_symbol is \
    #     intrinsic_call.scope.symbol_table.lookup(intrinsic_name)"
    assert (str(intrinsic_call)) == f"IntrinsicCall[name='{intrinsic_name}']"


@pytest.mark.parametrize(
    "code, expected_intrinsic, arg_names",
    [('x = exp(a)', IntrinsicCall.Intrinsic.EXP,
      ["x"]),
     ('x = sin(a)', IntrinsicCall.Intrinsic.SIN,
      ["x"]),
     ('x = asin(a)', IntrinsicCall.Intrinsic.ASIN,
      ["x"]),
     ('idx = ceiling(a)', IntrinsicCall.Intrinsic.CEILING,
      ["a"]),
     ('x = abs(a)', IntrinsicCall.Intrinsic.ABS,
      ["a"]),
     ('x = cos(a)', IntrinsicCall.Intrinsic.COS,
      ["x"]),
     ('x = acos(a)', IntrinsicCall.Intrinsic.ACOS,
      ["x"]),
     ('x = tan(a)', IntrinsicCall.Intrinsic.TAN,
      ["x"]),
     ('x = atan(a)', IntrinsicCall.Intrinsic.ATAN,
      ["x"]),
     ('x = real(a)', IntrinsicCall.Intrinsic.REAL,
      ["a"]),
     ('x = real(a, 8)', IntrinsicCall.Intrinsic.REAL,
      ["a", "kind"]),
     ('x = int(a)', IntrinsicCall.Intrinsic.INT,
      ["a"]),
     ('x = int(a, 8)', IntrinsicCall.Intrinsic.INT,
      ["a", "kind"]),
     ('x = log(a)', IntrinsicCall.Intrinsic.LOG,
      ["x"]),
     ('x = log10(a)', IntrinsicCall.Intrinsic.LOG10,
      ["x"]),
     ('x = mod(a, b)', IntrinsicCall.Intrinsic.MOD,
      ["a", "p"]),
     ('x = matmul(a, b)', IntrinsicCall.Intrinsic.MATMUL,
      ["matrix_a", "matrix_b"]),
     ('x = max(a, b)', IntrinsicCall.Intrinsic.MAX,
      [None, None]),
     ('x = mAx(a, b, c)', IntrinsicCall.Intrinsic.MAX,
      [None, None, None]),
     ('x = min(a, b)', IntrinsicCall.Intrinsic.MIN,
      [None, None]),
     ('x = min(a, b, c)', IntrinsicCall.Intrinsic.MIN,
      [None, None, None]),
     ('x = sign(a, b)', IntrinsicCall.Intrinsic.SIGN,
      ["a", "b"]),
     ('x = sqrt(a)', IntrinsicCall.Intrinsic.SQRT,
      ["x"]),
     ('x = aimag(a)', IntrinsicCall.Intrinsic.AIMAG,
      ["z"]),
     ('x = dprod(a, b)', IntrinsicCall.Intrinsic.DPROD,
      ["x", "y"]),
     ('x = reshape(a, b, c)', IntrinsicCall.Intrinsic.RESHAPE,
      ["source", "shape", "pad"]),
     ('x = sin(-3.0)', IntrinsicCall.Intrinsic.SIN,
      ["x"])])
@pytest.mark.usefixtures("f2008_parser")
def test_handling_intrinsics(code, expected_intrinsic, arg_names,
                             symbol_table):
    '''Test that the fparser2 _intrinsic_handler method deals with
    Intrinsic_Function_Reference nodes that are translated to PSyIR
    IntrinsicCall nodes.

    '''
    processor = Fparser2Reader()
    fake_parent = Schedule(symbol_table=symbol_table)
    reader = FortranStringReader(code)
    fp2node = Execution_Part.match(reader)[0][0]
    processor.process_nodes(fake_parent, [fp2node])
    assign = fake_parent.children[0]
    assert isinstance(assign, Assignment)
    assert isinstance(assign.rhs, IntrinsicCall), \
        "Fails when parsing '" + code + "'"
    assert assign.rhs.routine.symbol.intrinsic == expected_intrinsic, \
        "Fails when parsing '" + code + "'"
    assert len(assign.rhs.arguments) == len(assign.rhs.argument_names)
    for i, named_arg in enumerate(assign.rhs.argument_names):
        assert named_arg == arg_names[i]


def test_handling_unsupported_intrinsics(symbol_table):
    '''Test that unsupported intrinsics are converted to codeblock.
    (Note that all Fortran 2018 intrinsics are supported but there
    are specific-type intrinsics and specific-compiler intrinsics
    that may not be supported). This are returned as CodeBlocks.
    '''
    processor = Fparser2Reader()
    fake_parent = Schedule(symbol_table=symbol_table)
    code = "x = sin(a)"
    reader = FortranStringReader(code)
    fp2node = Execution_Part.match(reader)[0][0]
    # Change the instrinsic string name in order to create a new
    # intrinsic which is not recognised by the PSyIR parser
    fp2node.children[2].items[0].string = "Unsupported"
    processor.process_nodes(fake_parent, [fp2node])
    assert not fake_parent.walk(IntrinsicCall)
    assert isinstance(fake_parent.children[0].rhs, CodeBlock)


@pytest.mark.parametrize(
    "code, expected_intrinsic, expected_names",
    [('x = sin(a)',
      IntrinsicCall.Intrinsic.SIN, ["x"]),
     ('x = sin(x=a)',
      IntrinsicCall.Intrinsic.SIN, ["x"]),
     ('x = dot_product(a, b)',
      IntrinsicCall.Intrinsic.DOT_PRODUCT, ["vector_a", "vector_b"]),
     ('x = dot_product(a, vector_b=b)',
      IntrinsicCall.Intrinsic.DOT_PRODUCT, ["vector_a", "vector_b"]),
     ('x = dot_product(vector_a=a, vector_b=b)',
      IntrinsicCall.Intrinsic.DOT_PRODUCT, ["vector_a", "vector_b"]),
     ('x = max(a, b, c)',
      IntrinsicCall.Intrinsic.MAX, [None, None, None]),
     ('x = max(a1=a, a2=b, a3=c)',
      IntrinsicCall.Intrinsic.MAX, ["a1", "a2", "a3"]),
     ('x = max(a, b, a3=c)',
      IntrinsicCall.Intrinsic.MAX, [None, None, "a3"])])
@pytest.mark.usefixtures("f2008_parser")
def test_handling_intrinsics_named_args(
        code, expected_intrinsic, expected_names, symbol_table):
    '''Test that the fparser2 _intrinsic_handler method deals with
    Intrinsic_Function_Reference nodes that are translated to PSyIR
    Operation nodes and have named arguments.

    '''
    processor = Fparser2Reader()
    fake_parent = Schedule(symbol_table=symbol_table)
    reader = FortranStringReader(code)
    fp2node = Execution_Part.match(reader)[0][0]
    processor.process_nodes(fake_parent, [fp2node])
    assign = fake_parent.children[0]
    assert isinstance(assign, Assignment)
    assert isinstance(assign.rhs, IntrinsicCall), \
        "Fails when parsing '" + code + "'"
    assert assign.rhs.routine.symbol.intrinsic == expected_intrinsic, \
        "Fails when parsing '" + code + "'"
    assert len(assign.rhs.arguments) == len(assign.rhs._argument_names)
    for idx, child in enumerate(assign.rhs.arguments):
        assert (assign.rhs._argument_names[idx] ==
                (id(child), expected_names[idx]))
    assert assign.rhs.argument_names == expected_names


@pytest.mark.usefixtures("f2008_parser")
def test_intrinsic_no_args(symbol_table):
    ''' Check that an intrinsic with no arguments is parsed correctly. '''
    processor = Fparser2Reader()
    fake_parent = Schedule(symbol_table=symbol_table)
    reader = FortranStringReader("x = NULL()")
    fp2node = Execution_Part.match(reader)[0][0]
    processor.process_nodes(fake_parent, [fp2node])
    assign = fake_parent.children[0]
    assert isinstance(assign.rhs, IntrinsicCall)
    assert assign.rhs.intrinsic == IntrinsicCall.Intrinsic.NULL
    assert len(assign.rhs.arguments) == 0


@pytest.mark.usefixtures("f2008_parser")
def test_handling_nested_intrinsic():
    '''Check that we correctly handle nested intrinsic functions.'''
    processor = Fparser2Reader()
    fake_parent = Schedule()
    # Declare all the symbols needed by the test code.
    symtab = fake_parent.symbol_table
    symtab.add(DataSymbol("jk", INTEGER_TYPE))
    symtab.add(DataSymbol("wp", INTEGER_TYPE,
                          is_constant=True,
                          initial_value=Literal("4", INTEGER_TYPE)))
    symtab.add(DataSymbol("rcpi", REAL_TYPE))
    symtab.add(DataSymbol("rLfus", REAL_TYPE))
    symtab.add(DataSymbol("ze_z", REAL_TYPE))
    symtab.add(DataSymbol("zbbb", REAL_TYPE))
    symtab.add(DataSymbol("zccc", REAL_TYPE))
    symtab.add(DataSymbol("ztmelts", REAL_TYPE))
    symtab.add(DataSymbol("e1t", UnsupportedFortranType("blah :: e1t")))
    symtab.add(DataSymbol("e2t", UnsupportedFortranType("blah :: e2t")))
    symtab.add(DataSymbol(
                   "zav_tide", UnsupportedFortranType("blah :: zav_tide")))
    symtab.add(DataSymbol(
                   "tmask_i", UnsupportedFortranType("blah :: tmask_i")))
    symtab.add(DataSymbol("wmask", UnsupportedFortranType("blah :: wmask")))
    reader = FortranStringReader(
        "ze_z = SUM( e1t(:,:) * e2t(:,:) * zav_tide(:,:,jk) * "
        "tmask_i(:,:) ) &\n"
        "   &  / MAX( 1.e-20, SUM( e1t(:,:) * e2t(:,:) * wmask (:,:,jk) * "
        "tmask_i(:,:) ) )")
    fp2node = Execution_Part.match(reader)[0][0]
    processor.process_nodes(fake_parent, [fp2node])
    array_refs = fake_parent.walk(Reference)
    assert "sum" not in [str(ref.name) for ref in array_refs]
    reader = FortranStringReader(
        "zccc = SQRT(MAX(zbbb * zbbb - 4._wp * rcpi * rLfus * ztmelts, 0.0))")
    fp2node = Execution_Part(reader)
    # Check that the frontend does not produce any CodeBlocks
    processor.process_nodes(fake_parent, fp2node.content)
    cblocks = fake_parent.children[1].walk(CodeBlock)
    assert not cblocks
