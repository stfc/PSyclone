# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
from fparser.two import Fortran2003
from fparser.two.Fortran2003 import Execution_Part

from psyclone.errors import InternalError
from psyclone.psyir.frontend.fparser2 import (
    Fparser2Reader, _get_arg_names, _canonicalise_minmaxsum)
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


def _get_intrinsic_info(string):
    '''Utility function to take a Fortran string for a Fortran intrinsic
    call and return its fparser2 tree as well as its arguments as a
    list and its the names of its named arguments as a list.

    :param str string: Fortran instrinsic call to be processed.

    :returns: the fparser2 argument nodes as a list, the names of any \
        named arguments as a list and the fparser2 tree of the \
        intrinsic call.
    :rtype: Tuple[List[:py:class:`fparser.two.utils.Base`], \
            List[Union[str, None]], \
            :py:class:`fparser.two.Fortran2003.Intrinsic_Function_Reference`)

    '''
    reader = FortranStringReader(string)
    intrinsic = Fortran2003.Intrinsic_Function_Reference(reader)
    args = intrinsic.items[1].items
    arg_nodes, arg_names = _get_arg_names(args)
    return (arg_nodes, arg_names, intrinsic)


@pytest.mark.usefixtures("f2008_parser")
def test_canonicalise_minmaxsum():
    '''Check that the _canonicalise_minmaxsum function in fparser2.py
    works as expected.

    '''
    # All args named a) first arg first, b) first arg not first c)
    # array name not found.
    for string in [
            "sum(array=a, dim=d, mask=m)",
            "sum(dim=d, array=a, mask=m)",
            "sum(dim=d, mask=m, array=a)"]:
        arg_nodes, arg_names, intrinsic = _get_intrinsic_info(string)
        _canonicalise_minmaxsum(arg_nodes, arg_names, intrinsic)
        assert arg_names == [None, 'dim', 'mask']
        assert arg_nodes[0].string == "a"
        assert arg_nodes[1].string == "d"
        assert arg_nodes[2].string == "m"

    string = "sum(arg1=a, arg2=d, arg3=m)"
    arg_nodes, arg_names, intrinsic = _get_intrinsic_info(string)
    with pytest.raises(InternalError) as info:
        _canonicalise_minmaxsum(arg_nodes, arg_names, intrinsic)
    assert ("Invalid intrinsic arguments found. Expecting one of the named "
            "arguments to be 'array', but found 'SUM(arg1 = a, arg2 = d, "
            "arg3 = m)'." in str(info.value))

    # Two arguments and the second is not named. Returns
    # NotImplementedError which results in a CodeBlock as we need to
    # try to determine the datatypes to disambiguate and don't do that
    # yet.
    arg_nodes, arg_names, intrinsic = _get_intrinsic_info("sum(a, d)")
    with pytest.raises(NotImplementedError) as info:
        _canonicalise_minmaxsum(arg_nodes, arg_names, intrinsic)
    assert (str(info.value) ==
            "In 'SUM(a, d)' there are two arguments that are not named. "
            "The second could be a dim or a mask so we need datatype "
            "information to determine which and we do not determine "
            "this information at the moment.")

    # Optional arguments are not named but can be determined from
    # their order. Canonical form has them named. The last version
    # shows that an argument in canonical form remains unchanged. Any
    # upper case named-argument names become lower case.
    for string in [
            "sum(a, d, m)",
            "sum(a, d, mask=m)",
            "sum(a, d, MASK=m)"]:
        arg_nodes, arg_names, intrinsic = _get_intrinsic_info(string)
        _canonicalise_minmaxsum(arg_nodes, arg_names, intrinsic)
        assert arg_names == [None, 'dim', 'mask']
        assert arg_nodes[0].string == "a"
        assert arg_nodes[1].string == "d"
        assert arg_nodes[2].string == "m"

    # Already in canonical form so no change from input to output.
    for string in [
            "SUM(a)",
            "SUM(a, dim = d)",
            "SUM(a, dim = d, mask = m)",
            "SUM(a, mask = m)",
            "SUM(a, mask = m, dim = d)"]:
        arg_nodes, arg_names, intrinsic = _get_intrinsic_info(string)
        _canonicalise_minmaxsum(arg_nodes, arg_names, intrinsic)
        intrinsic._children = arg_nodes
        intrinsic.arg_names = arg_names
        assert str(intrinsic) == string


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
    assert result == f"{intrinsic_name}(a, dim=d, mask=m)"
    routine_symbol = intrinsic_call.routine

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
    routine_symbol = intrinsic_call.routine
    assert isinstance(routine_symbol, RoutineSymbol)
    assert intrinsic_call.routine.name == intrinsic_name
    assert isinstance(routine_symbol.interface, AutomaticInterface)
    # TODO: issue #2102, intrinsics are not currently added to the
    # symbol table "assert routine_symbol is \
    #     intrinsic_call.scope.symbol_table.lookup(intrinsic_name)"
    assert (str(intrinsic_call)) == f"IntrinsicCall[name='{intrinsic_name}']"


@pytest.mark.parametrize(
    "code, expected_intrinsic",
    [('x = exp(a)', IntrinsicCall.Intrinsic.EXP),
     ('x = sin(a)', IntrinsicCall.Intrinsic.SIN),
     ('x = asin(a)', IntrinsicCall.Intrinsic.ASIN),
     ('idx = ceiling(a)', IntrinsicCall.Intrinsic.CEILING),
     ('x = abs(a)', IntrinsicCall.Intrinsic.ABS),
     ('x = cos(a)', IntrinsicCall.Intrinsic.COS),
     ('x = acos(a)', IntrinsicCall.Intrinsic.ACOS),
     ('x = tan(a)', IntrinsicCall.Intrinsic.TAN),
     ('x = atan(a)', IntrinsicCall.Intrinsic.ATAN),
     ('x = real(a)', IntrinsicCall.Intrinsic.REAL),
     ('x = real(a, 8)', IntrinsicCall.Intrinsic.REAL),
     ('x = int(a)', IntrinsicCall.Intrinsic.INT),
     ('x = int(a, 8)', IntrinsicCall.Intrinsic.INT),
     ('x = log(a)', IntrinsicCall.Intrinsic.LOG),
     ('x = log10(a)', IntrinsicCall.Intrinsic.LOG10),
     ('x = mod(a, b)', IntrinsicCall.Intrinsic.MOD),
     ('x = matmul(a, b)', IntrinsicCall.Intrinsic.MATMUL),
     ('x = max(a, b)', IntrinsicCall.Intrinsic.MAX),
     ('x = mAx(a, b, c)', IntrinsicCall.Intrinsic.MAX),
     ('x = min(a, b)', IntrinsicCall.Intrinsic.MIN),
     ('x = min(a, b, c)', IntrinsicCall.Intrinsic.MIN),
     ('x = sign(a, b)', IntrinsicCall.Intrinsic.SIGN),
     ('x = sqrt(a)', IntrinsicCall.Intrinsic.SQRT),
     ('x = aimag(a)', IntrinsicCall.Intrinsic.AIMAG),
     ('x = dprod(a, b)', IntrinsicCall.Intrinsic.DPROD),
     ('x = reshape(a, b, c)', IntrinsicCall.Intrinsic.RESHAPE),
     ('x = sin(-3.0)', IntrinsicCall.Intrinsic.SIN)])
@pytest.mark.usefixtures("f2008_parser")
def test_handling_intrinsics(code, expected_intrinsic, symbol_table):
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
    assert assign.rhs._intrinsic == expected_intrinsic, \
        "Fails when parsing '" + code + "'"
    assert len(assign.rhs.children) == len(assign.rhs.argument_names)
    for named_arg in assign.rhs.argument_names:
        assert named_arg is None


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
      IntrinsicCall.Intrinsic.SIN, [None]),
     ('x = sin(array=a)',
      IntrinsicCall.Intrinsic.SIN, ["array"]),
     ('x = dot_product(a, b)',
      IntrinsicCall.Intrinsic.DOT_PRODUCT, [None, None]),
     ('x = dot_product(a, vector_b=b)',
      IntrinsicCall.Intrinsic.DOT_PRODUCT, [None, "vector_b"]),
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
    assert assign.rhs._intrinsic == expected_intrinsic, \
        "Fails when parsing '" + code + "'"
    assert len(assign.rhs.children) == len(assign.rhs._argument_names)
    for idx, child in enumerate(assign.rhs.children):
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
    assert len(assign.rhs.children) == 0


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
