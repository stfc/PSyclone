# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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
from fparser.two.Fortran2003 import Execution_Part, Name

from psyclone.errors import InternalError
from psyclone.psyir.frontend.fparser2 import (
    Fparser2Reader, _get_arg_names, _canonicalise_minmaxsum)
from psyclone.psyir.nodes import (
    UnaryOperation, BinaryOperation, NaryOperation, Schedule, Assignment,
    Reference, IntrinsicCall, CodeBlock)
from psyclone.psyir.symbols import (
    REAL_TYPE, DataSymbol, UnknownFortranType, INTEGER_TYPE, SymbolTable,
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
    assert isinstance(routine_symbol.interface, LocalInterface)
    # TODO: issue #2102, intrinsics are not currently added to the
    # symbol table "assert routine_symbol is \
    #     intrinsic_call.scope.symbol_table.lookup(intrinsic_name)"
    assert (str(intrinsic_call)) == f"IntrinsicCall[name='{intrinsic_name}']"


@pytest.mark.parametrize(
    "code, expected_type, expected_op",
    [('x = exp(a)', UnaryOperation, UnaryOperation.Operator.EXP),
     ('x = sin(a)', UnaryOperation, UnaryOperation.Operator.SIN),
     ('x = asin(a)', UnaryOperation, UnaryOperation.Operator.ASIN),
     ('idx = ceiling(a)', UnaryOperation, UnaryOperation.Operator.CEIL),
     ('x = abs(a)', UnaryOperation, UnaryOperation.Operator.ABS),
     ('x = cos(a)', UnaryOperation, UnaryOperation.Operator.COS),
     ('x = acos(a)', UnaryOperation, UnaryOperation.Operator.ACOS),
     ('x = tan(a)', UnaryOperation, UnaryOperation.Operator.TAN),
     ('x = atan(a)', UnaryOperation, UnaryOperation.Operator.ATAN),
     ('x = real(a)', UnaryOperation, UnaryOperation.Operator.REAL),
     ('x = real(a, 8)', BinaryOperation, BinaryOperation.Operator.REAL),
     ('x = int(a)', UnaryOperation, UnaryOperation.Operator.INT),
     ('x = int(a, 8)', BinaryOperation, BinaryOperation.Operator.INT),
     ('x = log(a)', UnaryOperation, UnaryOperation.Operator.LOG),
     ('x = log10(a)', UnaryOperation, UnaryOperation.Operator.LOG10),
     ('x = mod(a, b)', BinaryOperation, BinaryOperation.Operator.REM),
     ('x = matmul(a, b)', BinaryOperation,
      BinaryOperation.Operator.MATMUL),
     ('x = max(a, b)', BinaryOperation, BinaryOperation.Operator.MAX),
     ('x = mAx(a, b, c)', NaryOperation, NaryOperation.Operator.MAX),
     ('x = min(a, b)', BinaryOperation, BinaryOperation.Operator.MIN),
     ('x = min(a, b, c)', NaryOperation, NaryOperation.Operator.MIN),
     ('x = sign(a, b)', BinaryOperation, BinaryOperation.Operator.SIGN),
     ('x = sqrt(a)', UnaryOperation, UnaryOperation.Operator.SQRT),
     # Check that we get a CodeBlock for an unsupported unary operation
     ('x = aimag(a)', CodeBlock, None),
     # Check that we get a CodeBlock for an unsupported binary operation
     ('x = dprod(a, b)', CodeBlock, None),
     # Check that we get a CodeBlock for an unsupported N-ary operation
     ('x = reshape(a, b, c)', CodeBlock, None),
     # Check when the argument list is not an Actual_Arg_Spec_List for
     # a unary operator
     ('x = sin(-3.0)', UnaryOperation, UnaryOperation.Operator.SIN)])
@pytest.mark.usefixtures("f2008_parser")
def test_handling_intrinsics(code, expected_type, expected_op, symbol_table):
    '''Test that the fparser2 _intrinsic_handler method deals with
    Intrinsic_Function_Reference nodes that are translated to PSyIR
    Operation nodes. Includes tests for unsupported intrinsics that
    are returned as codeblocks.

    '''
    processor = Fparser2Reader()
    fake_parent = Schedule(symbol_table=symbol_table)
    reader = FortranStringReader(code)
    fp2node = Execution_Part.match(reader)[0][0]
    print(type(fp2node.children[2]))
    processor.process_nodes(fake_parent, [fp2node])
    assign = fake_parent.children[0]
    assert isinstance(assign, Assignment)
    assert isinstance(assign.rhs, expected_type), \
        "Fails when parsing '" + code + "'"
    if expected_type is not CodeBlock:
        assert assign.rhs._operator == expected_op, \
            "Fails when parsing '" + code + "'"
        assert len(assign.rhs.children) == len(assign.rhs.argument_names)
        for named_arg in assign.rhs.argument_names:
            assert named_arg is None


@pytest.mark.parametrize(
    "code, expected_type, expected_op, expected_names",
    [('x = sin(a)', UnaryOperation,
      UnaryOperation.Operator.SIN, [None]),
     ('x = sin(array=a)', UnaryOperation,
      UnaryOperation.Operator.SIN, ["array"]),
     ('x = dot_product(a, b)', BinaryOperation,
      BinaryOperation.Operator.DOT_PRODUCT, [None, None]),
     ('x = dot_product(a, vector_b=b)', BinaryOperation,
      BinaryOperation.Operator.DOT_PRODUCT, [None, "vector_b"]),
     ('x = dot_product(vector_a=a, vector_b=b)', BinaryOperation,
      BinaryOperation.Operator.DOT_PRODUCT, ["vector_a", "vector_b"]),
     ('x = max(a, b, c)', NaryOperation,
      NaryOperation.Operator.MAX, [None, None, None]),
     ('x = max(a1=a, a2=b, a3=c)', NaryOperation,
      NaryOperation.Operator.MAX, ["a1", "a2", "a3"]),
     ('x = max(a, b, a3=c)', NaryOperation,
      NaryOperation.Operator.MAX, [None, None, "a3"])])
@pytest.mark.usefixtures("f2008_parser")
def test_handling_intrinsics_named_args(
        code, expected_type, expected_op, expected_names, symbol_table):
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
    assert isinstance(assign.rhs, expected_type), \
        "Fails when parsing '" + code + "'"
    assert assign.rhs._operator == expected_op, \
        "Fails when parsing '" + code + "'"
    assert len(assign.rhs.children) == len(assign.rhs._argument_names)
    for idx, child in enumerate(assign.rhs.children):
        assert (assign.rhs._argument_names[idx] ==
                (id(child), expected_names[idx]))
    assert assign.rhs.argument_names == expected_names


@pytest.mark.usefixtures("f2008_parser")
def test_intrinsic_no_args():
    ''' Check that an intrinsic with no arguments results in a
    NotImplementedError. '''
    processor = Fparser2Reader()
    fake_parent = Schedule()
    reader = FortranStringReader("x = MAX(a, b)")
    fp2node = Execution_Part.match(reader)[0][0].items[2]
    # Manually remove the arguments
    fp2node.items = (fp2node.items[0],)
    with pytest.raises(NotImplementedError) as err:
        processor._intrinsic_handler(fp2node, fake_parent)
    assert ("Operator 'MAX' has no arguments but operators must have at "
            "least one." in str(err.value))


@pytest.mark.usefixtures("f2008_parser")
def test_unary_op_handler_error():
    ''' Check that the unary op handler raises the expected error if the
    parse tree has an unexpected structure. This is a hard error to
    provoke since fparser checks that the number of arguments is correct. '''
    processor = Fparser2Reader()
    fake_parent = Schedule()
    reader = FortranStringReader("x = exp(a)")
    fp2node = Execution_Part.match(reader)[0][0].items[2]
    # Create an fparser node for a binary operation so that we can steal
    # its operands
    reader = FortranStringReader("x = max(a, b)")
    maxnode = Execution_Part.match(reader)[0][0].items[2]
    # Break the number of arguments in the fparser node by using those
    # from the binary operation
    fp2node.items = (fp2node.items[0], maxnode.items[1])
    with pytest.raises(InternalError) as err:
        processor._unary_op_handler(fp2node, fake_parent)
    assert ("Operation 'EXP(a, b)' has more than one argument and is "
            "therefore not unary" in str(err.value))


@pytest.mark.usefixtures("f2008_parser")
def test_binary_op_handler_error():
    ''' Check that the binary op handler raises the expected errors if the
    parse tree has an unexpected structure. '''
    processor = Fparser2Reader()
    fake_parent = Schedule()
    reader = FortranStringReader("x = MAX(a, b)")
    fp2node = Execution_Part.match(reader)[0][0].items[2]
    # Break the number of arguments in the fparser node
    fp2node.items[1].items = (Name('a'),)
    with pytest.raises(InternalError) as err:
        processor._binary_op_handler(fp2node, fake_parent)
    assert ("Binary operator should have exactly two arguments but found 1 "
            "for 'MAX(a)'." in str(err.value))
    # Now break the 'items' tuple of this fparser node
    fp2node.items = (fp2node.items[0], Name('dummy'))
    with pytest.raises(InternalError) as err:
        processor._binary_op_handler(fp2node, fake_parent)
    assert ("binary intrinsic operation 'MAX(dummy)'. Expected second child "
            "to be Actual_Arg_Spec_List" in str(err.value))


@pytest.mark.usefixtures("f2008_parser")
def test_nary_op_handler_error():
    ''' Check that the Nary op handler raises the expected error if the parse
    tree has an unexpected structure. '''
    processor = Fparser2Reader()
    fake_parent = Schedule()
    reader = FortranStringReader("x = MAX(a, b, c)")
    fp2node = Execution_Part.match(reader)[0][0].items[2]
    # Give the node an incorrect number of arguments for the Nary handler
    fp2node.items[1].items = (Name('a'),)
    with pytest.raises(InternalError) as err:
        processor._nary_op_handler(fp2node, fake_parent)
    assert ("An N-ary operation must have more than two arguments but found 1 "
            "for 'MAX(a)'" in str(err.value))
    # Break the 'items' tuple of this fparser node
    fp2node.items = (fp2node.items[0], Name('dummy'))
    with pytest.raises(InternalError) as err:
        processor._nary_op_handler(fp2node, fake_parent)
    assert ("Expected second 'item' of N-ary intrinsic 'MAX(dummy)' in fparser"
            " parse tree to be an Actual_Arg_Spec_List" in str(err.value))


@pytest.mark.usefixtures("f2008_parser")
def test_handling_nested_intrinsic():
    '''Check that we correctly handle nested intrinsic functions.'''
    processor = Fparser2Reader()
    fake_parent = Schedule()
    # Declare all the symbols needed by the test code.
    symtab = fake_parent.symbol_table
    symtab.add(DataSymbol("jk", INTEGER_TYPE))
    symtab.add(DataSymbol("wp", INTEGER_TYPE))
    symtab.add(DataSymbol("rcpi", REAL_TYPE))
    symtab.add(DataSymbol("rLfus", REAL_TYPE))
    symtab.add(DataSymbol("ze_z", REAL_TYPE))
    symtab.add(DataSymbol("zbbb", REAL_TYPE))
    symtab.add(DataSymbol("zccc", REAL_TYPE))
    symtab.add(DataSymbol("ztmelts", REAL_TYPE))
    symtab.add(DataSymbol("e1t", UnknownFortranType("blah :: e1t")))
    symtab.add(DataSymbol("e2t", UnknownFortranType("blah :: e2t")))
    symtab.add(DataSymbol("zav_tide", UnknownFortranType("blah :: zav_tide")))
    symtab.add(DataSymbol("tmask_i", UnknownFortranType("blah :: tmask_i")))
    symtab.add(DataSymbol("wmask", UnknownFortranType("blah :: wmask")))
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
