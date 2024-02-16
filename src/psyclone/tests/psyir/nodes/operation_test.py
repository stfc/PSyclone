# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

'''Performs pytest tests on the Operation PSyIR node and its
sub-classes.

'''

from enum import Enum
import pytest

from psyclone.errors import GenerationError, InternalError
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import (
    ArrayReference, BinaryOperation, colored, IntrinsicCall,
    Literal, Range, Reference, Return, StructureReference, UnaryOperation)
from psyclone.psyir.symbols import (
    ArrayType, BOOLEAN_TYPE, DataSymbol, UnresolvedType, INTEGER_SINGLE_TYPE,
    REAL_DOUBLE_TYPE, REAL_SINGLE_TYPE, ScalarType, Symbol, StructureType,
    UnsupportedFortranType)
from psyclone.tests.utilities import check_links


# Test BinaryOperation class
def test_binaryoperation_initialization():
    ''' Check the initialization method of the BinaryOperation class works
    as expected.'''

    with pytest.raises(TypeError) as err:
        _ = BinaryOperation("not an operator")
    assert "BinaryOperation operator argument must be of type " \
           "BinaryOperation.Operator but found" in str(err.value)
    bop = BinaryOperation(BinaryOperation.Operator.ADD)
    assert bop._operator is BinaryOperation.Operator.ADD


def test_binaryoperation_operator():
    '''Test that the operator property returns the binaryoperator in the
    binaryoperation.

    '''
    binary_operation = BinaryOperation(BinaryOperation.Operator.ADD)
    assert binary_operation.operator == BinaryOperation.Operator.ADD


def test_binaryoperation_node_str():
    ''' Check the node_str method of the Binary Operation class.'''
    binary_operation = BinaryOperation(BinaryOperation.Operator.ADD)
    op1 = Literal("1", INTEGER_SINGLE_TYPE)
    op2 = Literal("1", INTEGER_SINGLE_TYPE)
    binary_operation.addchild(op1)
    binary_operation.addchild(op2)
    coloredtext = colored("BinaryOperation", BinaryOperation._colour)
    assert coloredtext+"[operator:'ADD']" in binary_operation.node_str()


def test_binaryoperation_can_be_printed():
    '''Test that a Binary Operation instance can always be printed (i.e. is
    initialised fully)'''
    binary_operation = BinaryOperation(BinaryOperation.Operator.ADD)
    assert "BinaryOperation[operator:'ADD']" in str(binary_operation)
    op1 = Literal("1", INTEGER_SINGLE_TYPE)
    op2 = Literal("2", INTEGER_SINGLE_TYPE)
    binary_operation.addchild(op1)
    binary_operation.addchild(op2)
    # Check the node children are also printed
    assert ("Literal[value:'1', Scalar<INTEGER, SINGLE>]\n"
            in str(binary_operation))
    assert ("Literal[value:'2', Scalar<INTEGER, SINGLE>]"
            in str(binary_operation))


def test_binaryoperation_create():
    '''Test that the create method in the BinaryOperation class correctly
    creates a BinaryOperation instance.

    '''
    lhs = Reference(DataSymbol("tmp1", REAL_SINGLE_TYPE))
    rhs = Reference(DataSymbol("tmp2", REAL_SINGLE_TYPE))
    oper = BinaryOperation.Operator.ADD
    binaryoperation = BinaryOperation.create(oper, lhs, rhs)
    check_links(binaryoperation, [lhs, rhs])
    result = FortranWriter().binaryoperation_node(binaryoperation)
    assert result == "tmp1 + tmp2"


def test_binaryoperation_create_invalid():
    '''Test that the create method in a BinaryOperation class raises the
    expected exception if the provided input is invalid.

    '''
    ref1 = Reference(DataSymbol("tmp1", REAL_SINGLE_TYPE))
    ref2 = Reference(DataSymbol("tmp2", REAL_SINGLE_TYPE))
    add = BinaryOperation.Operator.ADD

    # oper not a BinaryOperation.Operator.
    with pytest.raises(GenerationError) as excinfo:
        _ = BinaryOperation.create("invalid", ref1, ref2)
    assert ("operator argument in create method of BinaryOperation class "
            "should be a PSyIR BinaryOperation Operator but found 'str'."
            in str(excinfo.value))

    # lhs not a Node.
    with pytest.raises(GenerationError) as excinfo:
        _ = BinaryOperation.create(add, "invalid", ref2)
    assert ("Item 'str' can't be child 0 of 'BinaryOperation'. The valid "
            "format is: 'DataNode, DataNode'.") in str(excinfo.value)

    # rhs not a Node.
    with pytest.raises(GenerationError) as excinfo:
        _ = BinaryOperation.create(add, ref1, "invalid")
    assert ("Item 'str' can't be child 1 of 'BinaryOperation'. The valid "
            "format is: 'DataNode, DataNode'.") in str(excinfo.value)


def test_binaryoperation_children_validation():
    '''Test that children added to BinaryOperation are validated.
    BinaryOperations accept 2 DataNodes as children.

    '''
    operation = BinaryOperation(BinaryOperation.Operator.ADD)
    literal1 = Literal("1", INTEGER_SINGLE_TYPE)
    literal2 = Literal("2", INTEGER_SINGLE_TYPE)
    literal3 = Literal("3", INTEGER_SINGLE_TYPE)
    statement = Return()

    # Statements are not valid
    with pytest.raises(GenerationError) as excinfo:
        operation.addchild(statement)
    assert ("Item 'Return' can't be child 0 of 'BinaryOperation'. The valid "
            "format is: 'DataNode, DataNode'.") in str(excinfo.value)

    # First DataNodes is valid, but not subsequent ones
    operation.addchild(literal1)
    operation.addchild(literal2)
    with pytest.raises(GenerationError) as excinfo:
        operation.addchild(literal3)
    assert ("Item 'Literal' can't be child 2 of 'BinaryOperation'. The valid "
            "format is: 'DataNode, DataNode'.") in str(excinfo.value)


def test_binaryop_scalar_datatype():
    '''Test the datatype property of BinaryOperation for scalar arguments.'''
    ref1 = Reference(DataSymbol("tmp1", REAL_SINGLE_TYPE))
    ref2 = Reference(DataSymbol("tmp2", REAL_SINGLE_TYPE))
    oper = BinaryOperation.Operator.ADD
    binaryoperation = BinaryOperation.create(oper, ref1, ref2)
    assert binaryoperation.datatype == REAL_SINGLE_TYPE
    iref1 = Reference(DataSymbol("itmp1", INTEGER_SINGLE_TYPE))
    binop2 = BinaryOperation.create(oper, ref1.copy(), iref1)
    assert binop2.datatype == REAL_SINGLE_TYPE
    iref2 = Reference(DataSymbol("itmp2", INTEGER_SINGLE_TYPE))
    binop3 = BinaryOperation.create(oper, iref1.copy(), iref2)
    assert binop3.datatype == INTEGER_SINGLE_TYPE
    # When any one of the arguments is of UnsupportedType then we know
    # nothing about the type of the result.
    uref1 = Reference(
        DataSymbol("trouble",
                   UnsupportedFortranType("real, volatile :: trouble")))
    binop4 = BinaryOperation.create(oper, iref1.copy(), uref1)
    assert isinstance(binop4.datatype, UnresolvedType)
    binop5 = BinaryOperation.create(BinaryOperation.Operator.EQ,
                                    iref1.copy(), iref2.copy())
    assert binop5.datatype == BOOLEAN_TYPE
    # Non-numeric type as operand of numerical operation.
    binop6 = BinaryOperation.create(oper, iref1.copy(),
                                    Reference(DataSymbol("switch",
                                                         BOOLEAN_TYPE)))
    with pytest.raises(TypeError) as err:
        _ = binop6.datatype
    assert ("Invalid argument of type 'Intrinsic.BOOLEAN' to numerical "
            "operation 'Operator.ADD' in 'itmp1 + switch'" in str(err.value))


def test_binaryop_get_result_precision(monkeypatch):
    '''Test the _get_result_precision method of BinaryOperation gives the
    correct precision.'''
    ref1 = Reference(DataSymbol("tmp1", REAL_SINGLE_TYPE))
    oper = BinaryOperation.Operator.ADD
    # Two arguments of different precision (SINGLE and DOUBLE)
    ref2 = Reference(DataSymbol("dtmp1", REAL_DOUBLE_TYPE))
    binop1 = BinaryOperation.create(oper, ref1, ref2)
    bprecn1 = binop1._get_result_precision([ref1.datatype.precision,
                                            ref2.datatype.precision])
    assert bprecn1 == ScalarType.Precision.DOUBLE
    # Two arguments with different (integer) precision
    ref3 = Reference(DataSymbol("tmp3",
                                ScalarType(ScalarType.Intrinsic.REAL, 4)))
    dref3 = Reference(DataSymbol("dtmp3",
                                 ScalarType(ScalarType.Intrinsic.REAL, 8)))
    binop3 = BinaryOperation.create(oper, ref3, dref3)
    bprecn3 = binop3._get_result_precision([ref3.datatype.precision,
                                            dref3.datatype.precision])
    assert bprecn3 == 8
    # A mixture of precisions
    binop4 = BinaryOperation.create(oper, ref1.copy(), ref3.copy())
    bprecn4 = binop4._get_result_precision([ref1.datatype.precision,
                                            ref3.datatype.precision])
    assert bprecn4 == ScalarType.Precision.UNDEFINED
    # One argument of undefined precision
    ref5 = Reference(DataSymbol("tmp5",
                                ScalarType(ScalarType.Intrinsic.REAL,
                                           ScalarType.Precision.UNDEFINED)))
    binop5 = BinaryOperation.create(oper, ref5, ref1.copy())
    bprecn5 = binop5._get_result_precision([ref5.datatype.precision,
                                            ref1.datatype.precision])
    assert bprecn5 == ScalarType.Precision.UNDEFINED

    # Exercise the check that we aren't missing a supported precision. We have
    # to monkeypatch the enum containing the supported precisions for this.
    class FakePrecision(Enum):
        '''Fake version of Precision enum for testing.'''
        SINGLE = 1
        DOUBLE = 2
        UNDEFINED = 3
        OTHER = 4

    monkeypatch.setattr(ScalarType, "Precision", FakePrecision)
    monkeypatch.setattr(ref1.datatype, "_precision",
                        ScalarType.Precision.SINGLE)
    # pylint: disable=no-member
    monkeypatch.setattr(ref5.datatype, "_precision",
                        ScalarType.Precision.OTHER)
    with pytest.raises(InternalError) as err:
        _ = binop5._get_result_precision([ref5.datatype.precision,
                                          ref1.datatype.precision])
    assert ("got unsupported Precision value(s) 'FakePrecision.OTHER' and "
            "'FakePrecision.SINGLE' for operands 'tmp5' and 'tmp1'"
            in str(err.value))


def test_binaryop_array_datatype():
    '''Test the datatype property of BinaryOperation for array arguments.'''
    arrtype = ArrayType(REAL_SINGLE_TYPE, [10])
    iarrtype = ArrayType(INTEGER_SINGLE_TYPE, [5])
    sym1 = DataSymbol("tmp1", arrtype)
    sym2 = DataSymbol("tmp2", arrtype)
    ref1 = Reference(sym1)
    ref2 = Reference(sym2)
    oper = BinaryOperation.Operator.ADD
    # Addition of two arrays.
    binaryoperation = BinaryOperation.create(oper, ref1, ref2)
    dtype = binaryoperation.datatype
    assert isinstance(dtype, ArrayType)
    assert dtype == arrtype
    # Add one element of an array to all elements of another array.
    aref1 = ArrayReference.create(sym1, [Literal("2", INTEGER_SINGLE_TYPE)])
    binop2 = BinaryOperation.create(oper, ref1.copy(), aref1)
    dtype2 = binop2.datatype
    assert isinstance(dtype2, ArrayType)
    assert dtype2 == arrtype
    # Add two elements of an array together.
    binop3 = BinaryOperation.create(oper, aref1.copy(), aref1.copy())
    dtype3 = binop3.datatype
    assert dtype3 == REAL_SINGLE_TYPE
    # Add a scalar integer to all elements of a real array.
    binop4 = BinaryOperation.create(oper, ref1.copy(),
                                    Literal("3", INTEGER_SINGLE_TYPE))
    assert binop4.datatype == arrtype
    # Add a real scalar to all elements of an integer array.
    ref4 = Reference(DataSymbol("tmp4", iarrtype))
    ref5 = Reference(DataSymbol("tmp5", REAL_SINGLE_TYPE))
    binop5 = BinaryOperation.create(oper, ref4, ref5)
    dtype5 = binop5.datatype
    assert isinstance(dtype5, ArrayType)
    assert len(dtype5.shape) == 1
    assert dtype5.shape[0].lower.value == "1"
    assert dtype5.shape[0].upper.value == "5"
    assert dtype5.intrinsic == ScalarType.Intrinsic.REAL
    # Non-conforming shapes.
    arr2dtype = ArrayType(REAL_SINGLE_TYPE, [10, 5])
    ref6 = Reference(DataSymbol("tmp2d", arr2dtype))
    binop6 = BinaryOperation.create(oper, ref1.copy(), ref6)
    with pytest.raises(InternalError) as err:
        _ = binop6.datatype
    assert ("Binary operation 'tmp1 + tmp2d' has operands of different shape: "
            "'tmp1' has rank 1 and 'tmp2d' has rank 2" in str(err.value))


def test_binaryop_array_section_datatype():
    '''Test the datatype property of BinaryOperation when the operands are
    array sections.'''
    arrtype = ArrayType(REAL_SINGLE_TYPE, [10])
    iarrtype = ArrayType(INTEGER_SINGLE_TYPE, [10, 5])
    ref1 = ArrayReference.create(
        DataSymbol("tmp1", arrtype),
        [Range.create(Literal("2", INTEGER_SINGLE_TYPE),
                      Literal("3", INTEGER_SINGLE_TYPE))])
    ref2 = ArrayReference.create(
        DataSymbol("tmp2", arrtype),
        [Range.create(Literal("9", INTEGER_SINGLE_TYPE),
                      Literal("10", INTEGER_SINGLE_TYPE))])
    oper = BinaryOperation.Operator.ADD
    # Addition of two array sections.
    binaryoperation = BinaryOperation.create(oper, ref1, ref2)
    dtype1 = binaryoperation.datatype
    assert len(dtype1.shape) == 1
    assert dtype1.shape[0].lower.value == "1"
    assert dtype1.shape[0].upper.debug_string() == "3 - 2 + 1"
    # Repeat but for a non-contiguous 1D section of a rank 2, integer array.
    ref3 = ArrayReference.create(
        DataSymbol("tmp3", iarrtype),
        [Literal("2", INTEGER_SINGLE_TYPE),
         Range.create(Literal("4", INTEGER_SINGLE_TYPE),
                      Literal("5", INTEGER_SINGLE_TYPE))])
    binop3 = BinaryOperation.create(oper, ref1.copy(), ref3.copy())
    dtype3 = binop3.datatype
    assert dtype3.intrinsic == REAL_SINGLE_TYPE.intrinsic
    assert len(dtype3.shape) == 1
    assert dtype3.shape[0].lower.value == "1"
    assert dtype3.shape[0].upper.debug_string() == "3 - 2 + 1"


def test_binaryop_datatype_recursion():
    '''Test the datatype property of BinaryOperation when the operands are
    themselves BinaryOperations.'''
    iarrtype = ArrayType(INTEGER_SINGLE_TYPE, [10, 5])
    ref1 = ArrayReference.create(
        DataSymbol("tmp1", iarrtype),
        [Range.create(Literal("2", INTEGER_SINGLE_TYPE),
                      Literal("3", INTEGER_SINGLE_TYPE)),
         Literal("3", INTEGER_SINGLE_TYPE)])
    ref2 = ArrayReference.create(
        DataSymbol("tmp2", iarrtype),
        [Literal("2", INTEGER_SINGLE_TYPE),
         Range.create(Literal("4", INTEGER_SINGLE_TYPE),
                      Literal("5", INTEGER_SINGLE_TYPE))])
    ref3 = Reference(DataSymbol("tmp3", REAL_SINGLE_TYPE))
    oper = BinaryOperation.Operator.SUB
    binop1 = BinaryOperation.create(oper, ref2, ref3)
    # Create tmp1(2:3) - (tmp2(4:5) - tmp3)
    binop2 = BinaryOperation.create(oper, ref1, binop1)
    dtype1 = binop2.datatype
    assert isinstance(dtype1, ArrayType)
    # Since tmp3 is real, the result should be real.
    assert dtype1.intrinsic == REAL_SINGLE_TYPE.intrinsic
    assert len(dtype1.shape) == 1
    assert dtype1.shape[0].lower.value == "1"
    assert dtype1.shape[0].upper.debug_string() == "3 - 2 + 1"


def test_binaryop_structure_datatype():
    '''
    Test the BinaryOperation datatype works when one or both arguments involve
    structure types.

    '''
    arrtype = ArrayType(REAL_SINGLE_TYPE, [10, 5])
    stype = StructureType.create([
        ("nx", INTEGER_SINGLE_TYPE, Symbol.Visibility.PUBLIC, None),
        ("data", arrtype, Symbol.Visibility.PUBLIC, None)])
    sym1 = DataSymbol("field", stype)
    ref1 = StructureReference.create(sym1, ["nx"])
    oper = BinaryOperation.Operator.SUB
    # field%nx - 2.0
    binop1 = BinaryOperation.create(oper,
                                    ref1, Literal("2.0", REAL_SINGLE_TYPE))
    dtype1 = binop1.datatype
    assert dtype1 == REAL_SINGLE_TYPE
    ref2 = StructureReference.create(sym1, ["data"])
    # field%nx - field%data
    binop2 = BinaryOperation.create(oper, ref1.copy(), ref2)
    dtype2 = binop2.datatype
    assert isinstance(dtype2, ArrayType)
    assert len(dtype2.shape) == 2
    assert dtype2.shape == arrtype.shape
    assert dtype2.intrinsic == REAL_SINGLE_TYPE.intrinsic


def test_binaryop_unresolved_datatype():
    '''
    Test that the BinaryOperation datatype always returns UnresolvedType if
    either (or both) operand(s) is of UnresolvedType.

    '''
    wind = Reference(DataSymbol("wind", UnresolvedType()))
    sea = Reference(DataSymbol("sea", INTEGER_SINGLE_TYPE))
    oper = BinaryOperation.Operator.ADD
    binop1 = BinaryOperation.create(oper, wind, sea)
    assert isinstance(binop1.datatype, UnresolvedType)
    binop2 = BinaryOperation.create(oper, sea.copy(), wind.copy())
    assert isinstance(binop2.datatype, UnresolvedType)
    binop3 = BinaryOperation.create(oper, wind.copy(), wind.copy())
    assert isinstance(binop3.datatype, UnresolvedType)
    # An Array of UnresolvedType.
    arrtype = ArrayType(UnresolvedType(), [10])
    air = Reference(DataSymbol("air", arrtype))
    binop4 = BinaryOperation.create(oper, air, sea.copy())
    assert isinstance(binop4.datatype, UnresolvedType)


def test_binaryop_partial_datatype():
    '''
    Test the BinaryOperation datatype works when one or both arguments only
    have partial type information.

    '''
    iarrtype = ArrayType(INTEGER_SINGLE_TYPE, [10, 5])
    utype = UnsupportedFortranType(
                "integer, dimension(10,5), pointer :: ref1",
                partial_datatype=iarrtype)
    ref1 = Reference(DataSymbol("ref1", utype))
    ref2 = Reference(DataSymbol("ref2", REAL_SINGLE_TYPE))
    oper = BinaryOperation.Operator.MUL
    binop1 = BinaryOperation.create(oper, ref1, ref2)
    dtype1 = binop1.datatype
    assert isinstance(dtype1, ArrayType)
    assert dtype1.intrinsic == REAL_SINGLE_TYPE.intrinsic
    # Create a second Symbol of UnsupportedFortranType.
    arrtype3 = ArrayType(REAL_SINGLE_TYPE, [10, 10])
    utype3 = UnsupportedFortranType(
                "real, dimension(10,10), pointer :: ref3",
                partial_datatype=arrtype3)
    # We are unable to determine the type of a Reference to a
    # subsection of an array of UnsupportedType (since that would require
    # updating the original UnsupportedType with a new shape).
    ref3 = ArrayReference.create(
        DataSymbol("ref3", utype3),
        [":", Range.create(Literal("5", INTEGER_SINGLE_TYPE),
                           Literal("10", INTEGER_SINGLE_TYPE))])
    # Create ref1(:,:) * ref3(:,5:10)
    binop3 = BinaryOperation.create(oper, ref1.copy(), ref3)
    dtype3 = binop3.datatype
    assert isinstance(dtype3, UnresolvedType)
    # However, if a subsection is not involved then we are OK.
    arrtype4 = ArrayType(REAL_SINGLE_TYPE, [10, 5])
    utype4 = UnsupportedFortranType(
                "real, dimension(10,5), pointer :: ref3",
                partial_datatype=arrtype4)
    ref4 = Reference(DataSymbol("ref4", utype4))
    binop4 = BinaryOperation.create(oper, ref1.copy(), ref4)
    dtype4 = binop4.datatype
    assert len(dtype4.shape) == 2
    assert dtype4.shape[0].lower.value == "1"
    assert dtype4.shape[0].upper.value == "10"
    assert dtype4.shape[1].lower.value == "1"
    assert dtype4.shape[1].upper.value == "5"
    assert dtype4.intrinsic == REAL_SINGLE_TYPE.intrinsic
    # A reference to an array of UnsupportedType but with partial type info.
    utype5 = UnsupportedFortranType("real, pointer :: ref5",
                                    partial_datatype=REAL_SINGLE_TYPE)
    arrtype5 = ArrayType(utype5, [8])
    ref5 = Reference(DataSymbol("ref5", arrtype5))
    binop5 = BinaryOperation.create(oper, ref2.copy(), ref5)
    dtype5 = binop5.datatype
    assert isinstance(dtype5, ArrayType)
    assert dtype5.intrinsic == REAL_SINGLE_TYPE.intrinsic
    assert len(dtype5.shape) == 1
    # Reference to an array of UnsupportedType without partial type information
    utype6 = UnsupportedFortranType("real, dimension(10,5), pointer :: ref6")
    arrtype6 = ArrayType(utype6, [10, 5])
    ref6 = Reference(DataSymbol("ref6", arrtype6))
    binop6 = BinaryOperation.create(oper, ref4.copy(), ref6)
    dtype6 = binop6.datatype
    assert isinstance(dtype6, UnresolvedType)


def test_binaryoperation_intrinsic_fn_datatype():
    '''
    Check that we can get the datatype of an operation involving the result
    of an intrinsic function.

    TODO #1799 - this just returns UnresolvedType at the minute and needs
    implementing.

    '''
    arrtype = ArrayType(REAL_SINGLE_TYPE, [10, 5])
    aref = Reference(DataSymbol("array", arrtype))
    arg1 = IntrinsicCall.create(IntrinsicCall.Intrinsic.MAXVAL, [aref])
    arg2 = Reference(DataSymbol("scalar", INTEGER_SINGLE_TYPE))
    oper = BinaryOperation.Operator.ADD
    binop = BinaryOperation.create(oper, arg1, arg2)
    assert isinstance(binop.datatype, UnresolvedType)


# Test UnaryOperation class
def test_unaryoperation_initialization():
    ''' Check the initialization method of the UnaryOperation class works
    as expected.'''

    with pytest.raises(TypeError) as err:
        _ = UnaryOperation("not an operator")
    assert "UnaryOperation operator argument must be of type " \
           "UnaryOperation.Operator but found" in str(err.value)
    uop = UnaryOperation(UnaryOperation.Operator.MINUS)
    assert uop._operator is UnaryOperation.Operator.MINUS


@pytest.mark.parametrize("operator_name", ['MINUS', 'PLUS', 'NOT'])
def test_unaryoperation_operator(operator_name):
    '''Test that the operator property returns the unaryoperator in the
    unaryoperation.

    '''
    operator = getattr(UnaryOperation.Operator, operator_name)
    unary_operation = UnaryOperation(operator)
    assert unary_operation.operator == operator


def test_unaryoperation_node_str():
    ''' Check the view method of the UnaryOperation class.'''
    ref1 = Reference(DataSymbol("a", REAL_SINGLE_TYPE))
    unary_operation = UnaryOperation.create(UnaryOperation.Operator.MINUS,
                                            ref1)
    coloredtext = colored("UnaryOperation", UnaryOperation._colour)
    assert coloredtext+"[operator:'MINUS']" in unary_operation.node_str()


def test_unaryoperation_can_be_printed():
    '''Test that a UnaryOperation instance can always be printed (i.e. is
    initialised fully)'''
    unary_operation = UnaryOperation(UnaryOperation.Operator.MINUS)
    assert "UnaryOperation[operator:'MINUS']" in str(unary_operation)
    op1 = Literal("1", INTEGER_SINGLE_TYPE)
    unary_operation.addchild(op1)
    # Check the node children are also printed
    assert ("Literal[value:'1', Scalar<INTEGER, SINGLE>]"
            in str(unary_operation))


def test_unaryoperation_create():
    '''Test that the create method in the UnaryOperation class correctly
    creates a UnaryOperation instance.

    '''
    child = Reference(DataSymbol("tmp", REAL_SINGLE_TYPE))
    oper = UnaryOperation.Operator.MINUS
    unaryoperation = UnaryOperation.create(oper, child)
    check_links(unaryoperation, [child])
    result = FortranWriter().unaryoperation_node(unaryoperation)
    assert result == "-tmp"


def test_unaryoperation_create_invalid4():
    '''Test that the create method in a UnaryOperation class raises the
    expected exception if the provided argument is a tuple with the
    wrong number of arguments.

    '''
    # oper not a UnaryOperator.Operator.
    with pytest.raises(GenerationError) as excinfo:
        _ = UnaryOperation.create(
            "invalid",
            Reference(DataSymbol("tmp", REAL_SINGLE_TYPE)))
    assert ("operator argument in create method of UnaryOperation class "
            "should be a PSyIR UnaryOperation Operator but found 'str'."
            in str(excinfo.value))


def test_unaryoperation_children_validation():
    '''Test that children added to unaryOperation are validated.
    UnaryOperations accept just 1 DataNode as child.

    '''
    operation = UnaryOperation(UnaryOperation.Operator.MINUS)
    literal1 = Literal("1", INTEGER_SINGLE_TYPE)
    literal2 = Literal("2", INTEGER_SINGLE_TYPE)
    statement = Return()

    # Statements are not valid
    with pytest.raises(GenerationError) as excinfo:
        operation.addchild(statement)
    assert ("Item 'Return' can't be child 0 of 'UnaryOperation'. The valid "
            "format is: 'DataNode'.") in str(excinfo.value)

    # First DataNodes is valid, but not subsequent ones
    operation.addchild(literal1)
    with pytest.raises(GenerationError) as excinfo:
        operation.addchild(literal2)
    assert ("Item 'Literal' can't be child 1 of 'UnaryOperation'. The valid "
            "format is: 'DataNode'.") in str(excinfo.value)


def test_unaryop_datatype():
    '''
    Test the datatype property of UnaryOperation.
    '''
    # Numerical, scalar operation
    oper = UnaryOperation.Operator.MINUS
    uop = UnaryOperation.create(oper, Literal("1", INTEGER_SINGLE_TYPE))
    assert uop.datatype == INTEGER_SINGLE_TYPE
    # Numerical, array operation
    ntype = ArrayType(REAL_DOUBLE_TYPE, [20])
    nsym = DataSymbol("var", ntype)
    uop1 = UnaryOperation.create(oper, Reference(nsym))
    dtype = uop1.datatype
    assert dtype == ntype
    # Logical operation
    oper = UnaryOperation.Operator.NOT
    uop2 = UnaryOperation.create(oper, Literal("true", BOOLEAN_TYPE))
    assert uop2.datatype == BOOLEAN_TYPE
    # With logical array argument
    atype = ArrayType(BOOLEAN_TYPE, [10, 10])
    asym = DataSymbol("mask", atype)
    uop3 = UnaryOperation.create(oper, Reference(asym))
    dtype = uop3.datatype
    assert isinstance(dtype, ArrayType)
    assert dtype == atype
    assert dtype.intrinsic == ScalarType.Intrinsic.BOOLEAN


def test_operations_can_be_copied():
    ''' Test that an operation can be copied. '''

    operands = [Reference(DataSymbol("tmp1", REAL_SINGLE_TYPE)),
                Reference(DataSymbol("tmp2", REAL_SINGLE_TYPE))]
    operation = BinaryOperation.create(BinaryOperation.Operator.ADD, *operands)

    operation1 = operation.copy()
    assert isinstance(operation1, BinaryOperation)
    assert operation1 is not operation
    assert operation1.operator is BinaryOperation.Operator.ADD
    assert operation1.children[0].symbol.name == "tmp1"
    assert operation1.children[0] is not operands[0]
    assert operation1.children[0].parent is operation1
    assert operation1.children[1].symbol.name == "tmp2"
    assert operation1.children[1] is not operands[1]
    assert operation1.children[1].parent is operation1
    assert len(operation1.children) == 2
    assert len(operation.children) == 2

    # Modifying the new operation does not affect the original
    operation1._operator = BinaryOperation.Operator.MUL
    operation1.children.pop()
    assert len(operation1.children) == 1
    assert len(operation.children) == 2
    assert operation1.operator is BinaryOperation.Operator.MUL
    assert operation.operator is BinaryOperation.Operator.ADD


def test_operation_equality():
    ''' Test the __eq__ method of Operation'''
    tmp1 = DataSymbol("tmp1", REAL_SINGLE_TYPE)
    tmp2 = DataSymbol("tmp2", REAL_SINGLE_TYPE)
    lhs = Reference(tmp1)
    rhs = Reference(tmp2)
    oper = BinaryOperation.Operator.ADD
    binaryoperation1 = BinaryOperation.create(oper, lhs, rhs)

    oper = BinaryOperation.Operator.ADD
    binaryoperation2 = BinaryOperation.create(oper, lhs.copy(), rhs.copy())

    assert binaryoperation1 == binaryoperation2

    # change the operator
    binaryoperation2._operator = BinaryOperation.Operator.SUB
    assert binaryoperation1 != binaryoperation2
