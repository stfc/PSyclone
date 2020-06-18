import pytest
from psyclone.psyir.nodes import Literal, BinaryOperation, Reference, Range, Array, Assignment, Node, DataNode
from psyclone.psyGen import KernelSchedule
from psyclone.psyir.symbols import SymbolTable, DataSymbol, ArrayType, INTEGER_TYPE, REAL_TYPE
from psyclone.psyir.transformations import ArrayRange2LoopTrans
from psyclone.psyir.backend.fortran import FortranWriter

from psyclone.transformations import TransformationError, Transformation

# fparser2reader: is_bound_full_extent, is_range_full_extent
# fparser2reader: check_array_range_literal


def create_range(array_symbol, dim):
    ''' helper '''    
    int_dim = Literal(str(dim), INTEGER_TYPE)
    lbound = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND,
        Reference(array_symbol), int_dim)
    ubound = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND,
        Reference(array_symbol), int_dim)
    return Range.create(lbound, ubound)


def create_indexed_range(array_symbol, symbol_table):
    ''' helper '''    
    lbound = Literal("2", INTEGER_TYPE)
    ubound = Reference(symbol_table.lookup("n"))
    step = Literal("2", INTEGER_TYPE)
    return Range.create(lbound, ubound, step)


def create_literal(_):
    ''' helper '''
    return Literal("0.0", REAL_TYPE)


def create_array_x(symbol_table):
    ''' helper '''
    array_symbol = DataSymbol("x", ArrayType(REAL_TYPE, [10]))
    symbol_table.add(array_symbol)
    return Array.create(array_symbol, [create_range(array_symbol, 1)])


def create_array_y(symbol_table):
    ''' helper '''
    array_symbol = DataSymbol("y", ArrayType(REAL_TYPE, [10,10]))
    symbol_table.add(array_symbol)
    return Array.create(array_symbol, [Reference(symbol_table.lookup("n")),
                                       create_range(array_symbol, 2)])


def create_array_y_2(symbol_table):
    ''' helper '''
    array_symbol = DataSymbol("y", ArrayType(REAL_TYPE, [20,10]))
    symbol_table.add(array_symbol)
    return Array.create(array_symbol, [create_range(array_symbol, 1),
                                       create_range(array_symbol, 2)])


def create_array_z(symbol_table):
    ''' helper '''
    array_symbol = DataSymbol("z", ArrayType(REAL_TYPE, [20,10,10]))
    symbol_table.add(array_symbol)
    return Array.create(array_symbol, [create_range(array_symbol, 1),
                                       Reference(symbol_table.lookup("n")),
                                       create_range(array_symbol, 3)])


def create_array_y_3(symbol_table):
    ''' helper '''
    array_symbol = DataSymbol("y", ArrayType(REAL_TYPE, [10,10]))
    symbol_table.add(array_symbol)
    return Array.create(array_symbol, [Reference(symbol_table.lookup("n")),
                                       create_indexed_range(array_symbol, symbol_table)])

def create_expr(symbol_table):
    ''' xxx '''
    array_symbol = DataSymbol("x", ArrayType(REAL_TYPE, [10]))
    symbol_table.add(array_symbol)
    array_x = Array.create(array_symbol, [create_indexed_range(array_symbol, symbol_table)])
    array_symbol = DataSymbol("z", ArrayType(REAL_TYPE, [10,10]))
    symbol_table.add(array_symbol)
    array_z = Array.create(array_symbol, [Literal("1", INTEGER_TYPE),
                                          create_indexed_range(array_symbol, symbol_table)])
    array_symbol = DataSymbol("a", ArrayType(REAL_TYPE, [10]))
    array_a = Array.create(array_symbol, [Literal("1", INTEGER_TYPE)])
    symbol_table.add(array_symbol)
    mult = BinaryOperation.create(BinaryOperation.Operator.MUL, array_x, array_z)
    add = BinaryOperation.create(BinaryOperation.Operator.ADD, mult, array_a)
    return add


def test_transform():
    '''Check that it is possible to create an instance of
    ArrayRange2LoopTrans and that it is a Transformation.

    '''
    assert ArrayRange2LoopTrans()
    assert isinstance(ArrayRange2LoopTrans(), Transformation)


@pytest.mark.parametrize("lhs_create,rhs_create,result",
                         [(create_array_x, create_literal,
                           "  do idx = 1, 10, 1\n"
                           "    x(idx)=0.0\n"),
                          (create_array_x, create_array_y,
                           "  do idx = 1, 10, 1\n"
                           "    x(idx)=y(n,idx)\n"),
                          (create_array_y, create_array_x,
                           "  do idx = 1, 10, 1\n"
                           "    y(n,idx)=x(idx)\n"),
                          (create_array_y_2, create_array_z,
                           "  do idx = 1, 20, 1\n"
                           "    y(idx,:)=z(idx,n,:)\n"),
                          (create_array_y_3, create_expr,
                           "  do idx = 2, n, 2\n"
                           "    y(n,idx)=x(idx) * z(1,idx) + a(1)")])
def test_transform_apply(lhs_create, rhs_create, result):
    '''Check that the PSyIR is transformed as expected for various types
    of ranges in an array. The resultant Fortran code is used to
    confirm the transformation has worked correctly.

    The parametrised tests are 1: x(:)=0.0, 2: x(:)=y(n,:), 3:
    y(n,:)=x(:), 4: y(:,:)=z(:,n,:) and 5:
    y(n,2:n:2)=x(2:n:2)*z(1,2:n:2)+a(1)

    '''
    trans = ArrayRange2LoopTrans()
    symbol_table = SymbolTable()
    symbol = DataSymbol("n", INTEGER_TYPE)
    symbol_table.add(symbol)
    lhs = lhs_create(symbol_table)
    rhs = rhs_create(symbol_table)
    assignment = Assignment.create(lhs, rhs)
    routine = KernelSchedule.create("work", symbol_table,
                                    [assignment])
    trans.apply(assignment)
    writer = FortranWriter()
    print (writer(routine))
    assert result in writer(routine)

# test_transform_apply2 - check that we call self.validate with an example


def test_str():
    '''Test that the str of an instance of the ArrayRange2LoopTrans class
    returns the expected value.

    '''
    assert (str(ArrayRange2LoopTrans()) == "Convert a PSyIR Array Range to "
            "a PSyIR Loop.")


def test_name():
    '''Check that the name property of the ArrayRange2LoopTrans class
    returns the expected value.

    '''
    assert ArrayRange2LoopTrans().name == "ArrayRange2LoopTrans"


def test_validate():
    '''Test that the validate method in the ArrayRange2LoopTrans class
    raises the expected exceptions.

    '''
    trans = ArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(Node())
    assert (
        "Error in ArrayRange2LoopTrans transformation. The supplied node "
        "argument should be a PSyIR Assignment, but found 'Node'."
        in str(info.value))

    with pytest.raises(TransformationError) as info:
        trans.validate(Assignment.create(DataNode(), DataNode()))
    assert (
        "Error in ArrayRange2LoopTrans transformation. The lhs of the "
        "supplied Assignment node should be a PSyIR Array, but found "
        "'DataNode'." in str(info.value))

    array_symbol = DataSymbol("x", ArrayType(INTEGER_TYPE, [10, 10]))
    one = Literal("1", INTEGER_TYPE)
    array_assignment = Array.create(array_symbol, [one, one])
    with pytest.raises(TransformationError) as info:
        trans.validate(Assignment.create(array_assignment, DataNode()))
    assert (
        "Error in ArrayRange2LoopTrans transformation. The lhs of the "
        "supplied Assignment node should be a PSyIR Array with at least one "
        "of its dimensions being a Range, but found None." in str(info.value))

    # all ranges are the same (check as we go along instead?)


# Test fail with unsupported if ranges are different for different loops
