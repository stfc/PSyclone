import pytest
from psyclone.psyir.nodes import Literal, BinaryOperation, Reference, Range, Array, Assignment
from psyclone.psyGen import KernelSchedule
from psyclone.psyir.symbols import SymbolTable, DataSymbol, ArrayType, INTEGER_TYPE, REAL_TYPE
from psyclone.psyir.transformations import ArrayRange2LoopTrans
from psyclone.psyir.backend.fortran import FortranWriter

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
    array_symbol = DataSymbol("y", ArrayType(REAL_TYPE, [10,10]))
    symbol_table.add(array_symbol)
    return Array.create(array_symbol, [create_range(array_symbol, 1),
                                       create_range(array_symbol, 2)])


def create_array_z(symbol_table):
    ''' helper '''
    array_symbol = DataSymbol("z", ArrayType(REAL_TYPE, [10,10,10]))
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


@pytest.mark.parametrize("lhs_create,rhs_create,result",
                         [(create_array_x, create_literal,
                           "  do idx = LBOUND(x, 1), UBOUND(x, 1), 1\n"
                           "    x(idx)=0.0\n"),
                          (create_array_x, create_array_y,
                           "  do idx = LBOUND(x, 1), UBOUND(x, 1), 1\n"
                           "    x(idx)=y(n,idx)\n"),
                          (create_array_y, create_array_x,
                           "  do idx = LBOUND(y, 2), UBOUND(y, 2), 1\n"
                           "    y(n,idx)=x(idx)\n"),
                          (create_array_y_2, create_array_z,
                           "  do idx = LBOUND(y, 1), UBOUND(y, 1), 1\n"
                           "    y(idx,:)=z(idx,n,:)\n"),
                          (create_array_y_3, create_expr,
                           "  do idx = 2, n, 2\n"
                           "    y(n,idx)=x(idx) * z(1,idx) + a(1)")])
def test_simple_lhs(lhs_create, rhs_create, result):
    ''' Check that the code is transformed as expected when the lhs of an assignment is a simple array range.'''
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
    assert result in writer(routine)
