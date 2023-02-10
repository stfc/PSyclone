# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2023, Science and Technology Facilities Council.
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
# Author S. Siso, STFC Daresbury Lab
# Modified: R. W. Ford and A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests of the ArrayMixin PSyIR nodes trait. '''

import pytest
from psyclone.errors import InternalError
from psyclone.psyir.nodes import (
    ArrayOfStructuresReference, ArrayReference, BinaryOperation, Range,
    Literal, Routine, StructureReference, Assignment, Reference)
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.symbols import (
    ArrayType, DataSymbol, DataTypeSymbol, DeferredType, INTEGER_TYPE,
    REAL_TYPE, StructureType, Symbol)


_ONE = Literal("1", INTEGER_TYPE)
_TWO = Literal("2", INTEGER_TYPE)


class ConcreteArray(ArrayMixin, Reference):
    '''
    A concrete class that inherits the ArrayMixin trait to allow it to be
    tested.

    '''
    @classmethod
    def create(cls, sym, child_nodes):
        '''
        Create an instance of this class.

        :param sym: a Symbol.
        :type sym: :py:class:`psyclone.psyir.symbols.Symbol`
        :param child_nodes: nodes that the new instance will have as children.
        :type child_nodes: List[:py:class:`psyclone.psyir.nodes.Node`]

        :returns: an instance of this class.
        :rtype: :py:class:`ConcreteArray`

        '''
        obj = cls(sym)
        for child in child_nodes:
            obj.addchild(child)
        return obj


def test_get_outer_range_index():
    '''Check that the get_outer_range_index method returns the outermost index
    of the children list that is a range.

    '''
    symbol = DataSymbol("my_symbol", ArrayType(INTEGER_TYPE, [10, 10, 10]))
    array = ConcreteArray.create(symbol, [Range(), Range(), Range()])
    assert array.get_outer_range_index() == 2

    symbol = DataSymbol("my_symbol", DeferredType())
    aos = ConcreteArray.create(
        symbol, [_ONE.copy(), Range(), Range(), Range()])
    assert aos.get_outer_range_index() == 3  # +1 for the Literal child


def test_get_outer_range_index_error():
    '''Check that the get_outer_range_index method raises an IndexError if
    no range exist as child of the given array.

    '''
    symbol = DataSymbol("my_symbol", ArrayType(INTEGER_TYPE, [10]))
    array = ConcreteArray.create(symbol, [_TWO.copy()])
    with pytest.raises(IndexError):
        _ = array.get_outer_range_index()


def test_is_upper_lower_bound(fortran_reader):
    '''Test the is_lower_bound() and is_upper_bound() methods return the
    expected values if an array reference has literal bounds. Create
    and use an ArrayReference node to test the abstract ArrayMixin
    class.

    '''
    code = (
        "subroutine test()\n"
        "use some_mod\n"
        "real a(10)\n"
        "character(10) my_str\n"
        "a(1:10) = 0.0\n"
        "my_str(2:2) = 'b'\n"
        "var1(3:4) = 0\n"
        "end subroutine\n")

    # Return True as the literal values or the declaration and array
    # reference match.
    psyir = fortran_reader.psyir_from_source(code)
    assigns = psyir.walk(Assignment)
    array_ref = assigns[0].lhs
    assert array_ref.is_lower_bound(0)
    assert array_ref.is_upper_bound(0)

    # Remove the symbol from the symbol table to force the returning
    # of False.
    symbol = array_ref.symbol
    symbol_table = array_ref.scope.symbol_table
    norm_name = symbol_table._normalize(symbol.name)
    symbol_table._symbols.pop(norm_name)
    assert not array_ref.is_lower_bound(0)
    assert not array_ref.is_upper_bound(0)

    # Return False as the literal values of the array declaration and
    # array reference do not match.
    psyir = fortran_reader.psyir_from_source(code.replace("1:10", "2:9"))
    array_ref = psyir.children[0].children[0].lhs
    assert not array_ref.is_lower_bound(0)
    assert not array_ref.is_upper_bound(0)

    # Return False if the symbol being referenced is of UnknownFortranType.
    array_ref = assigns[1].lhs
    assert not array_ref.is_lower_bound(0)
    assert not array_ref.is_upper_bound(0)

    # Return False if the symbol has no associated type information.
    array_ref = assigns[2].lhs
    assert not isinstance(array_ref.symbol, DataSymbol)
    assert not array_ref.is_lower_bound(0)
    assert not array_ref.is_upper_bound(0)


def test_lbound():
    '''
    Tests for the lbound method on a ConcreteArray defined by an
    ArrayType.

    '''
    # Symbol is of ArrayType.
    symbol = DataSymbol("my_symbol", ArrayType(INTEGER_TYPE,
                                               [10, (2, 10), 10]))
    aref = ConcreteArray.create(symbol,
                                [_ONE.copy(), _ONE.copy(), _ONE.copy()])
    assert aref.lbound(0) == _ONE
    assert aref.lbound(1) == _TWO
    with pytest.raises(IndexError):
        aref.lbound(3)
    # Symbol is of DeferredType so the result should be an instance of the
    # LBOUND intrinsic.
    dtsym = DataSymbol("oops", DeferredType())
    dtref = ConcreteArray.create(dtsym,
                                 [_ONE.copy(), _ONE.copy(), _ONE.copy()])
    lbnd = dtref.lbound(1)
    assert isinstance(lbnd, BinaryOperation)
    assert lbnd.operator == BinaryOperation.Operator.LBOUND
    assert lbnd.children[0].symbol is dtsym
    assert lbnd.children[1] == Literal("2", INTEGER_TYPE)


def test_aref_to_AoS_lbound():
    '''
    Test the lbound() method for an ArrayReference to an array of structures.

    '''
    sgrid_type = StructureType.create(
        [("id", INTEGER_TYPE, Symbol.Visibility.PUBLIC)])
    sgrid_type_sym = DataTypeSymbol("subgrid_type", sgrid_type)
    sym = DataSymbol("subgrids", ArrayType(sgrid_type_sym, [(3, 10)]))
    one = Literal("1", INTEGER_TYPE)
    lbound = BinaryOperation.create(BinaryOperation.Operator.LBOUND,
                                    Reference(sym), one)
    ubound = BinaryOperation.create(BinaryOperation.Operator.UBOUND,
                                    Reference(sym), one.copy())
    array = ArrayReference.create(sym, [Range.create(lbound, ubound)])
    lbnd = array.lbound(0)
    assert lbnd.value == "3"


def test_member_lbound(fortran_writer):
    ''' Tests for the lbound() method when used with a sub-class of Member. '''
    one = Literal("1", INTEGER_TYPE)
    two = Literal("2", INTEGER_TYPE)
    # First, test when we don't have type information.
    grid_type = DataTypeSymbol("grid_type", DeferredType())
    sym = DataSymbol("grid_var", grid_type)
    ref = StructureReference.create(sym, [("data", [one.copy()])])
    lbnd = ref.member.lbound(0)
    assert isinstance(lbnd, BinaryOperation)
    out = fortran_writer(lbnd).lower()
    assert out == "lbound(grid_var%data, 1)"
    usym = DataSymbol("uvar", DeferredType())
    ref = ArrayOfStructuresReference.create(
        usym, [one.copy()],
        [("map", [one.copy(), two.copy()]),
         ("data", [one.copy()])])
    lbnd = ref.member.member.lbound(0)
    assert isinstance(lbnd, BinaryOperation)
    out = fortran_writer(lbnd).lower()
    assert out == "lbound(uvar(1)%map(1,2)%data, 1)"
    # Second, test when we do have type information.
    a2d = ArrayType(REAL_TYPE, [2, (2, 8)])
    # Structure that contains "map" which is a 2D array.
    stypedef = StructureType.create(
        [("map", a2d, Symbol.Visibility.PUBLIC)])
    stypedefsym = DataTypeSymbol("map_type", stypedef)
    # Structure containing a structure of stypedef and an array of such
    # structures.
    stypedef2 = StructureType.create(
        [("grid", stypedef, Symbol.Visibility.PUBLIC),
         ("subgrids", ArrayType(stypedefsym, [3, (2, 6)]),
          Symbol.Visibility.PUBLIC)])
    ssym = DataSymbol("var", stypedef2)
    sref = StructureReference.create(ssym,
                                     ["grid",
                                      ("map", [two.copy(), two.copy()])])
    assert sref.member.member.lbound(0) == one
    assert sref.member.member.lbound(1) == two
    sref2 = StructureReference.create(
        ssym, [("subgrids", [two.copy(), two.copy()]),
               ("map", [two.copy(), two.copy()])])
    assert sref2.member.lbound(1) == two
    assert sref2.member.member.lbound(1) == two


def test_get_effective_shape(fortran_reader, fortran_writer):
    '''Tests for the _get_effective_shape() method.'''
    code = (
        "subroutine test()\n"
        "  use some_mod\n"
        "  integer :: indices(8,3)\n"
        "  real a(10), b(10,10)\n"
        "  a(1:10) = 0.0\n"
        "  b(indices(2:3,1), 2:5) = 2.0\n"
        "  b(indices(2:3,1:2), 2:5) = 2.0\n"
        "  a(f()) = 2.0\n"
        "  a(2+3) = 1.0\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    # Direct array slice.
    shape = routine.children[0].lhs._get_effective_shape()
    assert len(shape) == 1
    assert isinstance(shape[0], BinaryOperation)
    # Indirect array slice.
    shape = routine.children[1].lhs._get_effective_shape()
    assert len(shape) == 2
    assert isinstance(shape[0], BinaryOperation)
    code = fortran_writer(shape[0])
    # An ArrayType does not store the number of elements, just lower and upper
    # bounds. Therefore, we end up recursively computing the no. of elements.
    # The answer is still "2" though!
    assert code == "((3 - 2) / 1 + 1 - 1) / 1 + 1"
    code = fortran_writer(shape[1])
    assert code == "(5 - 2) / 1 + 1"
    # An indirect array slice can only be 1D.
    with pytest.raises(InternalError) as err:
        _ = routine.children[2].lhs._get_effective_shape()
    assert ("array defining a slice of a dimension of another array must be "
            "1D but 'indices' used to index into 'b' has 2 dimensions" in
            str(err.value))
    # Indirect array access using function call.
    with pytest.raises(NotImplementedError) as err:
        _ = routine.children[3].lhs._get_effective_shape()
    assert "include a function call or expression" in str(err.value)
    # Array access with expression in indices.
    with pytest.raises(NotImplementedError) as err:
        _ = routine.children[4].lhs._get_effective_shape()
    assert "include a function call or expression" in str(err.value)
