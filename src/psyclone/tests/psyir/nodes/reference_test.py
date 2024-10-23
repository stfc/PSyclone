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
# Modified A. B. G. Chalk, STFC Daresbury Lab
# Modified J. G. Wallwork, University of Cambridge
# -----------------------------------------------------------------------------

''' Performs py.test tests on the Reference PSyIR node. '''

import pytest

from psyclone.core import VariablesAccessInfo
from psyclone.psyGen import GenerationError
from psyclone.psyir.nodes import (ArrayReference, Assignment, colored,
                                  KernelSchedule, Literal, Reference)
from psyclone.psyir.symbols import (ArrayType, CHARACTER_TYPE, ContainerSymbol,
                                    DataSymbol, UnresolvedType, ImportInterface,
                                    INTEGER_SINGLE_TYPE, REAL_SINGLE_TYPE,
                                    REAL_TYPE, ScalarType, Symbol, SymbolTable,
                                    UnresolvedInterface)


def test_reference_bad_init():
    '''Check that the __init__ method of the Reference class raises the
    expected exception if the symbol argument is not of the right
    type.

    '''
    with pytest.raises(TypeError) as excinfo:
        _ = Reference("hello")
    assert ("The Reference symbol setter expects a PSyIR Symbol object but "
            "found 'str'." in str(excinfo.value))


def test_reference_equality():
    '''
    Check that the __eq__ method of the Reference class behaves as expected,
    i.e. == is true iff:
    1. Both are the same type (Reference)
    2. They Reference the same symbol name
    '''
    symbol1 = DataSymbol("rname", INTEGER_SINGLE_TYPE)
    symbol2 = DataSymbol("rname2", INTEGER_SINGLE_TYPE)

    ref1 = Reference(symbol1)
    ref2 = Reference(symbol1)
    ref3 = Reference(symbol2)

    assert not ref1.is_character
    assert not ref2.is_character
    assert not ref3.is_character

    assert ref2 == ref1
    assert ref1 != ref3

    # Create another symbol with the same name (but not the same instance)
    symbol3 = DataSymbol("rname", INTEGER_SINGLE_TYPE)
    ref4 = Reference(symbol3)
    assert ref1 == ref4


def test_reference_node_str():
    ''' Check the node_str method of the Reference class.'''
    kschedule = KernelSchedule.create("kname")
    symbol = DataSymbol("rname", INTEGER_SINGLE_TYPE)
    kschedule.symbol_table.add(symbol)
    assignment = Assignment()
    ref = Reference(symbol, parent=assignment)
    coloredtext = colored("Reference", Reference._colour)
    assert coloredtext+"[name:'rname']" in ref.node_str()


def test_reference_can_be_printed():
    '''Test that a Reference instance can always be printed (i.e. is
    initialised fully)'''
    kschedule = KernelSchedule.create("kname")
    symbol = DataSymbol("rname", INTEGER_SINGLE_TYPE)
    kschedule.symbol_table.add(symbol)
    assignment = Assignment()
    ref = Reference(symbol, parent=assignment)
    assert "Reference[name:'rname']" in str(ref)


def test_reference_optional_parent():
    '''Test that the parent attribute is None if the optional parent
    argument is not supplied.

    '''
    ref = Reference(DataSymbol("rname", REAL_SINGLE_TYPE))
    assert ref.parent is None


def test_reference_children_validation():
    '''Test that children added to Reference are validated. A Reference node
    does not accept any children.

    '''
    ref = Reference(DataSymbol("rname", REAL_SINGLE_TYPE))
    with pytest.raises(GenerationError) as excinfo:
        ref.addchild(Literal("2", INTEGER_SINGLE_TYPE))
    assert ("Item 'Literal' can't be child 0 of 'Reference'. Reference is a"
            " LeafNode and doesn't accept children.") in str(excinfo.value)


def test_reference_is_array():
    '''Test that a non-array reference is marked as not an array.
    '''
    reference = Reference(DataSymbol("test", REAL_TYPE))
    assert reference.is_array is False
    assert not reference.is_character

    # Test that a standard symbol (which would raise an exception if
    # `is_array` of the symbol is called), does not raise an exception
    # and is reported as not being an array:
    ref = Reference(Symbol("symbol"))
    assert ref.is_array is False

    # Now add a real array to make sure this works as expected:
    array_symbol = DataSymbol("symbol", ArrayType(REAL_TYPE, [10]),
                              interface=UnresolvedInterface())
    ref = Reference(array_symbol)
    assert ref.is_array is True


def test_reference_is_character():
    '''Test that a character reference is marked correctly.
    '''
    reference = Reference(DataSymbol("test", CHARACTER_TYPE))
    assert reference.is_character


def test_reference_datatype():
    '''Test the datatype property.

    '''
    reference = Reference(DataSymbol("test", REAL_TYPE))
    assert isinstance(reference.datatype, ScalarType)
    assert reference.datatype.intrinsic == ScalarType.Intrinsic.REAL

    # Use a normal symbol, which should result in a UnresolvedType
    reference = Reference(Symbol("test"))
    assert isinstance(reference.datatype, UnresolvedType)


def test_reference_accesses():
    '''Test that the reference_accesses method behaves as expected in the
    usual case (see the next test for the unusual case).

    '''
    reference = Reference(DataSymbol("test", REAL_TYPE))
    var_access_info = VariablesAccessInfo()
    reference.reference_accesses(var_access_info)
    assert (str(var_access_info)) == "test: READ"

    # Test using reference_access with an array to check
    # that arrays are handled correctly.
    array_type = ArrayType(REAL_SINGLE_TYPE, [10])
    symbol_temp = DataSymbol("temp", array_type)
    symbol_i = DataSymbol("i", INTEGER_SINGLE_TYPE)
    array = ArrayReference.create(symbol_temp, [Reference(symbol_i)])
    assert array.is_array is True
    var_access_info = VariablesAccessInfo()
    array.reference_accesses(var_access_info)
    assert str(var_access_info) == "i: READ, temp: READ"

    # Test that renaming an imported reference is handled
    # correctly by the access info code:
    symbol = Symbol('renamed_name')
    symbol.interface = ImportInterface(ContainerSymbol("my_mod"),
                                       orig_name="orig_name")
    reference.symbol = symbol
    var_access_info = VariablesAccessInfo()
    reference.reference_accesses(var_access_info)
    assert "renamed_name: READ" in str(var_access_info)

    var_access_info = VariablesAccessInfo(options={"USE-ORIGINAL-NAMES": True})
    reference.reference_accesses(var_access_info)
    assert "orig_name: READ" in str(var_access_info)


def test_reference_can_be_copied():
    ''' Test that a reference can be copied. '''

    array_symbol = DataSymbol("symbol", ArrayType(REAL_TYPE, [10]))
    scalar_symbol = DataSymbol("other", REAL_TYPE)

    ref = Reference(array_symbol)

    ref1 = ref.copy()
    assert isinstance(ref1, Reference)
    assert ref1 is not ref
    assert ref1.symbol is array_symbol

    # Modifying the new reference does not affect the original
    ref1._symbol = scalar_symbol
    assert ref.symbol is array_symbol


def test_reference_next_access(fortran_reader):
    '''Test the next_access function for a Reference'''
    code = '''subroutine my_sub()
    integer :: a
    integer :: b
    a = 1
    b = 1
    a = b
    end subroutine my_sub'''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.children[0]
    a = routine.children[0].lhs
    b = routine.children[1].lhs
    a_2 = routine.children[2].lhs
    b_2 = routine.children[2].rhs
    assert a.next_access() is a_2
    assert b.next_access() is b_2
    assert a_2.next_access() is None

    code = '''subroutine my_sub()
    integer :: a
    integer :: b
    a = 1
    do a = 0, 10
        b = a
    end do
    end subroutine'''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.children[0]
    a = routine.children[0].lhs
    loop = routine.children[1]
    b_a = loop.loop_body.children[0].lhs
    a_2 = loop.loop_body.children[0].rhs
    assert a.next_access() is loop
    assert b_a.next_access() is None

    # Check the function for basic structures
    code = '''subroutine my_sub()
    type :: x
       integer :: a
       real :: b
    end type
    type(x) :: i
    i%a = 2
    i%b = 0.1
    i%a = 3
    i%b = 0.2
    end subroutine'''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.children[0]
    a = routine.children[0].lhs
    b = routine.children[1].lhs
    a_2 = routine.children[2].lhs
    b_2 = routine.children[3].lhs
    assert a.next_access() is a_2
    assert b.next_access() is b_2
    assert a_2.next_access() is None
    assert b_2.next_access() is None

    # Check the function for array access
    code = '''subroutine my_sub()
    integer, dimension(100) :: a
    a(0) = 2
    a(1) = 2
    end subroutine my_sub'''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.children[0]
    a = routine.children[0].lhs
    a_2 = routine.children[1].lhs
    assert a.next_access() is a_2
    assert a_2.next_access() is None

    # Check if statements don't affect
    code = '''subroutine my_sub()
   integer :: a
   logical :: b = .false.
   a = 1
   if(b) then
     a = 0
   end if
   a = 2
   end subroutine'''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.children[0]
    a = routine.children[0].lhs
    a_2 = routine.children[1].if_body.children[0].lhs
    a_3 = routine.children[2].lhs
    assert a.next_access() is a_2
    assert a_2.next_access() is a_3
    assert a_3.next_access() is None

    # Check else block behaviour
    code = '''subroutine my_sub()
   integer :: a
   logical :: b = .false.
   a = 1
   if(b) then
     a = 0
   else
     a = 1
   end if
   a = 2
   end subroutine'''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.children[0]
    a = routine.children[0].lhs
    a_2 = routine.children[1].if_body.children[0].lhs
    a_3 = routine.children[1].else_body.children[0].lhs
    a_4 = routine.children[2].lhs
    assert a.next_access() is a_2
    assert a_2.next_access() is a_3
    assert a_3.next_access() is a_4
    assert a_4.next_access() is None


def test_reference_next_access_with_codeblock(fortran_reader):
    ''' Test when te next_access is a Codeblock. '''
    code = '''subroutine my_sub()
    character, dimension(100) :: a
    a = "test"
    write(a, "A") "mytest"
    end subroutine
    '''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.children[0]
    a = routine.children[0].lhs
    codeblock = routine.children[1]
    if a.next_access() is not codeblock:
        pytest.xfail("#2271 Codeblocks don't currently support "
                     "reference_accesses")


def test_reference_previous_access(fortran_reader):
    '''Test the previous_access function for a Reference'''
    code = '''subroutine my_sub()
    integer :: a
    integer :: b
    a = 1
    b = 1
    a = b
    end subroutine my_sub'''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.children[0]
    a = routine.children[0].lhs
    b = routine.children[1].lhs
    a_2 = routine.children[2].lhs
    b_2 = routine.children[2].rhs
    assert a.previous_access() is None
    assert b.previous_access() is None
    assert a_2.previous_access() is a
    assert b_2.previous_access() is b

    code = '''subroutine my_sub()
    integer :: a
    integer :: b
    a = 1
    do a = 0, 10
        b = a
    end do
    end subroutine'''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.children[0]
    a = routine.children[0].lhs
    loop = routine.children[1]
    b_a = loop.loop_body.children[0].lhs
    a_2 = loop.loop_body.children[0].rhs
    assert a.previous_access() is None
    assert b_a.previous_access() is None
    assert a_2.previous_access() is loop

    # Check the function for basic structures
    code = '''subroutine my_sub()
    type :: x
       integer :: a
       real :: b
    end type
    type(x) :: i
    i%a = 2
    i%b = 0.1
    i%a = 3
    i%b = 0.2
    end subroutine'''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.children[0]
    a = routine.children[0].lhs
    b = routine.children[1].lhs
    a_2 = routine.children[2].lhs
    b_2 = routine.children[3].lhs
    assert a.previous_access() is None
    assert b.previous_access() is None
    assert a_2.previous_access() is a
    assert b_2.previous_access() is b

    # Check the function for array access
    code = '''subroutine my_sub()
    integer, dimension(100) :: a
    a(0) = 2
    a(1) = 2
    end subroutine my_sub'''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.children[0]
    a = routine.children[0].lhs
    a_2 = routine.children[1].lhs
    assert a.previous_access() is None
    assert a_2.previous_access() is a

    # Check if statements
    code = '''subroutine my_sub()
   integer :: a
   logical :: b = .false.
   a = 1
   if(b) then
     a = 0
   end if
   a = 2
   end subroutine'''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.children[0]
    a = routine.children[0].lhs
    a_2 = routine.children[1].if_body.children[0].lhs
    a_3 = routine.children[2].lhs
    assert a.previous_access() is None
    assert a_2.previous_access() is a
    assert a_3.previous_access() is a_2

    # Check else block behaviour
    code = '''subroutine my_sub()
   integer :: a
   logical :: b = .false.
   a = 1
   if(b) then
     a = 0
   else
     a = 1
   end if
   a = 2
   end subroutine'''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.children[0]
    a = routine.children[0].lhs
    a_2 = routine.children[1].if_body.children[0].lhs
    a_3 = routine.children[1].else_body.children[0].lhs
    a_4 = routine.children[2].lhs
    assert a.previous_access() is None
    assert a_2.previous_access() is a
    assert a_3.previous_access() is a_2
    assert a_4.previous_access() is a_3


def test_reference_accesses_initialisation_statement(fortran_reader):
    ''' Test the behaviour of next and previous access
    with initialisation statements, and also for initial_value since
    this is outside of a Routine tree. '''
    # Previous access doesn't find initial values from the symbol table
    code = '''module my_mod
   use external_mod, only: a
   contains
   subroutine my_sub()
      integer :: b = a + a +1
      b = a + 3
   end subroutine my_sub
end module my_mod'''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.children[0].children[0]
    a = routine.children[0].lhs
    assert a.previous_access() is None

    sym_tab = routine.symbol_table
    symbols = sym_tab.get_symbols()
    b_sym = symbols['b']
    refs = b_sym.initial_value.walk(Reference)
    assert refs[0].next_access() == refs[1]
    assert refs[1].previous_access() == refs[0]
    assert refs[0].previous_access() is None
    assert refs[1].next_access() is None


def test_reference_previous_access_with_codeblock(fortran_reader):
    ''' Test when te previous_access is a Codeblock. '''
    code = '''subroutine my_sub()
    character, dimension(100) :: a
    write(a, "A") "mytest"
    a = "test"
    end subroutine
    '''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.children[0]
    a = routine.children[1].lhs
    codeblock = routine.children[1]
    if a.previous_access() is not codeblock:
        pytest.xfail("#2271 Codeblocks don't currently support "
                     "reference_accesses")


def test_reference_replace_symbols_using():
    '''Test the replace_symbols_using() method correctly updates the Symbol
    to which the Reference refers.

    '''
    wp = DataSymbol("wp", INTEGER_SINGLE_TYPE)
    stype = ScalarType(ScalarType.Intrinsic.REAL, wp)
    asym = DataSymbol("asym", stype)
    ref = Reference(asym)
    table = SymbolTable()
    ref.replace_symbols_using(table)
    # Empty table so no change.
    assert ref.symbol is asym
    assert ref.symbol.datatype.precision is wp
    asym2 = asym.copy()
    table.add(asym2)
    ref.replace_symbols_using(table)
    assert ref.symbol is asym2
    assert ref.symbol.datatype.precision is wp
    # Check that the update does not recurse into the Symbol properties (as
    # that is handled by the SymbolTable).
    wp2 = wp.copy()
    table.add(wp2)
    ref.replace_symbols_using(table)
    assert ref.symbol is asym2
    assert ref.symbol.datatype.precision is wp
