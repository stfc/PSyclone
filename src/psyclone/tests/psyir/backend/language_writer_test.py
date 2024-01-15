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
# Author: J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

'''Performs pytest tests on the psyclone.psyir.backend.language_writer
module.'''

from __future__ import absolute_import

import pytest

from psyclone.psyir.backend.language_writer import LanguageWriter
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.symbols import ArrayType, DataSymbol, DataTypeSymbol, \
    INTEGER_TYPE, REAL_TYPE, Symbol, StructureType
from psyclone.psyir.nodes import ArrayOfStructuresReference, ArrayReference, \
    Literal, Member, StructureReference
from psyclone.tests.utilities import Compile


def test_language_writer_constructor():
    '''Tests the constructor.

    '''
    fwriter = LanguageWriter(("(", ")"), "%")
    assert fwriter.array_parenthesis == ("(", ")")
    assert fwriter.structure_character == "%"
    cwriter = LanguageWriter(("[", "]"), ".")
    assert cwriter.array_parenthesis == ("[", "]")
    assert cwriter.structure_character == "."


def test_language_writer_constructor_errors():
    '''Test that invalid parameters in the constructor are detected.
    '''
    for invalid_parenthesis in [123, "()", ['[', '[', ']'], ["(", ")"]]:
        with pytest.raises(TypeError) as err:
            _ = LanguageWriter(invalid_parenthesis, "%")
        assert "Invalid array-parenthesis parameter, must be " \
               "a tuple of two strings, got '" in str(err.value)

    for invalid_structure_character in [123, []]:
        with pytest.raises(TypeError) as err:
            _ = LanguageWriter(("(", ")"), invalid_structure_character)
        assert "Invalid structure_character parameter, must be " \
               "a string, got '" in str(err.value)


def test_lw_arrayreference_incomplete(fortran_writer):
    '''
    Test that the correct error is raised if an incomplete ArrayReference
    is encountered.
    '''
    array_type = ArrayType(REAL_TYPE, [10])
    symbol = DataSymbol("b", array_type)
    # create() must be supplied with a shape
    array = ArrayReference.create(symbol, [Literal("1", INTEGER_TYPE)])
    # Remove its children
    array._children = []
    with pytest.raises(VisitorError) as err:
        fortran_writer.arrayreference_node(array)
    assert ("Incomplete ArrayReference node (for symbol 'b') found: must "
            "have one or more children" in str(err.value))


def test_lw_arrayreference(fortran_reader, fortran_writer, tmpdir):
    '''Check the LanguageWriter class array method correctly prints
    out the representation of an array reference using the Fortran
    writer as instance.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a, n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n,n,n)\n"
        "    a(2,n,3) = 0.0\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = fortran_reader.psyir_from_source(code)

    # Generate Fortran from the PSyIR schedule
    result = fortran_writer(schedule)
    assert "a(2,n,3) = 0.0" in result
    assert Compile(tmpdir).string_compiles(result)


def test_lw_structureref(fortran_writer):
    ''' Test the LanguageWriter support for StructureReference
    using the FortranWriter as instance. '''
    region_type = StructureType.create([
        ("nx", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
        ("ny", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None)])
    region_type_sym = DataTypeSymbol("grid_type", region_type)
    region_array_type = ArrayType(region_type_sym, [2, 2])
    grid_type = StructureType.create([
        ("dx", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
        ("area", region_type_sym, Symbol.Visibility.PUBLIC, None),
        ("levels", region_array_type, Symbol.Visibility.PUBLIC, None)])
    grid_type_sym = DataTypeSymbol("grid_type", grid_type)
    grid_var = DataSymbol("grid", grid_type_sym)
    grid_ref = StructureReference.create(grid_var, ['area', 'nx'])
    assert fortran_writer.structurereference_node(grid_ref) == "grid%area%nx"
    level_ref = StructureReference.create(
        grid_var, [('levels', [Literal("1", INTEGER_TYPE),
                               Literal("2", INTEGER_TYPE)]), 'ny'])
    assert fortran_writer(level_ref) == "grid%levels(1,2)%ny"
    # Make the number of children invalid
    level_ref._children = []
    with pytest.raises(VisitorError) as err:
        fortran_writer(level_ref)
    assert ("StructureReference must have a single child but the reference "
            "to symbol 'grid' has 0" in str(err.value))
    # Single child but not of the right type
    level_ref._children = [Literal("1", INTEGER_TYPE)]
    with pytest.raises(VisitorError) as err:
        fortran_writer._visit(level_ref)
    assert ("StructureReference must have a single child which is a sub-"
            "class of Member but the reference to symbol 'grid' has a child "
            "of type 'Literal'" in str(err.value))


def test_lw_arrayofstructuresmember(fortran_writer):
    ''' Test the LanguageWriter support for ArrayOfStructuresMember
    using the FortranWriter. '''
    region_type = StructureType.create([
        ("nx", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
        ("ny", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None)])
    region_type_sym = DataTypeSymbol("grid_type", region_type)
    region_array_type = ArrayType(region_type_sym, [2, 2])
    # The grid type contains an array of region-type structures
    grid_type = StructureType.create([
        ("levels", region_array_type, Symbol.Visibility.PUBLIC, None)])
    grid_type_sym = DataTypeSymbol("grid_type", grid_type)
    grid_var = DataSymbol("grid", grid_type_sym)
    # Reference to an element of an array that is a structure
    level_ref = StructureReference.create(grid_var,
                                          [("levels",
                                            [Literal("1", INTEGER_TYPE),
                                             Literal("1", INTEGER_TYPE)])])
    assert (fortran_writer.structurereference_node(level_ref) ==
            "grid%levels(1,1)")
    # Reference to a member of a structure that is an element of an array
    grid_ref = StructureReference.create(grid_var,
                                         [("levels",
                                           [Literal("1", INTEGER_TYPE),
                                            Literal("1", INTEGER_TYPE)]),
                                          "nx"])
    assert (fortran_writer.structurereference_node(grid_ref) ==
            "grid%levels(1,1)%nx")
    # Reference to an *array* of structures
    grid_ref = StructureReference.create(grid_var, ["levels"])
    assert fortran_writer.structurereference_node(grid_ref) == "grid%levels"


def test_lw_arrayofstructuresref(fortran_writer):
    ''' Test the LanguageWriter support for ArrayOfStructuresReference
    using the FortranWriter as instance. '''
    grid_type = StructureType.create([
        ("dx", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None)])
    grid_type_sym = DataTypeSymbol("grid_type", grid_type)
    grid_array_type = ArrayType(grid_type_sym, [10])
    grid_var = DataSymbol("grid", grid_array_type)
    grid_ref = ArrayOfStructuresReference.create(grid_var,
                                                 [Literal("3", INTEGER_TYPE)],
                                                 ["dx"])
    assert (fortran_writer.arrayofstructuresreference_node(grid_ref) ==
            "grid(3)%dx")
    # Break the node to trigger checks
    # Make the first node something other than a member
    grid_ref._children = [grid_ref._children[1], grid_ref._children[1]]
    with pytest.raises(VisitorError) as err:
        fortran_writer.arrayofstructuresreference_node(grid_ref)
    assert ("An ArrayOfStructuresReference must have a Member as its first "
            "child but found 'Literal'" in str(err.value))
    # Remove a child
    grid_ref._children = [grid_ref._children[0]]
    with pytest.raises(VisitorError) as err:
        fortran_writer.arrayofstructuresreference_node(grid_ref)
    assert ("An ArrayOfStructuresReference must have at least two children "
            "but found 1" in str(err.value))


def test_member_node(fortran_writer):
    '''Explicitly test the member_node function.'''
    region_type = StructureType.create([
        ("nx", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None),
        ("ny", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None)])
    region_type_sym = DataTypeSymbol("grid_type", region_type)
    region_array_type = ArrayType(region_type_sym, [2, 2])
    # The grid type contains an array of region-type structures
    grid_type = StructureType.create([
        ("levels", region_array_type, Symbol.Visibility.PUBLIC, None)])
    grid_type_sym = DataTypeSymbol("grid_type", grid_type)
    grid_var = DataSymbol("grid", grid_type_sym)
    # Reference to a member of a structure that is an element of an array
    grid_ref = StructureReference.create(grid_var,
                                         [("levels",
                                           [Literal("1", INTEGER_TYPE),
                                            Literal("1", INTEGER_TYPE)]),
                                          "nx"])

    # Specifically test member_node:
    member = grid_ref.children[0]
    assert isinstance(member, Member)
    assert fortran_writer.member_node(member) == "levels(1,1)%nx"
