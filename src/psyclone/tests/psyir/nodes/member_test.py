# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2023, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# Author: J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains pytest tests for the Member class. '''

import pytest
from psyclone.psyir import nodes, symbols


def test_member_constructor():
    ''' Test that we can construct an instance of Member. '''
    mem = nodes.Member("fred")
    assert mem.name == "fred"
    assert str(mem) == "Member[name:'fred']"
    assert mem.children == []


def test_member_constructor_errors():
    ''' Test the validation checks in the constructor. '''
    with pytest.raises(TypeError) as err:
        nodes.Member("hello", parent="wrong")
    assert ("parent of a Member must be either a "
            "(ArrayOf)Structure(s)Reference or (ArrayOf)Structure(s)Member "
            "but found 'str'" in str(err.value))


def test_member_can_be_copied():
    ''' Test that a Member node can be copied. '''

    member = nodes.Member("name1")

    member1 = member.copy()
    assert isinstance(member1, nodes.Member)
    assert member1 is not member
    assert member1.name == "name1"

    # Modifying the new member does not affect the original
    member1._component_name = "name2"
    assert member1.name == "name2"
    assert member.name == "name1"


def test_member_is_array():
    ''' Test that we can check if a member is an array. '''
    mem = nodes.Member("fred")
    assert mem.is_array is False


def test_member_get_signature():
    ''' Test that we get the expected signature from a member. '''
    mem = nodes.Member("fred")
    signature, indices = mem.get_signature_and_indices()
    assert str(signature) == "fred"
    assert indices == [[]]


def test_member_equality():
    ''' Test member equality. '''
    mem = nodes.Member("m1")
    mem2 = nodes.Member("m1")
    mem3 = nodes.Member("notm1")

    assert mem == mem2
    assert mem != mem3


def test_member_lbound(fortran_writer):
    ''' Tests for the lbound() method of Member. '''
    one = nodes.Literal("1", symbols.INTEGER_TYPE)
    two = nodes.Literal("2", symbols.INTEGER_TYPE)
    # First, test when we don't have type information.
    grid_type = symbols.DataTypeSymbol("grid_type", symbols.DeferredType())
    sym = symbols.DataSymbol("grid_var", grid_type)
    ref = nodes.StructureReference.create(sym, [("data", [one.copy()])])
    lbnd = ref.member.lbound(0)
    assert isinstance(lbnd, nodes.BinaryOperation)
    out = fortran_writer(lbnd).lower()
    assert out == "lbound(grid_var%data, 1)"
    usym = symbols.DataSymbol("uvar", symbols.DeferredType())
    ref = nodes.ArrayOfStructuresReference.create(
        usym, [one.copy()],
        [("map", [one.copy(), two.copy()]),
         ("data", [one.copy()])])
    lbnd = ref.member.member.lbound(0)
    assert isinstance(lbnd, nodes.BinaryOperation)
    out = fortran_writer(lbnd).lower()
    assert out == "lbound(uvar(1)%map(1,2)%data, 1)"
    # Second, test when we do have type information.
    a2d = symbols.ArrayType(symbols.REAL_TYPE, [2, (2, 8)])
    # Structure that contains "map" which is a 2D array.
    stypedef = symbols.StructureType.create(
        [("map", a2d, symbols.Symbol.Visibility.PUBLIC)])
    stypedefsym = symbols.DataTypeSymbol("map_type", stypedef)
    # Structure containing a structure of stypedef and an array of such
    # structures.
    stypedef2 = symbols.StructureType.create(
        [("grid", stypedef, symbols.Symbol.Visibility.PUBLIC),
         ("subgrids", symbols.ArrayType(stypedefsym, [3, (2, 6)]),
          symbols.Symbol.Visibility.PUBLIC)])
    ssym = symbols.DataSymbol("var", stypedef2)
    sref = nodes.StructureReference.create(ssym,
                                           ["grid",
                                            ("map", [two.copy(), two.copy()])])
    assert sref.member.member.lbound(0) == one
    assert sref.member.member.lbound(1) == two
    sref2 = nodes.StructureReference.create(
        ssym,
        [("subgrids", [two.copy(), two.copy()]),
         ("map", [two.copy(), two.copy()])])
    assert sref2.member.lbound(1) == two
    assert sref2.member.member.lbound(1) == two
