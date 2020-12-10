# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
# -----------------------------------------------------------------------------

''' This module contains pytest tests for the ArrayStructureMemberReference
    class. '''

import pytest
from psyclone.psyir import symbols, nodes
from psyclone.errors import GenerationError
from psyclone.tests.utilities import check_links


def test_asmr_constructor():
    ''' Test that we can construct an ArrayStructureMemberReference. '''
    # For this we need a structure that contains an array of structures.
    region_type = symbols.StructureType.create([
        ("sub_mesh",
         symbols.ArrayType(symbols.INTEGER_TYPE,
                           [nodes.Literal("10", symbols.INTEGER_TYPE)]),
         symbols.Symbol.Visibility.PUBLIC)])
    rtype_symbol = symbols.TypeSymbol("region_type", region_type)
    grid_type = symbols.StructureType.create([
        ("regions",
         symbols.ArrayType(rtype_symbol,
                           [nodes.Literal("3", symbols.INTEGER_TYPE)]),
         symbols.Symbol.Visibility.PUBLIC)])
    asmr = nodes.ArrayStructureMemberReference(grid_type, "regions")
    assert isinstance(asmr, nodes.ArrayStructureMemberReference)
    assert len(asmr.children) == 0
    check_links(asmr, asmr.children)


def test_asmr_create():
    ''' Test the create method of ArrayStructureMemberReference. '''
    region_type = symbols.StructureType.create([
        ("sub_mesh",
         symbols.ArrayType(symbols.INTEGER_TYPE,
                           [nodes.Literal("10", symbols.INTEGER_TYPE)]),
         symbols.Symbol.Visibility.PUBLIC)])
    rtype_symbol = symbols.TypeSymbol("region_type", region_type)
    grid_type = symbols.StructureType.create([
        ("regions",
         symbols.ArrayType(rtype_symbol,
                           [nodes.Literal("3", symbols.INTEGER_TYPE)]),
         symbols.Symbol.Visibility.PUBLIC)])
    asmr = nodes.ArrayStructureMemberReference.create(
        grid_type, "regions",
        indices=[nodes.Literal("3", symbols.INTEGER_TYPE)])
    assert len(asmr.children) == 2
    assert asmr.children[0] is None
    assert asmr.children[1].value == "3"


def test_asmr_validate_child():
    ''' Test the _validate_child method of ArrayStructureMemberReference. '''
    region_type = symbols.StructureType.create([
        ("sub_mesh",
         symbols.ArrayType(symbols.INTEGER_TYPE,
                           [nodes.Literal("10", symbols.INTEGER_TYPE)]),
         symbols.Symbol.Visibility.PUBLIC)])
    rtype_symbol = symbols.TypeSymbol("region_type", region_type)
    grid_type = symbols.StructureType.create([
        ("regions",
         symbols.ArrayType(rtype_symbol,
                           [nodes.Literal("3", symbols.INTEGER_TYPE)]),
         symbols.Symbol.Visibility.PUBLIC)])
    asmr = nodes.ArrayStructureMemberReference(grid_type, "regions")
    with pytest.raises(GenerationError) as err:
        asmr.addchild("wrong")
    assert ("'str' can't be child 0 of 'ArrayStructureMemberReference'" in
            str(err.value))
    asmr.addchild(nodes.MemberReference(region_type, "sub_mesh"))
    assert isinstance(asmr.children[0], nodes.MemberReference)
    with pytest.raises(GenerationError) as err:
        asmr.addchild("2")
    assert ("'str' can't be child 1 of 'ArrayStructureMemberReference'" in
            str(err.value))
    idx = nodes.Reference(symbols.DataSymbol("idx", symbols.INTEGER_TYPE))
    asmr.addchild(idx)
    assert asmr.children[1] is idx
