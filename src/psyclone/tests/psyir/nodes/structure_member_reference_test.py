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

''' Performs py.test tests on the StructureMemberReference PSyIR node. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir import nodes
from psyclone.psyir import symbols
from psyclone.errors import GenerationError


def create_structure_symbol(table):
    '''
    Utility to create a symbol of derived type and add it to the supplied
    symbol table.

    :param table: the symbol table to which to add the new symbol.
    :type table: :py:class:`psyclone.psyir.symbols.SymbolTable`

    :returns: the new DataSymbol representing a derived type.
    :rtype: :py:class:`psyclone.psyir.symbols.DataSymbol`

    '''
    region_type = symbols.StructureType.create([
        ("nx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC),
        ("ny", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC),
        ("domain", symbols.TypeSymbol("dom_type", symbols.DeferredType()),
         symbols.Symbol.Visibility.PUBLIC)])
    region_type_sym = symbols.TypeSymbol("grid_type", region_type)
    region_array_type = symbols.ArrayType(region_type_sym, [2, 2])
    grid_type = symbols.StructureType.create([
        ("dx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC),
        ("area", region_type_sym, symbols.Symbol.Visibility.PUBLIC),
        ("levels", region_array_type, symbols.Symbol.Visibility.PUBLIC)])
    grid_type_sym = symbols.TypeSymbol("grid_type", grid_type)
    grid_var = symbols.DataSymbol("grid", grid_type_sym)
    table.add(grid_type_sym)
    table.add(grid_var)
    return grid_var


def test_smr_constructor():
    ''' Test the StructureMemberReference constructor. '''
    region_type = symbols.StructureType.create([
        ("nx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC)])
    region_type_sym = symbols.TypeSymbol("region_type", region_type)
    grid_type = symbols.StructureType.create([
        ("dx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC),
        ("area", region_type_sym, symbols.Symbol.Visibility.PUBLIC)])
    grid_type_sym = symbols.TypeSymbol("grid_type", grid_type)
    # With a StructureType
    smref = nodes.StructureMemberReference(grid_type, "area")
    assert isinstance(smref, nodes.StructureMemberReference)
    assert smref.component.name == "area"
    # With a TypeSymbol
    smref = nodes.StructureMemberReference(grid_type_sym, "area")
    assert isinstance(smref, nodes.StructureMemberReference)
    assert smref.component.name == "area"


def test_smr_constructor_errors():
    ''' Test the validation checks in the constructor. '''
    # Attempt to reference something that cannot be a structure
    region_type = symbols.StructureType.create([
        ("nx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC)])
    with pytest.raises(TypeError) as err:
        nodes.StructureMemberReference(region_type, "nx")
    assert ("member 'nx' is not of DeferredType, StructureType or a "
            "TypeSymbol and therefore cannot" in str(err.value))


def test_smr_node_str():
    ''' Check the node_str method of the StructureMemberReference class.'''
    from psyclone.psyir.nodes.node import colored, SCHEDULE_COLOUR_MAP
    kschedule = nodes.KernelSchedule("kname")
    grid_var = create_structure_symbol(kschedule.symbol_table)
    assignment = nodes.Assignment(parent=kschedule)
    grid_ref = nodes.StructureReference.create(grid_var, ['area', 'nx'],
                                               parent=assignment)
    # The first child of the StructureReference is itself a reference to a
    # structure and is therefore a StructureMemberReference
    assert isinstance(grid_ref.children[0], nodes.StructureMemberReference)
    coloredtext = colored("StructureMemberReference",
                          SCHEDULE_COLOUR_MAP["Reference"])
    assert coloredtext+"[name:'area']" in grid_ref.children[0].node_str()


def test_smr_can_be_printed():
    '''Test that a StructureMemberReference instance can always be printed
    (i.e. is initialised fully)'''
    kschedule = nodes.KernelSchedule("kname")
    grid_var = create_structure_symbol(kschedule.symbol_table)
    assignment = nodes.Assignment(parent=kschedule)
    grid_ref = nodes.StructureReference.create(grid_var, ['area', 'nx'],
                                               parent=assignment)
    structure_member_ref = grid_ref.children[0]
    assert ("StructureMemberReference[name:'area']\n"
            "MemberReference[name:'nx']\n" in str(structure_member_ref))


def test_smr_child_validate():
    ''' Check the _validate_child() method of StructureMemberReference. '''
    region_type = symbols.StructureType.create([
        ("area", symbols.TypeSymbol("area_type", symbols.DeferredType()),
         symbols.Symbol.Visibility.PUBLIC),
        ("nx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC)])
    smr = nodes.StructureMemberReference(region_type, "area")
    with pytest.raises(GenerationError) as err:
        smr.addchild("hello")
    assert ("'str' can't be child 0 of 'StructureMemberReference'" in
            str(err.value))
    # StructureMemberReference is only permitted to have a single child
    # which may be None...
    smr.addchild(None)
    assert smr.children[0] is None
    # ...or a MemberReference()
    smr.children[0] = nodes.MemberReference(region_type, "nx")
    assert smr.children[0].name == "nx"
    # Attempting to add a second child should fail
    with pytest.raises(GenerationError) as err:
        smr.addchild(None)
    assert "'NoneType' can't be child 1 of" in str(err.value)
