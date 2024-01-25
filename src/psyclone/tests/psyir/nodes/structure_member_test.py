# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Modified by: R. W. Ford, STFC Daresbury Lab
# Modified by: A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests on the StructureMember PSyIR node. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir import nodes
from psyclone.psyir import symbols
from psyclone.errors import GenerationError, InternalError
from psyclone.psyir.nodes.node import colored


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
        ("nx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC, None),
        ("ny", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC, None),
        ("domain", symbols.DataTypeSymbol("dom_type",
                                          symbols.UnresolvedType()),
         symbols.Symbol.Visibility.PUBLIC, None)])
    region_type_sym = symbols.DataTypeSymbol("grid_type", region_type)
    region_array_type = symbols.ArrayType(region_type_sym, [2, 2])
    grid_type = symbols.StructureType.create([
        ("dx", symbols.INTEGER_TYPE, symbols.Symbol.Visibility.PUBLIC, None),
        ("area", region_type_sym, symbols.Symbol.Visibility.PUBLIC, None),
        ("levels", region_array_type, symbols.Symbol.Visibility.PUBLIC, None)])
    grid_type_sym = symbols.DataTypeSymbol("grid_type", grid_type)
    grid_var = symbols.DataSymbol("grid", grid_type_sym)
    table.add(grid_type_sym)
    table.add(grid_var)
    return grid_var


def test_sm_constructor():
    ''' Test the StructureMember constructor. '''
    smref = nodes.StructureMember("area")
    assert isinstance(smref, nodes.StructureMember)
    assert smref.name == "area"
    assert smref.children == []


def test_sm_node_str():
    ''' Check the node_str method of the StructureMember class.'''
    kschedule = nodes.KernelSchedule("kname")
    grid_var = create_structure_symbol(kschedule.symbol_table)
    assignment = nodes.Assignment(parent=kschedule)
    grid_ref = nodes.StructureReference.create(grid_var, ['area', 'nx'],
                                               parent=assignment)
    # The first child of the StructureReference is itself a reference to a
    # structure and is therefore a StructureMember
    assert isinstance(grid_ref.children[0], nodes.StructureMember)
    coloredtext = colored("StructureMember", nodes.StructureMember._colour)
    assert coloredtext+"[name:'area']" in grid_ref.children[0].node_str()


def test_sm_can_be_printed():
    '''Test that a StructureMember instance can always be printed
    (i.e. is initialised fully)'''
    kschedule = nodes.KernelSchedule("kname")
    grid_var = create_structure_symbol(kschedule.symbol_table)
    assignment = nodes.Assignment(parent=kschedule)
    grid_ref = nodes.StructureReference.create(grid_var, ['area', 'nx'],
                                               parent=assignment)
    structure_member_ref = grid_ref.children[0]
    assert ("StructureMember[name:'area']\n"
            "Member[name:'nx']" in str(structure_member_ref))


def test_sm_child_validate():
    ''' Check the _validate_child() method of StructureMember. '''
    smr = nodes.StructureMember("area")
    with pytest.raises(GenerationError) as err:
        smr.addchild("hello")
    assert "'str' can't be child 0 of 'StructureMember'" in str(err.value)
    # StructureMember is only permitted to have a single child which must
    # be a Member
    smr.addchild(nodes.Member("nx"))
    assert smr.children[0].name == "nx"
    # Attempting to add a second child should fail
    with pytest.raises(GenerationError) as err:
        smr.addchild(None)
    assert "'NoneType' can't be child 1 of" in str(err.value)


def test_sm_member_property():
    ''' Check the member property of StructureMember. '''
    kschedule = nodes.KernelSchedule("kname")
    grid_var = create_structure_symbol(kschedule.symbol_table)
    assignment = nodes.Assignment(parent=kschedule)
    grid_ref = nodes.StructureReference.create(grid_var, ['area', 'nx'],
                                               parent=assignment)
    smem_ref = grid_ref.member
    assert isinstance(smem_ref, nodes.StructureMember)
    assert isinstance(smem_ref.member, nodes.Member)
    assert smem_ref.member.name == "nx"
    # Break the node's children to check the exception
    smem_ref._children = ["wrong"]
    with pytest.raises(InternalError) as err:
        _ = smem_ref.member
    assert ("StructureMember malformed or incomplete. The first child must "
            "be an instance of Member, but found 'str'" in str(err.value))
