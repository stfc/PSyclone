# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Performs py.test tests on the StructureMember PSyIR node. '''

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
    region_type = symbols.StructureType.create(
        [
            symbols.StructureType.ComponentType(
                "nx",
                symbols.ScalarType.integer_type(),
                symbols.Symbol.Visibility.PUBLIC,
                None,
            ),
            symbols.StructureType.ComponentType(
                "ny",
                symbols.ScalarType.integer_type(),
                symbols.Symbol.Visibility.PUBLIC,
                None,
            ),
            symbols.StructureType.ComponentType(
                "domain",
                symbols.DataTypeSymbol("dom_type", symbols.UnresolvedType()),
                symbols.Symbol.Visibility.PUBLIC,
                None,
            ),
        ]
    )
    region_type_sym = symbols.DataTypeSymbol("grid_type", region_type)
    region_array_type = symbols.ArrayType(region_type_sym, [2, 2])
    grid_type = symbols.StructureType.create(
        [
            symbols.StructureType.ComponentType(
                "dx",
                symbols.ScalarType.integer_type(),
                symbols.Symbol.Visibility.PUBLIC,
                None,
            ),
            symbols.StructureType.ComponentType(
                "area", region_type_sym, symbols.Symbol.Visibility.PUBLIC, None
            ),
            symbols.StructureType.ComponentType(
                "levels",
                region_array_type,
                symbols.Symbol.Visibility.PUBLIC,
                None,
            ),
        ]
    )
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
    kschedule = nodes.KernelSchedule.create("kname")
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
    kschedule = nodes.KernelSchedule.create("kname")
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
    kschedule = nodes.KernelSchedule.create("kname")
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
    assert ("StructureMember malformed or incomplete. It must have a first "
            "child that must be a (sub-class of) Member, but found:"
            in str(err.value))
