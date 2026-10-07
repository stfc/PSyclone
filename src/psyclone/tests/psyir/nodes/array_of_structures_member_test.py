# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains pytest tests for the ArrayOfStructuresMember
    class. '''

import pytest
from psyclone.psyir import symbols, nodes
from psyclone.errors import GenerationError, InternalError
from psyclone.tests.utilities import check_links


def test_asmr_constructor():
    ''' Test that we can construct an ArrayOfStructuresMember. '''
    # For this we need a structure that contains an array of structures.
    asmr = nodes.ArrayOfStructuresMember("regions")
    assert isinstance(asmr, nodes.ArrayOfStructuresMember)
    assert len(asmr.children) == 0
    check_links(asmr, asmr.children)


def test_asmr_create():
    ''' Test the create method of ArrayOfStructuresMember. '''
    asmr = nodes.ArrayOfStructuresMember.create(
        "regions", [nodes.Literal("3", symbols.ScalarType.integer_type())],
        nodes.Member("sub_mesh"))
    assert len(asmr.children) == 2
    assert isinstance(asmr.children[0], nodes.Member)
    assert asmr.children[1].value == "3"


def test_asmr_validate_child():
    ''' Test the _validate_child method of ArrayOfStructuresMember. '''
    asmr = nodes.ArrayOfStructuresMember("regions")
    with pytest.raises(GenerationError) as err:
        asmr.addchild("wrong")
    assert ("'str' can't be child 0 of 'ArrayOfStructuresMember'" in
            str(err.value))
    asmr.addchild(nodes.Member("sub_mesh"))
    assert isinstance(asmr.children[0], nodes.Member)
    with pytest.raises(GenerationError) as err:
        asmr.addchild("2")
    assert ("'str' can't be child 1 of 'ArrayOfStructuresMember'" in
            str(err.value))
    idx = nodes.Reference(
        symbols.DataSymbol("idx", symbols.ScalarType.integer_type()))
    asmr.addchild(idx)
    assert asmr.children[1] is idx


def test_asmr_indices():
    ''' Test the indices property of ArrayOfStructuresMember. '''
    asmr = nodes.ArrayOfStructuresMember.create(
        "regions", [nodes.Literal("3", symbols.ScalarType.integer_type())],
        nodes.Member("sub_mesh"))
    indices = asmr.indices
    assert len(indices) == 1
    assert isinstance(indices[0], nodes.Literal)
    assert indices[0].value == "3"
    # Break the children of the node to check that we get the expected
    # error.
    asmr._children = [asmr._children[0]]
    with pytest.raises(InternalError) as err:
        asmr.indices
    assert ("must have one or more children representing array-index "
            "expressions but found none" in str(err.value))
    asmr._children = [asmr._children[0], "hello"]
    with pytest.raises(InternalError) as err:
        asmr.indices
    assert ("malformed or incomplete: child 1 must represent an array-index "
            "expression but found 'str' instead of psyir.nodes.DataNode or "
            "Range" in str(err.value))
