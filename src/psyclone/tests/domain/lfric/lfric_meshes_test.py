# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module uses pytest to test the LFRicMeshes class. '''

import os

from psyclone.lfric import LFRicMeshes
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir.symbols import DataSymbol, UnsupportedFortranType


BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "lfric")
TEST_API = "lfric"


def test_lfric_meshes_constructor(dist_mem):
    ''' Checks that we can create an LFRicMeshes object. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    meshes = LFRicMeshes(invoke, invoke._psy_unique_vars)
    if dist_mem:
        assert meshes._mesh_tag_names == ["mesh"]
    else:
        assert meshes._mesh_tag_names == []


def test_add_mesh_symbols():
    ''' Checks the _add_mesh_symbols method. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    meshes = LFRicMeshes(invoke, invoke._psy_unique_vars)
    # Check that supplying an empty list does nothing.
    meshes._add_mesh_symbols([])
    assert meshes._mesh_tag_names == []
    # Now supply a valid list.
    mesh_names = ["mesh_cs", "mesh_fn", "mesh_an"]
    meshes._add_mesh_symbols(mesh_names)
    # Call should have stored the (sorted) list of tag names.
    assert meshes._mesh_tag_names == sorted(mesh_names)
    # It should also have added appropriate symbols to the symbol table.
    sym_table = invoke.schedule.symbol_table
    for tag in mesh_names:
        sym = sym_table.lookup(tag)
        assert isinstance(sym, DataSymbol)
        assert isinstance(sym.datatype, UnsupportedFortranType)
        assert "mesh_type" in sym.datatype.type_text
