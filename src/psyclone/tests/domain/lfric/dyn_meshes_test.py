# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council
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

''' This module uses pytest to test the DynMeshes class. '''

from __future__ import absolute_import, print_function
import os

from psyclone.dynamo0p3 import DynMeshes
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir.symbols import DataSymbol, DataTypeSymbol


BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "dynamo0p3")
TEST_API = "dynamo0.3"


def test_dyn_meshes_constructor(dist_mem):
    ''' Checks that we can create a DynMeshes object. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    meshes = DynMeshes(invoke, invoke._psy_unique_vars)
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
    meshes = DynMeshes(invoke, invoke._psy_unique_vars)
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
        assert isinstance(sym.datatype, DataTypeSymbol)
        assert sym.datatype.name == "mesh_type"
