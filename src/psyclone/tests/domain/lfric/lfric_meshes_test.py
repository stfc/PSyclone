# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module uses pytest to test the LFRicMeshes class. '''

import pytest

from psyclone.domain.lfric import LFRicMeshes, LFRicKern, LFRicLoop
from psyclone.errors import GenerationError
from psyclone.psyir.symbols import DataSymbol, UnsupportedFortranType
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import ColourTrans, LFRicColourTrans

TEST_API = "lfric"


def test_lfric_meshes_constructor(dist_mem):
    ''' Checks that we can create an LFRicMeshes object. '''
    _, invoke = get_invoke("1_single_invoke.f90", dist_mem=dist_mem, idx=0,
                           api=TEST_API)
    meshes = LFRicMeshes(invoke, invoke._psy_unique_vars)
    if dist_mem:
        assert meshes._mesh_tag_names == ["mesh"]
    else:
        assert meshes._mesh_tag_names == []

    # Unlike other collections, the constructor only accepts an Invoke, not
    # a Kernel.
    kern = invoke.schedule.walk(LFRicKern)[0]
    with pytest.raises(TypeError,
                       match="must be created using an LFRicInvoke but got"):
        _ = LFRicMeshes(kern, [])

    # Test when kernel requires properties of the reference element.
    _, invoke = get_invoke("23.3_shared_ref_elem_invoke.f90",
                           dist_mem=dist_mem, idx=0, api=TEST_API)
    meshes = LFRicMeshes(invoke, invoke._psy_unique_vars)
    assert not meshes.intergrid_kernels

    # Because, the kernel requires reference-element properties, we always need
    # a mesh object.
    assert meshes._mesh_tag_names == ["mesh"]

    # For an inter-grid kernel.
    _, invoke = get_invoke("22.2_intergrid_3levels.f90",
                           dist_mem=dist_mem, idx=0, api=TEST_API)
    meshes = LFRicMeshes(invoke, invoke._psy_unique_vars)
    assert meshes._mesh_tag_names == ["mesh_cmap_fld_c", "mesh_fld_f",
                                      "mesh_fld_m"]

    # Check that we reject an invoke that contains both
    # an inter-grid and a general-purpose kernel '''
    with pytest.raises(GenerationError) as err:
        _ = get_invoke("22.3_intergrid_plus_general.f90", idx=0,
                       dist_mem=dist_mem, api=TEST_API)
    assert ("no other kernel types but kernels 'testkern_w2_only_code' in "
            "invoke 'invoke_0' are not inter-grid" in str(err.value))


def test_add_mesh_symbols():
    ''' Checks the _add_mesh_symbols method. '''
    _, invoke = get_invoke("1_single_invoke.f90", dist_mem=False, idx=0,
                           api=TEST_API)
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


@pytest.mark.parametrize("tiling", [True, False])
def test_lfric_meshes_colourmap_init(dist_mem, tiling, fortran_writer):
    '''
    Test colourmap_init() and initialise() for a normal kernel, with and
    without tiling.
    '''
    _, invoke = get_invoke("1_single_invoke.f90", dist_mem=dist_mem, idx=0,
                           api=TEST_API)
    meshes = LFRicMeshes(invoke, invoke._psy_unique_vars)
    table = invoke.schedule.symbol_table
    all_symbols = table.get_symbols()
    # Without colouring, this should be a no-op.
    meshes.colourmap_init()
    new_symbols = table.get_symbols()
    assert new_symbols == all_symbols

    loop = invoke.schedule.walk(LFRicLoop)[0]
    ctrans = LFRicColourTrans()
    # Call apply of the super class as that doesn't call colourmap_init()
    ColourTrans.apply(ctrans, loop, tiling=tiling)
    new_symbols = table.get_symbols()
    if tiling:
        assert set(new_symbols) - set(all_symbols) == {'colour', 'tile'}
    else:
        assert set(new_symbols) - set(all_symbols) == {'colour'}
    invoke.meshes.colourmap_init()
    new_symbols = table.get_symbols()
    if dist_mem:
        if tiling:
            expected_diff = {
                'colour', 'tmap', 'last_halo_tile_per_colour',
                'tile', 'ntilecolours', 'last_halo_cell_per_colour_and_tile'}
        else:
            expected_diff = {'colour', 'cmap', 'ncolour',
                             'last_halo_cell_all_colours'}
    else:
        if tiling:
            expected_diff = {
                'colour', 'mesh', 'mesh_mod', 'mesh_type', 'tile', 'tmap',
                'last_edge_cell_per_colour_and_tile',
                'last_edge_tile_per_colour', 'ntilecolours'}
        else:
            expected_diff = {'colour', 'mesh', 'mesh_mod', 'mesh_type',
                             'cmap', 'ncolour', 'last_edge_cell_all_colours'}

    assert set(new_symbols) - set(all_symbols) == expected_diff

    invoke.meshes.initialise(0)
    output = fortran_writer(invoke.schedule)
    assert """\
  ! Create a mesh object
  mesh => f1_proxy%vspace%get_mesh()""" in output

    if tiling:
        if dist_mem:
            assert "max_halo_depth_mesh = mesh%get_halo_depth()" in output
        else:
            assert """\
  ! Get the tiled colourmap
  ntilecolours = mesh%get_ntilecolours()
  tmap => mesh%get_coloured_tiling_map()""" in output
    else:
        assert """\
  ! Get the colourmap
  ncolour = mesh%get_ncolours()
  cmap => mesh%get_colour_map()""" in output

    # If there are no meshes, `initialise` does nothing so the cursor value
    # remains unchanged.
    invoke.meshes._mesh_tag_names = []
    assert invoke.meshes.initialise(10) == 10


@pytest.mark.parametrize("tiling", [True, False])
def test_lfric_meshes_intergrid_colourmap_init(dist_mem, tiling,
                                               fortran_writer):
    '''
    Test colourmap_init and initialise for an inter-grid kernel, with
    and without tiling.
    '''
    _, invoke = get_invoke("22.2_intergrid_3levels.f90",
                           dist_mem=dist_mem, idx=0, api=TEST_API)
    meshes = LFRicMeshes(invoke, invoke._psy_unique_vars)
    table = invoke.schedule.symbol_table
    all_symbols = table.get_symbols()
    # Without colouring, this should be a no-op.
    meshes.colourmap_init()
    new_symbols = table.get_symbols()
    assert new_symbols == all_symbols

    loop = invoke.schedule.walk(LFRicLoop)[0]
    ctrans = LFRicColourTrans()
    # Call apply of the super class as that doesn't call colourmap_init()
    ColourTrans.apply(ctrans, loop, tiling=tiling)
    new_symbols = table.get_symbols()
    if tiling:
        assert set(new_symbols) - set(all_symbols) == {'colour', 'tile'}
    else:
        assert set(new_symbols) - set(all_symbols) == {'colour'}
    invoke.meshes.colourmap_init()
    new_symbols = table.get_symbols()
    if dist_mem:
        if tiling:
            expected = {'colour', 'ntilecolour_cmap_fld_c', 'tile',
                        'tmap_cmap_fld_c',
                        'last_halo_tile_per_colour_cmap_fld_c',
                        'last_halo_cell_per_colour_and_tile_cmap_fld_c'}
        else:
            expected = {'colour', 'ncolour_cmap_fld_c', 'cmap_cmap_fld_c',
                        'last_halo_cell_all_colours_cmap_fld_c'}
    else:
        if tiling:
            expected = {'colour', 'ntilecolour_cmap_fld_c',
                        'tmap_cmap_fld_c',
                        'last_edge_tile_per_colour_cmap_fld_c',
                        'last_edge_cell_per_colour_and_tile_cmap_fld_c',
                        'tile'}
        else:
            expected = {'colour', 'ncolour_cmap_fld_c', 'cmap_cmap_fld_c',
                        'last_edge_cell_all_colours_cmap_fld_c'}
    assert set(new_symbols) - set(all_symbols) == expected

    invoke.meshes.initialise(0)
    output = fortran_writer(invoke.schedule)

    assert """\
  ! Look-up mesh objects and loop limits for inter-grid kernels
  mesh_fld_m => fld_m_proxy%vspace%get_mesh()""" in output

    if dist_mem:
        assert """\
  max_halo_depth_mesh_fld_m = mesh_fld_m%get_halo_depth()
  mesh_cmap_fld_c => cmap_fld_c_proxy%vspace%get_mesh()
  max_halo_depth_mesh_cmap_fld_c = mesh_cmap_fld_c%get_halo_depth()
  mmap_fld_m_cmap_fld_c => mesh_cmap_fld_c%get_mesh_map(mesh_fld_m)
  cell_map_cmap_fld_c => mmap_fld_m_cmap_fld_c%get_whole_cell_map()
  ncell_fld_m = mesh_fld_m%get_last_halo_cell(depth=2)""" in output
    else:
        assert """\
  mesh_cmap_fld_c => cmap_fld_c_proxy%vspace%get_mesh()
  mmap_fld_m_cmap_fld_c => mesh_cmap_fld_c%get_mesh_map(mesh_fld_m)
  cell_map_cmap_fld_c => mmap_fld_m_cmap_fld_c%get_whole_cell_map()
  ncell_fld_m = fld_m_proxy%vspace%get_ncell()""" in output

    assert """\
  ncpc_fld_m_cmap_fld_c_x = mmap_fld_m_cmap_fld_c%\
get_ntarget_cells_per_source_x()
  ncpc_fld_m_cmap_fld_c_y = mmap_fld_m_cmap_fld_c%\
get_ntarget_cells_per_source_y()""" in output

    if tiling:
        assert """\
  ntilecolour_cmap_fld_c = mesh_cmap_fld_c%get_ntilecolours()
  tmap_cmap_fld_c => mesh_cmap_fld_c%get_coloured_tiling_map()
""" in output
        if dist_mem:
            assert """\
  last_halo_tile_per_colour_cmap_fld_c = mesh_cmap_fld_c%\
get_last_halo_tile_all_colours()
  last_halo_cell_per_colour_and_tile_cmap_fld_c = mesh_cmap_fld_c%\
get_last_halo_cell_all_colours_all_tiles()
  mesh_fld_f => fld_f_proxy%vspace%get_mesh()
  max_halo_depth_mesh_fld_f = mesh_fld_f%get_halo_depth()
""" in output
        else:
            assert ("""\
  last_edge_tile_per_colour_cmap_fld_c = mesh_cmap_fld_c%\
get_last_edge_tile_all_colours()
  last_edge_cell_per_colour_and_tile_cmap_fld_c = mesh_cmap_fld_c%\
get_last_edge_cell_all_colours_all_tiles()
  mesh_fld_f => fld_f_proxy%vspace%get_mesh()
  mmap_fld_f_fld_m => mesh_fld_m%get_mesh_map(mesh_fld_f)
  cell_map_fld_m => mmap_fld_f_fld_m%get_whole_cell_map()
  ncell_fld_f = fld_f_proxy%vspace%get_ncell()
  ncpc_fld_f_fld_m_x = mmap_fld_f_fld_m%get_ntarget_cells_per_source_x()
  ncpc_fld_f_fld_m_y = mmap_fld_f_fld_m%get_ntarget_cells_per_source_y()
""" in output)
    else:
        # Not tiling
        if dist_mem:
            assert """\
  max_halo_depth_mesh_fld_m = mesh_fld_m%get_halo_depth()
  mesh_cmap_fld_c => cmap_fld_c_proxy%vspace%get_mesh()
  max_halo_depth_mesh_cmap_fld_c = mesh_cmap_fld_c%get_halo_depth()
  mmap_fld_m_cmap_fld_c => mesh_cmap_fld_c%get_mesh_map(mesh_fld_m)
  cell_map_cmap_fld_c => mmap_fld_m_cmap_fld_c%get_whole_cell_map()
""" in output

        else:
            assert """\
  last_edge_cell_all_colours_cmap_fld_c = mesh_cmap_fld_c%\
get_last_edge_cell_all_colours()
  mesh_fld_f => fld_f_proxy%vspace%get_mesh()
  mmap_fld_f_fld_m => mesh_fld_m%get_mesh_map(mesh_fld_f)
  cell_map_fld_m => mmap_fld_f_fld_m%get_whole_cell_map()
  ncell_fld_f = fld_f_proxy%vspace%get_ncell()
  ncpc_fld_f_fld_m_x = mmap_fld_f_fld_m%get_ntarget_cells_per_source_x()
  ncpc_fld_f_fld_m_y = mmap_fld_f_fld_m%get_ntarget_cells_per_source_y()
""" in output
