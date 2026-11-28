# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
This module contains the implementation of the LFRicMeshes collection class.

'''

from __future__ import annotations
from typing import TYPE_CHECKING

from psyclone.configuration import Config
from psyclone.domain.lfric.lfric_collection import LFRicCollection
from psyclone.domain.lfric.lfric_constants import LFRicConstants
from psyclone.domain.lfric.lfric_types import LFRicTypes
from psyclone.errors import GenerationError
from psyclone.psyir.nodes import (
    Assignment, Call, Literal, Reference, StructureReference)
from psyclone.psyir.symbols import (
    ArrayType, ContainerSymbol, DataSymbol, DataTypeSymbol, ImportInterface,
    ScalarType, UnresolvedType, UnsupportedFortranType)

if TYPE_CHECKING:
    from psyclone.domain.lfric.lfric_invoke import LFRicInvoke
    from psyclone.lfric import LFRicInterGrid, LFRicKernelArgument


class LFRicMeshes(LFRicCollection):
    '''
    Holds all mesh-related information (including colour maps if
    required).  If there are no inter-grid kernels then there is only
    one mesh object required (when calling kernels with operates_on==domain,
    colouring, doing distributed memory or querying the reference element).
    However, kernels performing inter-grid operations require multiple mesh
    objects as well as mesh maps and other quantities.

    There are two types of inter-grid operation; the first is "prolongation"
    where a field on a coarse mesh is mapped onto a fine mesh. The second
    is "restriction" where a field on a fine mesh is mapped onto a coarse
    mesh.

    :param invoke: the Invoke for which to extract information on all
                   required inter-grid operations.
    :param unique_psy_vars: list of arguments to the PSy-layer routine.

    '''
    def __init__(self,
                 invoke: LFRicInvoke,
                 unique_psy_vars: list[LFRicKernelArgument]):
        # An LFRicInvoke manages collections so we have to import here to
        # avoid circular dependency.
        # pylint: disable-next=import-outside-toplevel
        from psyclone.domain.lfric.lfric_invoke import LFRicInvoke
        # Unlike a normal LFRicCollection, this one can only be associated
        # with an invoke.
        if not isinstance(invoke, LFRicInvoke):
            raise TypeError(f"LFRicMeshes must be created using an "
                            f"LFRicInvoke but got '{type(invoke).__name__}'")
        super().__init__(invoke)
        # List of names of unique mesh variables referenced in the Invoke
        self._mesh_tag_names = []
        # Whether or not the associated Invoke requires colourmap information
        self._needs_colourmap = False
        self._needs_colourmap_halo = False
        self._needs_colourtilemap = False
        self._needs_colourtilemap_halo = False
        # Set used to generate a list of the unique mesh objects
        _name_set = set()

        # Find the first non-scalar argument to this PSy layer routine. We
        # will use this to look-up the mesh if there are no inter-grid
        # kernels in this invoke.
        self._first_var = None
        for var in unique_psy_vars:
            if not (var.is_scalar or var.is_scalar_array):
                self._first_var = var
                break

        # Loop over all kernel calls in the schedule. Keep a list of
        # any non-intergrid kernels so that we can generate a verbose error
        # message if necessary.
        non_intergrid_kernels = []
        has_intergrid = False
        for call in self._invoke.schedule.coded_kernels():

            if (call.reference_element.properties or call.mesh.properties or
                    call.iterates_over == "domain" or call.cma_operation):
                _name_set.add("mesh")

            if not call.is_intergrid:
                non_intergrid_kernels.append(call)
            else:
                has_intergrid = True
                # Create and store the names of the associated mesh objects
                _name_set.add(f"mesh_{call._intergrid_ref.fine.name}")
                _name_set.add(f"mesh_{call._intergrid_ref.coarse.name}")

        # If we found a mixture of both inter-grid and non-inter-grid kernels
        # then we reject the invoke()
        if non_intergrid_kernels and has_intergrid:
            raise GenerationError(
                f"An invoke containing inter-grid kernels must contain no "
                f"other kernel types but kernels "
                f"'{''', '''.join([c.name for c in non_intergrid_kernels])}' "
                f"in invoke '{invoke.name}' are not inter-grid kernels.")

        # If distributed memory is enabled then we will need at least
        # one mesh object if we have one or more kernels that operate
        # on cell-columns or are doing redundant computation for a
        # kernel that operates on dofs. Since the latter condition
        # comes about through the application of a transformation, we
        # don't yet know whether or not a mesh is required. Therefore,
        # the only solution is to assume that a mesh object is
        # required if distributed memory is enabled. We also require a
        # mesh object if any of the kernels require properties of
        # either the reference element or the mesh. (Colourmaps also
        # require a mesh object but that is handled in colourmap_init().)
        if not _name_set and Config.get().distributed_memory:
            # We didn't already have a requirement for a mesh so add one now.
            _name_set.add("mesh")

        self._add_mesh_symbols(list(_name_set))

    def _add_mesh_symbols(self, mesh_tags: list[str]) -> None:
        '''
        Add DataSymbols for the supplied list of mesh names and store the
        corresponding list of tags.

        A ContainerSymbol is created for the LFRic mesh module and a TypeSymbol
        for the mesh type. If distributed memory is enabled then a DataSymbol
        to hold the maximum halo depth is created for each mesh.

        :param mesh_tags: tag names for every mesh object required.

        '''
        if not mesh_tags:
            return

        self._mesh_tag_names = sorted(mesh_tags)

        # Look up the names of the module and type for the mesh object
        # from the LFRic constants class.
        const = LFRicConstants()
        mmod = const.MESH_TYPE_MAP["mesh"]["module"]
        mtype = const.MESH_TYPE_MAP["mesh"]["type"]
        # Create a Container symbol for the module
        csym = self.symtab.find_or_create_tag(
            mmod, symbol_type=ContainerSymbol)
        # Create a TypeSymbol for the mesh type
        mtype_sym = self.symtab.find_or_create_tag(
            mtype, symbol_type=DataTypeSymbol,
            datatype=UnresolvedType(),
            interface=ImportInterface(csym))

        name_list = []
        # Use the sorted list of names to ensure reproducible output.
        for name in self._mesh_tag_names:
            dt = UnsupportedFortranType(
                f"type({mtype_sym.name}), pointer :: {name} => null()")
            name_list.append(self.symtab.find_or_create_tag(
                name, symbol_type=DataSymbol, datatype=dt).name)

        if Config.get().distributed_memory:
            # If distributed memory is enabled then we require a variable
            # holding the maximum halo depth for each mesh.
            for name in self._mesh_tag_names:
                var_name = f"max_halo_depth_{name}"
                self.symtab.find_or_create(
                    var_name, tag=var_name,
                    symbol_type=DataSymbol,
                    datatype=LFRicTypes("LFRicIntegerScalarDataType")()
                )

    def colourmap_init(self) -> None:
        '''
        Sets-up information on any required colourmaps. Since colouring is
        applied by Transformations, this method is called as the final step
        of LFRicColourTrans.apply().

        '''
        # pylint: disable=too-many-locals
        const = LFRicConstants()
        non_intergrid_kern = None

        for call in [call for call in self._invoke.schedule.coded_kernels() if
                     call.is_coloured()]:
            # Keep a record of whether or not any kernels (loops) in this
            # invoke have been coloured and, if so, whether the associated loop
            # is tiled or it goes into the halo.
            loop = call.parent.parent
            is_tiled = loop.loop_type == "cells_in_tile"
            if is_tiled:
                has_halo = (loop.parent.parent.upper_bound_name in
                            const.HALO_ACCESS_LOOP_BOUNDS)
                if has_halo:
                    self._needs_colourtilemap_halo = True
                else:
                    self._needs_colourtilemap = True
            else:
                has_halo = (loop.upper_bound_name in
                            const.HALO_ACCESS_LOOP_BOUNDS)
                if has_halo:
                    self._needs_colourmap_halo = True
                else:
                    self._needs_colourmap = True

            if not call.is_intergrid:
                non_intergrid_kern = call
                continue

            # This is an inter-grid kernel so look-up the names of
            # the colourmap variables associated with the coarse
            # mesh (since that determines the iteration space).
            carg_name = call._intergrid_ref.coarse.name

            # We use different variables if it is a tiled or a regular
            # colour map
            if not is_tiled:
                base_name = "cmap_" + carg_name
                array_type = ArrayType(
                    LFRicTypes("LFRicRealScalarDataType")(),
                    [ArrayType.Extent.DEFERRED]*2)
                colour_map = self.symtab.find_or_create_tag(
                    base_name,
                    symbol_type=DataSymbol,
                    datatype=UnsupportedFortranType(
                        f"integer(kind=i_def), pointer, dimension(:,:) :: "
                        f"{base_name} => null()",
                        partial_datatype=array_type))
                # No. of colours
                base_name = "ncolour_" + carg_name
                ncolours = self.symtab.find_or_create_tag(
                    base_name,
                    symbol_type=DataSymbol,
                    datatype=LFRicTypes("LFRicIntegerScalarDataType")()
                )
                # Array holding the last cell of a given colour.
                if (Config.get().distributed_memory and
                        not call.all_updates_are_writes):
                    # This will require a loop into the halo and so the array
                    # is 2D (indexed by colour *and* halo depth).
                    base_name = "last_halo_cell_all_colours_" + carg_name
                    last_cell = self.symtab.find_or_create_tag(
                        base_name,
                        symbol_type=DataSymbol,
                        datatype=ArrayType(
                                LFRicTypes("LFRicIntegerScalarDataType")(),
                                [ArrayType.Extent.DEFERRED]*2))
                else:
                    # Array holding the last edge cell of a given colour. Just
                    # 1D as indexed by colour only.
                    base_name = "last_edge_cell_all_colours_" + carg_name
                    last_cell = self.symtab.find_or_create_tag(
                        base_name,
                        symbol_type=DataSymbol,
                        datatype=ArrayType(
                                LFRicTypes("LFRicIntegerScalarDataType")(),
                                [ArrayType.Extent.DEFERRED]*1))
                # Add these symbols into the LFRicInterGrid entry for this
                # kernel
                call._intergrid_ref.set_colour_info(colour_map, ncolours,
                                                    last_cell)
            else:
                # Tiled colour map
                base_name = "tmap_" + carg_name
                tilecolour_map = self.symtab.find_or_create_tag(
                    base_name, symbol_type=DataSymbol,
                    datatype=UnsupportedFortranType(
                        f"integer(kind=i_def), pointer :: {base_name}(:,:,:)"))
                base_name = "ntilecolour_" + carg_name
                ntilecolours = self.symtab.find_or_create(
                                    base_name, tag=base_name,
                                    symbol_type=DataSymbol,
                                    datatype=LFRicTypes(
                                        "LFRicIntegerScalarDataType")())
                # Array holding the last cell of a given colour.
                if (Config.get().distributed_memory and
                        not call.all_updates_are_writes):
                    # This will require a loop into the halo and so the array
                    # is 2D (indexed by colour *and* halo depth).
                    base_name = "last_halo_tile_per_colour_" + carg_name
                    last_tile = self.symtab.find_or_create(
                        base_name, tag=base_name, symbol_type=DataSymbol,
                        datatype=ArrayType(
                            LFRicTypes("LFRicIntegerScalarDataType")(),
                            2*[ArrayType.Extent.DEFERRED]))
                    base_name = ("last_halo_cell_per_colour_and_tile_" +
                                 carg_name)
                    last_cell_tile = self.symtab.find_or_create(
                        base_name, tag=base_name, symbol_type=DataSymbol,
                        datatype=ArrayType(
                            LFRicTypes("LFRicIntegerScalarDataType")(),
                            3*[ArrayType.Extent.DEFERRED]))
                else:
                    # Array holding the last edge cell of a given colour. Just
                    # 1D as indexed by colour only.
                    base_name = "last_edge_tile_per_colour_" + carg_name
                    last_tile = self.symtab.find_or_create(
                        base_name, tag=base_name, symbol_type=DataSymbol,
                        datatype=ArrayType(
                            LFRicTypes("LFRicIntegerScalarDataType")(),
                            [ArrayType.Extent.DEFERRED]))
                    base_name = ("last_edge_cell_per_colour_and_tile_"
                                 + carg_name)
                    last_cell_tile = self.symtab.find_or_create(
                        base_name, tag=base_name, symbol_type=DataSymbol,
                        datatype=ArrayType(
                            LFRicTypes("LFRicIntegerScalarDataType")(),
                            2*[ArrayType.Extent.DEFERRED]))
                # Add these symbols into the dictionary entry for this
                # inter-grid kernel
                call._intergrid_ref.set_tilecolour_info(
                    tilecolour_map, ntilecolours, last_tile, last_cell_tile)

        if non_intergrid_kern and (self._needs_colourmap or
                                   self._needs_colourmap_halo):
            # There aren't any inter-grid kernels but we do need colourmap
            # information and that means we'll need a mesh object
            self._add_mesh_symbols(["mesh"])
            # This creates the colourmap information for this invoke if we
            # don't already have one.
            colour_map = non_intergrid_kern.colourmap
            # No. of colours
            ncolours = self.symtab.find_or_create(
                "ncolour", tag="ncolour",
                symbol_type=DataSymbol,
                datatype=LFRicTypes("LFRicIntegerScalarDataType")()
            ).name
            if self._needs_colourmap_halo:
                self.symtab.find_or_create(
                    "last_halo_cell_all_colours",
                    symbol_type=DataSymbol,
                    datatype=ArrayType(
                            LFRicTypes("LFRicIntegerScalarDataType")(),
                            [ArrayType.Extent.DEFERRED]*2),
                    tag="last_halo_cell_all_colours")
            if self._needs_colourmap:
                self.symtab.find_or_create(
                    "last_edge_cell_all_colours",
                    symbol_type=DataSymbol,
                    datatype=ArrayType(
                            LFRicTypes("LFRicIntegerScalarDataType")(),
                            [ArrayType.Extent.DEFERRED]*1),
                    tag="last_edge_cell_all_colours")
        if non_intergrid_kern and (self._needs_colourtilemap or
                                   self._needs_colourtilemap_halo):
            # There aren't any inter-grid kernels but we do need colourtilemap
            # information and that means we'll need a mesh object
            self._add_mesh_symbols(["mesh"])
            # Creates the colourtilemap information for this invoke if we
            # don't already have one.
            colour_map = non_intergrid_kern.tilecolourmap
            # Create the No. of colours over tiles
            _ = self.symtab.find_or_create_tag(
                "ntilecolours",
                symbol_type=DataSymbol,
                datatype=LFRicTypes("LFRicIntegerScalarDataType")()
            )
            if self._needs_colourtilemap_halo:
                self.symtab.find_or_create(
                    "last_halo_tile_per_colour",
                    symbol_type=DataSymbol,
                    datatype=ArrayType(
                            LFRicTypes("LFRicIntegerScalarDataType")(),
                            [ArrayType.Extent.DEFERRED]*2),
                    tag="last_halo_tile_per_colour")
                self.symtab.find_or_create(
                    "last_halo_cell_per_colour_and_tile",
                    symbol_type=DataSymbol,
                    datatype=ArrayType(
                            LFRicTypes("LFRicIntegerScalarDataType")(),
                            [ArrayType.Extent.DEFERRED]*3),
                    tag="last_halo_cell_per_colour_and_tile")
            if self._needs_colourtilemap:
                self.symtab.find_or_create_tag(
                    "last_edge_tile_per_colour",
                    symbol_type=DataSymbol,
                    datatype=ArrayType(
                            LFRicTypes("LFRicIntegerScalarDataType")(),
                            [ArrayType.Extent.DEFERRED]*1))
                self.symtab.find_or_create_tag(
                    "last_edge_cell_per_colour_and_tile",
                    symbol_type=DataSymbol,
                    datatype=ArrayType(
                            LFRicTypes("LFRicIntegerScalarDataType")(),
                            [ArrayType.Extent.DEFERRED]*2))

    def invoke_declarations(self) -> None:
        '''
        Declare variables specific to mesh objects.

        '''
        if not self.intergrid_kernels:
            return

        const = LFRicConstants()
        mmap_type = const.MESH_TYPE_MAP["mesh_map"]["type"]
        mmap_mod = const.MESH_TYPE_MAP["mesh_map"]["module"]

        # Create a Container symbol for the module
        csym = self.symtab.find_or_create_tag(
            mmap_mod, symbol_type=ContainerSymbol)
        # Create a TypeSymbol for the mesh type
        self.symtab.find_or_create_tag(
            mmap_type, symbol_type=DataTypeSymbol,
            datatype=UnresolvedType(),
            interface=ImportInterface(csym))

    def initialise(self, cursor: int) -> int:
        '''
        Initialise parameters specific to inter-grid kernels.

        :param cursor: position where to add the next initialisation
            statements.

        :returns: Updated cursor value.

        '''
        # pylint: disable=too-many-branches
        # If we haven't got any need for a mesh in this invoke then we
        # don't do anything
        if not self._mesh_tag_names:
            return cursor

        symtab = self._invoke.schedule.symbol_table

        if len(self._mesh_tag_names) == 1:
            # We only require one mesh object which means that this invoke
            # contains no inter-grid kernels (which would require at least 2)
            mesh_sym = symtab.lookup_with_tag(self._mesh_tag_names[0])
            assignment = Assignment.create(
                lhs=Reference(mesh_sym),
                rhs=self._first_var.generate_method_call("get_mesh"),
                is_pointer=True)
            assignment.preceding_comment = "Create a mesh object"
            self._invoke.schedule.addchild(assignment, cursor)
            cursor += 1
            if Config.get().distributed_memory:
                # If distributed memory is enabled then we need the maximum
                # halo depth.
                depth_sym = self.symtab.lookup_with_tag(
                    f"max_halo_depth_{self._mesh_tag_names[0]}")
                self._invoke.schedule.addchild(Assignment.create(
                    lhs=Reference(depth_sym),
                    rhs=Call.create(StructureReference.create(
                        mesh_sym, ["get_halo_depth"]))),
                    cursor)
                cursor += 1
            if self._needs_colourmap or self._needs_colourmap_halo:
                # Look-up variable names for colourmap and number of colours
                cmap = self.symtab.find_or_create_tag("cmap")
                ncolour = self.symtab.find_or_create_tag("ncolour")
                # Get the number of colours
                assignment = Assignment.create(
                        lhs=Reference(ncolour),
                        rhs=Call.create(StructureReference.create(
                            mesh_sym, ["get_ncolours"])))
                assignment.preceding_comment = "Get the colourmap"
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
                # Get the colour map
                assignment = Assignment.create(
                        lhs=Reference(cmap),
                        rhs=Call.create(StructureReference.create(
                            mesh_sym, ["get_colour_map"])),
                        is_pointer=True)
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
            if self._needs_colourtilemap or self._needs_colourtilemap_halo:
                # Look-up variable names for colourmap and number of colours
                tmap = self.symtab.lookup_with_tag("tilecolourmap")
                ntc = self.symtab.lookup_with_tag("ntilecolours")
                # Get the number of colours
                assignment = Assignment.create(
                        lhs=Reference(ntc),
                        rhs=Call.create(StructureReference.create(
                            mesh_sym, ["get_ntilecolours"])))
                assignment.preceding_comment = "Get the tiled colourmap"
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
                # Get the colour map
                assignment = Assignment.create(
                        lhs=Reference(tmap),
                        rhs=Call.create(StructureReference.create(
                            mesh_sym, ["get_coloured_tiling_map"])),
                        is_pointer=True)
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1

        # Keep a list of quantities that we've already initialised so
        # that we don't generate duplicate assignments
        initialised = []

        comment_cursor = cursor
        # Loop over the LFRicInterGrid objects
        for dig in self.intergrid_kernels:
            # We need pointers to both the coarse and the fine mesh as well
            # as the maximum halo depth for each.
            fine_mesh = self.symtab.find_or_create_tag(f"mesh_{dig.fine.name}")
            coarse_mesh = self.symtab.find_or_create_tag(
                                            f"mesh_{dig.coarse.name}")
            if fine_mesh not in initialised:
                initialised.append(fine_mesh)
                assignment = Assignment.create(
                        lhs=Reference(fine_mesh),
                        rhs=dig.fine.generate_method_call("get_mesh"),
                        is_pointer=True)
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
                if Config.get().distributed_memory:
                    max_halo_f_mesh = (
                        self.symtab.find_or_create_tag(
                            f"max_halo_depth_mesh_{dig.fine.name}"))
                    assignment = Assignment.create(
                            lhs=Reference(max_halo_f_mesh),
                            rhs=Call.create(StructureReference.create(
                                fine_mesh, ["get_halo_depth"])))
                    self._invoke.schedule.addchild(assignment, cursor)
                    cursor += 1
            if coarse_mesh not in initialised:
                initialised.append(coarse_mesh)
                assignment = Assignment.create(
                        lhs=Reference(coarse_mesh),
                        rhs=dig.coarse.generate_method_call("get_mesh"),
                        is_pointer=True)
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
                if Config.get().distributed_memory:
                    max_halo_c_mesh = (
                        self.symtab.find_or_create_tag(
                            f"max_halo_depth_mesh_{dig.coarse.name}"))
                    assignment = Assignment.create(
                            lhs=Reference(max_halo_c_mesh),
                            rhs=Call.create(StructureReference.create(
                                coarse_mesh, ["get_halo_depth"])))
                    self._invoke.schedule.addchild(assignment, cursor)
                    cursor += 1
            # We also need a pointer to the mesh map which we get from
            # the coarse mesh
            if dig.mmap not in initialised:
                initialised.append(dig.mmap)
                digmmap = self.symtab.lookup(dig.mmap)
                assignment = Assignment.create(
                        lhs=Reference(digmmap),
                        rhs=Call.create(StructureReference.create(
                                coarse_mesh, ["get_mesh_map"]),
                            arguments=[Reference(fine_mesh)]),
                        is_pointer=True)
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1

            # Cell map. This is obtained from the mesh map.
            if dig.cell_map not in initialised:
                initialised.append(dig.cell_map)
                digcellmap = self.symtab.lookup(dig.cell_map)
                assignment = Assignment.create(
                        lhs=Reference(digcellmap),
                        rhs=Call.create(StructureReference.create(
                            digmmap, ["get_whole_cell_map"])),
                        is_pointer=True)
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1

            # Number of cells in the fine mesh
            if dig.ncell_fine not in initialised:
                initialised.append(dig.ncell_fine)
                digncellfine = self.symtab.lookup(dig.ncell_fine)
                if Config.get().distributed_memory:
                    # TODO this hardwired depth of 2 will need changing in
                    # order to support redundant computation
                    assignment = Assignment.create(
                            lhs=Reference(digncellfine),
                            rhs=Call.create(StructureReference.create(
                                fine_mesh, ["get_last_halo_cell"])))
                    assignment.rhs.append_named_arg(
                        "depth", Literal("2", ScalarType.integer_type()))
                    self._invoke.schedule.addchild(assignment, cursor)
                    cursor += 1
                else:
                    assignment = Assignment.create(
                            lhs=Reference(digncellfine),
                            rhs=dig.fine.generate_method_call("get_ncell"))
                    self._invoke.schedule.addchild(assignment, cursor)
                    cursor += 1

            # Number of fine cells per coarse cell in x.
            if dig.ncellpercellx not in initialised:
                initialised.append(dig.ncellpercellx)
                digncellpercellx = self.symtab.lookup(dig.ncellpercellx)
                assignment = Assignment.create(
                        lhs=Reference(digncellpercellx),
                        rhs=Call.create(StructureReference.create(
                            digmmap, ["get_ntarget_cells_per_source_x"])))
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1

            # Number of fine cells per coarse cell in y.
            if dig.ncellpercelly not in initialised:
                initialised.append(dig.ncellpercelly)
                digncellpercelly = self.symtab.lookup(dig.ncellpercelly)
                assignment = Assignment.create(
                        lhs=Reference(digncellpercelly),
                        rhs=Call.create(StructureReference.create(
                            digmmap, ["get_ntarget_cells_per_source_y"])))
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1

            # Colour map for the coarse mesh (if required)
            if dig.colourmap_symbol:
                # Number of colours
                assignment = Assignment.create(
                        lhs=Reference(dig.ncolours_var_symbol),
                        rhs=Call.create(StructureReference.create(
                            coarse_mesh, ["get_ncolours"])))
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
                # Colour map itself
                assignment = Assignment.create(
                        lhs=Reference(dig.colourmap_symbol),
                        rhs=Call.create(StructureReference.create(
                            coarse_mesh, ["get_colour_map"])),
                        is_pointer=True)
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
                # Last halo/edge cell per colour.
                sym = dig.last_cell_var_symbol
                if len(sym.datatype.shape) == 2:
                    # Array is 2D so is a halo access.
                    name = "get_last_halo_cell_all_colours"
                else:
                    # Array is just 1D so go to the last edge cell.
                    name = "get_last_edge_cell_all_colours"
                assignment = Assignment.create(
                        lhs=Reference(sym),
                        rhs=Call.create(StructureReference.create(
                            coarse_mesh, [name])))
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
            # Colour map for the coarse mesh (if required)
            if dig.tilecolourmap_symbol:
                # Number of colours
                assignment = Assignment.create(
                        lhs=Reference(dig.ntilecolours_var_symbol),
                        rhs=Call.create(StructureReference.create(
                            coarse_mesh, ["get_ntilecolours"])))
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
                # Colour map itself
                assignment = Assignment.create(
                        lhs=Reference(dig.tilecolourmap_symbol),
                        rhs=Call.create(StructureReference.create(
                            coarse_mesh, ["get_coloured_tiling_map"])),
                        is_pointer=True)
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
                # Last halo/edge tile per colour.
                sym = dig.last_tile_var_symbol
                if len(sym.datatype.shape) == 2:
                    # Array is 2D so is a halo access.
                    name = "get_last_halo_tile_all_colours"
                else:
                    # Array is just 1D so go to the last edge cell.
                    name = "get_last_edge_tile_all_colours"
                assignment = Assignment.create(
                        lhs=Reference(sym),
                        rhs=Call.create(StructureReference.create(
                            coarse_mesh, [name])))
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
                # Last halo/edge cell per colour and tile.
                sym = dig.last_cell_tile_var_symbol
                if len(sym.datatype.shape) == 3:
                    # Array is 3D so is a halo access.
                    name = "get_last_halo_cell_all_colours_all_tiles"
                else:
                    # Array is just 2D so go to the last edge cell.
                    name = "get_last_edge_cell_all_colours_all_tiles"
                assignment = Assignment.create(
                        lhs=Reference(sym),
                        rhs=Call.create(StructureReference.create(
                            coarse_mesh, [name])))
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
        if cursor != comment_cursor:
            self._invoke.schedule[comment_cursor].preceding_comment = (
                "Look-up mesh objects and loop limits for inter-grid kernels")

        return cursor

    @property
    def intergrid_kernels(self) -> list[LFRicInterGrid]:
        '''
        :returns: A list of objects describing the intergrid kernels used in
            this invoke.
        '''
        intergrids = []
        for call in self._invoke.schedule.coded_kernels():
            if call.is_intergrid:
                intergrids.append(call._intergrid_ref)
        return intergrids


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for.
__all__ = ['LFRicMeshes']
