# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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
# Author: R. W. Ford, STFC Daresbury Lab

'''This module implements a class that takes LFRic kernel metadata as
input and outputs the expected kernel subroutine arguments (based on
this metadata) within an LFRic PSyIR symbol table.

'''
from psyclone.domain import lfric
from psyclone.errors import InternalError
from psyclone.psyir import nodes, symbols

# TODO: many symbols here are not declared in LFRicTypes


class FormalKernelArgsFromMetadata(lfric.MetadataToArgumentsRules):
    '''Provides the expected kernel subroutine arguments within an LFRic
    PSyIR symbol table based in the provided LFRic Kernel metadata.

    '''
    _access_lookup = {
        "gh_read": symbols.ArgumentInterface.Access.READ,
        "gh_write": symbols.ArgumentInterface.Access.WRITE,
        "gh_readwrite": symbols.ArgumentInterface.Access.READWRITE,
        "gh_inc": symbols.ArgumentInterface.Access.READWRITE,
        "gh_sum": symbols.ArgumentInterface.Access.READWRITE}

    # It is clearer to use 'symbol_table' here rather than 'info'
    # pylint: disable=arguments-renamed
    @classmethod
    def _initialise(cls, symbol_table):
        '''Initialise any additional state for this class.

        :param symbol_table: the symbol table that the kernel \
            arguments should be added to. If it is set to None then a new \
            symbol table is created.
        :type symbol_table: \
            Optional[:py:class:`psyclone.psyir.symbols.SymbolTable`]

        :raises TypeError: if the symbol_table argument is an \
            unexpected type.

        '''
        # TODO: Should this be an LFRicSymbolTable?
        if symbol_table is None:
            symbol_table = symbols.SymbolTable()
        elif not isinstance(symbol_table, symbols.SymbolTable):
            raise TypeError(
                f"Expecting the optional info argument to be a symbol table "
                f"but found {type(symbol_table).__name__}.")
        # We could use super()._initialise(symbol_table) here but it is simpler
        # to just set the value of _info directly and it avoids pylint
        # complaining.
        cls._info = symbol_table

    @classmethod
    def _cell_position(cls):
        ''''cell' argument providing the cell position. This is an integer of
        type i_def and has intent in.

        '''
        cls._add_lfric_symbol_name("CellPositionDataSymbol", "cell")

    @classmethod
    def _mesh_height(cls):
        ''''nlayers' argument providing the mesh height. This is an integer
        of type i_def and has intent in.

        '''
        # TODO: if self._kern.iterates_over not in ["cell_column", "domain"]: return
        cls._add_lfric_symbol_name("MeshHeightDataSymbol", "nlayers")

    @classmethod
    def _mesh_ncell2d_no_halos(cls):
        ''''ncell_2d_no_halos' argument providing the number of columns in
        the mesh ignoring halos. This is an integer of type i_def and
        has intent in.

        '''
        cls._add_lfric_symbol_name(
            "LFRicIntegerScalarDataSymbol", "ncell_2d_no_halos")

    @classmethod
    def _mesh_ncell2d(cls):
        ''''ncell_2d' argument providing the number of columns in the mesh
        including halos. This is an integer of type i_def and has
        intent in.

        '''
        # This symbol might be used to dimension an array in another
        # method and therefore could have already been declared.
        symbol = cls._get_or_create_lfric_symbol(
            "LFRicIntegerScalarDataSymbol", "ncell_2d")
        cls._append_to_arg_list(symbol)

    @classmethod
    def _cell_map(cls):
        '''Four arguments providing a mapping from coarse to fine mesh for the
        current column. The first is 'cell_map', an integer array of
        rank two, kind i_def and intent in. This is followed by its
        extents, 'ncell_f_per_c_x' and 'ncell_f_per_c_y' the numbers
        of fine cells per coarse cell in the x and y directions,
        respectively. These are integers of kind i_def and have intent
        in. Lastly is 'ncell_f', the number of cells (columns) in the
        fine mesh. This is an integer of kind i_def and has intent in.

        '''
        # Create the cell_map array extent symbols
        x_symbol = cls._create_lfric_symbol(
            "LFRicIntegerScalarDataSymbol", "ncell_f_per_c_x")
        y_symbol = cls._create_lfric_symbol(
            "LFRicIntegerScalarDataSymbol", "ncell_f_per_c_y")
        # Use the array extent symbols to create the cell_map array symbol
        scalar_type = cls._create_datatype("LFRicIntegerScalarDataType")
        array_type = symbols.ArrayType(
            scalar_type, [nodes.Reference(x_symbol),
                          nodes.Reference(y_symbol)])
        interface = symbols.ArgumentInterface(
            symbols.ArgumentInterface.Access.READ)
        cell_map_symbol = symbols.DataSymbol(
            "cell_map", array_type, interface=interface)
        cls._add_to_symbol_table(cell_map_symbol)
        # Add the symbols to the symbol table argument list in the
        # required order
        cls._append_to_arg_list(cell_map_symbol)
        cls._append_to_arg_list(x_symbol)
        cls._append_to_arg_list(y_symbol)
        # TODO Should be get or create
        cls._add_lfric_symbol_name("LFRicIntegerScalarDataSymbol", "ncell_f")

    @classmethod
    def _scalar(cls, meta_arg):
        '''Argument providing an LFRic scalar value.

        :param meta_arg: the metadata associated with this scalar argument.
        :type meta_arg: \
            :py:class:`psyclone.domain.lfric.kernel.ScalarArgMetadata`

        '''
        # TODO: This should be a meta_arg function or a mapping, or
        # part of LFRicTypes?
        datatype = meta_arg.datatype[3:]
        # TODO: The name should come from meta_arg classes, or part of
        # LFRicTypes?
        datatype_char = meta_arg.datatype[3:4]
        meta_arg_index = cls._metadata.meta_args.index(meta_arg)
        cls._add_lfric_symbol_name(
            f"LFRic{datatype.capitalize()}ScalarDataSymbol",
            f"{datatype_char}scalar_{meta_arg_index+1}",
            access=cls._access_lookup[meta_arg.access])

    @classmethod
    def _field(cls, meta_arg):
        '''Argument providing an LFRic field.

        :param meta_arg: the metadata associated with this field argument.
        :type meta_arg: \
            :py:class:`psyclone.domain.lfric.kernel.FieldArgMetadata`

        '''
        # This symbol is used in other methods so might have already
        # been declared.
        print("1 ",meta_arg.function_space)
        undf_name = cls._undf_name(meta_arg.function_space)
        undf_symbol = cls._get_or_create_lfric_symbol(
            "NumberOfUniqueDofsDataSymbol", undf_name)

        name = cls._field_name(meta_arg)
        datatype = meta_arg.datatype[3:]
        cls._add_lfric_symbol_name(
            f"{datatype.capitalize()}FieldDataSymbol", name,
            dims=[undf_symbol],
            access=cls._access_lookup[meta_arg.access])

    @classmethod
    def _field_vector(cls, meta_arg):
        '''Arguments providing an LFRic field vector.

        :param meta_arg: the metadata associated with this field \
            vector argument.
        :type meta_arg: \
            :py:class:`psyclone.domain.lfric.kernel.FieldVectorArgMetadata`

        '''
        # This symbol is used in other methods so might have already
        # been declared.
        print("2 ",meta_arg.function_space)
        undf_name = cls._undf_name(meta_arg.function_space)
        undf_symbol = cls._get_or_create_lfric_symbol(
            "NumberOfUniqueDofsDataSymbol", undf_name)

        field_name = cls._field_name(meta_arg)
        datatype = meta_arg.datatype[3:]
        access = cls._access_lookup[meta_arg.access]
        for idx in range(int(meta_arg.vector_length)):
            name = f"{field_name}_v{idx+1}"
            cls._add_lfric_symbol_name(
                f"{datatype.capitalize()}VectorFieldDataSymbol", name,
                dims=[undf_symbol], access=access)

    @classmethod
    def _operator(cls, meta_arg):
        '''Arguments providing an LMA operator. First include an integer
        extent of kind i_def with intent in. The default name of this
        extent is <operator_name>'_ncell_3d'. Next include the
        operator. This is a rank-3, real array. Its precision (kind)
        depends on how it is defined in the algorithm layer and its
        intent depends on its metadata. Its default name is
        'op_'<argument_position>. The extents of the first two
        dimensions are the local degrees of freedom for the to and
        from function spaces, respectively, and that of the third is
        <operator_name>'_ncell_3d'.

        :param meta_arg: the metadata associated with the operator \
            arguments.
        :type meta_arg: \
            :py:class:`psyclone.domain.lfric.kernel.OperatorArgMetadata`

        '''
        meta_arg_index = cls._metadata.meta_args.index(meta_arg)
        name = f"op_{meta_arg_index+1}_ncell_3d"
        op_ncell_3d_symbol = cls._create_lfric_symbol(
            "LFRicIntegerScalarDataSymbol", name)
        cls._append_to_arg_list(op_ncell_3d_symbol)

        # This symbol is used in other methods so might have already
        # been declared.
        ndf_name_to = cls._ndf_name(meta_arg.function_space_to)
        ndf_to_symbol = cls._get_or_create_lfric_symbol(
            "NumberOfDofsDataSymbol", ndf_name_to)

        # This symbol is used in other methods so might have already
        # been declared.
        ndf_name_from = cls._ndf_name(meta_arg.function_space_from)
        ndf_from_symbol = cls._get_or_create_lfric_symbol(
            "NumberOfDofsDataSymbol", ndf_name_from)

        operator_name = cls._operator_name(meta_arg)
        access = cls._access_lookup[meta_arg.access]
        cls._add_lfric_symbol_name(
            "OperatorDataSymbol", operator_name,
            dims=[ndf_to_symbol, ndf_from_symbol, op_ncell_3d_symbol],
            access=access)

    @classmethod
    def _cma_operator(cls, meta_arg):
        '''Arguments providing a columnwise operator. First include a real,
        3-dimensional array of kind r_solver with its intent depending
        on its metadata. Its default name is
        'cma_op_'<argument_position>, hereon specified as
        '<operator_name>' and the default names of its dimensions are
        'bandwidth_'<operator_name>, 'nrow_'<operator_name>, and
        'ncell_2d'. Next the number of rows in the banded matrix is
        provided. This is an integer of kind i_def with intent in with
        default name 'nrow_'<operator_name>. If the from-space of the
        operator is not the same as the to-space then the number of
        columns in the banded matrix is provided next. This is an
        integer of kind i_def with intent in and has default name
        'ncol_'<operator_name>. Next the bandwidth of the banded
        matrix is added. This is an integer of kind i_def with intent
        in and has default name 'bandwidth_'<operator_name>. Next
        banded-matrix parameter alpha is added. This is an integer of
        kind i_def with intent in and has default name
        'alpha_'<operator_name>. Next banded-matrix parameter beta is
        added. This is an integer of kind i_def with intent in and has
        default name 'beta_<operator_name>. Next banded-matrix
        parameter gamma_m is added. This is an integer of kind i_def
        with intent in and has default name 'gamma_m_'<operator_name>.
        Finally banded-matrix parameter gamma_p is added. This is an
        integer of kind i_def with intent in and has default name
        'gamma_p_'<operator_name>.

        :param meta_arg: the metadata associated with the CMA operator \
            arguments.
        :type meta_arg: :py:class:`psyclone.domain.lfric.kernel.\
            ColumnwiseOperatorArgMetadata`

        '''
        operator_name = cls._cma_operator_name(meta_arg)

        bandwidth = cls._create_lfric_symbol(
            "LFRicIntegerScalarDataSymbol", f"bandwidth_{operator_name}")
        nrow = cls._create_lfric_symbol(
            "LFRicIntegerScalarDataSymbol", f"nrow_{operator_name}")
        # This symbol is used in other methods so might have already
        # been declared.
        ncell_2d = cls._get_or_create_lfric_symbol(
            "LFRicIntegerScalarDataSymbol", "ncell_2d")

        access = cls._access_lookup[meta_arg.access]
        # TODO: should be r_solver precision
        cls._add_lfric_symbol_name(
            "OperatorDataSymbol", operator_name,
            dims=[bandwidth, nrow, ncell_2d], access=access)
        cls._append_to_arg_list(nrow)
        if meta_arg.function_space_from != meta_arg.function_space_to:
            cls._add_lfric_symbol_name(
                "LFRicIntegerScalarDataSymbol", f"ncol_{operator_name}")
        cls._append_to_arg_list(bandwidth)
        cls._add_lfric_symbol_name(
            "LFRicIntegerScalarDataSymbol", f"alpha_{operator_name}")
        cls._add_lfric_symbol_name(
            "LFRicIntegerScalarDataSymbol", f"beta_{operator_name}")
        cls._add_lfric_symbol_name(
            "LFRicIntegerScalarDataSymbol", f"gamma_m_{operator_name}")
        cls._add_lfric_symbol_name(
            "LFRicIntegerScalarDataSymbol", f"gamma_p_{operator_name}")

    @classmethod
    def _ref_element_properties(cls, meta_ref_element):
        '''Arguments required if there are reference element properties
        specified in the metadata.

        If either the normals_to_horizontal_faces or
        outward_normals_to_horizontal_faces properties of the
        reference element are required then pass the number of
        horizontal faces of the reference element, with default name
        nfaces_re_h. Similarly, if either the
        normals_to_vertical_faces or outward_normals_to_vertical_faces
        are required then pass the number of vertical faces, with
        default name 'nfaces_re_v'. This also holds for the
        normals_to_faces and outward_normals_to_faces where the number
        of all faces of the reference element (with default name
        nfaces_re) is passed to the kernel. All of these quantities
        are integers of kind i_def with intent in.

        Then, in the order specified in the meta_reference_element
        metadata: For the
        [outward_]normals_to_horizontal/[outward_]vertical_faces, pass
        a rank-2 integer array of kind i_def with dimensions (3,
        nfaces_re_h/v) and intent in.  For normals_to_faces or
        outward_normals_to_faces pass a rank-2 integer array of kind
        i_def with dimensions (3, nfaces_re) and intent in. In each
        case the default name is the same name as the reference
        element property.

        :param meta_ref_element: the metadata capturing the reference \
            element properties required by the kernel.
        :type meta_ref_element: List[:py:class:`psyclone.domain.lfric.\
            kernel.MetaRefElementArgMetadata`]

        '''
        if [entry for entry in meta_ref_element if entry.reference_element in
                ["normals_to_horizontal_faces",
                 "outward_normals_to_horizontal_faces"]]:
            nfaces_re_h = cls._create_lfric_symbol(
                "NumberOfQrPointsInXyDataSymbol", "nfaces_re_h")
            cls._append_to_arg_list(nfaces_re_h)

        if [entry for entry in meta_ref_element if entry.reference_element in
                ["normals_to_vertical_faces",
                 "outward_normals_to_vertical_faces"]]:
            nfaces_re_v = cls._create_lfric_symbol(
                "NumberOfQrPointsInZDataSymbol", "nfaces_re_v")
            cls._append_to_arg_list(nfaces_re_v)

        if [entry for entry in meta_ref_element if entry.reference_element in
                ["normals_to_faces",
                 "outward_normals_to_faces"]]:
            nfaces_re = cls._create_lfric_symbol(
                "NumberOfQrPointsInFacesDataSymbol", "nfaces_re")
            cls._append_to_arg_list(nfaces_re)

        scalar_type = cls._create_datatype("LFRicIntegerScalarDataType")
        interface = symbols.ArgumentInterface(
            symbols.ArgumentInterface.Access.READ)

        for ref_element_property in meta_ref_element:

            if ref_element_property.reference_element in [
                    "normals_to_horizontal_faces",
                    "outward_normals_to_horizontal_faces"]:
                ref_element_dim = nfaces_re_h
            elif ref_element_property.reference_element in [
                    "normals_to_vertical_faces",
                    "outward_normals_to_vertical_faces"]:
                ref_element_dim = nfaces_re_v
            elif ref_element_property.reference_element in [
                    "normals_to_faces" or "outward_normals_to_faces"]:
                ref_element_dim = nfaces_re
            else:
                raise InternalError(
                    f"Unsupported reference element property "
                    f"'{ref_element_property.reference_element}' found.")

            array_type = symbols.ArrayType(
                scalar_type, [nodes.Literal("3", scalar_type),
                              nodes.Reference(ref_element_dim)])
            property_symbol = symbols.DataSymbol(
                ref_element_property.reference_element, array_type,
                interface=interface)
            cls._add_to_symbol_table(property_symbol)
            cls._append_to_arg_list(property_symbol)

    @classmethod
    def _mesh_properties(cls, meta_mesh):
        '''All arguments required for mesh properties specified in the kernel
        metadata.

        If the adjacent_face mesh property is required then, if the
        number of horizontal cell faces obtained from the reference
        element (nfaces_re_h) is not already being passed to the
        kernel via the reference element then supply it here. This is
        an integer of kind i_def with intent in and has default name
        'nfaces_re_h'.

        Also pass a rank-1, integer array with intent in of kind i_def
        and extent nfaces_re_h, with default name being the name of
        the property (adjacent_face in this case).

        :param meta_mesh: the metadata capturing the mesh properties \
            required by the kernel.
        :type meta_mesh: List[\
            :py:class:`psyclone.domain.lfric.kernel.MetaMeshArgMetadata`]

        raises InternalError: if the mesh property is not 'adjacent_face'.

        '''
        for mesh_property in meta_mesh:
            if mesh_property.mesh == "adjacent_face":
                # nfaces_re_h may have been passed via the reference
                # element logic.
                nfaces_re_h = cls._get_or_create_lfric_symbol(
                    "NumberOfQrPointsInXyDataSymbol", "nfaces_re_h")
                if not cls._metadata.meta_ref_element or not \
                   [entry for entry in cls._metadata.meta_ref_element
                    if entry.reference_element in [
                            "normals_to_horizontal_faces",
                            "outward_normals_to_horizontal_faces"]]:
                    # nfaces_re_h was not been passed via the
                    # reference element logic so add it the argument
                    # list.
                    cls._append_to_arg_list(nfaces_re_h)
                interface = symbols.ArgumentInterface(
                    symbols.ArgumentInterface.Access.READ)
                scalar_type = cls._create_datatype(
                    "LFRicIntegerScalarDataType")
                array_type = symbols.ArrayType(
                    scalar_type, [nodes.Reference(nfaces_re_h)])
                mesh_symbol = symbols.DataSymbol(
                    mesh_property.mesh, array_type,
                    interface=interface)
                cls._add_to_symbol_table(mesh_symbol)
                cls._append_to_arg_list(mesh_symbol)
            else:
                raise InternalError(
                    f"Unexpected mesh property '{mesh_property.mesh}' found. "
                    f"Expected 'adjacent_face'.")

    @classmethod
    def _fs_common(cls, function_space):
        '''Arguments associated with a function space that are common to
        fields and operators. Add the number of degrees of freedom for
        this function space. This is an integer of kind i_def with
        intent in and default name 'ndf_'<function_space>.

        :param str function_space: the current function space.

        '''
        # TODO: if self._kern.iterates_over not in ["cell_column", "domain"]: return

        function_space_name = cls._function_space_name(function_space)
        ndf_name = cls._ndf_name(function_space)
        # This symbol might be used to dimension an array in another
        # method and therefore could have already been declared.
        symbol = cls._get_or_create_lfric_symbol("NumberOfDofsDataSymbol", ndf_name)
        cls._append_to_arg_list(symbol)

    @classmethod
    def _fs_compulsory_field(cls, function_space):
        '''Compulsory arguments for this function space. First include the
        unique number of degrees of freedom for this function
        space. This is a scalar integer of kind i_def with intent in
        and it's default name is 'undf'_<function_space>. Second
        include the dof map for this function space. This is a 1D
        integer array with dimension 'ndf_<function_space>' of kind
        i_def with intent in and its' default name is
        'map_<function_space>.

        :param str function_space: the current function space.

        '''
        function_space_name = cls._function_space_name(function_space)
        undf_name = cls._undf_name(function_space)
        # This symbol might be used to dimension an array in another
        # method and therefore could have already been declared.
        undf_symbol = cls._get_or_create_lfric_symbol(
            "NumberOfUniqueDofsDataSymbol", undf_name)

        # TODO: if domain pass whole dofmap

        # get ndf
        ndf_name = cls._ndf_name(function_space)
        ndf_symbol = cls._get_or_create_lfric_symbol(
            "NumberOfDofsDataSymbol", ndf_name)
        # create dofmap(ndf)
        dofmap_name = cls._dofmap_name(function_space)
        dofmap_symbol = cls._create_array_symbol(
            "LFRicIntegerScalarDataType", dofmap_name,
            dims=[ndf_symbol])

        # Add the symbols to the symbol table argument list in the
        # required order
        cls._append_to_arg_list(undf_symbol)
        cls._append_to_arg_list(dofmap_symbol)

    @classmethod
    def _fs_intergrid(cls, meta_arg):
        '''Function-space related arguments for an intergrid kernel.

        For this field include the required dofmap information.

        If the dofmap is associated with an argument on the fine mesh,
        include the number of DoFs per cell for the FS of the field on
        the fine mesh. This is an integer with intent in and precision
        i_def. Its default name is 'ndf_'<function_space>. Next,
        include the number of unique DoFs per cell for the FS of the
        field on the fine mesh. This is an integer with intent in and
        precision i_def. Its default name is
        'undf_'<function_space>. Lastly include the whole dofmap for
        the fine mesh. This is an integer array of rank two and kind
        i_def with intent in. The extent of the first dimension is
        ndf_<function_space> and that of the second is ncell_f. Its
        default name is 'full_map_'<function_space>.

        If the dofmap is associated with an argument on the coarse
        mesh then include undf_coarse, the number of unique DoFs for
        the coarse field. This is an integer of kind i_def with intent
        in. Its default name is 'undf_'<function_space>. Lastly,
        include the dofmap for the current cell (column) in the coarse
        mesh. This is an integer array of rank one, kind i_def and
        has intent in. Its default name is 'map_'<function_space>.

        :param meta_arg: the metadata capturing the InterGrid argument \
            required by the kernel.
        :type meta_arg: \
        :py:class:`psyclone.domain.lfric.kernel.InterGridArgMetadata`]

        '''
        function_space = meta_arg.function_space
        function_space_name = cls._function_space_name(function_space)
        if meta_arg.mesh_arg == "gh_fine":
            # add ndf symbol
            cls._fs_common(function_space)
            # add undf symbol (may have already been declared)
            undf_name = cls._undf_name(function_space)
            symbol = cls._get_or_create_lfric_symbol(
                "NumberOfUniqueDofsDataSymbol", undf_name)
            cls._append_to_arg_list(symbol)
            # get ndf_symbol (has just been added)
            ndf_name = cls._ndf_name(function_space)
            ndf_symbol = cls._info.lookup(ndf_name)
            # get ncell_f symbol (may have already been declared)
            ncell_symbol = cls._get_or_create_lfric_symbol(
                "LFRicIntegerScalarDataSymbol", "ncell_f")
            # add full_dofmap(ndf, ncell_f)
            fullmap_name = cls._fullmap_name(function_space)
            cls._add_array_symbol_name(
                "LFRicIntegerScalarDataType", fullmap_name,
                dims=[ndf_symbol, ncell_symbol])
        else:  # "gh_coarse"
            # undf + dofmap
            cls._fs_compulsory_field(function_space)

    @classmethod
    def _basis_or_diff_basis(cls, name, function_space):
        '''Utility function for the basis and diff_basis methods.

        For each operation on the function space (name = ["basis",
        "diff_basis"]), in the order specified in the metadata, pass
        real arrays of kind r_def with intent in. For each shape
        specified in the gh_shape metadata entry:

        If shape is gh_quadrature_* then the arrays are of rank four
        and have default name
        "<name>_"<function_space>_<quadrature_arg_name>.

        If shape is gh_quadrature_xyoz then the arrays have extent
        (dimension, number_of_dofs, np_xy, np_z).

        If shape is gh_quadrature_face or gh_quadrature_edge then the
        arrays have extent (dimension, number_of_dofs, np_xyz, nfaces
        or nedges).

        If shape is gh_evaluator then pass one array for each target
        function space (i.e. as specified by
        gh_evaluator_targets). Each of these arrays are of rank three
        with extent (dimension, number_of_dofs,
        ndf_<target_function_space>). The default name of the argument
        is <name>"_"<function_space>"_on_"<target_function_space>.

        Here <quadrature_arg_name> is the name of the corresponding
        quadrature object being passed to the Invoke. dimension is 1
        or 3 and depends upon the function space and whether or not
        it is a basis or a differential basis function (see the table
        below). number_of_dofs is the number of degrees of freedom
        (DoFs) associated with the function space and np_* are the
        number of points to be evaluated: i) *_xyz in all directions
        (3D); ii) *_xy in the horizontal plane (2D); iii) *_x, *_y in
        the horizontal (1D); and iv) *_z in the vertical (1D). nfaces
        and nedges are the number of horizontal faces/edges obtained
        from the appropriate quadrature object supplied to the Invoke.

        Function Type    Dimension    Function Space Name

        Basis               1         W0, W2trace, W2Htrace, W2Vtrace, W3,
                                      Wtheta, Wchi
                            3         W1, W2, W2H, W2V, W2broken, ANY_W2

        Differential Basis  1         W2, W2H, W2V, W2broken, ANY_W2

                            3         W0, W1, W2trace, W2Htrace, W2Vtrace,
                                      W3, Wtheta, Wchi

        :param str name: 'basis' or 'diff_basis'.
        :param str function_space: the current function space.

        raises InternalError: if unexpected shape metadata is found.

        '''
        function_space_name = cls._function_space_name(function_space)
        const = lfric.LFRicConstants()
        if not cls._metadata.shapes:
            return
        for shape in cls._metadata.shapes:
            if shape in const.VALID_QUADRATURE_SHAPES:
                cls._add_lfric_symbol_name(
                    "LFRicIntegerScalar",
                    f"{name}_{function_space_name}_qr_{shape.split('_')[-1]}")
            elif shape in const.VALID_EVALUATOR_SHAPES:
                if cls._metadata.evaluator_targets:
                    target_function_spaces = cls._metadata.evaluator_targets
                else:
                    # Targets are the function spaces of all modified fields.
                    fields = [field for field in cls._metadata.meta_args
                              if type(field) in [
                                      lfric.kernel.FieldArgMetadata,
                                      lfric.kernel.FieldVectorArgMetadata,
                                      lfric.kernel.InterGridArgMetadata,
                                      lfric.kernel.InterGridVectorArgMetadata]
                              and field.access != "gh_read"]
                    target_function_spaces = []
                    for field in fields:
                        if field.function_space not in target_function_spaces:
                            target_function_spaces.append(field.function_space)
                for target_function_space in target_function_spaces:
                    cls._add_lfric_symbol_name(
                        "LFRicIntegerScalar",
                        f"{name}_{function_space_name}_to_"
                        f"{target_function_space}")
            else:
                raise InternalError(
                    f"Unexpected shape metadata. Found '{shape}' but expected "
                    f"one of {const.VALID_EVALUATOR_SHAPES}.")

    @classmethod
    def _basis(cls, function_space):
        '''Arguments associated with basis functions on the supplied function
        space.

        :param str function_space: the current function space.

        '''
        cls._basis_or_diff_basis("basis", function_space)

    @classmethod
    def _diff_basis(cls, function_space):
        '''Arguments associated with differential basis functions on the
        supplied function space.

        :param str function_space: the current function space.

        '''
        cls._basis_or_diff_basis("diff_basis", function_space)

    @classmethod
    def _quad_rule(cls, shapes):
        '''Quadrature information is required (gh_shape =
        gh_quadrature_*). For each shape in the order specified in the
        gh_shape metadata:

        Include integer, scalar arguments of kind i_def with intent in
        that specify the extent of the basis/diff-basis arrays:

        If gh_shape is gh_quadrature_XYoZ then pass
        np_xy_<quadrature_arg_name> and np_z_<quadrature_arg_name>.

        If gh_shape is gh_quadrature_face/_edge then pass
        nfaces/nedges_<quadrature_arg_name> and
        np_xyz_<quadrature_arg_name>.

        Include weights which are real arrays of kind r_def:

        If gh_quadrature_XYoZ pass in weights_xz_<quadrature_arg_name>
        (rank one, extent np_xy_<quadrature_arg_name>) and
        weights_z_<quadrature_arg_name> (rank one, extent
        np_z_<quadrature_arg_name>).

        If gh_quadrature_face/_edge pass in
        weights_xyz_<quadrature_arg_name> (rank two with extents
        [np_xyz_<quadrature_arg_name>,
        nfaces/nedges_<quadrature_arg_name>]).

        :param shapes: the metadata capturing the quadrature shapes \
            required by the kernel.
        :type shapes: List[str]

        raises InternalError: if unexpected (quadrature) shape \
            metadata is found.

        '''
        const = lfric.LFRicConstants()
        for quad in shapes:
            quad_name = quad.split('_')[-1]
            if quad == "gh_quadrature_xyoz":
                cls.arg_info.extend([
                    f"npxy_{quad_name}", f"np_z_{quad_name}",
                    f"weights_xz_{quad_name}", f"weights_z_{quad_name}"])
                cls._arg_index += 4
            elif quad == "gh_quadrature_face":
                cls.arg_info.extend([
                    f"nfaces_{quad_name}", f"np_xyz_{quad_name}",
                    f"weights_xyz_{quad_name}"])
                cls._arg_index += 3
            elif quad == "gh_quadrature_edge":
                cls.arg_info.extend([
                    f"nedges_{quad_name}", f"np_xyz_{quad_name}",
                    f"weights_xyz_{quad_name}"])
                cls._arg_index += 3
            else:
                raise InternalError(
                    f"Unexpected shape metadata. Found '{quad}' but expected "
                    f"one of {const.VALID_QUADRATURE_SHAPES}.")

    # pylint: disable=unidiomatic-typecheck
    @classmethod
    def _field_bcs_kernel(cls):
        '''Fix for the field boundary condition kernel. Adds a boundary dofs
        2D integer array with intent in and kind i_def. The size of
        the dimensions are ndf_<function_space>, 2. Its default name
        is "boundary_dofs_"<field_name>, where <field_name> is the
        default name of the field on the current function space.

        :raises InternalError: if the enforce_bc_kernel does not have \
            a single field argument on the any_space_1 function space.

        '''
        # Check that this kernel has a single field argument that is
        # on the any_space_1 function space.
        if len(cls._metadata.meta_args) != 1:
            raise InternalError(
                f"An enforce_bc_code kernel should have a single "
                f"argument but found '{len(cls._metadata.meta_args)}'.")
        meta_arg = cls._metadata.meta_args[0]
        if not type(meta_arg) == lfric.kernel.FieldArgMetadata:
            raise InternalError(
                f"An enforce_bc_code kernel should have a single field "
                f"argument but found '{type(meta_arg).__name__}'.")
        if not meta_arg.function_space == "any_space_1":
            raise InternalError(
                f"An enforce_bc_code kernel should have a single field "
                f"argument on the 'any_space_1' function space, but found "
                f"'{meta_arg.function_space}'.")

        field_name = cls._field_name(meta_arg)
        # 2d integer array
        print("TO BE ADDED")
        exit(1)
        # cls._add_lfric_symbol_name("xxx", f"boundary_dofs_{field_name}")

    @classmethod
    def _operator_bcs_kernel(cls):
        '''Fix for the operator boundary condition kernel. Adds a boundary
        dofs 2D integer array with intent in and kind i_def. The size
        of the dimensions are ndf_<function_space>, 2. Its default
        name is "boundary_dofs_"<field_name>, where <field_name> is
        the default name of the field on the current function space.

        :raises InternalError: if the enforce_operator_bc_kernel does \
            not have a single lma operator argument.

        '''
        # Check that this kernel has a single LMA argument.
        if len(cls._metadata.meta_args) != 1:
            raise InternalError(
                f"An enforce_operator_bc_code kernel should have a single "
                f"argument but found '{len(cls._metadata.meta_args)}'.")
        meta_arg = cls._metadata.meta_args[0]
        if not type(meta_arg) == OperatorArgMetadata:
            raise InternalError(
                f"An enforce_operator_bc_code kernel should have a single "
                f"lma operator argument but found "
                f"'{type(meta_arg).__name__}'.")

        lma_operator_name = cls._operator_name(meta_arg)
        print("TO BE ADDED")
        exit(1)
        # cls._add_lfric_symbol_name("", f"boundary_dofs_{lma_operator_name}")

    @classmethod
    def _stencil_2d_unknown_extent(cls, meta_arg):
        '''The field entry has a stencil access of type cross2d so add a 1D
        integer array of extent 4 and kind i_def stencil-size argument
        with intent in. The default name is
        <field_name>"_stencil_size", where <field_name> is the default
        name of the field with this stencil. This will supply the
        number of cells in each branch of the stencil.

        :param meta_arg: the metadata associated with a field argument \
            with a cross2d stencil access.
        :type meta_arg: \
            :py:class:`psyclone.domain.lfric.kernel.FieldArgMetadata`

        '''
        field_name = cls._field_name(meta_arg)
        # Array
        print("TO BE DONE")
        exit(1)
        # cls._add_lfric_symbol_name(f"{field_name}_stencil_size")

    @classmethod
    def _stencil_2d_max_extent(cls, meta_arg):
        '''The field entry has a stencil access of type cross2d so add an
        integer of kind i_def and intent in for the max branch
        length. The default name is <field_name>"_max_branch_length",
        where <field_name> is the default name of the field with this
        stencil. This is used in defining the dimensions of the
        stencil dofmap array and is required due to the varying length
        of the branches of the stencil when used on planar meshes.

        :param meta_arg: the metadata associated with a field argument \
            with a cross2d stencil access.
        :type meta_arg: \
            :py:class:`psyclone.domain.lfric.kernel.FieldArgMetadata`

        '''
        field_name = cls._field_name(meta_arg)
        cls._add_lfric_symbol_name(
            "LFRicIntegerScalar", f"{field_name}_max_branch_length")

    @classmethod
    def _stencil_unknown_extent(cls, meta_arg):
        '''The field entry has a stencil access so add an integer stencil-size
        argument with intent in and kind i_def. The default name is
        <field_name>"_stencil_size", where <field_name> is the default
        name of the field with this stencil. This argument will
        contain the number of cells in the stencil.

        :param meta_arg: the metadata associated with a field argument \
            with a stencil access.
        :type meta_arg: \
            :py:class:`psyclone.domain.lfric.kernel.FieldArgMetadata`

        '''
        field_name = cls._field_name(meta_arg)
        cls._add_lfric_symbol_name(
            "LFRicIntegerScalar", f"{field_name}_stencil_size")

    @classmethod
    def _stencil_unknown_direction(cls, meta_arg):
        '''The field entry stencil access is of type XORY1D so add an
        additional integer direction argument of kind i_def and with
        intent in, with default name <field_name>"_direction", where
        <field_name> is the default name of the field with this
        stencil.

        :param meta_arg: the metadata associated with a field argument \
            with a xory1d stencil access.
        :type meta_arg: \
            :py:class:`psyclone.domain.lfric.kernel.FieldArgMetadata`

        '''
        field_name = cls._field_name(meta_arg)
        cls._add_lfric_symbol_name("LFRicIntegerScalar", f"{field_name}_direction")

    @classmethod
    def _stencil_2d(cls, meta_arg):
        '''Stencil information that is passed from the Algorithm layer if the
        stencil is 'cross2d'. Add a 3D stencil dofmap array of type
        integer, kind i_def and intent in. The dimensions are
        (number-of-dofs-in-cell, max-branch-length, 4). The default
        name is <field_name>"_stencil_dofmap", where <field_name> is
        the default name of the field with this stencil.

        :param meta_arg: the metadata associated with a field argument \
            with a stencil access.
        :type meta_arg: \
            :py:class:`psyclone.domain.lfric.kernel.FieldArgMetadata`

        '''
        field_name = cls._field_name(meta_arg)
        cls._add_lfric_symbol_name(
            "LFRicIntegerScalar", f"{field_name}_stencil_dofmap")

    @classmethod
    def _stencil(cls, meta_arg):
        '''Stencil information that is passed from the Algorithm layer if the
        stencil is not 'cross2d'. Add a 2D stencil dofmap array of
        type integer, kind i_def and intent in. The dimensions are
        (number-of-dofs-in-cell, stencil-size). The default name is
        <field_name>"_stencil_dofmap, where <field_name> is the
        default name of the field with this stencil.

        :param meta_arg: the metadata associated with a field argument \
            with a stencil access.
        :type meta_arg: \
            :py:class:`psyclone.domain.lfric.kernel.FieldArgMetadata`

        '''
        field_name = cls._field_name(meta_arg)
        print("TO BE DONE")
        exit(1)
        # cls._add_lfric_symbol_name(f"{field_name}_stencil_dofmap")

    @classmethod
    def _banded_dofmap(cls, function_space, cma_operator):
        '''Adds a banded dofmap for the provided function space and cma
        operator when there is an assembly cma kernel.

        Include the column-banded dofmap, the list of offsets for the
        to/from-space. This is an integer array of rank 2 and kind
        i_def with intent in. The first dimension is
        "ndf_"<arg_function_space> and the second is nlayers. Its
        default name is 'cbanded_map_'<function_space>'_'<op_name>

        :param str function_space: the function space for this banded \
            dofmap.
        :param cma_operator: the cma operator metadata associated with \
            this banded dofmap.
        :type cma_operator: :py:class:`psyclone.domain.lfric.kernel.\
            ColumnwiseOperatorArgMetadata`

        '''
        function_space_name = cls._function_space_name(function_space)
        name = cls._cma_operator_name(cma_operator)
        print("TO BE DONE")
        exit(1)
        # cls._add_lfric_symbol_name(f"cbanded_map_{function_space_name}_{name}")

    @classmethod
    def _indirection_dofmap(cls, function_space, cma_operator):
        '''Adds an indirection dofmap for the provided function space and cma
        operator when there is an apply cma kernel.

        Include the indirection map for the 'to' function space of the
        supplied CMA operator. This is a rank-1 integer array of kind
        i_def and intent in with extent nrow. Its default name is
        'cma_indirection_map_'<function_space>'_'<operator_name>.

        If the from-space of the operator is not the same as the
        to-space then include the indirection map for the 'from'
        function space of the CMA operator. This is a rank-1 integer
        array of kind i_def and intent in with extent ncol. Its
        default name is
        'cma_indirection_map_'<function_space>'_'<operator_name>.

        Note, this method will not be called for the from space if the
        to and from function spaces are the same so there is no need
        to explicitly check.

        :param str function_space: the function space for this \
            indirection dofmap.
        :param cma_operator: the cma operator metadata associated with \
            this indirection dofmap.
        :type cma_operator: :py:class:`psyclone.domain.lfric.kernel.\
            ColumnwiseOperatorArgMetadata`

        '''
        function_space_name = cls._function_space_name(function_space)
        name = cls._cma_operator_name(cma_operator)

        print("TO BE DONE")
        exit(1)
        # cls._add_lfric_symbol_name("",
        #     f"cma_indirection_map_{function_space_name}_{name}")

    @classmethod
    def _create_datatype(cls, class_name):
        ''' xxx '''
        required_class = lfric.LFRicTypes(class_name)
        symbol = required_class()
        return symbol

    @classmethod
    def _create_array_symbol(cls, scalar_type_name, symbol_name, dims,
                       access=symbols.ArgumentInterface.Access.READ):
        ''' xxx '''
        scalar_type = cls._create_datatype(scalar_type_name)
        array_args = [nodes.Reference(symbol) for symbol in dims]
        array_type = symbols.ArrayType(scalar_type, array_args)
        interface = symbols.ArgumentInterface(access)
        array_symbol = symbols.DataSymbol(
            symbol_name, array_type, interface=interface)
        cls._add_to_symbol_table(array_symbol)
        return array_symbol

    @classmethod
    def _add_array_symbol_name(cls, scalar_type_name, symbol_name, dims,
                         access=symbols.ArgumentInterface.Access.READ):
        '''xxx'''
        symbol = cls._create_array_symbol(
            scalar_type_name, symbol_name, dims, access=access)
        cls._append_to_arg_list(symbol)

    @classmethod
    def _create_lfric_symbol(cls, class_name, symbol_name, dims=None,
                       access=symbols.ArgumentInterface.Access.READ):
        ''' xxx '''
        required_class = lfric.LFRicTypes(class_name)
        if dims:
            array_args = [nodes.Reference(symbol) for symbol in dims]
            symbol = required_class(symbol_name, array_args)
        else:
            symbol = required_class(symbol_name)
        symbol.interface = symbols.ArgumentInterface(access)
        cls._add_to_symbol_table(symbol)
        return symbol

    @classmethod
    def _add_lfric_symbol_name(cls, class_name, symbol_name, dims=None,
                         access=symbols.ArgumentInterface.Access.READ):
        '''Utility function to create an LFRic-PSyIR symbol of type class_name
        and name symbol_name and add it to the symbol table.

        :param str class_name: the name of the class to be created.
        :param str symbol_name: the name of the symbol to be created.

        '''
        symbol = cls._create_lfric_symbol(
            class_name, symbol_name, dims=dims, access=access)
        cls._append_to_arg_list(symbol)

    @classmethod
    def _get_or_create_lfric_symbol(cls, class_name, symbol_name,
                              access=symbols.ArgumentInterface.Access.READ):
        ''' xxx '''
        try:
            symbol = cls._info.lookup_with_tag(symbol_name)
        except KeyError:
            symbol = cls._create_lfric_symbol(
                class_name, symbol_name, access=access)
        return symbol

    @classmethod
    def _add_to_symbol_table(cls, symbol):
        ''' xxx '''
        cls._info.add(symbol, tag=symbol.name)

    @classmethod
    def _append_to_arg_list(cls, symbol):
        ''' xxx '''
        cls._info._argument_list.append(symbol)
        # TODO???
        # symbol_table.specify_argument_list([arg1])

    @classmethod
    def _add_lfric_symbol_name(cls, class_name, symbol_name, dims=None,
                         access=symbols.ArgumentInterface.Access.READ):
        '''Utility function to create an LFRic-PSyIR symbol of type class_name
        and name symbol_name and add it to the symbol table.

        :param str class_name: the name of the class to be created.
        :param str symbol_name: the name of the symbol to be created.

        '''
        symbol = cls._create_lfric_symbol(
            class_name, symbol_name, dims=dims, access=access)
        cls._append_to_arg_list(symbol)

    @classmethod
    def _field_name(cls, meta_arg):
        '''Utility function providing the default field name from its meta_arg
        metadata.

        :param meta_arg: metadata describing a field argument.
        :type meta_arg: \
            :py:class:`psyclone.domain.lfric.kernel.FieldArgMetadata`

        :returns: the default name for this meta_arg field.
        :rtype: str

        '''
        datatype = meta_arg.datatype[3:4]
        meta_arg_index = cls._metadata.meta_args.index(meta_arg)
        return f"{datatype}field_{meta_arg_index+1}"

    @classmethod
    def _operator_name(cls, meta_arg):
        '''Utility function providing the default field name from its meta_arg
        metadata.

        :param meta_arg: metadata describing an lma operator argument.
        :type meta_arg: \
            :py:class:`psyclone.domain.lfric.kernel.OperatorArgMetadata`

        :returns: the default name for this meta_arg field.
        :rtype: str

        '''
        meta_arg_index = cls._metadata.meta_args.index(meta_arg)
        return f"op_{meta_arg_index+1}"

    @classmethod
    def _cma_operator_name(cls, meta_arg):
        '''Utility function providing the default cma operator name from its
        meta_arg metadata.

        :param meta_arg: metadata describing a cma operator argument.
        :type meta_arg: :py:class:`psyclone.domain.lfric.kernel.\
            ColumnwiseOperatorArgMetadata`

        :returns: the default name for this meta_arg cma operator.
        :rtype: str

        '''
        meta_arg_index = cls._metadata.meta_args.index(meta_arg)
        return f"cma_op_{meta_arg_index+1}"

    @classmethod
    def _function_space_name(cls, function_space):
        '''Shortens the function space name if it is any_space_* or
        any_discontinuous_space_*.

        :param str function_space: the function space name.

        :returns: a shortened function space name.
        :rtype: str

        '''
        if "any_space_" in function_space:
            return f"as_{function_space[10:]}"
        if "any_discontinuous_space_" in function_space:
            return f"ads_{function_space[24:]}"
        return function_space

    @classmethod
    def _undf_name(cls, function_space):
        ''' xxx '''
        function_space_name = cls._function_space_name(function_space)
        return f"undf_{function_space_name}"

    @classmethod
    def _ndf_name(cls, function_space):
        ''' xxx '''
        function_space_name = cls._function_space_name(function_space)
        return f"ndf_{function_space_name}"

    @classmethod
    def _dofmap_name(cls, function_space):
        ''' xxx '''
        function_space_name = cls._function_space_name(function_space)
        return f"dofmap_{function_space_name}"

    @classmethod
    def _fullmap_name(cls, function_space):
        ''' xxx '''
        function_space_name = cls._function_space_name(function_space)
        return f"full_map_{function_space_name}"
