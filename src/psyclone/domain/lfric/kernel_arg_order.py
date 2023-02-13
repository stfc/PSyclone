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

'''This module implements the base class for managing arguments to
kernel calls. It makes use of the new style metadata classes,
demonstrating how they can be used. It is not meant to be a final
implementation as it is not yet clear what the sub-classes will
require and whether it should integrate with any existing sub-classes
that use the old style metadata parsing.

'''
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel import (
    OperatorArgMetadata, ColumnwiseOperatorArgMetadata, FieldArgMetadata,
    FieldVectorArgMetadata, InterGridArgMetadata, InterGridVectorArgMetadata,
    ScalarArgMetadata)
from psyclone.errors import InternalError


class KernelArgOrder():
    '''Base class capturing the type and order of LFRic kernel arguments
    as expected from the kernel metadata. Also provides a mapping from
    kernel arguments to metadata arguments.

    This class makes use of the new style metadata classes,
    demonstrating how they can be used. It is not meant to be a final
    implementation as it is not yet clear what any new sub-classes
    will require and whether it should make use of any existing
    sub-classes that use the old style metadata parsing.

    :param metadata: the LFRic kernel metadata.
    :type metadata: xxx
    :param str kernel_name: optional kernel name.

    '''
    def __init__(self, metadata, kernel_name=None):
        self._metadata = metadata
        self._arg_index = 0
        self.meta_arg_index_from_actual_index = {}
        # arg_info could be replaced with LFRic-specific PSyIR
        # datatype information as well as default names.
        self.arg_info = []
        self._generate(metadata, kernel_name=kernel_name)

    def cell_position(self):
        '''A cell position argument. This is an integer of type i_def and has
        intent in. Its default name is 'cell'.

        '''
        self.arg_info.append("cell")
        self._arg_index += 1

    def mesh_height(self):
        '''A mesh height argument. This is an integer of type i_def and has
        intent in. Its default name is 'nlayers'.

        '''
        self.arg_info.append("nlayers")
        self._arg_index += 1

    def mesh_ncell2d_no_halos(self):
        '''Argument providing the number of columns in the mesh ignoring
        halos. This is an integer of type i_def and has intent in.

        '''
        self.arg_info.append("ncell_2d_no_halos")
        self._arg_index += 1

    def mesh_ncell2d(self):
        '''Argument providing the number of columns in the mesh including
        halos. This is an integer of type i_def and has intent in. Its
        default name is 'ncell_2d'.

        '''
        self.arg_info.append("ncell_2d")
        self._arg_index += 1

    def cell_map(self):
        '''Four arguments providing a mapping from coarse to fine mesh for the
        current column. The first is 'cell_map', an integer array of
        rank two, kind i_def and intent in. This is followed by its
        extents, 'ncell_f_per_c_x' and 'ncell_f_per_c_y' the numbers
        of fine cells per coarse cell in the x and y directions,
        respectively. These are integers of kind i_def and have intent
        in. Lastly is 'ncell_f', the number of cells (columns) in the
        fine mesh. This is an integer of kind i_def and has intent in.

        '''
        self.arg_info.extend(
            ["cell_map", "ncell_f_per_c_x", "ncell_f_per_c_y", "ncell_f"])
        self._arg_index += 4

    def scalar(self, meta_arg):
        '''Argument providing an LFRic scalar value. As this is described in
        meta_arg metadata, provide an index map from the argument
        index to the metadata index as this can be useful.

        :param meta_arg: the metadata associated with this scalar argument.
        :type meta_arg: xxx

        '''
        datatype = meta_arg.datatype[3:4]
        meta_arg_index = self._metadata.meta_args.index(meta_arg)
        self.arg_info.append(f"{datatype}scalar_{meta_arg_index+1}")
        self.meta_arg_index_from_actual_index[self._arg_index] = meta_arg_index
        self._arg_index += 1

    def _field_name(self, meta_arg):
        '''Utility function providing the default field name from its meta_arg
        metadata.

        :param meta_arg: xxx
        :type meta_arg: xxx

        :returns: the default name for this meta_arg field.
        :rtype: str

        '''
        datatype = meta_arg.datatype[3:4]
        meta_arg_index = self._metadata.meta_args.index(meta_arg)
        return (f"{datatype}field_{meta_arg_index+1}")

    def field(self, meta_arg):
        '''Argument providing an LFRic field. As this is described in
        meta_arg metadata, provide an index map from the argument
        index to the metadata index as this can be useful.

        :param meta_arg: the metadata associated with this field argument.
        :type meta_arg: xxx

        '''
        name = self._field_name(meta_arg)
        self.arg_info.append(name)
        meta_arg_index = self._metadata.meta_args.index(meta_arg)
        self.meta_arg_index_from_actual_index[self._arg_index] = meta_arg_index
        self._arg_index += 1

    def field_vector(self, meta_arg):
        '''Arguments providing an LFRic field vector. As this is described in
        meta_arg metadata, provide an index map from the argument
        index to the metadata index as this can be useful. Each vector
        is provided as a separate field array argument so add an
        index_map for each of them.

        :param meta_arg: the metadata associated with this field \
            vector argument.
        :type meta_arg: xxx

        '''
        datatype = meta_arg.datatype[3:4]
        meta_arg_index = self._metadata.meta_args.index(meta_arg)
        for idx in range(int(meta_arg.vector_length)):
            self.arg_info.append(
                f"{datatype}field_{meta_arg_index+1}_v{idx+1}")
            self.meta_arg_index_from_actual_index[self._arg_index] = \
                meta_arg_index
            self._arg_index += 1

    def _operator_name(self, meta_arg):
        '''Utility function providing the default field name from its meta_arg
        metadata.

        :param meta_arg: xxx
        :type meta_arg: xxx

        :returns: the default name for this meta_arg field.
        :rtype: str

        '''
        meta_arg_index = self._metadata.meta_args.index(meta_arg)
        return (f"op_{meta_arg_index+1}")

    def operator(self, meta_arg):
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
        :type meta_arg: xxx

        '''
        meta_arg_index = self._metadata.meta_args.index(meta_arg)
        self.arg_info.append(f"op_{meta_arg_index+1}_ncell_3d")
        self._arg_index += 1
        operator_name = self._operator_name(meta_arg)
        self.arg_info.append(operator_name)
        self.meta_arg_index_from_actual_index[self._arg_index] = \
            self._metadata.meta_args.index(meta_arg)
        self._arg_index += 1

    def cma_operator(self, meta_arg):
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
        :type meta_arg: xxx

        '''
        index = self._metadata.meta_args.index(meta_arg)
        operator_name = f"cma_op_{index+1}"
        self.meta_arg_index_from_actual_index[self._arg_index] = index
        self.arg_info.extend([operator_name, f"nrow_{operator_name}"])
        self._arg_index += 2
        if meta_arg.function_space_from != meta_arg.function_space_to:
            self.arg_info.append(f"ncol_{operator_name}")
            self._arg_index += 1
        self.arg_info.extend([
            f"alpha_{operator_name}", f"beta_{operator_name}",
            f"gamma_m_{operator_name}", f"gamma_p_{operator_name}"])
        self._arg_index += 4

    def ref_element_properties(self, meta_ref_element):
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
        :type meta_ref_element: List [xxx]

        '''
        if [entry for entry in meta_ref_element if entry.reference_element in
                ["normals_to_horizontal_faces",
                 "outward_normals_to_horizontal_faces"]]:
            self.arg_info.append("nfaces_re_h")
            self._arg_index += 1
        if [entry for entry in meta_ref_element if entry.reference_element in
                ["normals_to_vertical_faces",
                 "outward_normals_to_vertical_faces"]]:
            self.arg_info.append("nfaces_re_v")
            self._arg_index += 1
        if [entry for entry in meta_ref_element if entry.reference_element in
                ["normals_to_faces",
                 "outward_normals_to_faces"]]:
            self.arg_info.append("nfaces_re")
            self._arg_index += 1
        for ref_element_property in meta_ref_element:
            self.arg_info.append(ref_element_property.reference_element)
            self._arg_index += 1

    def mesh_properties(self, meta_mesh):
        '''All arguments required for mesh properties specified in the kernel
        metadata.

        xxx type, intent, name, ....

        :param meta_mesh: the metadata capturing the mesh properties \
            required by the kernel.
        :type meta_mesh: List[\
            :py:class:`psyclone.domain.lfric.kernel.MetaMeshArgMetadata`]

        raises InternalError: if the mesh property is not 'adjacent_face'.

        '''
        for mesh_property in meta_mesh:
            if mesh_property.mesh == "adjacent_face":
                if not self._metadata.meta_ref_element or \
                   not [entry for entry in self._metadata.meta_ref_element
                    if entry.reference_element in [
                            "normals_to_horizontal_faces",
                            "outward_normals_to_horizontal_faces"]]:
                    # nfaces_re_h has not already been passed via reference
                    # element logic so add it here.
                    self.arg_info.append("nfaces_re_h")
                    self._arg_index += 1
                self.arg_info.append(mesh_property.mesh)
            else:
                raise InternalError(
                    f"Unexpected mesh property '{mesh_property.mesh}' found. "
                    f"Expected 'adjacent_face'.")


    def fs_common(self, function_space):
        '''Arguments associated with a function space that are common to
        fields and operators. Add the number of degrees of freedom for
        this function space. This is an integer of kind i_def with
        intent in and default name 'ndf_'<function_space>.

        :param str function_space: the current function space.

        '''
        self.arg_info.append(f"ndf_{function_space}")
        self._arg_index += 1

    def fs_compulsory_field(self, function_space):
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
        self.arg_info.append(f"undf_{function_space}")
        self.arg_info.append(f"map_{function_space}")
        self._arg_index += 2

    def fs_intergrid(self, meta_arg):
        '''Function-space related arguments for an intergrid kernel.

        xxx arg type information

        :param meta_arg: xxx
        :type meta_arg: xxx

        '''
        function_space = meta_arg.function_space
        if meta_arg.mesh_arg == "gh_fine":
            # ndf + undf
            self.fs_common(function_space)
            self._arg_index += 2
            # full dofmap
            self.arg_info.append(f"full_map_{function_space}")
            self._arg_index += 1
        else: # "gh_coarse"
            # undf + dofmap
            self.fs_compulsory_field(function_space)

    def _basis_or_diff_basis(self, name, function_space):
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
        const = LFRicConstants()
        if not self._metadata.shapes:
            return
        for shape in self._metadata.shapes:
            if shape in const.VALID_QUADRATURE_SHAPES:
                self.arg_info.append(
                    f"{name}_{function_space}_qr_{shape.split('_')[-1]}")
                self._arg_index += 1
            elif shape in const.VALID_EVALUATOR_SHAPES:
                if self._metadata.evaluator_targets:
                    target_function_spaces = self._metadata.evaluator_targets
                else:
                    # Targets are the function spaces of all modified fields.
                    fields = [field for field in self._metadata.meta_args
                              if type(field) in [
                                      FieldArgMetadata, FieldVectorArgMetadata,
                                      InterGridArgMetadata,
                                      InterGridVectorArgMetadata]
                              and field.access != "gh_read"]
                    target_function_spaces = []
                    for field in fields:
                        if field.function_space not in target_function_spaces:
                            target_function_spaces.append(field.function_space)
                for target_function_space in target_function_spaces:
                    self.arg_info.append(
                        f"{name}_{function_space}_to_{target_function_space}")
                    self._arg_index += 1
            else:
                raise InternalError(
                    f"Unexpected shape metadata. Found '{shape}' but expected "
                    f"one of {const.VALID_EVALUATOR_SHAPES}.")

    def basis(self, function_space):
        '''Arguments associated with basis functions on the supplied function
        space.

        :param str function_space: the current function space.

        '''
        self._basis_or_diff_basis("basis", function_space)

    def diff_basis(self, function_space):
        '''Arguments associated with differential basis functions on the
        supplied function space.

        :param str function_space: the current function space.

        '''
        self._basis_or_diff_basis("diff_basis", function_space)

    def quad_rule(self, shapes):
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

        :param shapes: xxx
        :type shapes: xxx

        raises InternalError: if unexpected (quadrature) shape \
            metadata is found.

        '''
        const = LFRicConstants()
        for quad in shapes:
            quad_name = quad.split('_')[-1]
            if quad == "gh_quadrature_xyoz":
                self.arg_info.extend([
                    f"npxy_{quad_name}", f"np_z_{quad_name}",
                    f"weights_xz_{quad_name}", f"weights_z_{quad_name}"])
                self._arg_index += 4
            elif quad == "gh_quadrature_face":
                self.arg_info.extend([
                    f"nfaces_{quad_name}", f"np_xyz_{quad_name}",
                    f"weights_xyz_{quad_name}"])
                self._arg_index += 3
            elif quad == "gh_quadrature_edge":
                self.arg_info.extend([
                    f"nedges_{quad_name}", f"np_xyz_{quad_name}",
                    f"weights_xyz_{quad_name}"])
                self._arg_index += 3
            else:
                raise InternalError(
                    f"Unexpected shape metadata. Found '{quad}' but expected "
                    f"one of {const.VALID_QUADRATURE_SHAPES}.")


    def field_bcs_kernel(self):
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
        if len(self._metadata.meta_args) != 1:
            raise InternalError(
                f"An enforce_bc_code kernel should have a single "
                f"argument but found '{len(self._metadata.meta_args)}'.")
        meta_arg = self._metadata.meta_args[0]
        if not type(meta_arg) == FieldArgMetadata:
            raise InternalError(
                f"An enforce_bc_code kernel should have a single field "
                f"argument but found '{type(meta_arg).__name__}'.")
        if not meta_arg.function_space == "any_space_1":
            raise InternalError(
                f"An enforce_bc_code kernel should have a single field "
                f"argument on the 'any_space_1' function space, but found "
                f"'{meta_arg.function_space}'.")

        field_name = self._field_name(meta_arg)
        self.arg_info.append(f"boundary_dofs_{field_name}")
        self._arg_index += 1
    
    def operator_bcs_kernel(self):
        '''Fix for the operator boundary condition kernel. Adds a boundary
        dofs 2D integer array with intent in and kind i_def. The size
        of the dimensions are ndf_<function_space>, 2. Its default
        name is "boundary_dofs_"<field_name>, where <field_name> is
        the default name of the field on the current function space.

        :raises InternalError: if the enforce_operator_bc_kernel does \
            not have a single lma operator argument.

        '''
        # Check that this kernel has a single LMA argument.
        if len(self._metadata.meta_args) != 1:
            raise InternalError(
                f"An enforce_operator_bc_code kernel should have a single "
                f"argument but found '{len(self._metadata.meta_args)}'.")
        meta_arg = self._metadata.meta_args[0]
        if not type(meta_arg) == OperatorArgMetadata:
            raise InternalError(
                f"An enforce_operator_bc_code kernel should have a single "
                f"lma operator argument but found "
                f"'{type(meta_arg).__name__}'.")

        lma_operator_name = self._operator_name(meta_arg)
        self.arg_info.append(f"boundary_dofs_{lma_operator_name}")
        self._arg_index += 1

    def _generate(self, metadata, kernel_name=None):

        '''Specifies which arguments appear in an argument list and their
        ordering. Calls methods for each type of argument that can be
        specialised by a child class for its particular need.

        xxx metadata, kernel_name

        :raises GenerationError: if the kernel arguments break the \
                                 rules for the LFRic API.

        '''
        # All operator types require the cell index to be provided
        if metadata.meta_args_get(
                [OperatorArgMetadata, ColumnwiseOperatorArgMetadata]):
            self.cell_position()

        # Pass the number of layers in the mesh unless this kernel is
        # applying a CMA operator or doing a CMA matrix-matrix calculation
        if metadata.kernel_type not in ["cma-apply", "cma-matrix-matrix"]:
            self.mesh_height()

        # Pass the number of cells in the mesh if this kernel has a
        # LMA operator argument
        # TODO this code should replace the code that currently includes
        # this quantity for *every* operator it encounters.
        # if metadata.meta_args_get(OperatorArgMetadata):
        #     self.mesh_ncell3d()

        # Pass the number of columns in the mesh if this kernel operates on
        # the 'domain' or has a CMA operator argument. For the former we
        # exclude halo columns.
        if metadata.operates_on == "domain":
            self.mesh_ncell2d_no_halos()
        if metadata.meta_args_get(ColumnwiseOperatorArgMetadata):
            self.mesh_ncell2d()

        if metadata.kernel_type == "inter-grid":
            # Inter-grid kernels require special arguments.  The
            # cell-map for the current column providing the mapping
            # from the coarse to the fine mesh.
            self.cell_map()

        # For each argument in the order they are specified in the
        # kernel metadata, call particular methods depending on what
        # type of argument we find (field, field vector, operator or
        # scalar). If the argument is a field or field vector and also
        # has a stencil access then also call appropriate stencil
        # methods.
        const = LFRicConstants()
        for meta_arg in metadata.meta_args:

            if type(meta_arg) in [
                    FieldArgMetadata, FieldVectorArgMetadata,
                    InterGridArgMetadata, InterGridVectorArgMetadata]:
                if type(meta_arg) in [FieldArgMetadata, InterGridArgMetadata]:
                    self.field(meta_arg)
                if type(meta_arg) in [
                        FieldVectorArgMetadata, InterGridVectorArgMetadata]:
                    self.field_vector(meta_arg)
                # TODO: Stencil support
                #if arg.descriptor.stencil:
                #    if not arg.descriptor.stencil['extent']:
                #        if arg.descriptor.stencil['type'] == "cross2d":
                #            # stencil extent is not provided in the
                #            # metadata so must be passed from the Algorithm
                #            # layer.
                #            self.stencil_2d_unknown_extent(
                #                arg, var_accesses=var_accesses)
                #            # Due to the nature of the stencil extent array
                #            # the max size of a stencil branch must be passed
                #            # from the Algorithm layer.
                #            self.stencil_2d_max_extent(
                #                arg, var_accesses=var_accesses)
                #        else:
                #            # stencil extent is not provided in the
                #            # metadata so must be passed from the Algorithm
                #            # layer.
                #            self.stencil_unknown_extent(
                #                arg, var_accesses=var_accesses)
                #    if arg.descriptor.stencil['type'] == "xory1d":
                #        # if "xory1d is specified then the actual
                #        # direction must be passed from the Algorithm layer.
                #        self.stencil_unknown_direction(arg,
                #                                       var_accesses)
                #    # stencil information that is always passed from the
                #    # Algorithm layer.
                #    if arg.descriptor.stencil['type'] == "cross2d":
                #        self.stencil_2d(arg, var_accesses=var_accesses)
                #    else:
                #        self.stencil(arg, var_accesses=var_accesses)
            elif type(meta_arg) == OperatorArgMetadata:
                self.operator(meta_arg)
            elif type(meta_arg) == ColumnwiseOperatorArgMetadata:
                self.cma_operator(meta_arg)
            elif type(meta_arg) == ScalarArgMetadata:
                self.scalar(meta_arg)
            else:
                raise GenerationError(
                    f"KernelArgOrderA.generate(): Unexpected argument type found. "
                    f"Expected one of 'XXX' but "
                    f"found '{meta_arg.check_name}'")

        # For each unique function space (in the order they appear in the
        # metadata arguments)
        function_space_args = metadata.meta_args_get(
            [FieldArgMetadata, FieldVectorArgMetadata,
             InterGridArgMetadata, InterGridVectorArgMetadata,
             OperatorArgMetadata, ColumnwiseOperatorArgMetadata])
        unique_function_spaces = []
        for arg in function_space_args:
            if type(arg) in [
                    OperatorArgMetadata, ColumnwiseOperatorArgMetadata]:
                if arg.function_space_to not in unique_function_spaces:
                    unique_function_spaces.append(arg.function_space_to)
                if arg.function_space_from not in unique_function_spaces:
                    unique_function_spaces.append(arg.function_space_from)
            else:
                if arg.function_space not in unique_function_spaces:
                    unique_function_spaces.append(arg.function_space)
                    
        for function_space in unique_function_spaces:
            # Provide arguments common to LMA operators and fields on
            # a space *unless* this is an inter-grid or CMA
            # matrix-matrix kernel
            if metadata.kernel_type not in [
                    "cma-matrix-matrix", "inter-grid"]:
                self.fs_common(function_space)

            # Provide additional arguments if there is a
            # field on this space
            if [arg for arg in metadata.meta_args_get(
                    [FieldArgMetadata, FieldVectorArgMetadata])
                if arg.function_space == function_space]:
                    self.fs_compulsory_field(function_space)

            # Provide additional arguments if there is a
            # intergrid field on this space
            if [arg for arg in metadata.meta_args_get(
                    [InterGridArgMetadata, InterGridVectorArgMetadata])
                if arg.function_space == function_space]:
                    self.fs_intergrid(arg)

            cma_ops = [arg for arg in metadata.meta_args_get(
                ColumnwiseOperatorArgMetadata) if arg.function_space_to ==
                       function_space or arg.function_space_from ==
                       function_space]
            if cma_ops:
                if metadata.kernel_type == "cma-assembly":
                    # CMA-assembly requires banded dofmaps
                    self.banded_dofmap(function_space)
                elif metadata.kernel_type == "cma-apply":
                    # Applying a CMA operator requires indirection dofmaps
                    self.indirection_dofmap(
                        function_space, operator=cma_ops[0])

            # Provide any optional arguments. These arguments are
            # associated with the keyword arguments (basis function
            # and differential basis function) for a function space.
            meta_funcs = metadata.meta_funcs \
                if metadata.meta_funcs else []
            if [func for func in meta_funcs if func.basis_function and
                    func.function_space == function_space]:
                self.basis(function_space)
            if [func for func in meta_funcs if func.diff_basis_function
                    and func.function_space == function_space]:
                self.diff_basis(function_space)

        # The boundary condition kernel (enforce_bc_kernel) is a
        # special case.
        if kernel_name and kernel_name.lower() == "enforce_bc_code":
            self.field_bcs_kernel()

        # The operator boundary condition kernel
        # (enforce_operator_bc_kernel) is a special case.
        if kernel_name and kernel_name.lower() == "enforce_operator_bc_code":
            self.operator_bcs_kernel()

        # Reference-element properties
        if metadata.meta_ref_element:
            self.ref_element_properties(metadata.meta_ref_element)

        # Mesh properties
        if metadata.meta_mesh:
            self.mesh_properties(metadata.meta_mesh)

        # Quadrature arguments are required if one or more basis or
        # differential basis functions are used by the kernel and a
        # quadrature shape is supplied.
        if metadata.meta_funcs and metadata.shapes and \
               [shape for shape in metadata.shapes if shape in
                const.VALID_QUADRATURE_SHAPES]:
            self.quad_rule(metadata.shapes)
