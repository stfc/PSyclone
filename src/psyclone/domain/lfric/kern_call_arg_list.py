# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, A. Coughtrie and L. Turner, Met Office
# Modified J. Henrichs, Bureau of Meteorology

'''This module implements a class that manages the argument for a kernel
call. It especially adds all implicitly required parameters.
It creates the argument in two formats: first as a list of strings, but also
as a list of PSyIR nodes. TODO #1930: the support for the string format
should be removed as we migrate to use PSyIR in LFRic.
'''

from collections import namedtuple

from psyclone import psyGen
from psyclone.core import AccessType, Signature
from psyclone.domain.lfric import ArgOrdering, LFRicConstants
# Avoid circular import:
from psyclone.domain.lfric.lfric_types import LFRicTypes
from psyclone.errors import GenerationError, InternalError
from psyclone.psyir.nodes import ArrayReference, Reference, StructureReference
from psyclone.psyir.symbols import (
    DataSymbol, DataTypeSymbol, UnresolvedType,
    ContainerSymbol, ImportInterface, ScalarType)

# psyir has classes created at runtime
# pylint: disable=no-member
# pylint: disable=too-many-lines


class KernCallArgList(ArgOrdering):
    # pylint: disable=too-many-public-methods
    # TODO: #845 Check that all implicit variables have the right type.
    '''Creates the argument list required to call kernel "kern" from the
    PSy-layer and captures the positions of the following arguments in
    the argument list: nlayers, number of quadrature points and number
    of degrees of freedom. The ordering and type of arguments is
    captured by the base class.

    :param kern: The kernel that is being called.
    :type kern: :py:class:`psyclone.domain.lfric.LFRicKern`

    '''
    NdfInfo = namedtuple("NdfInfo", ["position", "function_space"])

    def __init__(self, kern):
        super().__init__(kern)
        self._nlayers_positions = []
        self._nqp_positions = []
        self._ndf_positions = []

    def get_user_type(self, module_name, user_type, name, tag=None):
        # pylint: disable=too-many-arguments
        '''Returns the symbol for a user-defined type. If required, the
        required import statements will all be generated.

        :param str module_name: the name of the module from which the \
            user-defined type must be imported.
        :param str user_type: the name of the user-defined type.
        :param str name: the name of the variable to be used in the Reference.
        :param Optional[str] tag: tag to use for the variable, defaults to \
            the name

        :return: the symbol that is used in the reference
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        if not tag:
            tag = name

        try:
            sym = self._symtab.lookup_with_tag(tag)
            return sym
        except KeyError:
            pass

        # The symbol does not exist already. So we potentially need to
        # create the import statement for the type:
        try:
            # Check if the module is already declared:
            module = self._symtab.lookup(module_name)
        except KeyError:
            module = self._symtab.new_symbol(module_name,
                                             symbol_type=ContainerSymbol)

        # Get the symbol table in which the module is declared:
        mod_sym_tab = module.find_symbol_table(self._kern)

        # The user-defined type must be declared in the same symbol
        # table as the container (otherwise errors will happen later):
        user_type_symbol = mod_sym_tab.find_or_create(
            user_type,
            symbol_type=DataTypeSymbol,
            datatype=UnresolvedType(),
            interface=ImportInterface(module))
        # Declare the actual user symbol in the local symbol table, using
        # the datatype from the root table:
        sym = self._symtab.new_symbol(name, tag=tag,
                                      symbol_type=DataSymbol,
                                      datatype=user_type_symbol)
        return sym

    def append_structure_reference(self, module_name, user_type, member_list,
                                   name, tag=None, overwrite_datatype=None):
        # pylint: disable=too-many-arguments
        '''Creates a reference to a variable of a user-defined type. If
        required, the required import statements will all be generated.

        :param str module_name: the name of the module from which the
            user-defined type must be imported.
        :param str user_type: the name of the user-defined type.
        :param member_list: the members used hierarchically.
        :type member_list: List[str]
        :param str name: the name of the variable to be used in the Reference.
        :param Optional[str] tag: tag to use for the variable, defaults to
            the name
        :param overwrite_datatype: the datatype for the reference, which will
            overwrite the value determined by analysing the corresponding
            user defined type. This is useful when e.g. the module that
            declares the structure cannot be accessed.
        :type overwrite_datatype:
            Optional[:py:class:`psyclone.psyir.symbols.DataType`]

        :return: the symbol that is used in the reference
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        sym = self.get_user_type(module_name, user_type, name, tag)
        self.psyir_append(StructureReference.
                          create(sym, member_list,
                                 overwrite_datatype=overwrite_datatype))
        return sym

    def cell_position(self, var_accesses=None):
        '''Adds a cell argument to the argument list and if supplied stores
        this access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        cell_ref_name, ref = self.cell_ref_name(var_accesses)
        self.psyir_append(ref)
        self.append(cell_ref_name)

    def cell_map(self, var_accesses=None):
        '''Add cell-map and related cell counts (for inter-grid kernels)
        to the argument list. If supplied it also stores these accesses to the
        var_access object.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        cargs = psyGen.args_filter(self._kern.args, arg_meshes=["gh_coarse"])
        carg = cargs[0]
        fargs = psyGen.args_filter(self._kern.args, arg_meshes=["gh_fine"])
        farg = fargs[0]
        base_name = "cell_map_" + carg.name

        # Add the cell map to our argument list
        cell_ref_name, cell_ref = self.cell_ref_name(var_accesses)
        sym = self.append_array_reference(base_name, [":", ":", cell_ref],
                                          ScalarType.Intrinsic.INTEGER)
        self.append(f"{sym.name}(:,:,{cell_ref_name})",
                    var_accesses=var_accesses, var_access_name=sym.name)

        # No. of fine cells per coarse cell in x
        base_name = f"ncpc_{farg.name}_{carg.name}_x"
        sym = self.append_integer_reference(base_name)
        self.append(sym.name, var_accesses)
        # No. of fine cells per coarse cell in y
        base_name = f"ncpc_{farg.name}_{carg.name}_y"
        sym = self.append_integer_reference(base_name)
        self.append(sym.name, var_accesses)
        # No. of columns in the fine mesh
        base_name = f"ncell_{farg.name}"
        sym = self.append_integer_reference(base_name)
        self.append(sym.name, var_accesses)

    def mesh_height(self, var_accesses=None):
        '''Add mesh height (nlayers) to the argument list and if supplied
        stores this access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        if self._kern.iterates_over not in ["cell_column", "domain"]:
            return
        nlayers_symbol = self.append_integer_reference("nlayers")
        self.append(nlayers_symbol.name, var_accesses)
        self._nlayers_positions.append(self.num_args)

    def scalar(self, scalar_arg, var_accesses=None):
        '''
        Add the necessary argument for a scalar quantity as well as an
        appropriate Symbol to the SymbolTable.

        :param scalar_arg: the scalar kernel argument.
        :type scalar_arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance that \
            stores information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        super().scalar(scalar_arg, var_accesses)
        if scalar_arg.is_literal:
            self.psyir_append(scalar_arg.psyir_expression())
        else:
            sym = self._symtab.lookup(scalar_arg.name)
            self.psyir_append(Reference(sym))

    # TODO uncomment this method when ensuring we only pass ncell3d once
    # to any given kernel.
    # def mesh_ncell3d(self):
    #     ''' Add the number of cells in the full 3D mesh to the argument
    #     list '''
    #     ncell3d_name = self._name_space_manager.create_name(
    #         root_name="ncell_3d", context="PSyVars", label="ncell3d")
    #     self.append(ncell3d_name)

    def _mesh_ncell2d(self, var_accesses=None):
        '''Add the number of columns in the mesh to the argument list and if
        supplied stores this access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        sym = self.append_integer_reference("ncell_2d")
        self.append(sym.name, var_accesses)

    def _mesh_ncell2d_no_halos(self, var_accesses=None):
        '''Add the number of columns in the mesh (excluding those in the halo)
        to the argument list and store this access in var_accesses (if
        supplied).

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        ncell_symbol = self.append_integer_reference("ncell_2d_no_halos")
        self.append(ncell_symbol.name, var_accesses)

    def cma_operator(self, arg, var_accesses=None):
        '''Add the CMA operator and associated scalars to the argument
        list and optionally add them to the variable access
        information.

        :param arg: the CMA operator argument.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        components = ["matrix"]
        # Avoid circular import:
        # pylint: disable=import-outside-toplevel
        from psyclone.dynamo0p3 import DynCMAOperators
        if arg.function_space_to.orig_name != (arg.function_space_from.
                                               orig_name):
            components += DynCMAOperators.cma_diff_fs_params
        else:
            components += DynCMAOperators.cma_same_fs_params

        const = LFRicConstants()
        suffix = const.ARG_TYPE_SUFFIX_MAPPING["gh_columnwise_operator"]

        for component in components:
            # Matrix takes the access from the declaration of the argument
            # (i.e. read, write, ...), the rest are always read-only parameters
            if component == "matrix":
                # Matrix is a pointer to a 3d array
                # REAL(KIND=r_solver), pointer:: cma_op1_matrix(:,:,:)
                #    = > null()
                mode = arg.access
                sym = self._symtab.lookup_with_tag(f"{arg.name}:{suffix}")
                self.psyir_append(ArrayReference.create(sym, [":", ":", ":"]))
            else:
                # All other variables are scalar integers
                name = self._symtab.lookup_with_tag(
                    f"{arg.name}:{component}:{suffix}").name
                mode = AccessType.READ
                sym = self.append_integer_reference(
                    name, tag=f"{arg.name}:{component}:{suffix}")

            self.append(sym.name, var_accesses, mode=mode,
                        metadata_posn=arg.metadata_index)

    def field_vector(self, argvect, var_accesses=None):
        '''Add the field vector associated with the argument 'argvect' to the
        argument list. If supplied it also stores these accesses to the
        var_access object.

        :param argvect: the field vector to add.
        :type argvect: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        suffix = LFRicConstants().ARG_TYPE_SUFFIX_MAPPING[
            argvect.argument_type]
        # The range function below returns values from
        # 1 to the vector size which is what we
        # require in our Fortran code
        for idx in range(1, argvect.vector_size + 1):
            cmpt_sym = self._symtab.lookup_with_tag(
                f"{argvect.name}_{idx}:{suffix}")
            self.psyir_append(Reference(cmpt_sym))
            text = cmpt_sym.name
            self.append(text, metadata_posn=argvect.metadata_index)

        if var_accesses is not None:
            # We add the whole field-vector, not the individual accesses.
            var_accesses.add_access(Signature(argvect.name), argvect.access,
                                    self._kern)

    def field(self, arg, var_accesses=None):
        '''Add the field array associated with the argument 'arg' to the
        argument list. If supplied it also stores this access in var_accesses.

        :param arg: the field to be added.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store
            the information about variable accesses.
        :type var_accesses: :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        const = LFRicConstants()
        suffix = const.ARG_TYPE_SUFFIX_MAPPING[arg.argument_type]
        # Look-up the name of the variable that stores the reference to
        # the data in this field.
        sym = self._symtab.lookup_with_tag(f"{arg.name}:{suffix}")
        # Add the field data array as being read.
        self.append(sym.name, var_accesses, var_access_name=sym.name,
                    mode=arg.access, metadata_posn=arg.metadata_index)

        self.psyir_append(Reference(sym))

    def stencil_unknown_extent(self, arg, var_accesses=None):
        '''Add stencil information to the argument list associated with the
        argument 'arg' if the extent is unknown. If supplied it also stores
        this access in var_accesses.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        # The extent is not specified in the metadata so pass the value in
        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.lfric.lfric_stencils import LFRicStencils
        var_sym = LFRicStencils.dofmap_size_symbol(self._symtab, arg)
        cell_name, cell_ref = self.cell_ref_name(var_accesses)
        self.append_array_reference(var_sym.name, [cell_ref],
                                    ScalarType.Intrinsic.INTEGER,
                                    symbol=var_sym)
        self.append(f"{var_sym.name}({cell_name})", var_accesses,
                    var_access_name=var_sym.name)

    def stencil_2d_unknown_extent(self, arg, var_accesses=None):
        '''Add 2D stencil information to the argument list associated with the
        argument 'arg' if the extent is unknown. If supplied it also stores
        this access in var_accesses.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        # The extent is not specified in the metadata so pass the value in
        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.lfric.lfric_stencils import LFRicStencils
        var_sym = LFRicStencils.dofmap_size_symbol(self._symtab, arg)
        cell_name, cell_ref = self.cell_ref_name(var_accesses)
        self.append_array_reference(var_sym.name, [":", cell_ref],
                                    ScalarType.Intrinsic.INTEGER,
                                    symbol=var_sym)
        name = f"{var_sym.name}(:,{cell_name})"
        self.append(name, var_accesses, var_access_name=var_sym.name)

    def stencil_2d_max_extent(self, arg, var_accesses=None):
        '''Add the maximum branch extent for a 2D stencil associated with the
        argument 'arg' to the argument list. If supplied it also stores this
        in var_accesses.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`pclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional SingleVariableAccessInfo instance \
            to store the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.SingleVariableAccessInfo`

        '''
        # The maximum branch extent is not specified in the metadata so pass
        # the value in.
        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.lfric.lfric_stencils import LFRicStencils
        # TODO #1915, this duplicates code in
        # LFRicStencils.max_branch_length_name
        unique_tag = LFRicStencils.stencil_unique_str(arg, "length")
        root_name = arg.name + "_max_branch_length"

        sym = self.append_integer_reference(root_name, tag=unique_tag)
        self.append(sym.name, var_accesses)

    def stencil_unknown_direction(self, arg, var_accesses=None):
        '''Add stencil information to the argument list associated with the
        argument 'arg' if the direction is unknown (i.e. it's being supplied
        in a variable). If supplied it also stores this access in
        var_accesses.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        # the direction of the stencil is not known so pass the value in
        name = arg.stencil.direction_arg.varname
        self.append_integer_reference(name, f"AlgArgs_{name}")
        self.append(name, var_accesses)

    def stencil(self, arg, var_accesses=None):
        '''Add general stencil information associated with the argument 'arg'
        to the argument list. If supplied it also stores this access in
        var_accesses.

        :param arg: the meta-data description of the kernel \
            argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        # add in stencil dofmap
        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.lfric.lfric_stencils import LFRicStencils
        var_sym = LFRicStencils.dofmap_symbol(self._symtab, arg)
        cell_name, cell_ref = self.cell_ref_name(var_accesses)
        self.append_array_reference(var_sym.name, [":", ":", cell_ref],
                                    ScalarType.Intrinsic.INTEGER,
                                    symbol=var_sym)
        self.append(f"{var_sym.name}(:,:,{cell_name})", var_accesses,
                    var_access_name=var_sym.name)

    def stencil_2d(self, arg, var_accesses=None):
        '''Add general 2D stencil information associated with the argument
        'arg' to the argument list. If supplied it also stores this access in
        var_accesses.

        :param arg: the meta-data description of the kernel \
            argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        # The stencil_2D differs from the stencil in that the direction
        # of the branch is baked into the stencil_dofmap array.
        # The array dimensions are thus (dof_in_cell, cell_in_branch,
        # branch_in_stencil) where the branch_in_stencil is always ordered
        # West, South, East, North which is standard in LFRic. This allows
        # for knowledge of what direction a stencil cell is in relation
        # to the center even when the stencil is truncated at boundaries.
        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.lfric.lfric_stencils import LFRicStencils
        var_sym = LFRicStencils.dofmap_symbol(self._symtab, arg)
        cell_name, cell_ref = self.cell_ref_name(var_accesses)
        self.append_array_reference(var_sym.name,
                                    [":", ":", ":", cell_ref],
                                    ScalarType.Intrinsic.INTEGER,
                                    symbol=var_sym)
        name = f"{var_sym.name}(:,:,:,{cell_name})"
        self.append(name, var_accesses, var_access_name=var_sym.name)

    def operator(self, arg, var_accesses=None):
        '''Add the operator arguments to the argument list. If supplied it
        also stores this access in var_accesses.

        :param arg: the meta-data description of the operator.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        # TODO we should only be including ncell_3d once in the argument
        # list but this adds it for every operator
        # This argument is always read only:
        if arg.data_type == "r_solver_operator_type":
            op_name = "r_solver_operator"
        elif arg.data_type == "r_tran_operator_type":
            op_name = "r_tran_operator"
        else:
            op_name = "operator"
        const = LFRicConstants()
        operator = const.DATA_TYPE_MAP[op_name]
        self.append_structure_reference(
            operator["module"], operator["proxy_type"], ["ncell_3d"],
            arg.proxy_name_indexed,
            overwrite_datatype=LFRicTypes("LFRicIntegerScalarDataType")())
        self.append(arg.proxy_name_indexed + "%ncell_3d", var_accesses,
                    mode=AccessType.READ)

        sym = self._symtab.lookup_with_tag(
            f"{arg.name}:{const.ARG_TYPE_SUFFIX_MAPPING[arg.argument_type]}")
        self.psyir_append(Reference(sym))
        # The access mode of `local_stencil` is taken from the meta-data:
        self.append(sym.name, var_accesses,
                    mode=arg.access, metadata_posn=arg.metadata_index)

    def fs_common(self, function_space, var_accesses=None):
        '''Add function-space related arguments common to LMA operators and
        fields. If supplied it also stores this access in var_accesses.

        :param function_space: the function space for which the related \
            arguments common to LMA operators and fields are added.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        if self._kern.iterates_over not in ["cell_column", "domain"]:
            return
        super().fs_common(function_space, var_accesses)
        self._ndf_positions.append(
            KernCallArgList.NdfInfo(position=self.num_args,
                                    function_space=function_space.orig_name))

    def fs_compulsory_field(self, function_space, var_accesses=None):
        '''Add compulsory arguments associated with this function space to
        the list. If supplied it also stores this access in var_accesses.

        :param function_space: the function space for which the compulsory \
            arguments are added.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        sym = self.append_integer_reference(function_space.undf_name)
        self.append(sym.name, var_accesses)

        map_name = function_space.map_name
        if self._kern.iterates_over == 'domain':
            # This kernel takes responsibility for iterating over cells so
            # pass the whole dofmap.
            sym = self.append_array_reference(map_name, [":", ":"],
                                              ScalarType.Intrinsic.INTEGER)
            self.append(sym.name, var_accesses, var_access_name=sym.name)
        else:
            # Pass the dofmap for the cell column
            cell_name, cell_ref = self.cell_ref_name(var_accesses)
            sym = self.append_array_reference(map_name, [":", cell_ref],
                                              ScalarType.Intrinsic.INTEGER)
            self.append(f"{sym.name}(:,{cell_name})",
                        var_accesses, var_access_name=sym.name)

    def fs_intergrid(self, function_space, var_accesses=None):
        '''Add function-space related arguments for an intergrid kernel.
        If supplied it also stores this access in var_accesses.

        :param function_space: the function space for which to add arguments
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        # Is this FS associated with the coarse or fine mesh? (All fields
        # on a given mesh must be on the same FS.)
        arg = self._kern.arguments.get_arg_on_space(function_space)
        if arg.mesh == "gh_fine":
            # For the fine mesh, we need ndf, undf and the *whole*
            # dofmap
            self.fs_common(function_space, var_accesses=var_accesses)
            sym = self.append_integer_reference(function_space.undf_name)
            self.append(sym.name, var_accesses)
            map_name = function_space.map_name
            sym = self.append_array_reference(map_name, [":", ":"],
                                              ScalarType.Intrinsic.INTEGER)
            self.append(sym.name, var_accesses)
        else:
            # For the coarse mesh we only need undf and the dofmap for
            # the current column
            self.fs_compulsory_field(function_space,
                                     var_accesses=var_accesses)

    def basis(self, function_space, var_accesses=None):
        '''Add basis function information for this function space to the
        argument list and optionally to the variable access information.

        :param function_space: the function space for which the basis \
                               function is required.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        for rule in self._kern.qr_rules.values():
            basis_name = function_space.get_basis_name(qr_var=rule.psy_name)
            sym = self.append_array_reference(basis_name, [":", ":", ":", ":"],
                                              ScalarType.Intrinsic.REAL)
            self.append(sym.name, var_accesses)

        if "gh_evaluator" in self._kern.eval_shapes:
            # We are dealing with an evaluator and therefore need as many
            # basis functions as there are target function spaces.
            for fs_name in self._kern.eval_targets:
                # The associated FunctionSpace object is the first item in
                # the tuple dict entry associated with the name of the target
                # function space
                fspace = self._kern.eval_targets[fs_name][0]
                basis_name = function_space.get_basis_name(on_space=fspace)
                sym = self.append_array_reference(basis_name, [":", ":", ":"],
                                                  ScalarType.Intrinsic.REAL)
                self.append(sym.name, var_accesses)

    def diff_basis(self, function_space, var_accesses=None):
        '''Add differential basis information for the function space to the
        argument list. If supplied it also stores this access in
        var_accesses.

        :param function_space: the function space for which the differential \
            basis functions are required.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        for rule in self._kern.qr_rules.values():
            diff_basis_name = function_space.get_diff_basis_name(
                qr_var=rule.psy_name)
            sym = self.append_array_reference(diff_basis_name,
                                              [":", ":", ":", ":"],
                                              ScalarType.Intrinsic.REAL)
            self.append(sym.name, var_accesses)

        if "gh_evaluator" in self._kern.eval_shapes:
            # We are dealing with an evaluator and therefore need as many
            # basis functions as there are target function spaces.
            for fs_name in self._kern.eval_targets:
                # The associated FunctionSpace object is the first item in
                # the tuple dict entry associated with the name of the target
                # function space
                fspace = self._kern.eval_targets[fs_name][0]
                diff_basis_name = function_space.get_diff_basis_name(
                    on_space=fspace)
                sym = self.append_array_reference(diff_basis_name,
                                                  [":", ":", ":"],
                                                  ScalarType.Intrinsic.REAL)
                self.append(sym.name, var_accesses)

    def field_bcs_kernel(self, function_space, var_accesses=None):
        '''Implement the boundary_dofs array fix for a field. If supplied it
        also stores this access in var_accesses.

        :param function_space: the function space for which boundary dofs \
            are required.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        :raises GenerationError: if the bcs kernel does not contain \
            a field as argument (but e.g. an operator).

        '''
        fspace = None
        for fspace in self._kern.arguments.unique_fss:
            if fspace.orig_name == "any_space_1":
                break
        farg = self._kern.arguments.get_arg_on_space(fspace)
        # Sanity check - expect the enforce_bc_code kernel to only have
        # a field argument.
        if not farg.is_field:
            const = LFRicConstants()
            raise GenerationError(
                f"Expected an argument of {const.VALID_FIELD_NAMES} type "
                f"from which to look-up boundary dofs for kernel "
                f"{self._kern.name} but got '{farg.argument_type}'")

        base_name = "boundary_dofs_" + farg.name
        sym = self.append_array_reference(base_name, [":", ":"],
                                          ScalarType.Intrinsic.INTEGER)
        self.append(sym.name, var_accesses)

    def operator_bcs_kernel(self, function_space, var_accesses=None):
        '''Supply necessary additional arguments for the kernel that
        applies boundary conditions to a LMA operator. If supplied it
        also stores this access in var_accesses.

        :param function_space: unused, only for consistency with base class.
        :type function_space: :py:class:`psyclone.dynamo3.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        # This kernel has only a single LMA operator as argument.
        # Checks for this are performed in ArgOrdering.generate()
        op_arg = self._kern.arguments.args[0]
        base_name = "boundary_dofs_" + op_arg.name
        sym = self.append_array_reference(base_name, [":", ":"],
                                          ScalarType.Intrinsic.INTEGER)
        self.append(sym.name, var_accesses)

    def mesh_properties(self, var_accesses=None):
        '''Provide the kernel arguments required for the mesh properties
        specified in the kernel metadata. If supplied it also stores this
        access in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        if self._kern.mesh.properties:
            # Avoid circular import:
            # pylint: disable=import-outside-toplevel
            from psyclone.dynamo0p3 import LFRicMeshProperties
            self.extend(LFRicMeshProperties(self._kern).
                        kern_args(stub=False, var_accesses=var_accesses,
                                  kern_call_arg_list=self))

    def quad_rule(self, var_accesses=None):
        '''Add quadrature-related information to the kernel argument list.
        Adds the necessary arguments to the argument list, and optionally
        adds variable access information to the var_accesses object.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        # The QR shapes that this routine supports
        supported_qr_shapes = ["gh_quadrature_xyoz", "gh_quadrature_edge",
                               "gh_quadrature_face"]

        for shape, rule in self._kern.qr_rules.items():
            if shape == "gh_quadrature_xyoz":
                # XYoZ quadrature requires the number of quadrature points in
                # the horizontal and in the vertical.
                self._nqp_positions.append(
                    {"horizontal": self.num_args + 1,
                     "vertical": self.num_args + 2})
                self.extend(rule.kernel_args, var_accesses)
            elif shape == "gh_quadrature_edge":
                # TODO #705 support transformations supplying the number of
                # quadrature points for edge quadrature.
                self.extend(rule.kernel_args, var_accesses)
            elif shape == "gh_quadrature_face":
                # TODO #705 support transformations supplying the number of
                # quadrature points for face quadrature.
                self.extend(rule.kernel_args, var_accesses)
            else:
                raise NotImplementedError(
                    f"quad_rule: no support implemented for quadrature with a "
                    f"shape of '{shape}'. Supported shapes are: "
                    f"{supported_qr_shapes}.")
            # Now define the arguments using PSyIR:
            for arg in rule.kernel_args:
                # Each rule has a `psy_name` (e.g. qr_xyoz), which is appended
                # to all variable names (e.g. np_xy_qr_xyoz). Remove this
                # suffix to get the 'generic' name, from which we derive
                # the correct type:
                generic_name = arg[:-len(rule.psy_name)-1]
                if generic_name in ["np_xy", "np_z", "nfaces", "np_xyz",
                                    "nedges"]:
                    # np_xy, np_z, nfaces, np_xyz, nedges are all integers:
                    self.append_integer_reference(arg)
                elif generic_name in ["weights_xy", "weights_z"]:
                    # 1d arrays:
                    # TODO # 1910: These should be pointers
                    self.append_array_reference(arg, [":"],
                                                ScalarType.Intrinsic.REAL)
                elif generic_name in ["weights_xyz"]:
                    # 2d arrays:
                    # TODO #1910: These should be pointers
                    self.append_array_reference(arg, [":", ":"],
                                                ScalarType.Intrinsic.REAL)
                else:
                    raise InternalError(f"Found invalid kernel argument "
                                        f"'{arg}'.")

    @property
    def nlayers_positions(self):
        ''':returns: the position(s) in the argument list of the \
            variable(s) that passes the number of layers. The generate \
            method must be called first.
        :rtype: list of int.

        :raises InternalError: if the generate() method has not been called.

        '''
        if not self._generate_called:
            raise InternalError(
                "KernCallArgList: the generate() method should be called "
                "before the nlayers_positions() method")
        return self._nlayers_positions

    @property
    def nqp_positions(self):
        ''':return: the positions in the argument list of the variables that \
            pass the number of quadrature points. The number and type of \
            these will change depending on the type of quadrature. A list \
            of dictionaries is returned with the quadrature types \
            being the keys to the dictionaries and their position in the \
            argument list being the values. At the moment only XYoZ is \
            supported (which has horizontal and vertical quadrature \
            points). The generate method must be called first.
        :rtype: [{str: int, ...}]

        :raises InternalError: if the generate() method has not been \
        called.

        '''
        if not self._generate_called:
            raise InternalError(
                "KernCallArgList: the generate() method should be called "
                "before the nqp_positions() method")
        return self._nqp_positions

    @property
    def ndf_positions(self):
        ''':return: the position(s) in the argument list and the function \
            space(s) associated with the variable(s) that pass(es) the \
            number of degrees of freedom for the function space. The \
            generate method must be called first.
        :rtype: list of namedtuple (position=int, function_space=str).

        :raises InternalError: if the generate() method has not been \
            called.

        '''
        if not self._generate_called:
            raise InternalError(
                "KernCallArgList: the generate() method should be called "
                "before the ndf_positions() method")
        return self._ndf_positions

    def cell_ref_name(self, var_accesses=None):
        '''Utility routine which determines whether to return the cell value
        or the colourmap lookup value. If supplied it also stores this access
        in var_accesses.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        :returns: the Fortran code needed to access the current cell index.
        :rtype: Tuple[str, py:class:`psyclone.psyir.nodes.Reference`]

        '''
        cell_sym = self._symtab.find_or_create_integer_symbol(
            "cell", tag="cell_loop_idx")
        if self._kern.is_coloured():
            colour_sym = self._symtab.find_or_create_integer_symbol(
                "colour", tag="colours_loop_idx")
            if self._kern.is_intergrid:
                tag = None
            else:
                # If there is only one colourmap we need to specify the tag
                # to make sure we get the right symbol.
                tag = "cmap"
            array_ref = self.get_array_reference(self._kern.colourmap,
                                                 [Reference(colour_sym),
                                                  Reference(cell_sym)],
                                                 ScalarType.Intrinsic.INTEGER,
                                                 tag=tag)
            if var_accesses is not None:
                var_accesses.add_access(Signature(colour_sym.name),
                                        AccessType.READ, self._kern)
                var_accesses.add_access(Signature(cell_sym.name),
                                        AccessType.READ, self._kern)
                var_accesses.add_access(Signature(array_ref.name),
                                        AccessType.READ,
                                        self._kern, ["colour", "cell"])

            return (self._kern.colourmap + "(colour,cell)",
                    array_ref)

        if var_accesses is not None:
            var_accesses.add_access(Signature("cell"), AccessType.READ,
                                    self._kern)

        return (cell_sym.name, Reference(cell_sym))


# ============================================================================
# For automatic documentation creation:
__all__ = ["KernCallArgList"]
