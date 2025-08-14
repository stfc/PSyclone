# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
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
# Modified I. Kavcic, A. Coughtrie, L. Turner, and A. Pirrie, Met Office
# Modified J. Henrichs, Bureau of Meteorology

'''This module implements a class that manages the argument for a kernel
call. It especially adds all implicitly required parameters.
It creates the argument in two formats: first as a list of strings, but also
as a list of PSyIR nodes. TODO #1930: the support for the string format
should be removed as we migrate to use PSyIR in LFRic.
'''

from dataclasses import dataclass
from typing import Optional, Tuple

from psyclone import psyGen
from psyclone.core import AccessType, Signature, VariablesAccessMap
from psyclone.domain.lfric.arg_ordering import ArgOrdering
from psyclone.domain.lfric.lfric_constants import LFRicConstants
from psyclone.domain.lfric.lfric_types import LFRicTypes
from psyclone.errors import GenerationError, InternalError
from psyclone.psyir.nodes import (
    ArrayReference, Reference, StructureReference)
from psyclone.psyir.symbols import (
    DataSymbol, DataTypeSymbol, UnresolvedType, ContainerSymbol,
    ImportInterface, ScalarType, ArrayType, UnsupportedFortranType,
    ArgumentInterface)

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
    @dataclass(frozen=True)
    class NdfInfo:
        '''
        Holds information relating to the number-of-dofs kernel argument.

        :param position: the position of this argument in the argument list.
        :param function_space: the function space that this argument is for.
        '''
        position: int = None
        function_space: str = None

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
            # Get the symbol table in which the module is declared:
            mod_sym_tab = module.find_symbol_table(self._kern)
        except KeyError:
            module = self._symtab.new_symbol(module_name,
                                             symbol_type=ContainerSymbol)
            mod_sym_tab = self._symtab

        # The user-defined type must be declared in the same symbol
        # table as the container (otherwise errors will happen later):
        user_type_symbol = mod_sym_tab.find_or_create(
            user_type,
            symbol_type=DataTypeSymbol,
            datatype=UnresolvedType(),
            interface=ImportInterface(module))
        # Declare the actual user symbol in the local symbol table, using
        # the datatype from the root table:
        sym = self._symtab.find_or_create(name, tag=tag,
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

    def cell_position(self, var_accesses: Optional[VariablesAccessMap] = None):
        '''Adds a cell argument to the argument list and if supplied stores
        this access in var_accesses.

        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        '''
        cell_ref_name, ref = self.cell_ref_name(var_accesses)
        self.psyir_append(ref)
        self.append(cell_ref_name)

    def cell_map(self, var_accesses: Optional[VariablesAccessMap] = None):
        '''Add cell-map and related cell counts (for inter-grid kernels)
        to the argument list. If supplied it also stores these accesses to the
        var_access object.

        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        '''
        cargs = psyGen.args_filter(self._kern.args, arg_meshes=["gh_coarse"])
        carg = cargs[0]
        fargs = psyGen.args_filter(self._kern.args, arg_meshes=["gh_fine"])
        farg = fargs[0]
        base_name = "cell_map_" + carg.name

        # Add the cell map to our argument list
        cell_ref_name, cell_ref = self.cell_ref_name(var_accesses)
        sym = self.append_array_reference(base_name, [":", ":", cell_ref])
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

    def mesh_height(self, var_accesses: Optional[VariablesAccessMap] = None):
        '''Add mesh height (nlayers) to the argument list and if supplied
        stores this access in var_accesses.

        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        '''
        if self._kern.iterates_over == "dof":
            return
        name = f"nlayers_{self._kern.arguments.first_field_or_operator.name}"
        nlayers_symbol = self.append_integer_reference(name, tag=name)
        self.append(nlayers_symbol.name, var_accesses)
        self._nlayers_positions.append(self.num_args)

    def scalar(self, scalar_arg,
               var_accesses: Optional[VariablesAccessMap] = None):
        '''
        Add the necessary argument for a scalar quantity as well as an
        appropriate Symbol to the SymbolTable.

        :param scalar_arg: the scalar kernel argument.
        :type scalar_arg: :py:class:`psyclone.lfric.LFRicKernelArgument`
        :param var_accesses: optional VariablesAccessMap instance that
            stores information about variable accesses.

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

    def _mesh_ncell2d(self, var_accesses: Optional[VariablesAccessMap] = None):
        '''Add the number of columns in the mesh to the argument list and if
        supplied stores this access in var_accesses.

        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        '''
        sym = self.append_integer_reference("ncell_2d")
        self.append(sym.name, var_accesses)

    def _mesh_ncell2d_no_halos(
            self, var_accesses: Optional[VariablesAccessMap] = None):
        '''Add the number of columns in the mesh (excluding those in the halo)
        to the argument list and store this access in var_accesses (if
        supplied).

        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        '''
        ncell_symbol = self.append_integer_reference("ncell_2d_no_halos")
        self.append(ncell_symbol.name, var_accesses)

    def cma_operator(self, arg,
                     var_accesses: Optional[VariablesAccessMap] = None):
        '''Add the CMA operator and associated scalars to the argument
        list and optionally add them to the variable access
        information.

        :param arg: the CMA operator argument.
        :type arg: :py:class:`psyclone.lfric.LFRicKernelArgument`
        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        '''
        components = ["matrix"]
        # Avoid circular import:
        # pylint: disable=import-outside-toplevel
        from psyclone.lfric import LFRicCMAOperators
        if arg.function_space_to.orig_name != (arg.function_space_from.
                                               orig_name):
            components += LFRicCMAOperators.cma_diff_fs_params
        else:
            components += LFRicCMAOperators.cma_same_fs_params

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
                sym = self._symtab.find_or_create_tag(
                    f"{arg.name}:{suffix}", arg.name,
                    symbol_type=DataSymbol, datatype=UnresolvedType(),
                )
                self.psyir_append(ArrayReference.create(sym, [":", ":", ":"]))
            else:
                # All other variables are scalar integers
                name = self._symtab.find_or_create_tag(
                    f"{arg.name}:{component}:{suffix}", arg.name,
                    symbol_type=DataSymbol,
                    datatype=LFRicTypes("LFRicIntegerScalarDataType")(),
                    interface=ArgumentInterface(ArgumentInterface.Access.READ)
                ).name
                mode = AccessType.READ
                sym = self.append_integer_reference(
                    name, tag=f"{arg.name}:{component}:{suffix}")

            self.append(sym.name, var_accesses, mode=mode,
                        metadata_posn=arg.metadata_index)

    def field_vector(self, argvect,
                     var_accesses: Optional[VariablesAccessMap] = None):
        '''Add the field vector associated with the argument 'argvect' to the
        argument list. If supplied it also stores these accesses to the
        var_access object.

        :param argvect: the field vector to add.
        :type argvect: :py:class:`psyclone.lfric.LFRicKernelArgument`
        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        '''
        suffix = LFRicConstants().ARG_TYPE_SUFFIX_MAPPING[
            argvect.argument_type]

        # The range function below returns values from
        # 1 to the vector size which is what we
        # require in our Fortran code
        for idx in range(1, argvect.vector_size + 1):
            # Look-up the name of the variable that stores the reference to
            # the data in this field.
            cmpt_sym = self._symtab.lookup_with_tag(
                f"{argvect.name}_{idx}:{suffix}")
            if self._kern.iterates_over == "dof":
                # If dof kernel, add access to the field by dof ref
                dof_sym = self._symtab.find_or_create_integer_symbol(
                    "df", tag="dof_loop_idx")
                # TODO #1010 removes the need to declare type and
                # allows this to be fixed
                self.append_array_reference(cmpt_sym.name,
                                            [Reference(dof_sym)],
                                            ScalarType.Intrinsic.INTEGER,
                                            symbol=cmpt_sym)
                # Append the dof symbol
                text = f"{cmpt_sym.name}({dof_sym.name})"
            else:
                self.psyir_append(Reference(cmpt_sym))
                text = cmpt_sym.name
            self.append(text, metadata_posn=argvect.metadata_index)

        if var_accesses is not None:
            # We add the whole field-vector, not the individual accesses.
            var_accesses.add_access(Signature(argvect.name), argvect.access,
                                    self._kern)

    def field(self, arg, var_accesses: Optional[VariablesAccessMap] = None):
        '''Add the field array associated with the argument 'arg' to the
        argument list. If supplied it also stores this access in var_accesses.

        :param arg: the field to be added.
        :type arg: :py:class:`psyclone.lfric.LFRicKernelArgument`
        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        '''
        const = LFRicConstants()
        suffix = const.ARG_TYPE_SUFFIX_MAPPING[arg.argument_type]
        # Look-up the name of the variable that stores the reference to
        # the data in this field.
        sym = self._symtab.lookup_with_tag(f"{arg.name}:{suffix}")

        if self._kern.iterates_over == "dof":
            # If dof kernel, add access to the field by dof ref
            dof_sym = self._symtab.find_or_create_integer_symbol(
                "df", tag="dof_loop_idx")
            # TODO #1010 removes the need to declare type and
            # allows this to be fixed
            self.append_array_reference(sym.name, [Reference(dof_sym)],
                                        ScalarType.Intrinsic.INTEGER,
                                        symbol=sym)
            # Then append our symbol
            name = f"{sym.name}({dof_sym.name})"
            self.append(name, var_accesses, var_access_name=sym.name)
        else:
            # Add the field data array as being read.
            self.append(sym.name, var_accesses, var_access_name=sym.name,
                        mode=arg.access, metadata_posn=arg.metadata_index)
            self.psyir_append(Reference(sym))

    def stencil_unknown_extent(
            self, arg, var_accesses: Optional[VariablesAccessMap] = None):
        '''Add stencil information to the argument list associated with the
        argument 'arg' if the extent is unknown. If supplied it also stores
        this access in var_accesses.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.lfric.LFRicKernelArgument`
        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        '''
        # The extent is not specified in the metadata so pass the value in
        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.lfric.lfric_stencils import LFRicStencils
        var_sym = LFRicStencils.dofmap_size_symbol(self._symtab, arg)
        cell_name, cell_ref = self.cell_ref_name(var_accesses)
        self.append_array_reference(var_sym.name, [cell_ref],
                                    symbol=var_sym)
        self.append(f"{var_sym.name}({cell_name})", var_accesses,
                    var_access_name=var_sym.name)

    def stencil_2d_unknown_extent(
            self, arg, var_accesses: Optional[VariablesAccessMap] = None):
        '''Add 2D stencil information to the argument list associated with the
        argument 'arg' if the extent is unknown. If supplied it also stores
        this access in var_accesses.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.lfric.LFRicKernelArgument`
        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        '''
        # The extent is not specified in the metadata so pass the value in
        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.lfric.lfric_stencils import LFRicStencils
        var_sym = LFRicStencils.dofmap_size_symbol(self._symtab, arg)
        cell_name, cell_ref = self.cell_ref_name(var_accesses)
        self.append_array_reference(var_sym.name, [":", cell_ref],
                                    symbol=var_sym)
        name = f"{var_sym.name}(:,{cell_name})"
        self.append(name, var_accesses, var_access_name=var_sym.name)

    def stencil_2d_max_extent(
            self, arg, var_accesses: Optional[VariablesAccessMap] = None):
        '''Add the maximum branch extent for a 2D stencil associated with the
        argument 'arg' to the argument list. If supplied it also stores this
        in var_accesses.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.lfric.LFRicKernelArgument`
        :param var_accesses: optional VariableAccessMap instance to store the
            information about variable accesses.

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

    def stencil_unknown_direction(
            self, arg, var_accesses: Optional[VariablesAccessMap] = None):
        '''Add stencil information to the argument list associated with the
        argument 'arg' if the direction is unknown (i.e. it's being supplied
        in a variable). If supplied it also stores this access in
        var_accesses.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.lfric.LFRicKernelArgument`
        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        '''
        # the direction of the stencil is not known so pass the value in
        name = arg.stencil.direction_arg.varname
        tag = arg.stencil.direction_arg.text
        self.append_integer_reference(name, f"AlgArgs_{tag}")
        self.append(name, var_accesses)

    def stencil(self, arg, var_accesses: Optional[VariablesAccessMap] = None):
        '''Add general stencil information associated with the argument 'arg'
        to the argument list. If supplied it also stores this access in
        var_accesses.

        :param arg: the meta-data description of the kernel
            argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.lfric.LFRicKernelArgument`
        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        '''
        # add in stencil dofmap
        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.lfric.lfric_stencils import LFRicStencils
        var_sym = LFRicStencils.dofmap_symbol(self._symtab, arg)
        cell_name, cell_ref = self.cell_ref_name(var_accesses)
        self.append_array_reference(var_sym.name, [":", ":", cell_ref],
                                    symbol=var_sym)
        self.append(f"{var_sym.name}(:,:,{cell_name})", var_accesses,
                    var_access_name=var_sym.name)

    def stencil_2d(
            self, arg, var_accesses: Optional[VariablesAccessMap] = None):
        '''Add general 2D stencil information associated with the argument
        'arg' to the argument list. If supplied it also stores this access in
        var_accesses.

        :param arg: the meta-data description of the kernel
            argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.lfric.LFRicKernelArgument`
        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

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
                                    symbol=var_sym)
        name = f"{var_sym.name}(:,:,:,{cell_name})"
        self.append(name, var_accesses, var_access_name=var_sym.name)

    def operator(self, arg, var_accesses: Optional[VariablesAccessMap] = None):
        '''Add the operator arguments to the argument list. If supplied it
        also stores this access in var_accesses.

        :param arg: the meta-data description of the operator.
        :type arg: :py:class:`psyclone.lfric.LFRicKernelArgument`
        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

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

    def fs_common(self, function_space,
                  var_accesses: Optional[VariablesAccessMap] = None):
        '''Add function-space related arguments common to LMA operators and
        fields. If supplied it also stores this access in var_accesses.

        :param function_space: the function space for which the related
            arguments common to LMA operators and fields are added.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        '''
        if self._kern.iterates_over == "dof":
            return
        super().fs_common(function_space, var_accesses)
        self._ndf_positions.append(
            KernCallArgList.NdfInfo(position=self.num_args,
                                    function_space=function_space.orig_name))

    def fs_compulsory_field(
            self, function_space,
            var_accesses: Optional[VariablesAccessMap] = None):
        '''Add compulsory arguments associated with this function space to
        the list. If supplied it also stores this access in var_accesses.

        :param function_space: the function space for which the compulsory
            arguments are added.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        '''
        if self._kern.iterates_over == "dof":
            # Dofmaps and `undf` are not required for DoF kernels
            return

        sym = self.append_integer_reference(function_space.undf_name)
        self.append(sym.name, var_accesses)

        map_name = function_space.map_name
        intrinsic_type = LFRicTypes("LFRicIntegerScalarDataType")()
        dtype = UnsupportedFortranType(
            f"{intrinsic_type.intrinsic.name}("
            f"kind={intrinsic_type.precision.name}), pointer, "
            f"dimension(:,:) :: {map_name} => null()",
            partial_datatype=ArrayType(
                intrinsic_type,
                [ArrayType.Extent.DEFERRED, ArrayType.Extent.DEFERRED]))
        sym = self._symtab.find_or_create_tag(
            map_name, symbol_type=DataSymbol, datatype=dtype)

        if self._kern.iterates_over == 'domain':
            # This kernel takes responsibility for iterating over cells so
            # pass the whole dofmap.
            self.append_array_reference(map_name, [":", ":"], symbol=sym)
            self.append(sym.name, var_accesses, var_access_name=sym.name)
        else:
            # Pass the dofmap for the cell column
            cell_name, cell_ref = self.cell_ref_name(var_accesses)
            self.append_array_reference(map_name, [":", cell_ref], symbol=sym)
            self.append(f"{sym.name}(:,{cell_name})",
                        var_accesses, var_access_name=sym.name)

    def fs_intergrid(self, function_space,
                     var_accesses: Optional[VariablesAccessMap] = None):
        '''Add function-space related arguments for an intergrid kernel.
        If supplied it also stores this access in var_accesses.

        :param function_space: the function space for which to add arguments
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

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
            sym = self.append_array_reference(map_name, [":", ":"])
            self.append(sym.name, var_accesses)
        else:
            # For the coarse mesh we only need undf and the dofmap for
            # the current column
            self.fs_compulsory_field(function_space,
                                     var_accesses=var_accesses)

    def basis(self, function_space,
              var_accesses: Optional[VariablesAccessMap] = None):
        '''Add basis function information for this function space to the
        argument list and optionally to the variable access information.

        :param function_space: the function space for which the basis
                               function is required.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        '''
        for rule in self._kern.qr_rules.values():
            basis_name = function_space.get_basis_name(qr_var=rule.psy_name)
            sym = self.append_array_reference(
                    basis_name, [":", ":", ":", ":"],
                    LFRicTypes("LFRicRealScalarDataType")()
                )
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
                sym = self.append_array_reference(basis_name, [":", ":", ":"])
                self.append(sym.name, var_accesses)

    def diff_basis(self, function_space,
                   var_accesses: Optional[VariablesAccessMap] = None):
        '''Add differential basis information for the function space to the
        argument list. If supplied it also stores this access in
        var_accesses.

        :param function_space: the function space for which the differential
            basis functions are required.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        '''
        for rule in self._kern.qr_rules.values():
            diff_basis_name = function_space.get_diff_basis_name(
                qr_var=rule.psy_name)
            sym = self.append_array_reference(
                    diff_basis_name,
                    [":", ":", ":", ":"],
                    LFRicTypes("LFRicRealScalarDataType")()
            )
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
                sym = self.append_array_reference(
                                  diff_basis_name,
                                  [":", ":", ":"],
                                  LFRicTypes("LFRicRealScalarDataType")())
                self.append(sym.name, var_accesses)

    def field_bcs_kernel(self, function_space,
                         var_accesses: Optional[VariablesAccessMap] = None):
        '''Implement the boundary_dofs array fix for a field. If supplied it
        also stores this access in var_accesses.

        :param function_space: the function space for which boundary dofs
            are required.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        :raises GenerationError: if the bcs kernel does not contain
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
        sym = self.append_array_reference(base_name, [":", ":"])
        self.append(sym.name, var_accesses)

    def operator_bcs_kernel(self, function_space,
                            var_accesses: Optional[VariablesAccessMap] = None):
        '''Supply necessary additional arguments for the kernel that
        applies boundary conditions to a LMA operator. If supplied it
        also stores this access in var_accesses.

        :param function_space: unused, only for consistency with base class.
        :type function_space: :py:class:`psyclone.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        '''
        # This kernel has only a single LMA operator as argument.
        # Checks for this are performed in ArgOrdering.generate()
        op_arg = self._kern.arguments.args[0]
        base_name = "boundary_dofs_" + op_arg.name
        sym = self.append_array_reference(base_name, [":", ":"])
        self.append(sym.name, var_accesses)

    def mesh_properties(self,
                        var_accesses: Optional[VariablesAccessMap] = None):
        '''Provide the kernel arguments required for the mesh properties
        specified in the kernel metadata. If supplied it also stores this
        access in var_accesses.

        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        '''
        if self._kern.mesh.properties:
            # Avoid circular import:
            # pylint: disable=import-outside-toplevel
            from psyclone.lfric import LFRicMeshProperties
            self.extend(LFRicMeshProperties(self._kern).
                        kern_args(stub=False, var_accesses=var_accesses,
                                  kern_call_arg_list=self))

    def quad_rule(self, var_accesses: Optional[VariablesAccessMap] = None):
        '''Add quadrature-related information to the kernel argument list.
        Adds the necessary arguments to the argument list, and optionally
        adds variable access information to the var_accesses object.

        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

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
                    self.append_array_reference(
                                  arg, [":"],
                                  LFRicTypes("LFRicRealScalarDataType")())
                elif generic_name in ["weights_xyz"]:
                    # 2d arrays:
                    self.append_array_reference(
                                  arg, [":", ":"],
                                  LFRicTypes("LFRicRealScalarDataType")())
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

    def cell_ref_name(
            self, var_accesses: Optional[VariablesAccessMap] = None
    ) -> Tuple[str, Reference]:
        ''' Utility routine which determines whether to return the cell
        reference or the colourmap/tilemap lookup array references. If supplied
        with a "var_accesses" it also stores the Variables Access information.

        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        :returns: the variable name and a reference to access the cell index.

        TODO #2874: The name, argument, and first tuple component of this
        and similar methods should be refactored.

        '''
        cell_sym = self._symtab.find_or_create_integer_symbol(
            "cell", tag="cell_loop_idx")
        if var_accesses is not None:
            var_accesses.add_access(Signature(cell_sym.name), AccessType.READ,
                                    self._kern)

        if self._kern.is_coloured():
            colour_sym = self._symtab.find_or_create_integer_symbol(
                "colour", tag="colours_loop_idx")
            if var_accesses is not None:
                var_accesses.add_access(Signature(colour_sym.name),
                                        AccessType.READ, self._kern)

            # pylint: disable-next=import-outside-toplevel
            from psyclone.domain.lfric import LFRicLoop
            loop_type = self._kern.ancestor(LFRicLoop).loop_type

            if loop_type == "cells_in_tile":
                tile_sym = self._symtab.find_or_create_integer_symbol(
                    "tile", tag="tile_loop_idx")
                array_ref = self.get_array_reference(
                    self._kern.tilecolourmap,
                    [Reference(colour_sym), Reference(tile_sym),
                     Reference(cell_sym)],
                    tag="tmap" if self._kern.is_intergrid else None)
                if var_accesses is not None:
                    var_accesses.add_access(Signature(array_ref.name),
                                            AccessType.READ,
                                            self._kern,
                                            ["colour", "tile", "cell"])
            else:
                symbol = self._kern.colourmap
                array_ref = ArrayReference.create(
                        symbol,
                        [Reference(colour_sym), Reference(cell_sym)])
                if var_accesses is not None:
                    var_accesses.add_access(Signature(array_ref.name),
                                            AccessType.READ,
                                            self._kern, ["colour", "cell"])

            return (array_ref.debug_string(), array_ref)

        return (cell_sym.name, Reference(cell_sym))


# ============================================================================
# For automatic documentation creation:
__all__ = ["KernCallArgList"]
