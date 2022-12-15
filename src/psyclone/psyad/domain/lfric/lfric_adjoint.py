# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''
Module containing LFRic-specific functionality for the generation of
adjoint code.

'''

import logging

from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel import (
    OperatorArgMetadata, ColumnwiseOperatorArgMetadata, FieldArgMetadata,
    FieldVectorArgMetadata, InterGridArgMetadata, InterGridVectorArgMetadata)
from psyclone.psyad import AdjointVisitor
from psyclone.psyad.domain.common import create_adjoint_name, find_container
from psyclone.errors import InternalError
from psyclone.psyir.nodes import Routine
from psyclone.psyir.symbols import RoutineSymbol


class KernelArgOrder():
    ''' xxx '''

    def __init__(self, metadata, kernel_name=None):
        self._metadata = metadata
        self.arg_index = 0
        self.meta_arg_index_from_actual_index = {}
        self._generate(metadata, kernel_name=kernel_name)

    def cell_position(self):
        ''' xxx '''
        self.arg_index += 1

    def mesh_height(self):
        ''' xxx '''
        self.arg_index += 1

    def mesh_ncell2d_no_halos(self):
        ''' xxx '''
        self.arg_index += 1

    def mesh_ncell2d(self):
        ''' xxx '''
        self.arg_index += 1

    def scalar(self, meta_arg):
        ''' xxx '''
        self.meta_arg_index_from_actual_index[self.arg_index] = \
            self._metadata.meta_args.index(meta_arg)
        self.arg_index += 1

    def field(self, meta_arg):
        ''' xxx '''
        self.meta_arg_index_from_actual_index[self.arg_index] = \
            self._metadata.meta_args.index(meta_arg)
        self.arg_index += 1

    def field_vector(self, meta_arg):
        ''' xxx '''
        self.meta_arg_index_from_actual_index[self.arg_index] = \
            self._metadata.meta_args.index(meta_arg)
        self.arg_index += 1

    def operator(self, meta_arg):
        ''' xxx '''
        self.meta_arg_index_from_actual_index[self.arg_index] = \
            self._metadata.meta_args.index(meta_arg)
        self.arg_index += 1

    def cma_operator(self, meta_arg):
        ''' xxx '''
        self.meta_arg_index_from_actual_index[self.arg_index] = \
            self._metadata.meta_args.index(meta_arg)
        self.arg_index += 1

    def ref_element_properties(self):
        ''' xxx '''
        pass

    def meta_mesh_properties(self):
        ''' xxx '''
        pass

    def fs_common(self, function_space):
        ''' xxx '''
        pass

    def fs_compulsory_field(self, function_space):
        ''' xxx '''
        pass

    def quad_rule(self):
        ''' xxx '''
        pass

    def field_bcs_kernel(self, function_space):
        ''' xxx '''
        pass
    
    def operator_bcs_kernel(function_space_to):
        ''' xxx '''
        pass

    def _generate(self, metadata, kernel_name=None):
        '''Specifies which arguments appear in an argument list and their
        ordering. Calls methods for each type of argument that can be
        specialised by a child class for its particular need.

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
            # Inter-grid kernels require special arguments
            # The cell-map for the current column providing the mapping from
            # the coarse to the fine mesh.
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
            elif type(meta_arg) == ColumnwiseOperatorMetadata:
                self.cma_operator(arg)
            elif type(meta_arg) == ScalarArgMetadata:
                self.scalar(arg)
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
                    self.fs_intergrid(function_space)

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
            if kernel_name and kernel_name.lower() == "enforce_bc_code" and \
                   function_space.lower() == "any_space_1":
                self.field_bcs_kernel(function_space)

        # The operator boundary condition kernel
        # (enforce_operator_bc_kernel) is a special case.
        if kernel_name and kernel_name.lower() == "enforce_operator_bc_code":
            # Sanity checks - this kernel should only have a single
            # readwrite LMA operator as its argument.
            if len(metadata.meta_args) != 1:
                raise GenerationError(
                    f"Kernel {kernel_name} has "
                    f"{len(metadata,meta_args)} arguments when it "
                    f"should only have 1 (an LMA operator)")
            meta_arg = metadata.meta_args[0]
            if meta_arg.form != "gh_operator":
                raise GenerationError(
                    f"Expected an LMA operator from which to look-up boundary "
                    f"dofs but kernel {kernel_name} has argument "
                    f"{meta_arg.form}.")
            if meta_arg.access != "gh_readwrite":
                raise GenerationError(
                    f"Kernel {kernel_name} is recognised as a kernel which"
                    f" applies boundary conditions to an operator. However its"
                    f" operator argument has access {meta_arg.access} rather "
                    f"than gh_readwrite.")
            self.operator_bcs_kernel(meta_arg.function_space_to)

        # Reference-element properties
        if metadata.meta_ref_element:
            self.ref_element_properties()

        # Mesh properties
        if metadata.meta_mesh:
            self.mesh_properties()

        # Quadrature arguments are required if one or more basis or
        # differential basis functions are used by the kernel and a
        # quadrature shape is supplied.
        if metadata.meta_funcs and metadata.shape in \
               VALID_QUADRATURE_SHAPES:
            self.quad_rule()


def generate_lfric_adjoint(tl_psyir, active_variables):
    '''Takes an LFRic tangent-linear kernel represented in language-level PSyIR
    and returns its adjoint represented in language-level PSyIR.

    :param tl_psyir: language-level PSyIR containing the LFRic \
        tangent-linear kernel.
    :type tl_psyir: :py:class:`psyclone.psyir.Node`
    :param list of str active_variables: list of active variable names.

    :returns: language-level PSyIR containing the adjoint of the \
        supplied tangent-linear kernel.
    :rtype: :py:class:`psyclone.psyir.Node`

    :raises InternalError: if the PSyIR does not have a Container.
    :raises InternalError: if the PSyIR does not contain any Routines.

    '''
    logger = logging.getLogger(__name__)

    # Translate from TL to AD
    logger.debug("Translating from TL to AD.")
    adjoint_visitor = AdjointVisitor(active_variables)
    ad_psyir = adjoint_visitor(tl_psyir)

    container = find_container(ad_psyir)
    if not container:
        raise InternalError(
            "An LFRic kernel must be within a Container but the supplied "
            "PSyIR does not contain one.")

    # Infer the tl metadata name from the container name. This will be
    # used later to find and modify the metadata.
    container_name = container.name.lower()
    if container_name.endswith("_mod"):
        root_name = container_name[:(len(container_name)-len("_mod"))]
    else:
        raise Exception(
            f"LFRic modules should end with \"_mod\", but found "
            f"'{container_name}'")
    tl_metadata_name = root_name + "_type"

    # Re-name the Container for the adjoint code. Use the symbol table
    # for the existing TL code so that we don't accidentally clash with
    # e.g. the name of the kernel routine.
    container.name = container.symbol_table.next_available_name(
        create_adjoint_name(container.name))

    routines = ad_psyir.walk(Routine)

    if not routines:
        raise InternalError("The supplied PSyIR does not contain any "
                            "routines.")

    # TODO issue #1800 if there are multiple
    # modules/subroutines/program implementations in the file then
    # raise an exception unless a particular name is specified (by
    # e.g. command line -kernel_name=xyz. The name can be for a routine
    # or an interface.)

    # Until we can query the kernel metadata to see whether it points
    # to a kernel or an interface (issue #1807), we simply assume that we
    # should allow multiple routines as they imply an interface. We
    # further assume that the implementation of the routines in the
    # interface use the same variable names which allows us to
    # continue to use a single command line list of active
    # variables. This is the case for the implementations we care
    # about but in general may not be the case. Issue #1595 should
    # help fix this problem as it would only be arguments that would
    # need to have the same names.

    for routine in routines:
        # We need to re-name the kernel routine.
        kernel_sym = container.symbol_table.lookup(routine.name)
        adj_kernel_name = create_adjoint_name(routine.name)
        # A symbol's name is immutable so create a new RoutineSymbol
        adj_kernel_sym = container.symbol_table.new_symbol(
            adj_kernel_name, symbol_type=RoutineSymbol,
            visibility=kernel_sym.visibility)
        container.symbol_table.remove(kernel_sym)
        routine.name = adj_kernel_sym.name

        logger.debug("AD LFRic kernel will be named '%s'", routine.name)

    # Modify the kernel metadata
    # TODO Assumption that the first child of the container is a kernel
    arg_symbols = container.children[0].symbol_table.argument_list

    # 1: Raise the PSyIR from generic to LFRic-specific
    from psyclone.domain.lfric.transformations import RaisePSyIR2LFRicKernTrans
    raise_trans = RaisePSyIR2LFRicKernTrans()
    raise_trans.apply(
        ad_psyir, options={"metadata_name": tl_metadata_name})
    # 2: Find the metadata
    ad_container = find_container(ad_psyir)
    # TODO Test that ad_container finds a container - really should push this exception into find_container as we always want to test this.
    metadata = ad_container.metadata
    # 3: Change the type and procedure names
    metadata.name = create_adjoint_name(metadata.name)
    if metadata.procedure_name:
        metadata.procedure_name = create_adjoint_name(metadata.procedure_name)
    else:
        assert False # issue #xxx need to raise interface metadata

    # 4: Determine the order of the kernel arguments from the
    # metadata. This is needed to find the metadata associated with
    # the specified active variables.
    arg_order = KernelArgOrder(metadata)

    # 5: Change the meta_args access metadata
    for var_name in active_variables:
        # Determine whether this active variable is passed by argument.
        found_symbol = None
        for arg_symbol in arg_symbols:
            if arg_symbol.name == var_name:
                found_symbol = arg_symbol
                break
        else:
            continue

        arg_index = arg_symbols.index(found_symbol)
        meta_arg_index = arg_order.meta_arg_index_from_actual_index[arg_index]
        meta_arg = metadata.meta_args[meta_arg_index]
        print(f"{var_name} found at index {arg_index} has meta_arg index {meta_arg_index}.")

        # Determine the intent of this variable from its declaration
        # and update metadata appropriately.
        from psyclone.psyir.symbols.symbol import ArgumentInterface
        var_access = found_symbol.interface.access
        from psyclone.domain.lfric import LFRicConstants
        const = LFRicConstants()
        if var_access == ArgumentInterface.Access.READWRITE:
            if meta_arg.function_space in const.DISCONTINUOUS_FUNCTION_SPACES:
                meta_arg.access = "gh_readwrite"
            else:
                meta_arg.access = "gh_inc"
        if var_access == ArgumentInterface.Access.WRITE:
            meta_arg.access = "gh_write"
        if var_access == ArgumentInterface.Access.READ:
            meta_arg.access = "gh_read"

    return ad_psyir
