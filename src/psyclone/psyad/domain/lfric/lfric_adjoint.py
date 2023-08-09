# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2023, Science and Technology Facilities Council.
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

from psyclone.domain.lfric import ArgIndexToMetadataIndex, LFRicConstants
from psyclone.domain.lfric.kernel import (
    ScalarArgMetadata, FieldArgMetadata, OperatorArgMetadata,
    ColumnwiseOperatorArgMetadata, FieldVectorArgMetadata)
from psyclone.domain.lfric.transformations import RaisePSyIR2LFRicKernTrans
from psyclone.domain.lfric.utils import (
    find_container, metadata_name_from_module_name)
from psyclone.errors import InternalError
from psyclone.psyad import AdjointVisitor
from psyclone.psyad.domain.common import create_adjoint_name
from psyclone.psyir.nodes import Routine
from psyclone.psyir.symbols import RoutineSymbol
from psyclone.psyir.symbols.symbol import ArgumentInterface, ImportInterface


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

    # Infer the tangent-linear metadata name from the container
    # name. This will be used later to find and modify the metadata.
    tl_container = find_container(tl_psyir)
    tl_metadata_name = metadata_name_from_module_name(tl_container.name)

    # Translate from TL to AD
    logger.debug("Translating from TL to AD.")
    adjoint_visitor = AdjointVisitor(active_variables)
    ad_psyir = adjoint_visitor(tl_psyir)

    # Re-name the Container for the adjoint code if there is a name
    # clash. Use the symbol table for the existing TL code so that we
    # don't accidentally clash with e.g. the name of the kernel
    # routine.
    ad_container = find_container(ad_psyir)
    ad_container.name = ad_container.symbol_table.next_available_name(
        create_adjoint_name(ad_container.name))

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
        kernel_sym = ad_container.symbol_table.lookup(routine.name)
        adj_kernel_name = create_adjoint_name(routine.name)
        # A symbol's name is immutable so create a new RoutineSymbol
        adj_kernel_sym = ad_container.symbol_table.new_symbol(
            adj_kernel_name, symbol_type=RoutineSymbol,
            visibility=kernel_sym.visibility)
        ad_container.symbol_table.remove(kernel_sym)
        routine.name = adj_kernel_sym.name

        logger.debug("AD LFRic kernel will be named '%s'", routine.name)

    # Now we modify the kernel metadata.

    # Infer the adjoint metadata name from the container name.
    ad_metadata_name = metadata_name_from_module_name(ad_container.name)

    # Find the argument symbols. These will be used to determine the
    # metadata index of a particular symbol.
    if not isinstance(ad_container.children[0], Routine):
        raise GenerationError(
            f"A valid LFRic kernel should have a routine (subroutine) as the "
            "first child of it container (module), but found "
            "'{type(ad_container.children[0].__name__}'.")
    arg_symbols = ad_container.children[0].symbol_table.argument_list

    # Raise the PSyIR from generic to LFRic-specific. This gives us
    # access to the metadata.
    raise_trans = RaisePSyIR2LFRicKernTrans()
    raise_trans.apply(
        ad_psyir, options={"metadata_name": tl_metadata_name})

    # Find the kernel metadata. Get ad_container again as it may have
    # become invalid when we raised the PSyIR.
    ad_container = find_container(ad_psyir)
    metadata = ad_container.metadata

    # Change the type and procedure names
    metadata.name = create_adjoint_name(metadata.name)
    if metadata.procedure_name:
        metadata.procedure_name = create_adjoint_name(metadata.procedure_name)
    else:
        # TODO: issue #2236. We are not yet able to need to raise
        # generic PSyIR interface metadata to LFRic-specific interface
        # metadata.
        return ad_psyir

    # Determine the order of the kernel arguments from the
    # metadata. This is needed to find the metadata associated with
    # the specified active variables.
    meta_arg_index_from_arg_index = ArgIndexToMetadataIndex.mapping(metadata)

    # Change the meta_args access metadata
    for var_name in active_variables:
        # Determine whether this active variable is passed by argument.
        found_symbol = None
        for arg_symbol in arg_symbols:
            if arg_symbol.name.lower() == var_name.lower():
                found_symbol = arg_symbol
                break
        else:
            # No, the active variable is not passed by argument.
            continue

        arg_index = arg_symbols.index(found_symbol)
        try:
            meta_arg_index = meta_arg_index_from_arg_index[arg_index]
        except:
            raise GenerationError(
                f"The argument position '{arg_index}' of the active variable "
                f"'{found_symbol.name}' does not match any position as "
                f"specified by the metadata. The expected meta_arg positions "
                f"from argument positions are "
                f"'{meta_arg_index_from_arg_index}'. The most likely reason "
                f"for this is that the argument list does not conform to the "
                f"LFRic rules - perhaps it is a PSyKAl-lite kernel?")
        meta_arg = metadata.meta_args[meta_arg_index]

        # Determine the intent of this variable from its declaration
        # and update the metadata appropriately.
        var_access = found_symbol.interface.access
        const = LFRicConstants()
        access = None
        if var_access == ArgumentInterface.Access.READWRITE:
            if type(meta_arg) == ScalarArgMetadata:
                access = "gh_sum"
            elif type(meta_arg) in [OperatorArgMetadata, ColumnwiseOperatorArgMetadata]:
                access = "gh_readwrite"
            elif type(meta_arg) in [FieldArgMetadata, FieldVectorArgMetadata] and meta_arg.function_space in const.VALID_DISCONTINUOUS_NAMES:
                access = "gh_readwrite"
            elif type(meta_arg) in [FieldArgMetadata, FieldVectorArgMetadata]:
                access = "gh_inc"
            else:
                raise NotImplementedError(f"'{type(meta_arg).__name__}' '{[arg.function_space for arg in [meta_arg] if isinstance(arg, FieldArgMetadata)]}' not implemented")
        if var_access == ArgumentInterface.Access.WRITE:
            access = "gh_write"
        if var_access == ArgumentInterface.Access.READ:
            access = "gh_read"

        # Check that access name exists in symbol table and if not add it.
        kernel = ad_container.children[0]
        symbol_table = kernel.symbol_table
        try:
            argument_mod_symbol = symbol_table.lookup(access)
            # TODO, check it is imported from argument_mod
        except:
            arg_mod_symbol = symbol_table.lookup("argument_mod")
            symbol_table = arg_mod_symbol.find_symbol_table(kernel)
            symbol_table.new_symbol(root_name=access, interface=ImportInterface(arg_mod_symbol))
        meta_arg.access = access

    return ad_psyir
