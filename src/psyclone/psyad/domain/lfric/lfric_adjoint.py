# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
from psyclone.errors import InternalError, GenerationError
from psyclone.psyad import AdjointVisitor
from psyclone.psyad.domain.common import create_adjoint_name
from psyclone.psyir.nodes import Routine
from psyclone.psyir.symbols import RoutineSymbol, ContainerSymbol
from psyclone.psyir.symbols.symbol import ArgumentInterface, ImportInterface


# pylint: disable=too-many-locals
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

    # Find the argument symbols. These will be used to determine the
    # metadata index of a particular symbol.
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
        # Issue #2236. We are not yet able to to raise multi-precision
        # metadata to LFRic-specific metadata, so return without
        # making any further modifications.
        return ad_psyir

    # Change the meta_args access metadata
    for var_name in active_variables:
        access = _update_access_metadata(var_name, arg_symbols, metadata)
        if access:
            # Add in any new access symbols.
            _check_or_add_access_symbol(ad_container, access)

    return ad_psyir


# pylint: disable=too-many-branches
def _update_access_metadata(var_name, arg_symbols, metadata):
    '''If the access metadata for the variable var_name is incorrect then
    update it.

    :param str var_name: the name of the active variable whose
        metadata we are going to update if required.
    :param arg_symbols: a list containing all of the argument symbols
        that are passed into the associated kernel.
    :type arg_symbols: List[:py:class:`psyclone.psyir.symbols.DataSymbol`]
    :param metadata: the LFRic metadata.
    :type metadata:
        :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

    :returns: the access metadata or None if the active variable is
        not passed by argument.
    :rtype: Optional[str]

    :raises GenerationError: if the argument position does not match
        the metadata.
    :raises InternalError: if an unexpected metadata type is found.
    :raises InternalError: if an unexpected access type is found.

    '''
    # Determine the order of the kernel arguments from the
    # metadata. This is needed to find the metadata associated with
    # the specified active variable.
    meta_arg_index_from_arg_index = ArgIndexToMetadataIndex.mapping(metadata)

    # Determine whether this active variable is passed by argument.
    found_symbol = None
    for arg_symbol in arg_symbols:
        if arg_symbol.name.lower() == var_name.lower():
            found_symbol = arg_symbol
            break
    else:
        # No, the active variable is not passed by argument.
        return None

    arg_index = arg_symbols.index(found_symbol)
    try:
        meta_arg_index = meta_arg_index_from_arg_index[arg_index]
    except KeyError as exc:
        raise GenerationError(
            f"The position in the kernel subroutine argument list "
            f"'{arg_index}' of the active variable '{found_symbol.name}' "
            f"does not match any of the positions expected by the kernel "
            f"argument ('meta_arg') metadata descriptions. The expected "
            f"mapping of kernel subroutine argument positions to kernel "
            f"meta_arg positions is '{meta_arg_index_from_arg_index}'. "
            f"The most likely reason for this is that the kernel subroutine "
            f"argument list does not conform to the LFRic rules - perhaps "
            f"it is a PSyKAl-lite kernel?") from exc
    meta_arg = metadata.meta_args[meta_arg_index]

    # Determine the intent of this variable from its declaration
    # and update the metadata appropriately.
    var_access = found_symbol.interface.access
    const = LFRicConstants()
    access = None
    if var_access == ArgumentInterface.Access.READWRITE:
        # pylint: disable=unidiomatic-typecheck
        if type(meta_arg) is ScalarArgMetadata:
            # TODO #2333 - in LFRic only Builtin kernels are currently allowed
            # to write to a scalar argument (since this implies a reduction).
            # We therefore need to flag this case.
            access = "gh_sum"
        elif type(meta_arg) in [
                OperatorArgMetadata, ColumnwiseOperatorArgMetadata]:
            access = "gh_readwrite"
        elif type(meta_arg) in [
                FieldArgMetadata, FieldVectorArgMetadata] and (
                    meta_arg.function_space in
                    const.VALID_DISCONTINUOUS_NAMES):
            access = "gh_readwrite"
        elif type(meta_arg) in [FieldArgMetadata, FieldVectorArgMetadata]:
            access = "gh_inc"
        else:
            raise InternalError(
                f"Found unexpected meta arg class "
                f"'{type(meta_arg).__name__}'.")
    elif var_access == ArgumentInterface.Access.WRITE:
        access = "gh_write"
    elif var_access == ArgumentInterface.Access.READ:
        access = "gh_read"
    else:
        raise InternalError(
            f"Found unexpected access '{var_access}' for "
            f"'{found_symbol.name}'.")
    meta_arg.access = access
    return access


def _check_or_add_access_symbol(container, access):
    '''Check whether the LFRic access metadata name (e.g. 'gh_write')
    provided in argument 'access' is already declared in the symbol
    table and if not add it.

    :param container: the adjoint PSyIR.
    :type container: :py:class:`psyclone.psyir.nodes.Container`
    :param str access: the access metadata.

    '''
    kernel = container.children[0]
    symbol_table = kernel.symbol_table
    try:
        argument_mod_symbol = symbol_table.lookup(access)
        if not isinstance(argument_mod_symbol.interface, ImportInterface):
            raise GenerationError(
                f"The existing symbol '{access}' is not imported from a use "
                "statement.")
        if not (argument_mod_symbol.interface.container_symbol.name.lower()
                == "argument_mod"):
            raise GenerationError(
                f"The existing symbol '{access}' is imported from "
                f"'{argument_mod_symbol.interface.container_symbol.name}' but "
                f"should be imported from 'argument_mod'.")
    except KeyError:
        arg_mod_symbol = symbol_table.find_or_create(
            "argument_mod", symbol_type=ContainerSymbol)
        symbol_table = arg_mod_symbol.find_symbol_table(kernel)
        symbol_table.new_symbol(
            root_name=access, interface=ImportInterface(arg_mod_symbol))
