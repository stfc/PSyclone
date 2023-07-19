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

from psyclone.psyad import AdjointVisitor
from psyclone.psyad.domain.common import create_adjoint_name, find_container
from psyclone.errors import InternalError
from psyclone.psyir.nodes import Routine
from psyclone.psyir.symbols import RoutineSymbol


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

    container = find_container(tl_psyir)
    if not container:
        raise InternalError(
            "An LFRic kernel must be within a Container but the supplied "
            "PSyIR does not contain one.")

    # Infer the tangent-linear metadata name from the container
    # name. This will be used later to find and modify the metadata.
    container_name = container.name.lower()
    if container_name.endswith("_mod"):
        root_name = container_name[:(len(container_name)-len("_mod"))]
    else:
        raise Exception(
            f"LFRic modules should end with \"_mod\", but found "
            f"'{container_name}'")
    tl_metadata_name = root_name + "_type"

    # Translate from TL to AD
    logger.debug("Translating from TL to AD.")
    adjoint_visitor = AdjointVisitor(active_variables)
    ad_psyir = adjoint_visitor(tl_psyir)

    container = find_container(ad_psyir)
    if not container:
        raise InternalError(
            "An LFRic kernel must be within a Container but the supplied "
            "PSyIR does not contain one.")

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

    # Infer the adjoint metadata name from the container name. This will be
    # used later to find and modify the metadata.
    container_name = container.name.lower()
    if container_name.endswith("_mod"):
        root_name = container_name[:(len(container_name)-len("_mod"))]
    else:
        raise Exception(
            f"LFRic modules should end with \"_mod\", but found "
            f"'{container_name}'")
    adj_metadata_name = root_name + "_type"

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
        # issue #xxx need to raise interface metadata
        return ad_psyir

    # 4: Determine the order of the kernel arguments from the
    # metadata. This is needed to find the metadata associated with
    # the specified active variables.
    from psyclone.domain.lfric import ArgIndexToMetadataIndex
    meta_arg_index_from_arg_index = ArgIndexToMetadataIndex.mapping(metadata)

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
        try:
            meta_arg_index = meta_arg_index_from_arg_index[arg_index]
        except:
            raise Exception(f"The argument position '{arg_index}' of the active variable '{found_symbol.name}' does not match any position as specified by the metadata. The expected meta_arg positions from argument positions are '{meta_arg_index_from_arg_index}' ")
        meta_arg = metadata.meta_args[meta_arg_index]
        print(f"{var_name} found at index {arg_index} has meta_arg index {meta_arg_index}.")

        # Determine the intent of this variable from its declaration
        # and update metadata appropriately.
        from psyclone.psyir.symbols.symbol import ArgumentInterface, ImportInterface
        from psyclone.domain.lfric.kernel import ScalarArgMetadata
        var_access = found_symbol.interface.access
        from psyclone.domain.lfric import LFRicConstants
        const = LFRicConstants()
        # TODO different data types (operator?)
        access = None
        if var_access == ArgumentInterface.Access.READWRITE:
            if type(meta_arg) == ScalarArgMetadata:
                access = "gh_sum"
            elif meta_arg.function_space in const.DISCONTINUOUS_FUNCTION_SPACES:
                access = "gh_readwrite"
            else:
                access = "gh_inc"
        if var_access == ArgumentInterface.Access.WRITE:
            access = "gh_write"
        if var_access == ArgumentInterface.Access.READ:
            access = "gh_read"

        # Check that access name exists in symbol table and if not add it.
        # TODO Assumption that the first child of the container is a kernel
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
