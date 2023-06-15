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
# Author L. Turner, Met Office

''' This module handles the declarations of array kernel arguments appearing in either
    an Invoke or a Kernel stub.'''

# Imports
from collections import OrderedDict, Counter

from psyclone.domain.lfric import LFRicCollection, LFRicConstants
from psyclone.errors import GenerationError, InternalError
from psyclone.f2pygen import DeclGen
from psyclone.psyGen import FORTRAN_INTENT_NAMES


class LFRicArrayArgs(LFRicCollection):
    '''
    Handles the declarations of array kernel arguments appearing in either
    an Invoke or a Kernel stub.

    :param node: the Invoke or Kernel stub for which to manage the array \
                 arguments.
    :type node: :py:class:`psyclone.dynamo0p3.DynKern` or \
                :py:class:`psyclone.dynamo0p3.LFRicInvoke`

    '''
    def __init__(self, node):
        super(LFRicArrayArgs, self).__init__(node)

        # Initialise dictionaries of 'real', 'integer' and 'logical'
        # array arguments by data type and intent
        self._array_args = {}
        self._real_arrays = {}
        self._integer_arrays = {}
        self._logical_arrays = {}
        for intent in FORTRAN_INTENT_NAMES:
            self._array_args[intent] = []
            self._real_arrays[intent] = []
            self._integer_arrays[intent] = []
            self._logical_arrays[intent] = []

    def _invoke_declarations(self, parent):
        '''
        Create argument lists and declarations for all array arguments
        in an Invoke.

        :param parent: the f2pygen node representing the PSy-layer routine \
                       to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises InternalError: for unsupported argument intrinsic types.
        :raises GenerationError: if the same array argument has different \
                                 data types in different Kernel calls \
                                 within the same Invoke.

        '''
        # Create dictionary of all array arguments for checks
        const = LFRicConstants()
        self._array_args = self._invoke.unique_declns_by_intent(
            const.VALID_ARRAY_NAMES)
        # Filter array arguments by intent and intrinsic type
        self._real_arrays = self._invoke.unique_declns_by_intent(
            const.VALID_ARRAY_NAMES,
            intrinsic_type=const.MAPPING_DATA_TYPES["gh_real"])
        self._integer_arrays = self._invoke.unique_declns_by_intent(
            const.VALID_ARRAY_NAMES,
            intrinsic_type=const.MAPPING_DATA_TYPES["gh_integer"])
        self._logical_arrays = self._invoke.unique_declns_by_intent(
            const.VALID_ARRAY_NAMES,
            intrinsic_type=const.MAPPING_DATA_TYPES["gh_logical"])

        for intent in FORTRAN_INTENT_NAMES:
            scal = [arg.declaration_name for arg in self._array_args[intent]]
            rscal = [arg.declaration_name for
                     arg in self._real_arrays[intent]]
            iscal = [arg.declaration_name for
                     arg in self._integer_arrays[intent]]
            lscal = [arg.declaration_name for
                     arg in self._logical_arrays[intent]]
            # Add "real", "integer" and "logical" array lists for checks
            decl_scal = rscal + iscal + lscal
            # Check for unsupported intrinsic types
            scal_inv = sorted(set(scal) - set(decl_scal))
            if scal_inv:
                raise InternalError(
                    f"Found unsupported intrinsic types for the array "
                    f"arguments {scal_inv} to Invoke '{self._invoke.name}'. "
                    f"Supported types are {const.VALID_INTRINSIC_TYPES}.")
            # Check that the same array name is not found in either of
            # 'real', 'integer' or 'logical' array lists (for instance if
            # passed to one kernel as a 'real' and to another kernel as an
            # 'integer' array)
            scal_multi_type = [item for item, count in
                               Counter(decl_scal).items() if count > 1]
            if scal_multi_type:
                raise GenerationError(
                    f"Scalar argument(s) {scal_multi_type} in Invoke "
                    f"'{self._invoke.name}' have different metadata for data "
                    f"type ({list(const.MAPPING_DATA_TYPES.keys())}) in "
                    f"different kernels. This is invalid.")

        # Create declarations
        self._create_declarations(parent)

    def _stub_declarations(self, parent):
        '''
        Create and add declarations for all array arguments in
        a Kernel stub.

        :param parent: node in the f2pygen AST representing the Kernel stub \
                       to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises InternalError: for an unsupported argument data type.

        '''
        # Extract all array arguments
        for arg in self._calls[0].arguments.args:
            if arg.is_array:
                self._array_args[arg.intent].append(arg)

        const = LFRicConstants()
        # Filter array arguments by intent and data type
        for intent in FORTRAN_INTENT_NAMES:
            for arg in self._array_args[intent]:
                if arg.descriptor.data_type == "gh_real":
                    self._real_arrays[intent].append(arg)
                elif arg.descriptor.data_type == "gh_integer":
                    self._integer_arrays[intent].append(arg)
                elif arg.descriptor.data_type == "gh_logical":
                    self._logical_arrays[intent].append(arg)
                else:
                    raise InternalError(
                        f"Found an unsupported data type "
                        f"'{arg.descriptor.data_type}' for the array "
                        f"argument '{arg.declaration_name}'. Supported types "
                        f"are {const.VALID_ARRAY_DATA_TYPES}.")

        # Create declarations
        self._create_declarations(parent)

    def _create_declarations(self, parent):
        '''Add declarations for the array arguments.

        :param parent: the f2pygen node in which to insert declarations \
                       (Invoke or Kernel).
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises InternalError: if neither self._invoke nor \
            self._kernel are set.

        '''
        const = LFRicConstants()
        const_mod = const.UTILITIES_MOD_MAP["constants"]["module"]
        const_mod_list = None
        if self._invoke:
            const_mod_list = self._invoke.invokes.psy. \
                infrastructure_modules[const_mod]
        # Real array arguments
        for intent in FORTRAN_INTENT_NAMES:
            if self._real_arrays[intent]:
                # Filter arrays based on precision
                real_arrays_precision_map = OrderedDict()
                for real_array in self._real_arrays[intent]:
                    try:
                        real_arrays_precision_map[
                            real_array.precision].append(real_array)
                    except KeyError:
                        # This precision has not been seen before so
                        # create a new entry
                        real_arrays_precision_map[
                            real_array.precision] = [real_array]
                # Declare arrays
                for real_array_kind, real_arrays_list in \
                        real_arrays_precision_map.items():
                    real_array_type = real_arrays_list[0].intrinsic_type
                    real_array_names = [arg.declaration_name for arg
                                        in real_arrays_list]
                    parent.add(
                        DeclGen(parent, datatype=real_array_type,
                                kind=real_array_kind,
                                entity_decls=real_array_names,
                                intent=intent))
                    if self._invoke:
                        if real_array_kind not in const_mod_list:
                            const_mod_list.append(real_array_kind)
                    elif self._kernel:
                        self._kernel.argument_kinds.add(real_array_kind)
                    else:
                        raise InternalError(
                            "Expected the declaration of real array kernel "
                            "arguments to be for either an invoke or a "
                            "kernel stub, but it is neither.")

        # Integer array arguments
        for intent in FORTRAN_INTENT_NAMES:
            if self._integer_arrays[intent]:
                dtype = self._integer_arrays[intent][0].intrinsic_type
                dkind = self._integer_arrays[intent][0].precision
                integer_array_names = [arg.declaration_name for arg
                                       in self._integer_arrays[intent]]
                parent.add(
                    DeclGen(parent, datatype=dtype, kind=dkind,
                            entity_decls=integer_array_names,
                            intent=intent))
                if self._invoke:
                    if dkind not in const_mod_list:
                        const_mod_list.append(dkind)
                elif self._kernel:
                    self._kernel.argument_kinds.add(dkind)
                else:
                    raise InternalError(
                        "Expected the declaration of integer array kernel "
                        "arguments to be for either an invoke or a "
                        "kernel stub, but it is neither.")

        # Logical array arguments
        for intent in FORTRAN_INTENT_NAMES:
            if self._logical_arrays[intent]:
                dtype = self._logical_arrays[intent][0].intrinsic_type
                dkind = self._logical_arrays[intent][0].precision
                logical_array_names = [arg.declaration_name for arg
                                       in self._logical_arrays[intent]]
                parent.add(
                    DeclGen(parent, datatype=dtype, kind=dkind,
                            entity_decls=logical_array_names,
                            intent=intent))
                if self._invoke:
                    if dkind not in const_mod_list:
                        const_mod_list.append(dkind)
                elif self._kernel:
                    self._kernel.argument_kinds.add(dkind)
                else:
                    raise InternalError(
                        "Expected the declaration of logical array kernel "
                        "arguments to be for either an invoke or a "
                        "kernel stub, but it is neither.")


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for. (See https://psyclone-ref.readthedocs.io)
__all__ = ['LFRicArrayArgs']
