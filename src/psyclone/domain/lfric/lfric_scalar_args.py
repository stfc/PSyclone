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
# Modified I. Kavcic, A. Coughtrie, L. Turner and O. Brunt, Met Office
# Modified J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk and N. Nobre, STFC Daresbury Lab

'''
This module contains the LFRicScalarArgs class which handles the
declarations of scalar arguments to the kernel found in either
an Invoke or a Kernel stub.
'''

# Imports
from collections import OrderedDict, Counter

from psyclone.domain.lfric import LFRicCollection, LFRicConstants
from psyclone.errors import GenerationError, InternalError
from psyclone.f2pygen import DeclGen
from psyclone.psyGen import FORTRAN_INTENT_NAMES

# pylint: disable=too-many-lines
# pylint: disable=too-many-locals
# pylint: disable=too-many-branches


class LFRicScalarArgs(LFRicCollection):
    '''
    Handles the declarations of scalar kernel arguments appearing in either
    an Invoke or a Kernel stub.

    :param node: the Invoke or Kernel stub for which to manage the scalar \
                 arguments.
    :type node: :py:class:`psyclone.domain.lfric.LFRicKern` or \
                :py:class:`psyclone.domain.lfric.LFRicInvoke`

    '''
    def __init__(self, node):
        super().__init__(node)

        # Initialise dictionaries of 'real', 'integer' and 'logical'
        # scalar arguments by data type and intent
        self._scalar_args = {}
        self._real_scalars = {}
        self._integer_scalars = {}
        self._logical_scalars = {}
        for intent in FORTRAN_INTENT_NAMES:
            self._scalar_args[intent] = []
            self._real_scalars[intent] = []
            self._integer_scalars[intent] = []
            self._logical_scalars[intent] = []

    def _invoke_declarations(self, parent):
        '''
        Create argument lists and declarations for all scalar arguments
        in an Invoke.

        :param parent: the f2pygen node representing the PSy-layer routine \
                       to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises InternalError: for unsupported argument intrinsic types.
        :raises GenerationError: if the same scalar argument has different \
                                 data types in different Kernel calls \
                                 within the same Invoke.

        '''
        # Create dictionary of all scalar arguments for checks
        const = LFRicConstants()
        self._scalar_args = self._invoke.unique_declns_by_intent(
            const.VALID_SCALAR_NAMES)
        # Filter scalar arguments by intent and intrinsic type
        self._real_scalars = self._invoke.unique_declns_by_intent(
            const.VALID_SCALAR_NAMES,
            intrinsic_type=const.MAPPING_DATA_TYPES["gh_real"])
        self._integer_scalars = self._invoke.unique_declns_by_intent(
            const.VALID_SCALAR_NAMES,
            intrinsic_type=const.MAPPING_DATA_TYPES["gh_integer"])
        self._logical_scalars = self._invoke.unique_declns_by_intent(
            const.VALID_SCALAR_NAMES,
            intrinsic_type=const.MAPPING_DATA_TYPES["gh_logical"])

        for intent in FORTRAN_INTENT_NAMES:
            scal = [arg.declaration_name for arg in self._scalar_args[intent]]
            rscal = [arg.declaration_name for
                     arg in self._real_scalars[intent]]
            iscal = [arg.declaration_name for
                     arg in self._integer_scalars[intent]]
            lscal = [arg.declaration_name for
                     arg in self._logical_scalars[intent]]
            # Add "real", "integer" and "logical" scalar lists for checks
            decl_scal = rscal + iscal + lscal
            # Check for unsupported intrinsic types
            scal_inv = sorted(set(scal) - set(decl_scal))
            if scal_inv:
                raise InternalError(
                    f"Found unsupported intrinsic types for the scalar "
                    f"arguments {scal_inv} to Invoke '{self._invoke.name}'. "
                    f"Supported types are {const.VALID_INTRINSIC_TYPES}.")
            # Check that the same scalar name is not found in either of
            # 'real', 'integer' or 'logical' scalar lists (for instance if
            # passed to one kernel as a 'real' and to another kernel as an
            # 'integer' scalar)
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
        Create and add declarations for all scalar arguments in
        a Kernel stub.

        :param parent: node in the f2pygen AST representing the Kernel stub \
                       to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises InternalError: for an unsupported argument data type.

        '''
        # Extract all scalar arguments
        for arg in self._calls[0].arguments.args:
            if arg.is_scalar:
                self._scalar_args[arg.intent].append(arg)

        const = LFRicConstants()
        # Filter scalar arguments by intent and data type
        for intent in FORTRAN_INTENT_NAMES:
            for arg in self._scalar_args[intent]:
                if arg.descriptor.data_type == "gh_real":
                    self._real_scalars[intent].append(arg)
                elif arg.descriptor.data_type == "gh_integer":
                    self._integer_scalars[intent].append(arg)
                elif arg.descriptor.data_type == "gh_logical":
                    self._logical_scalars[intent].append(arg)
                else:
                    raise InternalError(
                        f"Found an unsupported data type "
                        f"'{arg.descriptor.data_type}' for the scalar "
                        f"argument '{arg.declaration_name}'. Supported types "
                        f"are {const.VALID_SCALAR_DATA_TYPES}.")

        # Create declarations
        self._create_declarations(parent)

    def _create_declarations(self, parent):
        '''Add declarations for the scalar arguments.

        :param parent: the f2pygen node in which to insert declarations \
                       (Invoke or Kernel).
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises InternalError: if neither self._invoke nor \
            self._kernel are set.

        '''
        const = LFRicConstants()
        const_mod = const.UTILITIES_MOD_MAP["constants"]["module"]
        const_mod_uses = None
        if self._invoke:
            const_mod_uses = self._invoke.invokes.psy.infrastructure_modules[
                const_mod]
        # Real scalar arguments
        for intent in FORTRAN_INTENT_NAMES:
            if self._real_scalars[intent]:
                # Filter scalars based on precision
                real_scalars_precision_map = OrderedDict()
                for real_scalar in self._real_scalars[intent]:
                    try:
                        real_scalars_precision_map[
                            real_scalar.precision].append(real_scalar)
                    except KeyError:
                        # This precision has not been seen before so
                        # create a new entry
                        real_scalars_precision_map[
                            real_scalar.precision] = [real_scalar]
                # Declare scalars
                for real_scalar_kind, real_scalars_list in \
                        real_scalars_precision_map.items():
                    real_scalar_type = real_scalars_list[0].intrinsic_type
                    real_scalar_names = [arg.declaration_name for arg
                                         in real_scalars_list]
                    parent.add(
                        DeclGen(parent, datatype=real_scalar_type,
                                kind=real_scalar_kind,
                                entity_decls=real_scalar_names,
                                intent=intent))
                    if self._invoke:
                        const_mod_uses.add(real_scalar_kind)
                    elif self._kernel:
                        self._kernel.argument_kinds.add(real_scalar_kind)
                    else:
                        raise InternalError(
                            "Expected the declaration of real scalar kernel "
                            "arguments to be for either an invoke or a "
                            "kernel stub, but it is neither.")

        # Integer scalar arguments
        for intent in FORTRAN_INTENT_NAMES:
            if self._integer_scalars[intent]:
                dtype = self._integer_scalars[intent][0].intrinsic_type
                dkind = self._integer_scalars[intent][0].precision
                integer_scalar_names = [arg.declaration_name for arg
                                        in self._integer_scalars[intent]]
                parent.add(
                    DeclGen(parent, datatype=dtype, kind=dkind,
                            entity_decls=integer_scalar_names,
                            intent=intent))
                if self._invoke:
                    const_mod_uses.add(dkind)
                elif self._kernel:
                    self._kernel.argument_kinds.add(dkind)
                else:
                    raise InternalError(
                        "Expected the declaration of integer scalar kernel "
                        "arguments to be for either an invoke or a "
                        "kernel stub, but it is neither.")

        # Logical scalar arguments
        for intent in FORTRAN_INTENT_NAMES:
            if self._logical_scalars[intent]:
                dtype = self._logical_scalars[intent][0].intrinsic_type
                dkind = self._logical_scalars[intent][0].precision
                logical_scalar_names = [arg.declaration_name for arg
                                        in self._logical_scalars[intent]]
                parent.add(
                    DeclGen(parent, datatype=dtype, kind=dkind,
                            entity_decls=logical_scalar_names,
                            intent=intent))
                if self._invoke:
                    const_mod_uses.add(dkind)
                elif self._kernel:
                    self._kernel.argument_kinds.add(dkind)
                else:
                    raise InternalError(
                        "Expected the declaration of logical scalar kernel "
                        "arguments to be for either an invoke or a "
                        "kernel stub, but it is neither.")


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for. (See https://psyclone-ref.readthedocs.io)
__all__ = ['LFRicScalarArgs']
