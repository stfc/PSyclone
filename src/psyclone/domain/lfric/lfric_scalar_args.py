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

from psyclone.psyir.frontend.fparser2 import INTENT_MAPPING
from psyclone.domain.lfric import LFRicCollection, LFRicConstants, LFRicTypes
from psyclone.errors import GenerationError, InternalError
from psyclone.psyGen import FORTRAN_INTENT_NAMES
from psyclone.psyir.symbols import DataSymbol, ArgumentInterface

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

    def invoke_declarations(self):
        '''
        Create argument lists and declarations for all scalar arguments
        in an Invoke.


        :raises InternalError: for unsupported argument intrinsic types.
        :raises GenerationError: if the same scalar argument has different \
                                 data types in different Kernel calls \
                                 within the same Invoke.

        '''
        super().invoke_declarations()
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
        self._create_declarations()

    def stub_declarations(self):
        '''
        Create and add declarations for all scalar arguments in
        a Kernel stub.

        :raises InternalError: for an unsupported argument data type.

        '''
        super().stub_declarations()
        # Extract all scalar arguments
        for arg in self.kernel_calls[0].arguments.args:
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
        self._create_declarations()

    def _create_declarations(self):
        '''
        Add declarations for the scalar arguments.

        '''
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
                for real_scalars_list in real_scalars_precision_map.values():
                    for arg in real_scalars_list:
                        symbol = self.symtab.find_or_create(
                            arg.declaration_name,
                            symbol_type=DataSymbol,
                            datatype=LFRicTypes("LFRicRealScalarDataType")())
                        symbol.interface = ArgumentInterface(
                                            INTENT_MAPPING[intent])
                        self.symtab.append_argument(symbol)

        # Integer scalar arguments
        for intent in FORTRAN_INTENT_NAMES:
            if self._integer_scalars[intent]:
                for arg in self._integer_scalars[intent]:
                    symbol = self.symtab.find_or_create(
                        arg.declaration_name,
                        symbol_type=DataSymbol,
                        datatype=LFRicTypes("LFRicIntegerScalarDataType")())
                    symbol.interface = ArgumentInterface(
                                        INTENT_MAPPING[intent])
                    self.symtab.append_argument(symbol)

        # Logical scalar arguments
        for intent in FORTRAN_INTENT_NAMES:
            if self._logical_scalars[intent]:
                for arg in self._logical_scalars[intent]:
                    symbol = self.symtab.find_or_create(
                        arg.declaration_name,
                        symbol_type=DataSymbol,
                        datatype=LFRicTypes("LFRicLogicalScalarDataType")())
                    symbol.interface = ArgumentInterface(
                                        INTENT_MAPPING[intent])
                    self.symtab.append_argument(symbol)


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for.
__all__ = ['LFRicScalarArgs']
