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
# Author A. Pirrie, Met Office

'''
This module contains the LFRicScalarArrayArgs class which handles the
declarations of ScalarArray arguments to the kernel found in either
an Invoke or a Kernel stub.
'''

# Imports
from collections import Counter

from psyclone.psyir.frontend.fparser2 import INTENT_MAPPING
from psyclone.domain.lfric import LFRicCollection, LFRicConstants, LFRicTypes
from psyclone.errors import GenerationError, InternalError
from psyclone.psyGen import FORTRAN_INTENT_NAMES
from psyclone.psyir.nodes import Literal, ArrayReference
from psyclone.psyir.symbols import (DataSymbol, ArrayType,
                                    INTEGER_TYPE, ArgumentInterface)

# pylint: disable=too-many-lines
# pylint: disable=too-many-locals
# pylint: disable=too-many-branches


class LFRicScalarArrayArgs(LFRicCollection):
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
        self._scalar_array_args = {}
        self._real_scalar_arrays = {}
        self._integer_scalar_arrays = {}
        self._logical_scalar_arrays = {}
        for intent in FORTRAN_INTENT_NAMES:
            self._scalar_array_args[intent] = []
            self._real_scalar_arrays[intent] = []
            self._integer_scalar_arrays[intent] = []
            self._logical_scalar_arrays[intent] = []

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
        self._scalar_array_args = self._invoke.unique_declns_by_intent(
            const.VALID_ARRAY_NAMES)
        # Filter scalar arguments by intent and intrinsic type
        self._real_scalar_arrays = self._invoke.unique_declns_by_intent(
            const.VALID_ARRAY_NAMES,
            intrinsic_type=const.MAPPING_DATA_TYPES["gh_real"])
        self._integer_scalar_arrays = self._invoke.unique_declns_by_intent(
            const.VALID_ARRAY_NAMES,
            intrinsic_type=const.MAPPING_DATA_TYPES["gh_integer"])
        self._logical_scalar_arrays = self._invoke.unique_declns_by_intent(
            const.VALID_ARRAY_NAMES,
            intrinsic_type=const.MAPPING_DATA_TYPES["gh_logical"])

        for intent in FORTRAN_INTENT_NAMES:
            scal = [arg.declaration_name
                    for arg in self._scalar_array_args[intent]]
            rscalarr = [arg.declaration_name for
                        arg in self._real_scalar_arrays[intent]]
            iscalarr = [arg.declaration_name for
                        arg in self._integer_scalar_arrays[intent]]
            lscalarr = [arg.declaration_name for
                        arg in self._logical_scalar_arrays[intent]]
            print(scal)
            # Add "real", "integer" and "logical" ScalarArray lists for checks
            decl_scal = rscalarr + iscalarr + lscalarr
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
        print(self.kernel_calls[0].arguments.args)
        for arg in self.kernel_calls[0].arguments.args:
            if arg.is_scalar_array:
                self._scalar_array_args[arg.intent].append(arg)

        const = LFRicConstants()
        # Filter scalar arguments by intent and data type
        for intent in FORTRAN_INTENT_NAMES:
            for arg in self._scalar_array_args[intent]:
                print(arg)
                # Distinguish whether they are ScalarArrays
                if arg.descriptor.data_type == "gh_real":
                    self._real_scalar_arrays[intent].append(arg)
                elif arg.descriptor.data_type == "gh_integer":
                    self._integer_scalar_arrays[intent].append(arg)
                elif arg.descriptor.data_type == "gh_logical":
                    self._logical_scalar_arrays[intent].append(arg)
                else:
                    raise InternalError(
                        f"Found an unsupported data type "
                        f"'{arg.descriptor.data_type}' for the "
                        f"ScalarArray argument '{arg.declaration_name}'"
                        f". Supported types are "
                        f"{const.VALID_SCALAR_DATA_TYPES}.")
            print(self._real_scalar_arrays[intent])
            print(self._integer_scalar_arrays[intent])
            print(self._logical_scalar_arrays[intent])

        print(self._scalar_array_args)

        # Create declarations
        self._create_declarations()

    def _create_declarations(self):
        '''
        Add declarations for the scalar arguments.

        '''
        # print("symtab - ")
        # print(self.symtab)
        # print("symtab.tags_dict - ")
        # print(self.symtab.tags_dict)
        # Real ScalarArray arguments

        # It seems that the symbols are not being added in the stub
        # declaration phase in the same way as they are in the invoke
        # declaration. LFRicCollections seems to initialise differently
        # for these two - could be to do with it.

        for intent in FORTRAN_INTENT_NAMES:
            if self._real_scalar_arrays[intent]:
                for arg in self._real_scalar_arrays[intent]:
                    print(self.symtab)
                    print(self.symtab.tags_dict)
                    if arg._array_ndims >= 1:
                        # Create the dimensions array symbol
                        dims_array_symbol = self.symtab.find_or_create(
                            "dims_" + arg.name,
                            symbol_type=DataSymbol,
                            datatype=ArrayType(
                                LFRicTypes("LFRicIntegerScalarDataType")(),
                                [arg._array_ndims]))
                        dims_array_symbol.interface = ArgumentInterface(
                                            INTENT_MAPPING[intent])
                        self.symtab.append_argument(dims_array_symbol)
                        print(dims_array_symbol)
                        # Create list of dims_array references
                        sym_list = [ArrayReference.create(
                            dims_array_symbol,
                            [Literal(str(idx), INTEGER_TYPE)])
                                for idx in range(1, arg._array_ndims + 1)]
                        print(sym_list)
                        # Find ScalarArray tag and convert it to an ArrayType
                        if not self._kernel:
                            # For code generation
                            array_symbol = self.symtab.lookup_with_tag(
                                "AlgArgs_" + arg.name)
                            array_symbol.datatype = ArrayType(
                                LFRicTypes("LFRicRealScalarDataType")(),
                                sym_list)
                        else:
                            # For stub generation
                            array_symbol = self.symtab.find_or_create(
                            arg.name,
                            symbol_type=DataSymbol,
                            datatype=ArrayType(
                                LFRicTypes("LFRicRealScalarDataType")(),
                                [arg._array_ndims]))
                        array_symbol.interface = ArgumentInterface(
                                            INTENT_MAPPING[intent])
                        self.symtab.append_argument(array_symbol)
                        print(array_symbol)

        # Integer ScalarArray arguments
        for intent in FORTRAN_INTENT_NAMES:
            if self._integer_scalar_arrays[intent]:
                for arg in self._integer_scalar_arrays[intent]:
                    print(self.symtab)
                    if arg._array_ndims >= 1:
                        # Create the dimensions array symbol
                        dims_array_symbol = self.symtab.find_or_create(
                            "dims_" + arg.name,
                            symbol_type=DataSymbol,
                            datatype=ArrayType(
                                LFRicTypes("LFRicIntegerScalarDataType")(),
                                [arg._array_ndims]))
                        dims_array_symbol.interface = ArgumentInterface(
                                            INTENT_MAPPING[intent])
                        self.symtab.append_argument(dims_array_symbol)
                        print(dims_array_symbol)
                        # Create list of dims_array references
                        sym_list = [ArrayReference.create(
                            dims_array_symbol,
                            [Literal(str(idx), INTEGER_TYPE)])
                                for idx in range(1, arg._array_ndims + 1)]
                        print(sym_list)
                        # Find ScalarArray tag and convert it to an ArrayType
                        if not self._kernel:
                            # For code generation
                            array_symbol = self.symtab.lookup_with_tag(
                                "AlgArgs_" + arg.name)
                            array_symbol.datatype = ArrayType(
                                LFRicTypes("LFRicIntegerScalarDataType")(),
                                sym_list)
                        else:
                            # For stub generation
                            array_symbol = self.symtab.find_or_create(
                            arg.name,
                            symbol_type=DataSymbol,
                            datatype=ArrayType(
                                LFRicTypes("LFRicIntegerScalarDataType")(),
                                [arg._array_ndims]))
                        array_symbol.interface = ArgumentInterface(
                                            INTENT_MAPPING[intent])
                        self.symtab.append_argument(array_symbol)
                        print(array_symbol)

        # Logical ScalarArray arguments
        for intent in FORTRAN_INTENT_NAMES:
            if self._logical_scalar_arrays[intent]:
                for arg in self._logical_scalar_arrays[intent]:
                    print(self.symtab)
                    if arg._array_ndims >= 1:
                        # Create the dimensions array symbol
                        dims_array_symbol = self.symtab.find_or_create(
                            "dims_" + arg.name,
                            symbol_type=DataSymbol,
                            datatype=ArrayType(
                                LFRicTypes("LFRicIntegerScalarDataType")(),
                                [arg._array_ndims]))
                        dims_array_symbol.interface = ArgumentInterface(
                                            INTENT_MAPPING[intent])
                        self.symtab.append_argument(dims_array_symbol)

                        # Create list of dims_array references
                        sym_list = [ArrayReference.create(
                            dims_array_symbol,
                            [Literal(str(idx), INTEGER_TYPE)])
                                for idx in range(1, arg._array_ndims + 1)]

                        # Find ScalarArray tag and convert it to an ArrayType
                        if not self._kernel:
                            # For code generation
                            array_symbol = self.symtab.lookup_with_tag(
                                "AlgArgs_" + arg.name)
                            array_symbol.datatype = ArrayType(
                                LFRicTypes("LFRicLogicalScalarDataType")(),
                                sym_list)
                        else:
                            # For stub generation
                            array_symbol = self.symtab.find_or_create(
                            arg.name,
                            symbol_type=DataSymbol,
                            datatype=ArrayType(
                                LFRicTypes("LFRicLogicalScalarDataType")(),
                                [arg._array_ndims]))
                        array_symbol.interface = ArgumentInterface(
                                            INTENT_MAPPING[intent])
                        self.symtab.append_argument(array_symbol)
                        print(array_symbol)


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for.
__all__ = ['LFRicScalarArrayArgs']
