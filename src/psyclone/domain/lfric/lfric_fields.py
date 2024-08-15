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
This module contains the LFRicFields class which manages the
declarations for field arguments required by an Invoke or
Kernel stub.
'''

# Imports
from collections import OrderedDict

from psyclone import psyGen
from psyclone.domain.lfric import LFRicCollection, LFRicConstants
from psyclone.errors import InternalError
from psyclone.f2pygen import DeclGen, TypeDeclGen
from psyclone.psyir.nodes import Reference
from psyclone.psyir.symbols import (
    ArgumentInterface, DataSymbol, ScalarType, ArrayType, UnresolvedType,
    ImportInterface)


class LFRicFields(LFRicCollection):
    '''
    Manages the declarations for all field arguments required by an Invoke
    or Kernel stub.

    '''
    def _invoke_declarations(self, cursor):
        '''
        Add field-related declarations to the PSy-layer routine.
        Note: PSy layer in LFRic does not modify the field objects. Hence,
        their Fortran intents are always in (the data updated in the kernels
        is only pointed to from the field object and is thus not a part of
        the object).

        :param int cursor: position where to add the next initialisation
            statements.
        :returns: Updated cursor value.
        :rtype: int

        :raises InternalError: for unsupported intrinsic types of field
                               argument data.

        '''
        # Create dict of all field arguments for checks
        const = LFRicConstants()
        fld_args = self._invoke.unique_declarations(
            argument_types=const.VALID_FIELD_NAMES)
        # Filter field arguments by intent and intrinsic type
        real_field_args = self._invoke.unique_declarations(
            argument_types=const.VALID_FIELD_NAMES,
            intrinsic_type=const.MAPPING_DATA_TYPES["gh_real"])
        int_field_args = self._invoke.unique_declarations(
            argument_types=const.VALID_FIELD_NAMES,
            intrinsic_type=const.MAPPING_DATA_TYPES["gh_integer"])

        # Create lists of field names for real- and integer-valued fields
        fld_arg_list = [arg.declaration_name for arg in fld_args]
        real_field_arg_list = [arg.declaration_name for arg in real_field_args]
        int_field_arg_list = [arg.declaration_name for arg in int_field_args]
        # Check for unsupported intrinsic types
        fld_inv = (set(fld_arg_list) -
                   set(real_field_arg_list).union(set(int_field_arg_list)))
        if fld_inv:
            raise InternalError(
                f"Found unsupported intrinsic types for the field arguments "
                f"{list(fld_inv)} to Invoke '{self._invoke.name}'. Supported "
                f"types are {const.VALID_FIELD_INTRINSIC_TYPES}.")

        # Create a field argument map that splits the (real and
        # integer) fields into their different datatypes.
        field_datatype_map = OrderedDict()
        for arg in real_field_args + int_field_args:
            try:
                field_datatype_map[
                    (arg.data_type, arg.module_name)].append(arg)
            except KeyError:
                # This datatype has not been seen before so create a
                # new entry
                field_datatype_map[(arg.data_type, arg.module_name)] = [arg]

        symtab = self._invoke.schedule.symbol_table
        # Add the Invoke subroutine argument declarations for the
        # different fields types. They are declared as intent "in" as
        # they contain a pointer to the data that is modified.
        for fld_type, fld_mod in field_datatype_map:
            args = field_datatype_map[(fld_type, fld_mod)]
            for arg in args:
                arg_symbol = symtab.lookup(arg.name)
                arg_symbol.interface.access = ArgumentInterface.Access.READ

        return cursor

    def _stub_declarations(self, cursor):
        '''
        Add field-related declarations to a Kernel stub.

        :param int cursor: position where to add the next initialisation
            statements.
        :returns: Updated cursor value.
        :rtype: int

        :raises InternalError: for an unsupported data type of field
                               argument data.

        '''
        const = LFRicConstants()

        fld_args = psyGen.args_filter(
            self._kernel.args, arg_types=const.VALID_FIELD_NAMES)
        for fld in fld_args:
            undf_name = fld.function_space.undf_name
            fld_dtype = fld.intrinsic_type
            fld_kind = fld.precision

            # Check for invalid descriptor data type
            fld_ad_dtype = fld.descriptor.data_type
            if fld_ad_dtype not in const.VALID_FIELD_DATA_TYPES:
                raise InternalError(
                    f"Found an unsupported data type '{fld_ad_dtype}' in "
                    f"kernel stub declarations for the field argument "
                    f"'{fld.declaration_name}'. Supported types are "
                    f"{const.VALID_FIELD_DATA_TYPES}.")

            # Create the PSyIR DataType
            kind_sym = self._symbol_table.find_or_create(
                fld_kind, symbol_type=DataSymbol, datatype=UnresolvedType(),
                interface=ImportInterface(
                    self._symbol_table.lookup("constants_mod")))
            if fld.intrinsic_type == "real":
                intr = ScalarType(ScalarType.Intrinsic.REAL, kind_sym)
            elif fld.intrinsic_type == "integer":
                intr = ScalarType(ScalarType.Intrinsic.INTEGER, kind_sym)
            else:
                raise NotImplementedError()
            undf_sym = self._symbol_table.find_or_create(undf_name)
            datatype = ArrayType(intr, [Reference(undf_sym)])

            if fld.intent == "in":
                intent = ArgumentInterface.Access.READ
            elif fld.intent == "inout":
                intent = ArgumentInterface.Access.READWRITE
            else:
                raise NotImplementedError()

            if fld.vector_size > 1:
                for idx in range(1, fld.vector_size+1):
                    text = (fld.name + "_" +
                            fld.function_space.mangled_name +
                            "_v" + str(idx))
                    arg = self._symbol_table.find_or_create(
                        text, symbol_type=DataSymbol, datatype=datatype)
                    arg.interface = ArgumentInterface(intent)
                    self._symbol_table.append_argument(arg)
                    # parent.add(
                    #     DeclGen(parent, datatype=fld_dtype, kind=fld_kind,
                    #             dimension=undf_name,
                    #             intent=fld.intent, entity_decls=[text]))
            else:
                name = fld.name + "_" + fld.function_space.mangled_name
                arg = self._symbol_table.find_or_create(
                    name, symbol_type=DataSymbol, datatype=datatype)
                arg.interface = ArgumentInterface(intent)
                self._symbol_table.append_argument(arg)
                # parent.add(
                #     DeclGen(parent, datatype=fld_dtype, kind=fld_kind,
                #             intent=fld.intent,
                #             dimension=undf_name,
                #             entity_decls=[fld.name + "_" +
                #                           fld.function_space.mangled_name]))
        return cursor


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for (see [https://psyclone-ref.readthedocs.io]).
__all__ = ['LFRicFields']
