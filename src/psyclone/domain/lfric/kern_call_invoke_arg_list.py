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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, A. Coughtrie and L. Turner, Met Office
# Modified J. Henrichs, Bureau of Meteorology

'''
This module implements a class that captures all the arguments
of a Kernel as required by an `invoke` of that kernel.
'''

from psyclone.domain.lfric import ArgOrdering, LFRicConstants
# Avoid circular dependency:
from psyclone.domain.lfric.lfric_types import LFRicTypes
from psyclone.psyir.symbols import (
    ArrayType, DataSymbol, DataTypeSymbol, UnresolvedType, SymbolTable,
    ContainerSymbol, ImportInterface)


class KernCallInvokeArgList(ArgOrdering):
    '''Determines the arguments that must be provided to an `invoke` of a
    kernel, according to that kernel's metadata.

    :param kern: the kernel object for which to determine arguments.
    :type kern: :py:class:`psyclone.domain.lfric.LFRicKern`
    :param symbol_table: the symbol table associated with the routine that \
        contains the `invoke` of this kernel.
    :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`

    :raises TypeError: if supplied symbol table is of incorrect type.

    '''
    def __init__(self, kern, symbol_table):
        super().__init__(kern)
        if not isinstance(symbol_table, SymbolTable):
            raise TypeError(
                f"Argument 'symbol_table' must be a SymbolTable "
                f"instance but got '{type(symbol_table).__name__}'")
        self._symtab = symbol_table
        # Once generate() is called, this list will contain 2-tuples, each
        # containing a Symbol and a function space (string).
        self._fields = []
        self._scalars = []
        self._qr_objects = []
        # Once generate() is called, this list will contain 3-tuples, each
        # containing a Symbol and from- and to-function spaces (strings).
        self._operators = []

    @property
    def fields(self):
        '''
        :returns: the field (and field-vector) arguments plus their \
                  corresponding function spaces.
        :rtype: List[Tuple(:py:class:`psyclone.psyir.symbols.DataSymbol`, str)]
        '''
        return self._fields

    @property
    def scalars(self):
        '''
        :returns: the scalar arguments to the kernel.
        :rtype: List[:py:class:`psyclone.psyir.symbols.DataSymbol`]
        '''
        return self._scalars

    @property
    def quadrature_objects(self):
        '''
        :returns: the symbols representing the quadrature objects required by \
                  the kernel along with the shape of each.
        :rtype: List[Tuple[:py:class:`psyclone.psyir.symbols.DataSymbol`, str]]
        '''
        return self._qr_objects

    @property
    def operators(self):
        '''
        :returns: the symbols representing the operators required by the \
                  kernel along with the names of the from- and to- function \
                  spaces.
        :rtype: List[Tuple[:py:class:`psyclone.psyir.symbols.DataSymbol`, \
                           str, str]]
        '''
        return self._operators

    def generate(self, var_accesses=None):
        ''' Ensures that our internal lists of arguments of various
        types are reset (as calling generate() populates them) before calling
        this method in the parent class.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`
        '''
        self._fields = []
        self._scalars = []
        self._qr_objects = []
        self._operators = []
        super().generate(var_accesses)

    def scalar(self, scalar_arg, var_accesses=None):
        '''
        Add the necessary argument for a scalar quantity as well as an
        appropriate Symbol to the SymbolTable.

        :param scalar_arg: the scalar kernel argument.
        :type scalar_arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance that \
            stores information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        :raises NotImplementedError: if a scalar of type other than real \
            or integer is found.

        '''
        super().scalar(scalar_arg, var_accesses)

        # Create a DataSymbol for this kernel argument.
        if scalar_arg.intrinsic_type == "real":
            datatype = LFRicTypes("LFRicRealScalarDataType")()
        elif scalar_arg.intrinsic_type == "integer":
            datatype = LFRicTypes("LFRicIntegerScalarDataType")()
        elif scalar_arg.intrinsic_type == "logical":
            datatype = LFRicTypes("LFRicLogicalScalarDataType")()
        else:
            raise NotImplementedError(
                f"Scalar of type '{scalar_arg.intrinsic_type}' not supported.")

        consts = LFRicConstants()
        precision_name = consts.SCALAR_PRECISION_MAP[scalar_arg.intrinsic_type]
        self._symtab.add_lfric_precision_symbol(precision_name)

        sym = self._symtab.new_symbol(scalar_arg.name,
                                      symbol_type=DataSymbol,
                                      datatype=datatype)
        self._scalars.append(sym)

    def fs_common(self, function_space, var_accesses=None):
        ''' Does nothing as there are no arguments associated with function
        spaces at the algorithm level.

        :param function_space: the function space for which arguments \
            should be added.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`
        '''

    def field_vector(self, argvect, var_accesses=None):
        '''Add the field vector associated with the argument 'argvect' to the
        argument list and an associated Symbol to the SymbolTable.

        :param argvect: the field vector to add.
        :type argvect: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        ftype = self._symtab.lookup("field_type")
        dtype = ArrayType(ftype, [argvect.vector_size])

        sym = self._symtab.new_symbol(argvect.name,
                                      symbol_type=DataSymbol, datatype=dtype)
        self._fields.append((sym,
                             LFRicConstants().specific_function_space(
                                 argvect.function_space.orig_name)))
        self.append(sym.name, var_accesses, mode=argvect.access,
                    metadata_posn=argvect.metadata_index)

    def field(self, arg, var_accesses=None):
        '''Add the field array associated with the argument 'arg' to the
        argument list and an appropriate Symbol to the SymbolTable.

        :param arg: the field to be added.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        ftype = self._symtab.lookup("field_type")
        sym = self._symtab.new_symbol(arg.name,
                                      symbol_type=DataSymbol, datatype=ftype)
        self._fields.append((sym,
                             LFRicConstants().specific_function_space(
                                 arg.function_space.orig_name)))
        self.append(sym.name, var_accesses, mode=arg.access,
                    metadata_posn=arg.metadata_index)

    def stencil(self, arg, var_accesses=None):
        '''Add general stencil information associated with the argument 'arg'
        to the argument list.

        :param arg: the meta-data description of the kernel \
            argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        :raises NotImplementedError: stencils are not yet supported.

        '''
        raise NotImplementedError("Stencils are not yet supported")

    def stencil_2d(self, arg, var_accesses=None):
        '''Add general 2D stencil information associated with the argument
        'arg' to the argument list. This method passes through to the
        :py:meth:`KernCallInvokeArgList.stencil` method.

        :param arg: the meta-data description of the kernel \
            argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        self.stencil(arg, var_accesses)

    def stencil_unknown_extent(self, arg, var_accesses=None):
        '''Add stencil information to the argument list associated with the
        argument 'arg' if the extent is unknown. If supplied it also stores
        this access in var_accesses.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        :raises NotImplementedError: stencils are not yet supported.

        '''
        raise NotImplementedError(
            "stencil_unknown_extent not yet implemented.")

    def stencil_2d_unknown_extent(self, arg, var_accesses=None):
        '''Add 2D stencil information to the argument list associated with the
        argument 'arg' if the extent is unknown. If supplied it also stores
        this access in var_accesses.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        :raises NotImplementedError: stencils are not yet supported.

        '''
        raise NotImplementedError(
            "stencil_2d_unknown_extent not yet implemented.")

    def operator(self, arg, var_accesses=None):
        '''Add the operator argument. If supplied it also stores this access
        in var_accesses.

        :param arg: the meta-data description of the operator.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        consts = LFRicConstants()
        tmap = consts.DATA_TYPE_MAP
        try:
            otype = self._symtab.lookup(tmap["operator"]["type"])
        except KeyError:
            csym = self._symtab.new_symbol(tmap["operator"]["module"],
                                           symbol_type=ContainerSymbol)
            otype = self._symtab.new_symbol(tmap["operator"]["type"],
                                            symbol_type=DataTypeSymbol,
                                            datatype=UnresolvedType(),
                                            interface=ImportInterface(csym))
        sym = self._symtab.new_symbol(arg.name,
                                      symbol_type=DataSymbol, datatype=otype)
        fs_from = consts.specific_function_space(
            arg.function_space_from.orig_name)
        fs_to = consts.specific_function_space(arg.function_space_to.orig_name)
        self._operators.append((sym, fs_from, fs_to))
        self.append(sym.name, var_accesses, mode=arg.access,
                    metadata_posn=arg.metadata_index)

    def quad_rule(self, var_accesses=None):
        '''Add quadrature-related information to the kernel argument list.
        Adds the necessary arguments to the argument list and suitable
        symbols to the SymbolTable. Optionally also adds variable access
        information to the var_accesses object.

        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        lfric_const = LFRicConstants()

        for shape, rule in self._kern.qr_rules.items():
            mod_name = lfric_const.QUADRATURE_TYPE_MAP[shape]["module"]
            type_name = lfric_const.QUADRATURE_TYPE_MAP[shape]["type"]
            quad_container = self._symtab.find_or_create(
                mod_name, symbol_type=ContainerSymbol)
            quad_type = self._symtab.find_or_create(
                type_name, symbol_type=DataTypeSymbol,
                datatype=UnresolvedType(),
                interface=ImportInterface(quad_container))
            sym = self._symtab.new_symbol(rule.psy_name,
                                          symbol_type=DataSymbol,
                                          datatype=quad_type)
            self._qr_objects.append((sym, shape))
            self.append(sym.name, var_accesses)


# ============================================================================
# For automatic documentation creation:
__all__ = ["KernCallInvokeArgList"]
