# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
This module implements a class that captures all the arguments
of a Kernel as required by an `invoke` of that kernel.
'''

from typing import Optional, TYPE_CHECKING
from psyclone.core import VariablesAccessMap
from psyclone.domain.lfric.arg_ordering import ArgOrdering
from psyclone.domain.lfric.function_space import FunctionSpace
from psyclone.domain.lfric.lfric_constants import LFRicConstants
from psyclone.domain.lfric.lfric_kern import LFRicKern
from psyclone.domain.lfric.lfric_types import LFRicTypes
from psyclone.psyir.symbols import (
    ArrayType, DataSymbol, DataTypeSymbol, UnresolvedType, SymbolTable,
    ContainerSymbol, ImportInterface)
if TYPE_CHECKING:
    from psyclone.lfric import LFRicKernelArgument


class KernCallInvokeArgList(ArgOrdering):
    '''Determines the arguments that must be provided to an `invoke` of a
    kernel, according to that kernel's metadata.

    :param kern: the kernel object for which to determine arguments.
    :param symbol_table: the symbol table associated with the routine that
        contains the `invoke` of this kernel.

    :raises TypeError: if supplied symbol table is of incorrect type.

    '''
    def __init__(self, kern: LFRicKern, symbol_table: SymbolTable):
        super().__init__(kern)
        if not isinstance(symbol_table, SymbolTable):
            raise TypeError(
                f"Argument 'symbol_table' must be a SymbolTable "
                f"instance but got '{type(symbol_table).__name__}'")
        # TODO #2503: This reference will not survive some tree modifications
        self._forced_symtab = symbol_table
        # Once generate() is called, this list will contain 2-tuples, each
        # containing a Symbol and a function space (string).
        self._fields = []
        self._scalars = []
        self._qr_objects = []
        # Once generate() is called, this list will contain 3-tuples, each
        # containing a Symbol and from- and to-function spaces (strings).
        self._operators = []

    @property
    def fields(self) -> list[tuple[DataSymbol, str]]:
        '''
        :returns: the field (and field-vector) arguments plus their
                  corresponding function spaces.
        '''
        return self._fields

    @property
    def scalars(self) -> list[DataSymbol]:
        '''
        :returns: the scalar (and scalar-array) arguments to the kernel.
        '''
        return self._scalars

    @property
    def quadrature_objects(self) -> list[tuple[DataSymbol, str]]:
        '''
        :returns: the symbols representing the quadrature objects required by
                  the kernel along with the shape of each.
        '''
        return self._qr_objects

    @property
    def operators(self) -> list[tuple[DataSymbol, str, str]]:
        '''
        :returns: the symbols representing the operators required by the
                  kernel along with the names of the from- and to- function
                  spaces.
        '''
        return self._operators

    def generate(self,
                 var_accesses: Optional[VariablesAccessMap] = None) -> None:
        ''' Ensures that our internal lists of arguments of various
        types are reset (as calling generate() populates them) before calling
        this method in the parent class.

        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.
        '''
        self._fields = []
        self._scalars = []
        self._qr_objects = []
        self._operators = []
        self._halo_depth = None

        super().generate(var_accesses)

    def scalar(self,
               scalar_arg: "LFRicKernelArgument",
               var_accesses: Optional[VariablesAccessMap] = None
               ) -> None:
        '''
        Add the necessary argument for a scalar quantity as well as an
        appropriate Symbol to the SymbolTable.

        :param scalar_arg: the scalar kernel argument.
        :param var_accesses: optional information about variable accesses.

        :raises NotImplementedError: if a scalar of type other than real,
            logical or integer is found.

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
        LFRicTypes.add_precision_symbol(self._symtab, precision_name)

        if scalar_arg._array_ndims:
            datatype = ArrayType(
                datatype, scalar_arg._array_ndims*[ArrayType.Extent.ATTRIBUTE])

        sym = self._symtab.find_or_create_tag(scalar_arg.name,
                                              symbol_type=DataSymbol,
                                              datatype=datatype)
        self._scalars.append(sym)

    def fs_common(self, function_space: FunctionSpace,
                  var_accesses: Optional[VariablesAccessMap] = None) -> None:
        ''' Does nothing as there are no arguments associated with function
        spaces at the algorithm level.

        :param function_space: the function space for which arguments
            should be added.
        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.
        '''

    def field_vector(
            self,
            argvect: "LFRicKernelArgument",
            var_accesses: Optional[VariablesAccessMap] = None) -> None:
        '''Add the field vector associated with the argument 'argvect' to the
        argument list and an associated Symbol to the SymbolTable.

        :param argvect: the field vector to add.
        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        '''
        ftype = self._symtab.lookup("field_type")
        dtype = ArrayType(ftype, [argvect.vector_size])

        sym = self._symtab.find_or_create_tag(
            argvect.name, symbol_type=DataSymbol, datatype=dtype)
        self._fields.append((sym,
                             LFRicConstants().specific_function_space(
                                 argvect.function_space.orig_name)))
        self.append(sym.name, var_accesses, mode=argvect.access,
                    metadata_posn=argvect.metadata_index)

    def field(self,
              arg: "LFRicKernelArgument",
              var_accesses: VariablesAccessMap = None) -> None:
        '''Add the field array associated with the argument 'arg' to the
        argument list and an appropriate Symbol to the SymbolTable.

        :param arg: the field to be added.
        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        '''
        ftype = self._symtab.lookup("field_type")
        sym = self._symtab.find_or_create_tag(
            arg.name, symbol_type=DataSymbol, datatype=ftype)
        self._fields.append((sym,
                             LFRicConstants().specific_function_space(
                                 arg.function_space.orig_name)))
        self.append(sym.name, var_accesses, mode=arg.access,
                    metadata_posn=arg.metadata_index)

    def stencil(self,
                arg: "LFRicKernelArgument",
                var_accesses: VariablesAccessMap = None) -> None:
        '''Add general stencil information associated with the argument 'arg'
        to the argument list.

        :param arg: the meta-data description of the kernel
            argument with which the stencil is associated.
        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        :raises NotImplementedError: stencils are not yet supported.

        '''
        raise NotImplementedError("Stencils are not yet supported")

    def stencil_2d(self,
                   arg: "LFRicKernelArgument",
                   var_accesses: Optional[VariablesAccessMap] = None) -> None:
        '''Add general 2D stencil information associated with the argument
        'arg' to the argument list. This method passes through to the
        :py:meth:`KernCallInvokeArgList.stencil` method.

        :param arg: the meta-data description of the kernel
            argument with which the stencil is associated.
        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        '''
        self.stencil(arg, var_accesses)

    def stencil_unknown_extent(
            self,
            arg: "LFRicKernelArgument",
            var_accesses: Optional[VariablesAccessMap] = None) -> None:
        '''Add stencil information to the argument list associated with the
        argument 'arg' if the extent is unknown. If supplied it also stores
        this access in var_accesses.

        :param arg: the kernel argument with which the stencil is associated.
        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        :raises NotImplementedError: stencils are not yet supported.

        '''
        raise NotImplementedError(
            "stencil_unknown_extent not yet implemented.")

    def stencil_2d_unknown_extent(
            self,
            arg: "LFRicKernelArgument",
            var_accesses: Optional[VariablesAccessMap] = None) -> None:
        '''Add 2D stencil information to the argument list associated with the
        argument 'arg' if the extent is unknown. If supplied it also stores
        this access in var_accesses.

        :param arg: the kernel argument with which the stencil is associated.
        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

        :raises NotImplementedError: stencils are not yet supported.

        '''
        raise NotImplementedError(
            "stencil_2d_unknown_extent not yet implemented.")

    def operator(self,
                 arg: "LFRicKernelArgument",
                 var_accesses: Optional[VariablesAccessMap] = None) -> None:
        '''Add the operator argument. If supplied it also stores this access
        in var_accesses.

        :param arg: the meta-data description of the operator.
        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

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
        sym = self._symtab.find_or_create_tag(
            arg.name, symbol_type=DataSymbol, datatype=otype)
        fs_from = consts.specific_function_space(
            arg.function_space_from.orig_name)
        fs_to = consts.specific_function_space(arg.function_space_to.orig_name)
        self._operators.append((sym, fs_from, fs_to))
        self.append(sym.name, var_accesses, mode=arg.access,
                    metadata_posn=arg.metadata_index)

    def quad_rule(self,
                  var_accesses: Optional[VariablesAccessMap] = None) -> None:
        '''Add quadrature-related information to the kernel argument list.
        Adds the necessary arguments to the argument list and suitable
        symbols to the SymbolTable. Optionally also adds variable access
        information to the var_accesses object.

        :param var_accesses: optional VariablesAccessMap instance to store
            the information about variable accesses.

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
            sym = self._symtab.find_or_create_tag(rule.psy_name,
                                                  symbol_type=DataSymbol,
                                                  datatype=quad_type)
            self._qr_objects.append((sym, shape))
            self.append(sym.name, var_accesses)

    def halo_depth(self,
                   var_accesses: Optional[VariablesAccessMap] = None) -> None:
        '''
        Add a halo-depth argument to the Kernel argument list.
        Optionally, also adds variable access information to the var_accesses
        object.

        :param var_accesses: optional VariablesAccessMap instance to store
            information about variable accesses.

        '''
        self.append(self._kern.halo_depth.symbol.name, var_accesses)


# ============================================================================
# For automatic documentation creation:
__all__ = ["KernCallInvokeArgList"]
