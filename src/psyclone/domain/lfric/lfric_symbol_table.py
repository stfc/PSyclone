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
# Authors: R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# Modified: O.Brunt, Met Office
# -----------------------------------------------------------------------------

''' This module contains the LFRic-specific SymbolTable implementation.
It provides convenience functions to create often used symbols.
'''
from collections import namedtuple
from dataclasses import dataclass

from psyclone.configuration import Config
from psyclone.domain.lfric import LFRicConstants
# Avoid circular import:
from psyclone.domain.lfric.lfric_types import LFRicTypes
from psyclone.errors import InternalError

from psyclone.psyir.symbols import (ArrayType, ContainerSymbol, DataSymbol,
                                    ImportInterface, INTEGER_TYPE, ScalarType,
                                    Symbol, SymbolTable)


class LFRicSymbolTable(SymbolTable):
    # pylint: disable=abstract-method
    '''
    Sub-classes SymbolTable to provide a LFRic-specific implementation.

    :param node: reference to the Schedule or Container to which this
        symbol table belongs.
    :type node: :py:class:`psyclone.psyir.nodes.Schedule` |
        :py:class:`psyclone.psyir.nodes.Container` | NoneType
    :param default_visibility: optional default visibility value for this
        symbol table, if not provided it defaults to PUBLIC visibility.
    :type default_visibillity:
        :py:class:`psyclone.psyir.symbols.Symbol.Visibility`

    '''

    def __init__(self, node=None, default_visibility=Symbol.Visibility.PUBLIC):
        super().__init__(node, default_visibility)

        # The container symbol for all precision variables
        self._constants_mod = None
        # A mapping of 'i_def' etc. to the corresponding DataSymbol
        self._precision_map = {}
        self._type_name_to_class = {}
        const = LFRicConstants()
        mod_name = const.UTILITIES_MOD_MAP["constants"]["module"]
        self._constants_mod = self.lookup(mod_name,
                                          otherwise=ContainerSymbol(mod_name))
        # First time an instance of this is created, define
        # the precision mapping.
        #if not LFRicSymbolTable._precision_map:
        #    const = LFRicConstants()
        #    mod_name = const.UTILITIES_MOD_MAP["constants"]["module"]
        #    LFRicSymbolTable._constants_mod = ContainerSymbol(mod_name)

        #    api_config = Config.get().api_conf("lfric")
        #    for precision in api_config.precision_map:
        #        LFRicSymbolTable._precision_map[precision] = \
        #            DataSymbol(precision, INTEGER_TYPE,
        #                       interface=ImportInterface(self._constants_mod))

    def find_or_create_integer_symbol(self, name, tag=None):
        '''This function returns a symbol for an integer reference. If a
        tag is specified, it will be used to search for an existing symbol,
        otherwise the name will be used. If the symbol should not already
        exist in the symbol table, it will be returned, otherwise a new
        symbol will be created.

        :param str name: name of the integer variable to declare.
        :param tag: optional tag of the integer variable to declare.
        :type tag: Optional[str]

        :returns: the symbol for the variable.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

        :raises TypeError: TypeError if the symbol exists but is not
            a DataSymbol.
        :raises TypeError: TypeError if the symbol exists and is a
            DataSymbol, but not an Integer.

        '''
        if tag:
            try:
                sym = self.lookup_with_tag(tag)
            except KeyError:
                sym = None
        else:
            try:
                sym = self.lookup(name)
            except KeyError:
                sym = None

        datatype = self.get_type("LFRicIntegerScalarDataType")()
        if sym is None:
            # Create a DataSymbol for this kernel argument.
            sym = self.new_symbol(name, tag=tag,
                                  symbol_type=DataSymbol,
                                  datatype=datatype)
        else:
            # The symbol already exists, check that is the right type:
            if not isinstance(sym, DataSymbol):
                raise TypeError(f"Symbol '{sym.name}' already exists, but is "
                                f"not a DataSymbol, but '{type(sym)}'.")
            if sym.datatype != datatype:
                raise TypeError(f"Symbol '{sym.name}' already exists, but is "
                                f"not an integer, but '{sym.datatype}'.")
        return sym

    # ------------------------------------------------------------------------
    def find_or_create_array(self, array_name, num_dimensions, intrinsic_type,
                             tag=None):
        '''This function returns a symbol for an ArrayReference. If the
        symbol does not exist, it is created. If a new array symbol is
        created, it gets the DEFERRED attribute, which in Fortran means
        it will be declared as an allocatable array.

        :param str array_name: the name and tag of the array.
        :param int num_dimensions: the number of dimensions of this array.
        :param intrinsic_type: the intrinsic type of the array.
        :type intrinsic_type: \
            :py:class:`psyclone.psyir.symbols.datatypes.ScalarType.Intrinsic`
        :param tag: optional tag to be used in searching and defining.
        :type tag: Optional[str]

        :returns: the requested symbol
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

        :raises TypeError: if the symbol exists, but is not a DataSymbol, \
            or not an Array, or has different number of dimensions.

        '''
        if intrinsic_type == ScalarType.Intrinsic.REAL:
            datatype = self.get_type("LFRicRealScalarDataType")()
        elif intrinsic_type == ScalarType.Intrinsic.INTEGER:
            datatype = self.get_type("LFRicIntegerScalarDataType")()
        elif intrinsic_type == ScalarType.Intrinsic.BOOLEAN:
            datatype = self.get_type("LFRicLogicalScalarDataType")()
        else:
            raise TypeError(f"Unsupported data type "
                            f"'{intrinsic_type}' in "
                            f"find_or_create_array")

        try:
            if tag:
                sym = self.lookup_with_tag(tag)
            else:
                sym = self.lookup(array_name)
        except KeyError:
            # pylint: disable=raise-missing-from
            # Create a DataSymbol for this kernel argument.
            array_type = ArrayType(datatype,
                                   [ArrayType.Extent.DEFERRED]*num_dimensions)

            sym = self.new_symbol(array_name, tag=tag,
                                  symbol_type=DataSymbol,
                                  datatype=array_type)
            return sym

        # Symbol already exists, check consistency:
        if not isinstance(sym, DataSymbol):
            raise TypeError(f"Symbol '{sym.name}' already exists, but is "
                            f"not a DataSymbol, but '{type(sym)}'.")

        if not isinstance(sym.datatype, ArrayType):
            raise TypeError(f"Symbol '{sym.name}' already exists, but is "
                            f"not an ArraySymbol, but "
                            f"'{type(sym.datatype)}'.")

        if sym.datatype.datatype != datatype:
            raise TypeError(f"Symbol '{sym.name}' already exists, but is "
                            f"not of type '{intrinsic_type}', but "
                            f"'{type(sym.datatype.datatype)}'.")

        if len(sym.shape) != num_dimensions:
            raise TypeError(f"Array '{sym.name}' already exists, but has "
                            f"{len(sym.shape)} dimensions, not "
                            f"{num_dimensions}.")

        return sym

    # ------------------------------------------------------------------------
    def add_lfric_precision_symbol(self, name):
        '''
        If the named LFRic precision symbol is not already in the table then
        add it. Also ensure that the Container symbol from which it is
        imported is in the table.

        :param str name: name of the LFRic precision symbol to add to table.

        :returns: the specified LFRic precision symbol.
        :rtype: :py:class:`psyclone.psyir.symbols.DataSymbol`

        :raises ValueError: if the supplied name is not a recognised LFRic
            precision variable.
        :raises ValueError: if a symbol with the same name is already in the
            table but is not imported from the correct container.

        '''
        api_config = Config.get().api_conf("lfric")
        lname = name.lower()
        if lname not in api_config.precision_map.keys():
            raise ValueError(f"'{lname}' is not a recognised LFRic precision.")

        #sym = self._precision_map[lname]

        try:
            # Sanity check that the existing symbol is the right one.
            existing_sym = self.lookup(lname)
            if (not isinstance(existing_sym.interface, ImportInterface) or
                    existing_sym.interface.container_symbol.name !=
                    self._constants_mod.name):
                raise ValueError(
                    f"Precision symbol '{lname}' already exists in the "
                    f"supplied symbol table but is not imported from the "
                    f"LFRic constants module ({self._constants_mod.name}).")
            return existing_sym
        except KeyError:
            pass

        csym = self.lookup(self._constants_mod.name, otherwise=None)
        if not csym:
            self.add(self._constants_mod)
        sym = DataSymbol(lname, INTEGER_TYPE,
                         interface=ImportInterface(self._constants_mod))
        self.add(sym)
        return sym

    def get_type(self, name):
        '''
        '''
        if not self._type_name_to_class:
            self._init_types()

        try:
            return self._type_name_to_class[name]
        except KeyError as err:
            raise InternalError(f"Unknown LFRic type '{name}'. Valid values "
                                f"are {LFRicTypes._name_to_class.keys()}") \
                  from err

    def _init_types(self):
        '''
        '''
        self._create_generic_scalars()
        self._create_lfric_dimension()
        self._create_specific_scalars()
        self._create_fields()

        # Generate LFRic vector-field-data symbols as subclasses of
        # field-data symbols
        const = LFRicConstants()
        for intrinsic in const.VALID_FIELD_INTRINSIC_TYPES:
            name = f"{intrinsic.title()}VectorFieldDataSymbol"
            baseclass = self.get_type(f"{intrinsic.title()}FieldDataSymbol")
            self._type_name_to_class[name] = type(name, (baseclass, ), {})

    def _create_generic_scalars(self):
        '''This function adds the generic data types and symbols for
        integer, real, and booleans to the global mapping.

        '''
        GenericScalar = namedtuple('GenericScalar', ["name", "intrinsic",
                                                     "precision"])
        generic_scalar_datatypes = [
            GenericScalar("LFRicIntegerScalar", ScalarType.Intrinsic.INTEGER,
                          self.add_lfric_precision_symbol("I_DEF")),
            GenericScalar("LFRicRealScalar", ScalarType.Intrinsic.REAL,
                          self.add_lfric_precision_symbol("R_DEF")),
            GenericScalar("LFRicLogicalScalar", ScalarType.Intrinsic.BOOLEAN,
                          self.add_lfric_precision_symbol("L_DEF"))]

        # Generate generic LFRic scalar datatypes and symbols from definitions
        for info in generic_scalar_datatypes:

            # Create the generic data
            type_name = f"{info.name}DataType"
            self._create_generic_scalar_data_type(type_name,
                                                  info.intrinsic,
                                                  info.precision)
            type_class = self._type_name_to_class[type_name]
            # Create the generic data symbol
            symbol_name = f"{info.name}DataSymbol"
            self._create_generic_scalar_data_symbol(symbol_name,
                                                    type_class)

    def _create_generic_scalar_data_type(self, name, intrinsic,
                                         default_precision):
        '''This function creates a generic scalar data type class and adds
        it to the global mapping.

        :param str name: name of the data type to create.
        :param intrinsic: the intrinsic type to use.
        :type intrinsic: \
            :py:class:`pyclone.psyir.datatypes.ScalarType.Intrinsic`
        :param str default_precision: the default precision this class \
            should have if the precision is not specified.

        '''
        # This is the constructor for the dynamically created class:
        def __my_generic_scalar_type_init__(self, precision=None):
            if not precision:
                precision = self.default_precision
            ScalarType.__init__(self, self.intrinsic, precision)

        # ---------------------------------------------------------------------
        # Create the class, and set the above function as constructor. Note
        # that the values of 'intrinsic' and 'default_precision' must be stored
        # in the class: the `__my_generic_scalar_type_init__` function is
        # defined over and over again, each time with different values for
        # intrinsic/default_precision. So when these arguments would be
        # directly used in the constructor, they would remain at the values
        # used the last time this function was defined, but obviously each
        # class need the constructor using the right values. So these values
        # are stored in the class and then used in the constructor.
        self._type_name_to_class[name] = \
            type(name, (ScalarType, ),
                 {"__init__": __my_generic_scalar_type_init__,
                  "intrinsic": intrinsic,
                  "default_precision": default_precision})

    def _create_generic_scalar_data_symbol(self, name, type_class):
        '''This function creates a data symbol class with the specified name
        and data type, and adds it to the global mapping.

        :param str name: the name of the class to creates
        :param type_class: the data type for the symbol
        :type type_class: py:class:`pyclone.psyir.datatypes.ScalarType`

        '''
        # This is the constructor for the dynamically created class:
        def __my_generic_scalar_symbol_init__(self, name, precision=None,
                                              **kwargs):
            DataSymbol.__init__(self, name,
                                self.type_class(precision=precision),
                                **kwargs)

        # ---------------------------------------------------------------------
        # Create the class, set the constructor and store the ScalarType as
        # an attribute, so it can be accessed in the constructor.
        self._type_name_to_class[name] = \
            type(name, (DataSymbol, ),
                 {"__init__": __my_generic_scalar_symbol_init__,
                  "type_class": type_class})

    # ------------------------------------------------------------------------
    def _create_lfric_dimension(self):
        '''This function adds the LFRicDimension class to the global mapping,
        and creates the two instances for scalar and vector dimension.

        '''
        # The actual class:
        from psyclone.psyir.nodes.literal import Literal
        class LFRicDimension(Literal):
            '''An LFRic-specific scalar integer that captures a literal array
            dimension which can have a value between 1 and 3, inclusive. This
            is used for one of the dimensions in basis and differential basis
            functions and also for the vertical-boundary dofs mask.

            :param str value: the value of the scalar integer.

            :raises ValueError: if the supplied value is not '1', '2' or '3'.

            '''
            # pylint: disable=undefined-variable
            def __init__(self, value, dtype):
                super().__init__(value, dtype())
                if value not in ['1', '2', '3']:
                    raise ValueError(f"An LFRic dimension object must be '1', "
                                     f"'2' or '3', but found '{value}'.")
        # --------------------------------------------------------------------

        # Create the required entries in the dictionary
        dtype = self.get_type("LFRicIntegerScalarDataType")
        self._type_name_to_class.update({
            "LFRicDimension": LFRicDimension,
            "LFRIC_SCALAR_DIMENSION": LFRicDimension("1",
                                                     dtype=dtype),
            "LFRIC_VERTICAL_BOUNDARIES_DIMENSION": LFRicDimension("2",
                                                                  dtype=dtype),
            "LFRIC_VECTOR_DIMENSION": LFRicDimension("3", dtype=dtype)})

    def _create_specific_scalars(self):
        '''This function creates all required specific scalar, which are
        derived from the corresponding generic classes (e.g.
        LFRicIntegerScalarData)

        '''
        # The Scalar namedtuple has 3 properties: the first
        # determines the names of the resultant datatype and datasymbol
        # classes, the second references the generic scalar type
        # classes declared above and the third specifies any
        # additional class properties that should be declared in the generated
        # datasymbol class.
        Scalar = namedtuple('Scalar', ["name", "generic_type_name",
                                       "properties"])
        specific_scalar_datatypes = [
            Scalar("CellPosition", "LFRicIntegerScalarData", []),
            Scalar("MeshHeight", "LFRicIntegerScalarData", []),
            Scalar("NumberOfCells", "LFRicIntegerScalarData", []),
            Scalar("NumberOfDofs", "LFRicIntegerScalarData", ["fs"]),
            Scalar("NumberOfUniqueDofs", "LFRicIntegerScalarData", ["fs"]),
            Scalar("NumberOfFaces", "LFRicIntegerScalarData", []),
            Scalar("NumberOfEdges", "LFRicIntegerScalarData", []),
            Scalar("NumberOfQrPointsInXy", "LFRicIntegerScalarData", []),
            Scalar("NumberOfQrPointsInZ", "LFRicIntegerScalarData", []),
            Scalar("NumberOfQrPointsInFaces", "LFRicIntegerScalarData", []),
            Scalar("NumberOfQrPointsInEdges", "LFRicIntegerScalarData", [])]

        for info in specific_scalar_datatypes:
            type_name = f"{info.name}DataType"
            self._type_name_to_class[type_name] = \
                type(type_name,
                     (self.get_type(f"{info.generic_type_name}Type"), ),
                     {})

            symbol_name = f"{info.name}DataSymbol"
            base_class = self.get_type(f"{info.generic_type_name}Symbol")
            self._create_scalar_data_type(symbol_name, base_class,
                                          info.properties)

    def _create_scalar_data_type(self, class_name, base_class, parameters):
        '''This function creates a specific scalar data type with the given
        name, derived from the specified base class.

        :param str class_name: name of the class to create.
        :param base_class: the class on which to base the newly created class.
        :type base_class: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param parameters: additional required arguments of the constructor, \
            which will be set as attributes in the base class.
        :type parameters: List[str]

        '''
        # ---------------------------------------------------------------------
        # This is the __init__ function for the newly declared scalar data
        # types, which will be added as an attribute for the newly created
        # class. It parses the additional positional and keyword arguments
        # and sets them as attributes.

        def __my_scalar_init__(self, name, *args, **kwargs):
            # Set all the positional arguments as attributes:
            for i, arg in enumerate(args):
                setattr(self, self.parameters[i], arg)
            # Now handle the keyword arguments: any keyword arguments
            # that are declared as parameter will be set as attribute,
            # anything else will be passed to the constructor of the
            # base class.
            remaining_kwargs = {}
            for key, value in kwargs.items():
                # It is one of the additional parameters, set it as
                # attribute:
                if key in self.parameters:
                    setattr(self, key, value)
                else:
                    # Otherwise add it as keyword parameter for the
                    # base class constructor
                    remaining_kwargs[key] = value
            self.base_class.__init__(self, name, **remaining_kwargs)

        # ----------------------------------------------------------------

        # Now create the actual class. We need to keep a copy of the parameters
        # of this class as attributes, otherwise the __my_scalar_init__
        # function (there is only one, which gets re-defined over and over)
        # will all be using the values for base_class and parameters that were
        # active the last time the function was defined. E.g. the
        # __my_scalar_init__ function set for CellPosition is the same as the
        # function set in NumberOfQrPointsInEdges (i.e. based on the same
        # values for base_class and parameters).
        self._type_name_to_class[class_name] = \
            type(class_name, (base_class, ),
                 {"__init__": __my_scalar_init__,
                  "base_class": base_class,
                  "parameters": parameters})

    def _create_fields(self):
        '''This function creates the data symbol and types for LFRic fields.

        '''
        # Note, field_datatypes are no different to array_datatypes and are
        # treated in the same way. They are only separated into a different
        # list because they are used to create vector field datatypes and
        # symbols.

        @dataclass(frozen=True)
        class Array:
            '''
            Holds the properties of an LFRic array type, used when generating
            DataSymbol and DataSymbolType classes.

            :param name: the base name to use for the datatype and datasymbol.
            :param scalar_type: the name of the LFRic scalar type that this is
                                an array of.
            :param dims: textual description of each of the dimensions.
            :param properties: names of additional class properties that should
                               be declared in the generated datasymbol class.
            '''
            name: str
            scalar_type: str
            dims: list        # list[str] notation supported in Python 3.9+
            properties: list  # ditto

        field_datatypes = [
            Array("RealField", "LFRicRealScalarDataType",
                  ["number of unique dofs"], ["fs"]),
            Array("IntegerField", "LFRicIntegerScalarDataType",
                  ["number of unique dofs"], ["fs"]),
            Array("LogicalField", "LFRicLogicalScalarDataType",
                  ["number of unique dofs"], ["fs"])]

        # TBD: #918 the dimension datatypes and their ordering is captured in
        # field_datatypes and array_datatypes but is not stored in the
        # generated classes.

        # TBD: #926 attributes will be constrained to certain datatypes and
        # values. For example, a function space attribute should be a string
        # containing the name of a supported function space. These are not
        # currently checked.

        # TBD: #927 in some cases the values of attributes can be inferred, or
        # at least must be consistent. For example, a field datatype has an
        # associated function space attribute, its dimension symbol (if there
        # is one) must be a NumberOfUniqueDofsDataSymbol which also has a
        # function space attribute and the two function spaces must be
        # the same. This is not currently checked.
        array_datatypes = [
            Array("Operator", "LFRicRealScalarDataType",
                  ["number of dofs", "number of dofs", "number of cells"],
                  ["fs_from", "fs_to"]),
            Array("DofMap", "LFRicIntegerScalarDataType",
                  ["number of dofs"], ["fs"]),
            Array("BasisFunctionQrXyoz", "LFRicRealScalarDataType",
                  [self.get_type("LFRicDimension"), "number of dofs",
                   "number of qr points in xy",
                   "number of qr points in z"], ["fs"]),
            Array("BasisFunctionQrFace", "LFRicRealScalarDataType",
                  [self.get_type("LFRicDimension"), "number of dofs",
                   "number of qr points in faces",
                   "number of faces"], ["fs"]),
            Array("BasisFunctionQrEdge", "LFRicRealScalarDataType",
                  [self.get_type("LFRicDimension"), "number of dofs",
                   "number of qr points in edges",
                   "number of edges"], ["fs"]),
            Array("DiffBasisFunctionQrXyoz", "LFRicRealScalarDataType",
                  [self.get_type("LFRicDimension"), "number of dofs",
                   "number of qr points in xy",
                   "number of qr points in z"], ["fs"]),
            Array("DiffBasisFunctionQrFace", "LFRicRealScalarDataType",
                  [self.get_type("LFRicDimension"), "number of dofs",
                   "number of qr points in faces",
                   "number of faces"], ["fs"]),
            Array("DiffBasisFunctionQrEdge", "LFRicRealScalarDataType",
                  [self.get_type("LFRicDimension"), "number of dofs",
                   "number of qr points in edges", "number of edges"], ["fs"]),
            Array("QrWeightsInXy", "LFRicRealScalarDataType",
                  ["number of qr points in xy"], []),
            Array("QrWeightsInZ", "LFRicRealScalarDataType",
                  ["number of qr points in z"], []),
            Array("QrWeightsInFaces", "LFRicRealScalarDataType",
                  ["number of qr points in faces"], []),
            Array("QrWeightsInEdges", "LFRicRealScalarDataType",
                  ["number of qr points in edges"], []),
            Array("VerticalBoundaryDofMask", "LFRicIntegerScalarDataType",
                  ["number of dofs", self.get_type("LFRicDimension")], [])
            ]

        for array_type in array_datatypes + field_datatypes:
            name = f"{array_type.name}DataType"
            self._create_array_data_type_class(
                name, len(array_type.dims),
                self.get_type(array_type.scalar_type))

            my_datatype_class = self.get_type(name)
            name = f"{array_type.name}DataSymbol"
            self._create_array_data_symbol_class(name, my_datatype_class,
                                                 array_type.properties)

    def _create_array_data_type_class(self, name, num_dims, scalar_type):
        '''This function create a data type class for the specified field.

        :param str name: name of the class to create.
        :param int num_dims: number of dimensions
        :param scalar_type: the scalar base type for this field.
        :type scalar_type: :py:class:`psyclone.psyir.datatypes.DataType`

        '''
        # ---------------------------------------------------------------------
        # This is the constructor for the newly declared classes, which
        # verifies that the dimension argument has the expected number of
        # elements.
        def __my_type_init__(self, dims):
            '''
            Constructor for the array data type class.

            :param list dims: the shape argument for the ArrayType constructor.

            '''
            if len(dims) != self.num_dims:
                raise TypeError(f"'{type(self).__name__}' expected the number "
                                f"of supplied dimensions to be {self.num_dims}"
                                f" but found {len(dims)}.")
            ArrayType.__init__(self, self.scalar_class(), dims)

        # ---------------------------------------------------------------------
        # Create the class, and set the constructor. Store the scalar_type
        # and num_dims as attributes, so they are indeed different for all
        # the various classes created here.
        self._type_name_to_class[name] = \
            type(name, (ArrayType, ),
                 {"__init__": __my_type_init__,
                  "scalar_class": scalar_type,
                  "num_dims": num_dims})

    def _create_array_data_symbol_class(self, name, datatype_class,
                                        parameters):
        '''This function creates an array-data-symbol-class and adds it to
        the internal type dictionary.

        :param str name: the name of the class to be created.
        :param datatype_class: the corresponding data type class.
        :type datatype_class: :py:class:`psyclone.psyir.datatypes.DataType`

        :param parameters: the list of additional required properties \
            to be passed to the constructor.
        :type parameters: List[str]

        '''
        # ---------------------------------------------------------------------
        def __my_symbol_init__(self, name, dims, *args, **kwargs):
            '''This is the __init__ function for the newly declared array data
            type classes. It sets the required arguments automatically as
            attributes of the class.

            :param str name: the name of the data symbol to create.
            :param list dims: the shape argument for the ArrayType constructor.
            :param *args: other required positional parameters.
            :param **kwargs: other required keyword parameters.
            '''
            # Set all the positional arguments as attributes:
            for i, arg in enumerate(args):
                setattr(self, self.parameters[i], arg)
            # Now handle the keyword arguments: any keyword arguments
            # that are declared as parameter will be set as attribute,
            # anything else will be passed to the constructor of the
            # base class.
            remaining_kwargs = {}
            for key, value in kwargs.items():
                # It is one of the additional parameters, set it as
                # attribute:
                if key in self.parameters:
                    setattr(self, key, value)
                else:
                    # Otherwise add it as keyword parameter for the
                    # base class constructor
                    remaining_kwargs[key] = value
            DataSymbol.__init__(self, name, self.datatype_class(dims),
                                **remaining_kwargs)
        # ----------------------------------------------------------------

        # Now create the actual class. We need to keep a copy of the parameters
        # of this class as attributes, otherwise they would be shared among the
        # several instances of the __my_symbol_init__function: this affects the
        # required arguments (array_type.properties) and scalar class:
        self._type_name_to_class[name] = \
            type(name, (DataSymbol, ),
                 {"__init__": __my_symbol_init__,
                  "datatype_class": datatype_class,
                  "parameters": parameters})
