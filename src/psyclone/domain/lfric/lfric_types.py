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
# Author J. Henrichs, Bureau of Meteorology

'''This module contains a singleton class that manages LFRic types. '''


from collections import namedtuple

from psyclone.domain.lfric import LFRicConstants
from psyclone.psyir.nodes import Literal
from psyclone.psyir.symbols import (ArrayType, ContainerSymbol, DataSymbol,
                                    ImportInterface, INTEGER_TYPE, ScalarType)


class LFRicTypes:
    '''This class implements a singleton that manages LFRic types.
    Using the 'call' interface, you can query the data type for
    LFRic types, e.g.:

    >>> from psyclone.domain.lfric import LFRicTypes
    >>> lfric_types = LFRicTypes()
    >>> num_dofs_class = lfric_types("NumberOfUniqueDofsDataSymbol")
    >>> my_var = num_dofs_class("my_num_dofs")
    >>> print(my_var.name)
    my_num_dofs

    '''

    # Class variable to store the singleton instance
    _instance = None

    # ------------------------------------------------------------------------
    def __new__(cls):
        '''Implement a singleton - only one instance will ever be created.

        :returns: the singleton instance of LFRicTypes.
        :rtype: :py:class:`psyclone.domain.lfric.LFRicTypes`

        '''
        if LFRicTypes._instance is None:
            # Return a new instance. The constructor will set _instance
            # in this case
            return super().__new__(cls)

        # Return the existing instance, in which case the constructor will
        # not re-initialise the internal data structures
        return LFRicTypes._instance

    # ------------------------------------------------------------------------
    def __init__(self):

        # Test if this is returning the existing instance, if so, skip
        # initialisation
        if self == LFRicTypes._instance:
            return

        # First time __init__ is called, initialise all data structures.
        LFRicTypes._instance = self

        # The global mapping of names to the corresponding classes or instances
        self._name_to_class = {}

        self._create_precision_from_const_module()
        self._create_generic_scalars()
        self._create_lfric_dimension()
        self._create_specific_scalars()
        self._create_fields()
        # Generate LFRic vector-field-data symbols as subclasses of
        # field-data symbols
        for intrinsic in ["Real", "Integer", "Logical"]:
            # TODO #2050: we end up with DataData
            name = f"{intrinsic}VectorFieldDataDataSymbol"
            baseclass = self(f"{intrinsic}FieldDataDataSymbol")
            self._name_to_class[name] = type(name, (baseclass, ), {})

    # ------------------------------------------------------------------------
    def __call__(self, name):
        ''':returns: the class of the required type.
        :rtype:  Class instance

        '''
        return self._name_to_class[name]

    # ------------------------------------------------------------------------
    def _create_precision_from_const_module(self):
        '''This function implements all precisions defined in
        ``LFRicConstants.PRECISION_MAP``. It adds "constants_mod" as
        ContainerSymbol. The names are added to the global mapping.

        '''
        # The first Module namedtuple argument specifies the name of the
        # module and the second argument declares the name(s) of any symbols
        # declared by the module.
        lfric_const = LFRicConstants()
        lfric_kinds = list(lfric_const.PRECISION_MAP.keys())
        constants_mod = lfric_const.UTILITIES_MOD_MAP["constants"]["module"]
        Module = namedtuple('Module', ["name", "vars"])
        modules = [Module(constants_mod, lfric_kinds)]

        # Generate LFRic module symbols from definitions
        for module_info in modules:
            module_name = module_info.name.lower()
            # Create the module (using a PSyIR ContainerSymbol)
            self._name_to_class[module_name] = \
                ContainerSymbol(module_info.name)
            # Create the variables specified by the module (using
            # PSyIR DataSymbols)
            for module_var in module_info.vars:
                var_name = module_var.upper()
                self._name_to_class[var_name] = \
                    DataSymbol(module_var, INTEGER_TYPE,
                               interface=ImportInterface(self(module_name)))

    # ------------------------------------------------------------------------
    def _create_generic_scalars(self):
        '''This function adds the generic data types and symbols for
        integer, real, and booleans to the global mapping.

        '''
        GenericScalar = namedtuple('GenericScalar', ["name", "intrinsic",
                                                     "precision"])
        generic_scalar_datatypes = [
            GenericScalar("LfricIntegerScalar", ScalarType.Intrinsic.INTEGER,
                          self("I_DEF")),
            GenericScalar("LfricRealScalar", ScalarType.Intrinsic.REAL,
                          self("R_DEF")),
            GenericScalar("LfricLogicalScalar", ScalarType.Intrinsic.BOOLEAN,
                          self("L_DEF"))]

        # Generate generic LFRic scalar datatypes and symbols from definitions
        for info in generic_scalar_datatypes:

            # Create the generic data
            type_name = f"{info.name}DataType"
            self._create_generic_scalar_data_type(type_name,
                                                  info.intrinsic,
                                                  info.precision)
            type_class = self(type_name)
            # Create the generic data symbol
            symbol_name = f"{info.name}DataSymbol"
            self._create_generic_scalar_data_symbol(symbol_name, type_class)

    # ------------------------------------------------------------------------
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
        self._name_to_class[name] = \
            type(name, (ScalarType, ),
                 {"__init__": __my_generic_scalar_type_init__,
                  "intrinsic": intrinsic,
                  "default_precision": default_precision})

    # ------------------------------------------------------------------------
    def _create_generic_scalar_data_symbol(self, name, type_class):
        '''This function creates a data symbol class with the specified name
        and data type, and adds it to the global mapping.

        :param str name: the name of the class to creates
        :param type_class: the data type for the symbol
        :type type_class: py:class:`pyclone.psyir.datatypes.ScalarType`

        '''
        # THis is the constructor for the dynamically created class:
        def __my_generic_scalar_symbol_init__(self, name, precision=None,
                                              **kwargs):
            DataSymbol.__init__(self, name,
                                self.type_class(precision=precision),
                                **kwargs)

        # ---------------------------------------------------------------------
        # Create the class, set the constructor and store the ScalarType as
        # an attribute, so it can be accessed in the constructor.
        self._name_to_class[name] = \
            type(name, (DataSymbol, ),
                 {"__init__": __my_generic_scalar_symbol_init__,
                  "type_class": type_class})

    # ------------------------------------------------------------------------
    def _create_lfric_dimension(self):
        '''This function adds the LfricDimension class to the global mapping,
        and creates the two instances for scalar and vector dimension.

        '''
        # The actual class:
        class LfricDimension(Literal):
            '''An LFRic-specific scalar integer that captures a literal array
            dimension which can either have the value 1 or 3. This is used for
            one of the dimensions in basis and differential basis
            functions.

            :param str value: the value of the scalar integer.

            :raises ValueError: if the supplied value is not '1 or '3'.

            '''
            # pylint: disable=undefined-variable
            def __init__(self, value):
                super().__init__(value,
                                 LFRicTypes()("LfricIntegerScalarDataType")())
                if value not in ['1', '3']:
                    raise ValueError(f"An LFRic dimension object must be '1' "
                                     f"or '3', but found '{value}'.")
        # --------------------------------------------------------------------

        # Create the required entries in the dictionary
        self._name_to_class["LfricDimension"] = LfricDimension
        self._name_to_class["LFRIC_SCALAR_DIMENSION"] = LfricDimension("1")
        self._name_to_class["LFRIC_VECTOR_DIMENSION"] = LfricDimension("3")

    # ------------------------------------------------------------------------
    def _create_specific_scalars(self):
        '''This function creates all required specific scalar, which are
        derived from the corresponding generic classes (e.g.
        LfricIntegerScalarData)

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
            Scalar("CellPosition", "LfricIntegerScalarData", []),
            Scalar("MeshHeight", "LfricIntegerScalarData", []),
            Scalar("NumberOfCells", "LfricIntegerScalarData", []),
            Scalar("NumberOfDofs", "LfricIntegerScalarData", ["fs"]),
            Scalar("NumberOfUniqueDofs", "LfricIntegerScalarData", ["fs"]),
            Scalar("NumberOfFaces", "LfricIntegerScalarData", []),
            Scalar("NumberOfEdges", "LfricIntegerScalarData", []),
            Scalar("NumberOfQrPointsInXy", "LfricIntegerScalarData", []),
            Scalar("NumberOfQrPointsInZ", "LfricIntegerScalarData", []),
            Scalar("NumberOfQrPointsInFaces", "LfricIntegerScalarData", []),
            Scalar("NumberOfQrPointsInEdges", "LfricIntegerScalarData", [])]

        for info in specific_scalar_datatypes:
            # TODO #2050: we end up with DataDataType
            type_name = f"{info.name}DataType"
            self._name_to_class[type_name] = \
                type(type_name, (self(f"{info.generic_type_name}Type"), ), {})

            # TODO #2050: we end up with DataDataSymbol
            symbol_name = f"{info.name}DataSymbol"
            base_class = self(f"{info.generic_type_name}Symbol")
            self._create_scalar_data_type(symbol_name, base_class,
                                          info.properties)

    # ------------------------------------------------------------------------
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
        self._name_to_class[class_name] = \
            type(class_name, (base_class, ),
                 {"__init__": __my_scalar_init__,
                  "base_class": base_class,
                  "parameters": parameters})

    # ------------------------------------------------------------------------
    def _create_fields(self):
        '''This function creates the data symbol and types for LFRic fields.

        '''
        # Note, field_datatypes are no different to array_datatypes and are
        # treated in the same way. They are only separated into a different
        # list because they are used to create vector field datatypes and
        # symbols.

        # The Array namedtuple has 4 properties: the first determines the
        # names of the resultant datatype and datasymbol classes, the second
        # references the generic scalar type classes declared above, the third
        # property is a textual description of each of the dimensions.
        # The fourth specifies any additional class properties that should be
        # declared in the generated datasymbol class.
        Array = namedtuple('Array',
                           ["name", "scalar_type", "dims", "properties"])
        field_datatypes = [
            Array("RealFieldData", "LfricRealScalarDataType",
                  ["number of unique dofs"], ["fs"]),
            Array("IntegerFieldData", "LfricIntegerScalarDataType",
                  ["number of unique dofs"], ["fs"]),
            Array("LogicalFieldData", "LfricLogicalScalarDataType",
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
            Array("Operator", "LfricRealScalarDataType",
                  ["number of dofs", "number of dofs", "number of cells"],
                  ["fs_from", "fs_to"]),
            Array("DofMap", "LfricIntegerScalarDataType",
                  ["number of dofs"], ["fs"]),
            Array("BasisFunctionQrXyoz", "LfricRealScalarDataType",
                  [self("LfricDimension"), "number of dofs",
                   "number of qr points in xy",
                   "number of qr points in z"], ["fs"]),
            Array("BasisFunctionQrFace", "LfricRealScalarDataType",
                  [self("LfricDimension"), "number of dofs",
                   "number of qr points in faces",
                   "number of faces"], ["fs"]),
            Array("BasisFunctionQrEdge", "LfricRealScalarDataType",
                  [self("LfricDimension"), "number of dofs",
                   "number of qr points in edges",
                   "number of edges"], ["fs"]),
            Array("DiffBasisFunctionQrXyoz", "LfricRealScalarDataType",
                  [self("LfricDimension"), "number of dofs",
                   "number of qr points in xy",
                   "number of qr points in z"], ["fs"]),
            Array("DiffBasisFunctionQrFace", "LfricRealScalarDataType",
                  [self("LfricDimension"), "number of dofs",
                   "number of qr points in faces",
                   "number of faces"], ["fs"]),
            Array("DiffBasisFunctionQrEdge", "LfricRealScalarDataType",
                  [self("LfricDimension"), "number of dofs",
                   "number of qr points in edges", "number of edges"], ["fs"]),
            Array("QrWeightsInXy", "LfricRealScalarDataType",
                  ["number of qr points in xy"], []),
            Array("QrWeightsInZ", "LfricRealScalarDataType",
                  ["number of qr points in z"], []),
            Array("QrWeightsInFaces", "LfricRealScalarDataType",
                  ["number of qr points in faces"], []),
            Array("QrWeightsInEdges", "LfricRealScalarDataType",
                  ["number of qr points in edges"], [])
            ]

        for array_type in array_datatypes + field_datatypes:
            name = f"{array_type.name}DataType"
            self._create_array_data_type_class(name, len(array_type.dims),
                                               self(array_type.scalar_type))

            my_datatype_class = self(name)
            name = f"{array_type.name}DataSymbol"
            self._create_array_data_symbol_class(name, my_datatype_class,
                                                 array_type.properties)

    # ------------------------------------------------------------------------
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
        self._name_to_class[name] = \
            type(name, (ArrayType, ),
                 {"__init__": __my_type_init__,
                  "scalar_class": scalar_type,
                  "num_dims": num_dims})

    # ------------------------------------------------------------------------
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
        self._name_to_class[name] = \
            type(name, (DataSymbol, ),
                 {"__init__": __my_symbol_init__,
                  "datatype_class": datatype_class,
                  "parameters": parameters})
