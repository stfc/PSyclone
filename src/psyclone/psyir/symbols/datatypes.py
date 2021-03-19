# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2021, Science and Technology Facilities Council.
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
# -----------------------------------------------------------------------------

''' This module contains the datatype definitions.'''

from __future__ import absolute_import
import abc
from collections import OrderedDict, namedtuple
from enum import Enum
import six
from psyclone.errors import InternalError
from psyclone.psyir.symbols import TypeSymbol, DataSymbol
from psyclone.psyir.symbols.symbol import Symbol


@six.add_metaclass(abc.ABCMeta)
class DataType(object):
    '''Abstract base class from which all types are derived.'''

    @abc.abstractmethod
    def __str__(self):
        '''
        :returns: a description of this type.
        :rtype: str

        '''


class DeferredType(DataType):
    '''Indicates that the type is unknown at this point.'''

    def __str__(self):
        '''
        :returns: a description of this deferred type.
        :rtype: str

        '''
        return "DeferredType"


@six.add_metaclass(abc.ABCMeta)
class UnknownType(DataType):
    '''
    Indicates that a variable declaration is not supported by the PSyIR.
    This class is abstract and must be subclassed for each language
    supported by the PSyIR frontends.

    :param str declaration_txt: string containing the original variable \
                                declaration.

    :raises TypeError: if the supplied declaration_txt is not a str.

    '''
    def __init__(self, declaration_txt):
        if not isinstance(declaration_txt, str):
            raise TypeError(
                "UnknownType constructor expects the original variable "
                "declaration as a string but got an argument of type '{0}'".
                format(type(declaration_txt).__name__))
        self._declaration = None
        self.declaration = declaration_txt

    @abc.abstractmethod
    def __str__(self):
        ''' Abstract method that must be implemented in subclass. '''

    @property
    def declaration(self):
        '''
        :returns: the original declaration of the symbol. This is obviously \
                  language specific and hence this class must be subclassed.
        :rtype: str
        '''
        return self._declaration

    @declaration.setter
    def declaration(self, value):
        '''
        Sets the original declaration that this instance represents.

        :param str value: the original declaration.

        '''
        self._declaration = value[:]


class UnknownFortranType(UnknownType):
    '''
    Indicates that a Fortran declaration is not supported by the PSyIR.

    :param str declaration_txt: string containing the original variable \
                                declaration.

    :raises TypeError: if the supplied declaration_txt is not a str.

    '''
    def __str__(self):
        return "UnknownFortranType('{0}')".format(self._declaration)


class ScalarType(DataType):
    '''Describes a scalar datatype (and its precision).

    :param intrinsic: the intrinsic of this scalar type.
    :type intrinsic: :py:class:`pyclone.psyir.datatypes.ScalarType.Intrinsic`
    :param precision: the precision of this scalar type.
    :type precision: :py:class:`psyclone.psyir.symbols.ScalarType.Precision`, \
        int or :py:class:`psyclone.psyir.symbols.DataSymbol`

    :raises TypeError: if any of the arguments are of the wrong type.
    :raises ValueError: if any of the argument have unexpected values.

    '''

    class Intrinsic(Enum):
        '''Enumeration of the different intrinsic scalar datatypes that are
        supported by the PSyIR.

        '''
        INTEGER = 1
        REAL = 2
        BOOLEAN = 3
        CHARACTER = 4

    class Precision(Enum):
        '''Enumeration of the different types of 'default' precision that may
        be specified for a scalar datatype.

        '''
        SINGLE = 1
        DOUBLE = 2
        UNDEFINED = 3

    def __init__(self, intrinsic, precision):
        if not isinstance(intrinsic, ScalarType.Intrinsic):
            raise TypeError(
                "ScalarType expected 'intrinsic' argument to be of type "
                "ScalarType.Intrinsic but found '{0}'."
                "".format(type(intrinsic).__name__))
        if not isinstance(precision, (int, ScalarType.Precision, DataSymbol)):
            raise TypeError(
                "ScalarType expected 'precision' argument to be of type "
                "int, ScalarType.Precision or DataSymbol, but found '{0}'."
                "".format(type(precision).__name__))
        if isinstance(precision, int) and precision <= 0:
            raise ValueError(
                "The precision of a DataSymbol when specified as an "
                "integer number of bytes must be > 0 but found '{0}'."
                "".format(precision))
        if (isinstance(precision, DataSymbol) and
                not (isinstance(precision.datatype, ScalarType) and
                     precision.datatype.intrinsic ==
                     ScalarType.Intrinsic.INTEGER) and
                not isinstance(precision.datatype, DeferredType)):
            raise ValueError(
                "A DataSymbol representing the precision of another "
                "DataSymbol must be of either 'deferred' or scalar, "
                "integer type but got: {0}".format(str(precision)))

        self._intrinsic = intrinsic
        self._precision = precision

    @property
    def intrinsic(self):
        '''
        :returns: the intrinsic used by this scalar type.
        :rtype: :py:class:`pyclone.psyir.datatypes.ScalarType.Intrinsic`
        '''
        return self._intrinsic

    @property
    def precision(self):
        '''
        :returns: the precision of this scalar type.
        :rtype: :py:class:`psyclone.psyir.symbols.ScalarType.Precision`, \
            int or :py:class:`psyclone.psyir.symbols.DataSymbol`
        '''
        return self._precision

    def __str__(self):
        '''
        :returns: a description of this scalar datatype.
        :rtype: str

        '''
        if isinstance(self.precision, ScalarType.Precision):
            precision_str = self.precision.name
        else:
            precision_str = str(self.precision)
        return "Scalar<{0}, {1}>".format(self.intrinsic.name, precision_str)


class ArrayType(DataType):
    '''Describes an array datatype. Can be an array of intrinsic types (e.g.
    integer) or of structure types. For the latter, the type must currently be
    specified as a TypeSymbol (see #1031).

    :param datatype: the datatype of the array elements.
    :type datatype: :py:class:`psyclone.psyir.datatypes.DataType` or \
        :py:class:`psyclone.psyir.symbols.TypeSymbol`
    :param list shape: shape of the symbol in column-major order \
        (leftmost index is contiguous in memory). Each entry \
        represents an array dimension. If it is \
        ArrayType.Extent.ATTRIBUTE the extent of that dimension is \
        unknown but can be obtained by querying the run-time system \
        (e.g. using the SIZE intrinsic in Fortran). If it is \
        ArrayType.Extent.DEFERRED then the extent is also unknown and \
        may or may not be defined at run-time (e.g. the array is \
        ALLOCATABLE in Fortran). Otherwise it can be an int or a \
        DataNode (that returns an int). Note that providing an int is \
        supported as a convenience, the provided value will be stored \
        internally as a Literal node.

    :raises TypeError: if the arguments are of the wrong type.
    :raises NotImplementedError: if a structure type does not have a \
                                 TypeSymbol as its type.
    '''
    class Extent(Enum):
        '''
        Enumeration of array shape extents that are unspecified at compile
        time. When the extent must exist and is accessible via the run-time
        system it is an 'ATTRIBUTE'. When it may or may not be defined in the
        current scope (e.g. the array may need to be allocated/malloc'd) it
        is 'DEFERRED'.

        '''
        DEFERRED = 1
        ATTRIBUTE = 2

    def __init__(self, datatype, shape):

        # This import must be placed here to avoid circular
        # dependencies.
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes import Literal

        if isinstance(datatype, DataType):
            if isinstance(datatype, StructureType):
                # TODO #1031 remove this restriction.
                raise NotImplementedError(
                    "When creating an array of structures, the type of "
                    "those structures must be supplied as a TypeSymbol but "
                    "got a StructureType instead.")
            self._intrinsic = datatype.intrinsic
            self._precision = datatype.precision
        elif isinstance(datatype, TypeSymbol):
            self._intrinsic = datatype
            self._precision = None
        else:
            raise TypeError(
                "ArrayType expected 'datatype' argument to be of type "
                "DataType or TypeSymbol but found '{0}'."
                "".format(type(datatype).__name__))
        # We do not have a setter for shape as it is an immutable property,
        # therefore we have a separate validation routine.
        self._validate_shape(shape)
        # Replace any ints in shape with a Literal. int's are only
        # supported as they allow a more concise dimension
        # declaration.
        self._shape = [Literal(str(dim), INTEGER_TYPE)
                       if isinstance(dim, int) else dim for dim in shape]
        self._datatype = datatype

    @property
    def intrinsic(self):
        '''
        :returns: the intrinsic type of each element in the array.
        :rtype: :py:class:`pyclone.psyir.datatypes.ScalarType.Intrinsic` or \
                :py:class:`psyclone.psyir.symbols.DataSymbol`
        '''
        return self._intrinsic

    @property
    def precision(self):
        '''
        :returns: the precision of each element in the array.
        :rtype: :py:class:`psyclone.psyir.symbols.ScalarType.Precision`, \
            int or :py:class:`psyclone.psyir.symbols.DataSymbol`
        '''
        return self._precision

    @property
    def shape(self):
        '''
        :returns: the shape of the symbol in column-major order \
            (leftmost index is contiguous in memory) with each entry \
            representing an array dimension.

        :rtype: a list of ArrayType.Extent.ATTRIBUTE, \
            ArrayType.Extent.DEFERRED, or \
            :py:class:`psyclone.psyir.nodes.DataNode`. If an entry is \
            ArrayType.Extent.ATTRIBUTE the extent of that dimension \
            is unknown but can be obtained by querying the run-time \
            system (e.g. using the SIZE intrinsic in Fortran). If it \
            is ArrayType.Extent.DEFERRED then the extent is also \
            unknown and may or may not be defined at run-time \
            (e.g. the array is ALLOCATABLE in Fortran). Otherwise an \
            entry is a DataNode that returns an int.

        '''
        return self._shape

    def _validate_shape(self, extents):
        '''Check that the supplied shape specification is valid. This is not
        implemented as a setter because the shape property is immutable.

        :param extents: list of extents, one for each array dimension.
        :type extents: list of \
            :py:class:`psyclone.psyir.symbols.ArrayType.Extent`, int \
            or subclass of :py:class:`psyclone.psyir.nodes.DataNode`

        :raises TypeError: if extents is not a list.
        :raises TypeError: if one or more of the supplied extents is a \
            DataSymbol that is not a scalar integer or of Unknown/DeferredType.
        :raises TypeError: if one or more of the supplied extents is not an \
            int, ArrayType.Extent or DataNode.

        '''
        # This import must be placed here to avoid circular
        # dependencies.
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes import DataNode, Reference
        if not isinstance(extents, list):
            raise TypeError(
                "ArrayType 'shape' must be of type list but found '{0}'."
                "".format(type(extents).__name__))

        for dimension in extents:
            if isinstance(dimension, DataNode):
                # When issue #685 is addressed then check that the
                # datatype returned is an int (or is unknown). For the
                # moment, just check that if the DataNode is a
                # Reference then the associated symbol is a scalar
                # integer or is unknown.
                if isinstance(dimension, Reference):
                    # Check the DataSymbol instance is a scalar
                    # integer or is unknown
                    symbol = dimension.symbol
                    if not ((symbol.is_scalar and symbol.datatype.intrinsic ==
                             ScalarType.Intrinsic.INTEGER) or
                            isinstance(symbol.datatype,
                                       (UnknownFortranType, DeferredType))):
                        raise TypeError(
                            "If a datasymbol is used as a dimension "
                            "declaration then it should be a scalar "
                            "integer or an unknown type, but '{0}' is a "
                            "'{1}'.".format(symbol.name, symbol.datatype))
                # TODO #1089 - add check that any References are not to a
                # local datasymbol that is not constant (as this would have
                # no value).
            elif not isinstance(dimension, (self.Extent, int)):
                raise TypeError(
                    "DataSymbol shape list elements can only be "
                    "'int', ArrayType.Extent or 'DataNode' but found "
                    "'{0}'.".format(type(dimension).__name__))

    def __str__(self):
        '''
        :returns: a description of this array datatype.
        :rtype: str

        :raises InternalError: if an unsupported dimensions type is \
            found.

        '''
        # This import must be placed here to avoid circular
        # dependencies.
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes import DataNode
        dims = []
        for dimension in self.shape:
            if isinstance(dimension, DataNode):
                dims.append(str(dimension))
            elif isinstance(dimension, ArrayType.Extent):
                dims.append("'{0}'".format(dimension.name))
            else:
                raise InternalError(
                    "ArrayType shape list elements can only be 'DataNode', "
                    "or 'ArrayType.Extent', but found '{0}'."
                    "".format(type(dimension).__name__))
        return ("Array<{0}, shape=[{1}]>".format(
            self._datatype, ", ".join(dims)))


class StructureType(DataType):
    '''
    Describes a 'structure' or 'derived' datatype that is itself composed
    of a list of other datatypes. Those datatypes are stored as an
    OrderedDict of namedtuples.

    Note, we could have chosen to use a SymbolTable to store the properties
    of the constituents of the type. (Since they too have a name, a type,
    and visibility.) If this class ends up duplicating a lot of the
    SymbolTable functionality then this decision could be revisited.

    '''
    # Each member of a StructureType is represented by a ComponentType
    # (named tuple).
    ComponentType = namedtuple("ComponentType", ["name", "datatype",
                                                 "visibility"])

    def __init__(self):
        self._components = OrderedDict()

    def __str__(self):
        return "StructureType<>"

    @staticmethod
    def create(components):
        '''
        Creates a StructureType from the supplied list of properties.

        :param components: the name, type and visibility of each component.
        :type components: list of 3-tuples

        :returns: the new type object.
        :rtype: :py:class:`psyclone.psyir.symbols.StructureType`

        '''
        stype = StructureType()
        for component in components:
            if len(component) != 3:
                raise TypeError(
                    "Each component must be specified using a 3-tuple of "
                    "(name, type, visibility) but found a tuple with {0} "
                    "members: {1}".format(
                        len(component), str(component)))
            stype.add(component[0], component[1], component[2])
        return stype

    @property
    def components(self):
        '''
        :returns: Ordered dictionary of the components of this type.
        :rtype: :py:class:`collections.OrderedDict`
        '''
        return self._components

    def add(self, name, datatype, visibility):
        '''
        Create a component with the supplied attributes and add it to
        this StructureType.

        :param str name: the name of the new component.
        :param datatype: the type of the new component.
        :type datatype: :py:class:`psyclone.psyir.symbols.DataType` or \
                        :py:class:`psyclone.psyir.symbols.TypeSymbol`
        :param visibility: whether this component is public or private.
        :type visibility: :py:class:`psyclone.psyir.symbols.Symbol.Visibility`

        :raises TypeError: if any of the supplied values are of the wrong type.

        '''
        if not isinstance(name, str):
            raise TypeError(
                "The name of a component of a StructureType must be a 'str' "
                "but got '{0}'".format(type(name).__name__))
        if not isinstance(datatype, (DataType, TypeSymbol)):
            raise TypeError(
                "The type of a component of a StructureType must be a "
                "'DataType' or 'TypeSymbol' but got '{0}'".format(
                    type(datatype).__name__))
        if not isinstance(visibility, Symbol.Visibility):
            raise TypeError(
                "The visibility of a component of a StructureType must be "
                "an instance of 'Symbol.Visibility' but got '{0}'".format(
                    type(visibility).__name__))
        if datatype is self:
            # A StructureType cannot contain a component of its own type
            raise TypeError(
                "Error attempting to add component '{0}' - a StructureType "
                "definition cannot be recursive - i.e. it cannot contain "
                "components with the same type as itself.".format(name))

        self._components[name] = self.ComponentType(name, datatype, visibility)

    def lookup(self, name):
        '''
        :returns: the ComponentType tuple describing the named member of this \
                  StructureType.
        :rtype: :py:class:`psyclone.psyir.symbols.StructureType.ComponentType`
        '''
        return self._components[name]


# Create common scalar datatypes
REAL_TYPE = ScalarType(ScalarType.Intrinsic.REAL,
                       ScalarType.Precision.UNDEFINED)
REAL_SINGLE_TYPE = ScalarType(ScalarType.Intrinsic.REAL,
                              ScalarType.Precision.SINGLE)
REAL_DOUBLE_TYPE = ScalarType(ScalarType.Intrinsic.REAL,
                              ScalarType.Precision.DOUBLE)
REAL4_TYPE = ScalarType(ScalarType.Intrinsic.REAL, 4)
REAL8_TYPE = ScalarType(ScalarType.Intrinsic.REAL, 8)
INTEGER_TYPE = ScalarType(ScalarType.Intrinsic.INTEGER,
                          ScalarType.Precision.UNDEFINED)
INTEGER_SINGLE_TYPE = ScalarType(ScalarType.Intrinsic.INTEGER,
                                 ScalarType.Precision.SINGLE)
INTEGER_DOUBLE_TYPE = ScalarType(ScalarType.Intrinsic.INTEGER,
                                 ScalarType.Precision.DOUBLE)
INTEGER4_TYPE = ScalarType(ScalarType.Intrinsic.INTEGER, 4)
INTEGER8_TYPE = ScalarType(ScalarType.Intrinsic.INTEGER, 8)
BOOLEAN_TYPE = ScalarType(ScalarType.Intrinsic.BOOLEAN,
                          ScalarType.Precision.UNDEFINED)
CHARACTER_TYPE = ScalarType(ScalarType.Intrinsic.CHARACTER,
                            ScalarType.Precision.UNDEFINED)

# Mapping from PSyIR scalar data types to intrinsic Python types
# ignoring precision.
TYPE_MAP_TO_PYTHON = {ScalarType.Intrinsic.INTEGER: int,
                      ScalarType.Intrinsic.CHARACTER: str,
                      ScalarType.Intrinsic.BOOLEAN: bool,
                      ScalarType.Intrinsic.REAL: float}


# For automatic documentation generation
__all__ = ["UnknownType", "UnknownFortranType", "DeferredType", "ScalarType",
           "ArrayType", "StructureType"]
