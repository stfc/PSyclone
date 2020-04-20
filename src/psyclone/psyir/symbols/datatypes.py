# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council.
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
import six
from enum import Enum
from psyclone.errors import InternalError


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
        from psyclone.psyir.symbols.datasymbol import DataSymbol
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
    '''Describes an array datatype.

    :param datatype: the datatype of the array elements.
    :type datatype: :py:class:`psyclone.psyir.datatypes.DataType`
    :param list shape: shape of the symbol in column-major order (leftmost \
        index is contiguous in memory). Each entry represents an array \
        dimension. If it is DataSymbol.Extent.ATTRIBUTE the extent of that \
        dimension is unknown but can be obtained by querying the run-time \
        system (e.g. using the SIZE intrinsic in Fortran). If it is \
        DataSymbol.Extent.DEFERRED then the extent is also unknown and may or \
        may not be defined at run-time (e.g. the array is ALLOCATABLE in \
        Fortran). Otherwise it holds an integer literal or a reference to an \
        integer symbol with the extent.

    :raises TypeError: if the arguments are of the wrong type.

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

        from psyclone.psyir.symbols.datasymbol import DataSymbol
        if not isinstance(datatype, DataType):
            raise TypeError(
                "ArrayType expected 'datatype' argument to be of type "
                "DataType but found '{0}'."
                "".format(type(datatype).__name__))
        if not isinstance(shape, list):
            raise TypeError(
                "ArrayType expected 'shape' argument to be of type "
                "list but found '{0}'."
                "".format(type(shape).__name__))

        for dimension in shape:
            if isinstance(dimension, DataSymbol):
                if not (dimension.is_scalar and
                        dimension.datatype.intrinsic ==
                        ScalarType.Intrinsic.INTEGER):
                    raise TypeError(
                        "DataSymbols that are part of another symbol shape can"
                        " only be scalar integers, but found '{0}'."
                        "".format(str(dimension)))
            elif not isinstance(dimension, (self.Extent, int)):
                raise TypeError(
                    "DataSymbol shape list elements can only be "
                    "'DataSymbol', 'integer' or ArrayType.Extent, but "
                    "found '{0}'.".format(type(dimension).__name__))

        self._shape = shape
        self._intrinsic = datatype.intrinsic
        self._precision = datatype.precision
        self._datatype = datatype

    @property
    def intrinsic(self):
        '''
        :returns: the intrinsic of each element in the array
        :rtype: :py:class:`pyclone.psyir.datatypes.ScalarType.Intrinsic`
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
        :rtype: a list of DataSymbol.Extent.ATTRIBUTE, \
            DataSymbol.Extent.DEFERRED, Literal or Reference. If an \
            entry is DataSymbol.Extent.ATTRIBUTE the extent of that \
            dimension is unknown but can be obtained by querying the \
            run-time system (e.g. using the SIZE intrinsic in \
            Fortran). If it is DataSymbol.Extent.DEFERRED then the \
            extent is also unknown and may or may not be defined at \
            run-time (e.g. the array is ALLOCATABLE in \
            Fortran). Otherwise an entry is an integer literal or a \
            reference to an integer symbol with the extent.
        :returns: the shape of the array.
        :rtype: :py:class:`psyclone.psyir.symbols.ScalarType.Precision`, \
            int or :py:class:`psyclone.psyir.symbols.DataSymbol`
        '''
        return self._shape

    def __str__(self):
        '''
        :returns: a description of this array datatype.
        :rtype: str

        :raises InternalError: if an unsupported dimensions type is \
            found.

        '''
        from psyclone.psyir.symbols import DataSymbol
        dims = []
        for dimension in self.shape:
            if isinstance(dimension, DataSymbol):
                dims.append(dimension.name)
            elif isinstance(dimension, int):
                dims.append(str(dimension))
            elif isinstance(dimension, ArrayType.Extent):
                dims.append("'{0}'".format(dimension.name))
            else:
                raise InternalError(
                    "ArrayType shape list elements can only be 'DataSymbol', "
                    "'int' or 'ArrayType.Extent', but found '{0}'."
                    "".format(type(dimension).__name__))
        return ("Array<{0}, shape=[{1}]>".format(
            self._datatype, ", ".join(dims)))


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
