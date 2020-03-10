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

from enum import Enum


#class DataType(Enum):
#    '''
#    Enumeration of the different datatypes that are supported by the
#    PSyIR.
#    '''
#    INTEGER = 1
#    REAL = 2
#    BOOLEAN = 3
#    CHARACTER = 4
#    DEFERRED = 5


# Mapping from PSyIR data types to intrinsic Python types
#TYPE_MAP_TO_PYTHON = {DataType.INTEGER: int,
#                      DataType.CHARACTER: str,
#                      DataType.BOOLEAN: bool,
#                      DataType.REAL: float}


class DataType():
    '''Base class from which all types are derived.'''


class DeferredType(DataType):
    '''Indicates that the type is unknown at this point.'''


class ScalarType(DataType):
    '''Describes a scalar datatype (and its precision).

    :param type_name: the name of this scalar type.
    :type type_name: :py:class:`pyclone.psyir.datatypes.ScalarType.Name`
    :param precision: the precision of this scalar type.
    :type precision: :py:class:`psyclone.psyir.datatypes.ScalarType.Precision`

    :raises TypeError: if the arguments are of the wrong type.

    '''

    class Name(Enum):
        '''Enumeration of the different scalar datatypes that are supported
        by the PSyIR.

        '''
        INTEGER = 1
        REAL = 2
        BOOLEAN = 3
        CHARACTER = 4

    class Precision(Enum):
        '''Enumeration of the different types of 'default' precision that may
        be specified for a Symbol.

        '''
        SINGLE = 1
        DOUBLE = 2

    def __init__(self, type_name, precision):
        
        if not isinstance(type_name, ScalarType.Name):
            raise TypeError(
                "ScalarType expected 'type_name' argument to be of type "
                "ScalarType.Name but found '{0}'."
                "".format(type(type_name).__name__))
        if not isinstance(precision, ScalarType.Precision):
            raise TypeError(
                "ScalarType expected 'precision' argument to be of type "
                "ScalarType.Precision but found '{0}'."
                "".format(type(precision).__name__))
        
        self._type_name = type_name
        self._precision = precision


class ArrayType(DataType):
    '''Describes an array datatype.

    :param datatype: the datatype of the array elements.
    :type datatype: :py:class:`psyclone.psyir.datatypes.DataType`
    :param shape: the shape of the array.
    :type shape: list of ???

    :raises TypeError: if the arguments are of the wrong type.

    '''

    def __init__(self, datatype, shape):

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

        self._datatype = datatype
        self._shape = shape


#class StructureType(DataType):
#    ''' xxx '''
#
#    def __init__(self, datatypes):
#
#        if not isinstance(datatypes, list):
#            raise TypeError("XXX")
#        for datatype in datatypes:
#            if not isinstance(datatype, DataType):
#                raise TypeError("XXX")
#
#        self._datatypes = datatypes


#1 Example of current implementation
#scalar_type = ScalarType(ScalarType.Precision.SINGLE,
#                         ScalarType.Name.REAL)
#integer_one = Literal("1", scalar_type)
#integer_two = Literal("2", scalar_type)

#2 Sergi's suggestion
#integer_type = IntegerType(precision)
#integer_one = Literal("1", integer_type)
#integer_two = Literal("2", integer_type)
