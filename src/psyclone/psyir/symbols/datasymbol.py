# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2019, Science and Technology Facilities Council.
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
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' File Description '''

from enum import Enum
from psyclone.psyir.symbols import Symbol


class DataSymbolInterface(object):
    pass


class DataSymbol(Symbol):
    '''
    Symbol identifying a data element. It contains information about:
    the datatype, the shape (in column-major order) and, for a symbol
    representing data that exists outside of the local scope, the interface
    to that symbol (i.e. the mechanism by which it is accessed).

    :param str name: Name of the symbol.
    :param str datatype: Data type of the symbol. (One of \
                     :py:attr:`psyclone.psyGen.DataSymbol.valid_data_types`.)
    :param list shape: Shape of the symbol in column-major order (leftmost \
                       index is contiguous in memory). Each entry represents \
                       an array dimension. If it is 'None' the extent of that \
                       dimension is unknown, otherwise it holds an integer \
                       literal or a reference to an integer symbol with the \
                       extent. If it is an empty list then the symbol \
                       represents a scalar.
    :param interface: Object describing the interface to this symbol (i.e. \
                      whether it is passed as a routine argument or accessed \
                      in some other way) or None if the symbol is local.
    :type interface: :py:class:`psyclone.psyGen.SymbolInterface` or NoneType.
    :param constant_value: Sets a fixed known value for this DataSymbol. \
                           If the value is None (the default) \
                           then this symbol is not a constant. The \
                           datatype of the constant value must be \
                           compatible with the datatype of the symbol.
    :type constant_value: int, str or bool

    :raises NotImplementedError: Provided parameters are not supported yet.
    :raises TypeError: Provided parameters have invalid error type.
    :raises ValueError: Provided parameters contain invalid values.

    '''
    ## Tuple with the valid datatypes.
    valid_data_types = ('real',  # Floating point
                        'integer',
                        'character',
                        'boolean',
                        'deferred')  # Type of this symbol not yet determined

    ## Mapping from supported data types for constant values to
    #  internal Python types
    mapping = {'integer': int,
               'character': str,
               'boolean': bool,
               'real': float}

    class Access(Enum):
        '''
        Enumeration for the different types of access that a DataSymbol is
        permitted to have.

        '''
        ## The symbol is only ever read within the current scoping block.
        READ = 1
        ## The first access of the symbol in the scoping block is a write and
        # therefore any value that it may have had upon entry is discarded.
        WRITE = 2
        ## The first access of the symbol in the scoping block is a read but
        # it is subsequently written to.
        READWRITE = 3
        ## The way in which the symbol is accessed in the scoping block is
        # unknown
        UNKNOWN = 4

    class Local(DataSymbolInterface):

        def __str__(str):
            return "Local"

    class Global(DataSymbolInterface):
        '''
        Describes the interface to a DataSymbol representing data that
        is supplied as some sort of global variable, and therefore it is
        defined in a external PSyIR container.

        :param container_symbol: symbol of the external container from which \
            the symbol is imported.
        :type container_symbol: \
            :py:class:`psyclone.psyir.symbols.ContainerSymbol`

        :raise TypeError: if the container_symbol is not a ContainerSymbol
        '''
        def __init__(self, container_symbol):
            from psyclone.psyir.symbols import ContainerSymbol

            super(DataSymbol.Global, self).__init__()

            if not isinstance(container_symbol, ContainerSymbol):
                raise TypeError(
                    "Global container_symbol parameter must be of type"
                    " ContainerSymbol, but found {0}."
                    "".format(type(container_symbol)))

            self._container_symbol = container_symbol

        @property
        def container_symbol(self):
            return self._container_symbol

        def __str__(self):
            return "Global(container='{0}')".format(self.container_symbol.name)

    class Argument(DataSymbolInterface):
        '''
        Captures the interface to a symbol that is accessed as a routine
        argument.

        :param access: how the symbol is accessed within the local scope.
        :type access: :py:class:`psyclone.psyGen.DataSymbol.Access`
        '''
        def __init__(self, access=None):
            super(DataSymbol.Argument, self).__init__()
            self._pass_by_value = False
            self._access = None
            # Use the setter as that has error checking
            if not access:
                self.access = DataSymbol.Access.UNKNOWN
            else:
                self.access = access

        @property
        def access(self):
            '''
            :returns: the access-type for this symbol.
            :rtype: :py:class:`psyclone.psyGen.DataSymbol.Access`
            '''
            return self._access

        @access.setter
        def access(self, value):
            '''
            :param value: the new access type.
            :type value: :py:class:`psyclon.psyGen.DataSymbol.Access`

            :raises TypeError: if the supplied value is not a \
                DataSymbol.Access
            '''
            if not isinstance(value, DataSymbol.Access):
                raise TypeError(
                    "SymbolInterface.access must be a 'DataSymbol.Access' "
                    "but got '{0}'.".format(type(value)))
            self._access = value

        def __str__(self):
            return "Argument(pass-by-value={0})".format(self._pass_by_value)

    def __init__(self, name, datatype, shape=None, constant_value=None,
                 interface=None):

        super(DataSymbol, self).__init__(name)

        if datatype not in DataSymbol.valid_data_types:
            raise NotImplementedError(
                "DataSymbol can only be initialised with {0} datatypes but "
                "found '{1}'.".format(str(DataSymbol.valid_data_types),
                                      datatype))
        self._datatype = datatype

        if shape is None:
            shape = []
        elif not isinstance(shape, list):
            raise TypeError("DataSymbol shape attribute must be a list.")

        for dimension in shape:
            if isinstance(dimension, DataSymbol):
                if dimension.datatype != "integer" or dimension.shape:
                    raise TypeError(
                        "DataSymbols that are part of another symbol shape can"
                        " only be scalar integers, but found '{0}'."
                        "".format(str(dimension)))
            elif not isinstance(dimension, (type(None), int)):
                raise TypeError("DataSymbol shape list elements can only be "
                                "'DataSymbol', 'integer' or 'None'.")

        if not interface:
            self.interface = DataSymbol.Local()
        else:
            self.interface = interface

        assert isinstance(self._interface, DataSymbolInterface)

        self._shape = shape
        # The following attributes have setter methods (with error checking)
        self._constant_value = None
        self.constant_value = constant_value

    def resolve_deferred(self):
        if self.datatype == "deferred":
            if isinstance(self.interface, DataSymbol.Global):
                # Copy all the symbol properties but the interface
                tmp = self.interface
                module = self.interface.container_symbol
                extern_symbol = module.container.symbol_table.lookup(self.name)
                self.copy_properties(extern_symbol)
                self.interface = tmp
            else:
                raise NotImplementedError(
                    "Lazy evalution of deferred {0} is not supported yet."
                    "".format(self.interface))

    @property
    def datatype(self):
        '''
        :returns: Datatype of the DataSymbol.
        :rtype: str
        '''
        return self._datatype


    @property
    def shape(self):
        '''
        :returns: Shape of the symbol in column-major order (leftmost \
                  index is contiguous in memory). Each entry represents \
                  an array dimension. If it is 'None' the extent of that \
                  dimension is unknown, otherwise it holds an integer \
                  literal or a reference to an integer symbol with the \
                  extent. If it is an empty list then the symbol \
                  represents a scalar.
        :rtype: list
        '''
        return self._shape

    @property
    def interface(self):
        '''
        :returns: the an object describing the external interface to \
                  this DataSymbol or None (if it is local).
        :rtype: Sub-class of :py:class:`psyclone.psyGen.SymbolInterface` or \
                NoneType.
        '''
        return self._interface

    @interface.setter
    def interface(self, value):
        '''
        Setter for the Interface associated with this DataSymbol.

        :param value: an Interface object describing how the DataSymbol is \
                      accessed by the code or None if it is local.
        :type value: Sub-class of :py:class:`psyclone.psyGen.SymbolInterface` \
                     or NoneType.

        :raises TypeError: if the supplied `value` is of the wrong type.
        '''
        if value is not None and not isinstance(value, DataSymbolInterface):
            raise TypeError("The interface to a DataSymbol must be a "
                            "SymbolInterface or None but got '{0}'".
                            format(type(value)))
        self._interface = value

    @property
    def is_constant(self):
        '''
        :returns: Whether the symbol is a constant with a fixed known \
        value (True) or not (False).
        :rtype: bool

        '''
        return self._constant_value is not None

    @property
    def is_scalar(self):
        '''
        :returns: True if this symbol is a scalar and False otherwise.
        :rtype: bool

        '''
        # If the shape variable is an empty list then this symbol is a
        # scalar.
        return self.shape == []

    @property
    def is_array(self):
        '''
        :returns: True if this symbol is an array and False otherwise.
        :rtype: bool

        '''
        # The assumption in this method is that if this symbol is not
        # a scalar then it is an array. If this assumption becomes
        # invalid then this logic will need to be changed
        # appropriately.
        return not self.is_scalar

    @property
    def constant_value(self):
        '''
        :returns: The fixed known value for this symbol if one has \
        been set or None if not.
        :rtype: int, str, bool or NoneType

        '''
        return self._constant_value

    @constant_value.setter
    def constant_value(self, new_value):
        '''
        :param constant_value: Set or change the fixed known value of \
        the constant for this DataSymbol. If the value is None then this \
        symbol does not have a fixed constant. The datatype of \
        new_value must be compatible with the datatype of the symbol.
        :type constant_value: int, str or bool

        :raises ValueError: If a non-None value is provided and 1) \
        this DataSymbol instance does not have local scope, or 2) this \
        DataSymbol instance is not a scalar (as the shape attribute is not \
        empty), or 3) a constant value is provided but the type of the \
        value does not support this, or 4) the type of the value \
        provided is not compatible with the datatype of this DataSymbol \
        instance.

        '''
        if new_value is not None:
            if not isinstance(self.interface, DataSymbol.Local):
                raise ValueError(
                    "DataSymbol with a constant value is currently limited to "
                    "having a Local interface but found '{0}'."
                    "".format(self.interface))
            if self.is_array:
                raise ValueError(
                    "DataSymbol with a constant value must be a scalar but the"
                    " shape attribute is not empty.")
            try:
                lookup = DataSymbol.mapping[self.datatype]
            except KeyError:
                raise ValueError(
                    "A constant value is not supported for "
                    "datatype '{0}'.".format(self.datatype))
            if not isinstance(new_value, lookup):
                raise ValueError(
                    "This DataSymbol instance's datatype is '{0}' which means "
                    "the constant value is expected to be '{1}' but found "
                    "'{2}'.".format(self.datatype,
                                    DataSymbol.mapping[self.datatype],
                                    type(new_value)))
        self._constant_value = new_value

    def __str__(self):
        from psyclone.psyGen import InternalError
        ret = self.name + ": <" + self.datatype + ", "
        if self.is_array:
            ret += "Array["
            for dimension in self.shape:
                if isinstance(dimension, DataSymbol):
                    ret += dimension.name
                elif isinstance(dimension, int):
                    ret += str(dimension)
                elif dimension is None:
                    ret += "'Unknown bound'"
                else:
                    raise InternalError(
                        "DataSymbol shape list elements can only be "
                        "'DataSymbol', 'integer' or 'None', but found '{0}'."
                        "".format(type(dimension)))
                ret += ", "
            ret = ret[:-2] + "]"  # Deletes last ", " and adds "]"
        else:
            ret += "Scalar"
        ret += ", " + str(self._interface)
        if self.is_constant:
            ret += ", constant_value={0}".format(self.constant_value)
        return ret + ">"

    def copy(self):
        '''Create and return a copy of this object. Any references to the
        original will not be affected so the copy will not be referred
        to by any other object.

        :returns: A symbol object with the same properties as this \
                  symbol object.
        :rtype: :py:class:`psyclone.psyGen.DataSymbol`

        '''
        return DataSymbol(self.name, self.datatype, shape=self.shape[:],
                          constant_value=self.constant_value,
                          interface=self.interface)

    def copy_properties(self, symbol_in):
        '''Replace all properties in this object with the properties from
        symbol_in, apart from the name which is immutable.

        :param symbol_in: The symbol from which the properties are \
                          copied from.
        :type symbol_in: :py:class:`psyclone.psyGen.DataSymbol`

        :raises TypeError: If the argument is not the expected type.

        '''
        if not isinstance(symbol_in, DataSymbol):
            raise TypeError("Argument should be of type 'DataSymbol' but found"
                            " '{0}'.".format(type(symbol_in).__name__))

        self._datatype = symbol_in.datatype
        self._shape = symbol_in.shape[:]
        self._constant_value = symbol_in.constant_value
        self._interface = symbol_in.interface
