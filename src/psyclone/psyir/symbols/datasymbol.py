# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2020, Science and Technology Facilities Council.
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

''' This module contains the DataSymbol and its interfaces.'''

from __future__ import absolute_import
from enum import Enum
from psyclone.psyir.symbols.symbol import Symbol, SymbolError


class DataSymbol(Symbol):
    '''
    Symbol identifying a data element. It contains information about:
    the datatype, the shape (in column-major order) and the interface
    to that symbol (i.e. Local, Global, Argument).

    :param str name: name of the symbol.
    :param datatype: data type of the symbol.
    :param visibility: the visibility of this symbol.
    :type visibility: :py:class:`psyclone.psyir.symbols.Visibility`
    :type datatype: :py:class:`psyclone.psyir.symbols.DataType`
    :param constant_value: sets a fixed known expression as a permanent \
        value for this DataSymbol. If the value is None then this \
        symbol does not have a fixed constant. Otherwise it can receive \
        PSyIR expressions or Python intrinsic types available in the \
        TYPE_MAP_TO_PYTHON map. By default it is None.
    :type constant_value: NoneType, item of TYPE_MAP_TO_PYTHON or \
        :py:class:`psyclone.psyir.nodes.Node`
    :param interface: object describing the interface to this symbol (i.e. \
        whether it is passed as a routine argument or accessed in some other \
        way).
    :type interface: \
        :py:class:`psyclone.psyir.symbols.datasymbols.DataSymbolInterface`

    '''
    def __init__(self, name, datatype, visibility=Symbol.DEFAULT_VISIBILITY,
                 constant_value=None, interface=None):
        super(DataSymbol, self).__init__(name, visibility)

        self._datatype = None
        self.datatype = datatype

        # The following attributes have setter methods (with error checking)
        self._interface = None
        self._constant_value = None

        # If an interface is not provided, use LocalInterface by default
        if not interface:
            self.interface = LocalInterface()
        else:
            self.interface = interface

        self.constant_value = constant_value

    def resolve_deferred(self):
        ''' If the symbol has a deferred datatype, find where it is defined
        (i.e. an external container) and obtain the properties of the symbol.

        :raises SymbolError: if the module pointed to by the symbol interface \
                             does not contain the symbol (or the symbol is \
                             not public).
        :raises NotImplementedError: if the deferred symbol is not a Global.

        '''
        from psyclone.psyir.symbols.datatypes import DeferredType
        if isinstance(self.datatype, DeferredType):
            if self.is_global:
                # Copy all the symbol properties but the interface and
                # visibility (the latter is determined by the current
                # scoping unit)
                tmp = self.interface
                module = self.interface.container_symbol
                try:
                    extern_symbol = module.container.symbol_table.lookup(
                        self.name, visibility=Symbol.Visibility.PUBLIC)
                except KeyError:
                    raise SymbolError(
                        "Error trying to resolve the properties of symbol "
                        "'{0}'. The interface points to module '{1}' but "
                        "could not find the definition of '{0}' in that "
                        "module.".format(self.name, module.name))
                except SymbolError as err:
                    raise SymbolError(
                        "Error trying to resolve the properties of symbol "
                        "'{0}' in module '{1}': {2}".format(
                            self.name, module.name, str(err.value)))
                self.copy_properties(extern_symbol)
                self.interface = tmp
            else:
                raise NotImplementedError(
                    "Error trying to resolve symbol '{0}' properties, the lazy"
                    " evaluation of '{1}' interfaces is not supported."
                    "".format(self.name, self.interface))

    @property
    def datatype(self):
        '''
        :returns: datatype of the DataSymbol.
        :rtype: str
        '''
        return self._datatype

    @datatype.setter
    def datatype(self, value):
        ''' Setter for DataSymbol datatype.

        :param value: new value for datatype.
        :type value: :py:class:`psyclone.psyir.symbols.DataType`

        :raises TypeError: if value is not of the correct type.
        :raises NotImplementedError: if the specified data type is invalid.
        '''
        from psyclone.psyir.symbols import DataType
        if not isinstance(value, DataType):
            raise TypeError(
                "The datatype of a DataSymbol must be specified using a "
                "DataType but got: '{0}'".format(type(value).__name__))
        self._datatype = value

    @property
    def shape(self):
        '''
        :returns: shape of the symbol in column-major order (leftmost \
                  index is contiguous in memory). Each entry represents \
                  an array dimension. If it is 'None' the extent of that \
                  dimension is unknown, otherwise it holds an integer \
                  literal or a reference to an integer symbol with the \
                  extent. If it is an empty list then the symbol \
                  represents a scalar.
        :rtype: list
        '''
        from psyclone.psyir.symbols import ArrayType
        if isinstance(self._datatype, ArrayType):
            return self._datatype.shape
        return []

    @property
    def interface(self):
        '''
        :returns: the an object describing the interface to this DataSymbol.
        :rtype: Sub-class of \
            :py:class:`psyclone.psyir.symbols.datasymbol.DataSymbolInterface`
        '''
        return self._interface

    @interface.setter
    def interface(self, value):
        '''
        Setter for the Interface associated with this DataSymbol.

        :param value: an Interface object describing how the DataSymbol is \
                      accessed by the code.
        :type value: Sub-class of \
            :py:class:`psyclone.psyir.symbols.datasymbol.DataSymbolInterface`

        :raises TypeError: if the supplied `value` is of the wrong type.
        '''
        if not isinstance(value, DataSymbolInterface):
            raise TypeError("The interface to a DataSymbol must be a "
                            "DataSymbolInterface but got '{0}'".
                            format(type(value).__name__))
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
        from psyclone.psyir.symbols import ScalarType
        return isinstance(self.datatype, ScalarType)

    @property
    def is_array(self):
        '''
        :returns: True if this symbol is an array and False otherwise.
        :rtype: bool

        '''
        from psyclone.psyir.symbols import ArrayType
        return isinstance(self.datatype, ArrayType)

    @property
    def constant_value(self):
        '''
        :returns: the fixed known value of this symbol.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        return self._constant_value

    @property
    def is_local(self):
        '''
        :returns: whether the DataSymbol has a Local interface.
        :rtype: bool

        '''
        return isinstance(self._interface, LocalInterface)

    @property
    def is_global(self):
        '''
        :returns: whether the DataSymbol has a Global interface.
        :rtype: bool

        '''
        return isinstance(self._interface, GlobalInterface)

    @property
    def is_argument(self):
        '''
        :returns: whether the DataSymbol has an Argument interface.
        :rtype: bool

        '''
        return isinstance(self._interface, ArgumentInterface)

    @property
    def unresolved_interface(self):
        '''
        :returns: whether the DataSymbol has an unresolved interface.
        :rtype: bool

        '''
        return isinstance(self._interface, UnresolvedInterface)

    @constant_value.setter
    def constant_value(self, new_value):
        '''
        :param new_value: set or change the fixed known value of the \
            constant for this DataSymbol. If the value is None then this \
            symbol does not have a fixed constant. Otherwise it can receive \
            PSyIR expressions or Python intrinsic types available in the \
            TYPE_MAP_TO_PYTHON map.
        :type new_value: NoneType, item of TYPE_MAP_TO_PYTHON or \
            :py:class:`psyclone.psyir.nodes.Node`

        :raises ValueError: if a non-None value is provided and 1) this \
            DataSymbol instance does not have local scope, or 2) this \
            DataSymbol instance is not a scalar (as the shape attribute is \
            not empty), or 3) a constant value is provided but the type of \
            the value does is not supported, or 4) the type of the value \
            provided is not compatible with the datatype of this DataSymbol \
            instance, or 5) the provided PSyIR expression is unsupported.

        '''
        from psyclone.psyir.nodes import Node, Literal, Operation, Reference
        from psyclone.psyir.symbols import ScalarType, ArrayType
        if new_value is not None:
            if self.is_argument:
                raise ValueError(
                    "Error setting constant value for symbol '{0}'. A "
                    "DataSymbol with an ArgumentInterface can not have a "
                    "constant value.".format(self.name))
            if not isinstance(self.datatype, (ScalarType, ArrayType)):
                raise ValueError(
                    "Error setting constant value for symbol '{0}'. A "
                    "DataSymbol with a constant value must be a scalar or an "
                    "array but found '{1}'.".format(
                        self.name, type(self.datatype).__name__))

            if isinstance(new_value, Node):
                for node in new_value.walk(Node):
                    if not isinstance(node, (Literal, Operation, Reference)):
                        raise ValueError(
                            "Error setting constant value for symbol '{0}'. "
                            "PSyIR static expressions can only contain PSyIR "
                            "literal, operation or reference nodes but found:"
                            " {1}".format(self.name, node))
                self._constant_value = new_value
            else:
                from psyclone.psyir.symbols.datatypes import TYPE_MAP_TO_PYTHON
                # No need to check that self.datatype has an intrinsic
                # attribute as we know it is a ScalarType or ArrayType
                # due to an earlier test.
                lookup = TYPE_MAP_TO_PYTHON[self.datatype.intrinsic]
                if not isinstance(new_value, lookup):
                    raise ValueError(
                        "Error setting constant value for symbol '{0}'. This "
                        "DataSymbol instance datatype is '{1}' which means the"
                        " constant value is expected to be '{2}' but found "
                        "'{3}'.".format(self.name, self.datatype, lookup,
                                        type(new_value)))
                if self.datatype.intrinsic == ScalarType.Intrinsic.BOOLEAN:
                    # In this case we know new_value is a Python boolean as it
                    # has passed the isinstance(new_value, lookup) check.
                    if new_value:
                        self._constant_value = Literal('true', self.datatype)
                    else:
                        self._constant_value = Literal('false', self.datatype)
                else:
                    # Otherwise we convert the Python intrinsic to a PSyIR
                    # Literal using its string representation.
                    self._constant_value = Literal(str(new_value),
                                                   self.datatype)
        else:
            self._constant_value = None

    def __str__(self):
        ret = self.name + ": <" + str(self.datatype)
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
        :rtype: :py:class:`psyclone.psyir.symbols.DataSymbol`

        '''
        return DataSymbol(self.name, self.datatype,
                          constant_value=self.constant_value,
                          interface=self.interface)

    def copy_properties(self, symbol_in):
        '''Replace all properties in this object with the properties from
        symbol_in, apart from the name (which is immutable) and visibility.

        :param symbol_in: the symbol from which the properties are copied.
        :type symbol_in: :py:class:`psyclone.psyir.symbols.DataSymbol`

        :raises TypeError: if the argument is not the expected type.

        '''
        if not isinstance(symbol_in, DataSymbol):
            raise TypeError("Argument should be of type 'DataSymbol' but found"
                            " '{0}'.".format(type(symbol_in).__name__))

        self._datatype = symbol_in.datatype
        self._constant_value = symbol_in.constant_value
        self._interface = symbol_in.interface


class DataSymbolInterface(object):
    ''' Abstract class of a DataSymbol Interface '''


class LocalInterface(DataSymbolInterface):
    ''' The data just exists in the Local context '''

    def __str__(self):
        return "Local"


class UnresolvedInterface(DataSymbolInterface):
    '''We have a data symbol but we don't know where it is declared.'''

    def __str__(self):
        return "Unresolved"


class GlobalInterface(DataSymbolInterface):
    '''
    Describes the interface to a DataSymbol representing data that
    is supplied as some sort of global variable, and therefore it is
    defined in an external PSyIR container.

    :param container_symbol: symbol representing the external container \
        from which the symbol is imported.
    :type container_symbol: \
        :py:class:`psyclone.psyir.symbols.ContainerSymbol`

    :raise TypeError: if the container_symbol is not a ContainerSymbol.
    '''
    def __init__(self, container_symbol):
        from psyclone.psyir.symbols import ContainerSymbol

        super(GlobalInterface, self).__init__()

        if not isinstance(container_symbol, ContainerSymbol):
            raise TypeError(
                "Global container_symbol parameter must be of type"
                " ContainerSymbol, but found {0}."
                "".format(type(container_symbol)))

        self._container_symbol = container_symbol

    @property
    def container_symbol(self):
        '''
        :return: symbol representing the container containing this DataSymbol.
        :rtype: :py:class:`psyclone.psyir.symbols.ContainerSymbol`
        '''
        return self._container_symbol

    def __str__(self):
        return "Global(container='{0}')".format(self.container_symbol.name)


class ArgumentInterface(DataSymbolInterface):
    '''
    Captures the interface to a DataSymbol that is accessed as a routine
    argument.

    :param access: specifies how the argument is used in the Schedule
    :type access: :py:class:`psyclone.psyir.symbols.ArgumentInterface.Access`
    '''

    class Access(Enum):
        '''
        Enumeration for the different types of access that a Argument
        DataSymbol is permitted to have.

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

    def __init__(self, access=None):
        super(ArgumentInterface, self).__init__()
        self._pass_by_value = False
        self._access = None
        # Use the setter as that has error checking
        if not access:
            self.access = ArgumentInterface.Access.UNKNOWN
        else:
            self.access = access

    @property
    def access(self):
        '''
        :returns: the access-type for this argument.
        :rtype: :py:class:`psyclone.psyir.symbols.ArgumentInterface.Access`
        '''
        return self._access

    @access.setter
    def access(self, value):
        '''
        :param value: the new access type.
        :type value: :py:class:`psyclon.psyir.symbols.ArgumentInterface.Access`

        :raises TypeError: if the supplied value is not an \
            ArgumentInterface.Access
        '''
        if not isinstance(value, ArgumentInterface.Access):
            raise TypeError(
                "SymbolInterface.access must be a 'ArgumentInterface.Access' "
                "but got '{0}'.".format(type(value)))
        self._access = value

    def __str__(self):
        return "Argument(pass-by-value={0})".format(self._pass_by_value)
