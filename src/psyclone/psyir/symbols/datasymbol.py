# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2022, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the DataSymbol and its interfaces.'''

from __future__ import absolute_import
from psyclone.psyir.symbols.typed_symbol import TypedSymbol


class DataSymbol(TypedSymbol):
    '''
    Symbol identifying a data element. It contains information about:
    the datatype, the shape (in column-major order) and the interface
    to that symbol (i.e. Local, Global, Argument).

    :param str name: name of the symbol.
    :param datatype: data type of the symbol.
    :type datatype: :py:class:`psyclone.psyir.symbols.DataType`
    :param constant_value: sets a fixed known expression as a permanent \
        value for this DataSymbol. If the value is None then this \
        symbol does not have a fixed constant. Otherwise it can receive \
        PSyIR expressions or Python intrinsic types available in the \
        TYPE_MAP_TO_PYTHON map. By default it is None.
    :type constant_value: NoneType, item of TYPE_MAP_TO_PYTHON or \
        :py:class:`psyclone.psyir.nodes.Node`
    :param kwargs: additional keyword arguments provided by \
                   :py:class:`psyclone.psyir.symbols.TypedSymbol`
    :type kwargs: unwrapped dict.

    '''
    def __init__(self, name, datatype, constant_value=None, **kwargs):
        super(DataSymbol, self).__init__(name, datatype)
        self._constant_value = None
        self._process_arguments(constant_value=constant_value,
                                **kwargs)

    def _process_arguments(self, **kwargs):
        ''' Process the arguments for the constructor and the specialise
        methods. In this case the constant_value argument.

        :param kwargs: keyword arguments which can be:\n
            :param constant_value: sets a fixed known expression as a \
                permanent value for this DataSymbol. If the value is None \
                then this symbol does not have a fixed constant. Otherwise \
                it can receive PSyIR expressions or Python intrinsic types \
                available in the TYPE_MAP_TO_PYTHON map. By default it is \
                set to None. \n
            :type constant_value: NoneType, item of TYPE_MAP_TO_PYTHON or \
                :py:class:`psyclone.psyir.nodes.Node`\n
            and the arguments in :py:class:`psyclone.psyir.symbols.TypedSymbol`
        :type kwargs: unwrapped dict.

        '''
        new_constant_value = None
        if "constant_value" in kwargs:
            new_constant_value = kwargs.pop("constant_value")
        elif not hasattr(self, '_constant_value'):
            # At least initialise it if we reach this point and it doesn't
            # exist
            self._constant_value = None

        # We need to consume the 'constant_value' before calling the super
        # because otherwise there will be an unknown argument in kwargs but
        # we need to call the 'constant_value' setter after this because it
        # uses the self.datatype which is in turn set in the super.
        super(DataSymbol, self)._process_arguments(**kwargs)

        # Now that we have a datatype we can use the constant_value setter
        # with proper error checking
        if new_constant_value:
            self.constant_value = new_constant_value

    @property
    def is_constant(self):
        '''
        :returns: Whether the symbol is a constant with a fixed known \
        value (True) or not (False).
        :rtype: bool

        '''
        return self._constant_value is not None

    @property
    def constant_value(self):
        '''
        :returns: the fixed known value of this symbol.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        return self._constant_value

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
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes import (Node, Literal, Operation, Reference,
                                          CodeBlock)
        from psyclone.psyir.symbols.datatypes import ScalarType, ArrayType

        if new_value is not None:
            if self.is_argument:
                raise ValueError(
                    f"Error setting constant value for symbol '{self.name}'. "
                    f"A DataSymbol with an ArgumentInterface can not have a "
                    f"constant value.")
            if not isinstance(self.datatype, (ScalarType, ArrayType)):
                raise ValueError(
                    f"Error setting constant value for symbol '{self.name}'. "
                    f"A DataSymbol with a constant value must be a scalar or "
                    f"an array but found '{type(self.datatype).__name__}'.")

            if isinstance(new_value, Node):
                for node in new_value.walk(Node):
                    if not isinstance(node, (Literal, Operation, Reference,
                                             CodeBlock)):
                        raise ValueError(
                            f"Error setting constant value for symbol "
                            f"'{self.name}'. PSyIR static expressions can only"
                            f" contain PSyIR Literal, Operation, Reference or "
                            f"CodeBlock nodes but found: {node}")
                self._constant_value = new_value
            else:
                from psyclone.psyir.symbols.datatypes import TYPE_MAP_TO_PYTHON
                # No need to check that self.datatype has an intrinsic
                # attribute as we know it is a ScalarType or ArrayType
                # due to an earlier test.
                lookup = TYPE_MAP_TO_PYTHON[self.datatype.intrinsic]
                if not isinstance(new_value, lookup):
                    raise ValueError(
                        f"Error setting constant value for symbol "
                        f"'{self.name}'. This DataSymbol instance datatype is "
                        f"'{self.datatype}' meaning the constant value should "
                        f"be '{lookup}' but found '{type(new_value)}'.")
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
        ret = self.name + ": DataSymbol<" + str(self.datatype)
        ret += ", " + str(self._interface)
        if self.is_constant:
            ret += f", constant_value={self.constant_value}"
        return ret + ">"

    def copy(self):
        '''Create and return a copy of this object. Any references to the
        original will not be affected so the copy will not be referred
        to by any other object.

        :returns: An object with the same properties as this symbol object.
        :rtype: :py:class:`psyclone.psyir.symbols.DataSymbol`

        '''
        return DataSymbol(self.name, self.datatype, visibility=self.visibility,
                          interface=self.interface,
                          constant_value=self.constant_value)

    def copy_properties(self, symbol_in):
        '''Replace all properties in this object with the properties from
        symbol_in, apart from the name (which is immutable) and visibility.

        :param symbol_in: the symbol from which the properties are copied.
        :type symbol_in: :py:class:`psyclone.psyir.symbols.DataSymbol`

        :raises TypeError: if the argument is not the expected type.

        '''
        if not isinstance(symbol_in, DataSymbol):
            raise TypeError(f"Argument should be of type 'DataSymbol' but "
                            f"found '{type(symbol_in).__name__}'.")
        super(DataSymbol, self).copy_properties(symbol_in)
        self._constant_value = symbol_in.constant_value
