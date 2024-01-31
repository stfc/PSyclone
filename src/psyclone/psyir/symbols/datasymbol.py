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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the DataSymbol and its interfaces.'''

from psyclone.psyir.symbols.typed_symbol import TypedSymbol
from psyclone.psyir.symbols.interfaces import StaticInterface


class DataSymbol(TypedSymbol):
    '''
    Symbol identifying a data element. It contains information about:
    the datatype, the shape (in column-major order) and the interface
    to that symbol (i.e. Local, Global, Argument).

    :param str name: name of the symbol.
    :param datatype: data type of the symbol.
    :type datatype: :py:class:`psyclone.psyir.symbols.DataType`
    :param bool is_constant: whether this DataSymbol is a compile-time
        constant (default is False). If True then an `initial_value` must
        also be provided.
    :param initial_value: sets a fixed known expression as an initial
        value for this DataSymbol. If `is_constant` is True then this
        Symbol will always have this value. If the value is None then this
        symbol does not have an initial value (and cannot be a constant).
        Otherwise it can receive PSyIR expressions or Python intrinsic types
        available in the TYPE_MAP_TO_PYTHON map. By default it is None.
    :type initial_value: Optional[item of TYPE_MAP_TO_PYTHON |
        :py:class:`psyclone.psyir.nodes.Node`]
    :param kwargs: additional keyword arguments provided by
                   :py:class:`psyclone.psyir.symbols.TypedSymbol`
    :type kwargs: unwrapped dict.

    '''
    def __init__(self, name, datatype, is_constant=False, initial_value=None,
                 **kwargs):
        super().__init__(name, datatype)
        self._is_constant = False
        self._initial_value = None
        self._process_arguments(is_constant=is_constant,
                                initial_value=initial_value,
                                **kwargs)

    def _process_arguments(self, **kwargs):
        ''' Process the arguments for the constructor and the specialise
        methods. In this case the initial_value and is_constant arguments.

        :param kwargs: keyword arguments which can be:\n
            :param bool is_constant: whether this DataSymbol is a compile-time
                constant (default is False). If True then an `initial_value`
                must also be provided.\n
            :param initial_value: sets a fixed known expression as an initial
                value for this DataSymbol. If `is_constant` is True then this
                Symbol will always have this value. If the value is None then
                this symbol does not have an initial value (and cannot be a
                constant). Otherwise it can receive PSyIR expressions or Python
                intrinsic types available in the TYPE_MAP_TO_PYTHON map. By
                default it is None.\n
            :type initial_value: Optional[item of TYPE_MAP_TO_PYTHON |
                :py:class:`psyclone.psyir.nodes.Node`]\n
            and the arguments in :py:class:`psyclone.psyir.symbols.TypedSymbol`
        :type kwargs: unwrapped dict.

        :raises ValueError: if the symbol is a run-time constant but is not
            given an initial value.
        :raises ValueError: if the symbol is a run-time constant and an
            interface other than StaticInterface is specified.
        '''
        new_initial_value = None
        new_is_constant_value = None

        # We need to consume 'initial_value' and 'is_constant' before calling
        # the super because otherwise there will be an unknown argument in
        # kwargs. However, we can only call the 'initial_value' setter after
        # the super because it uses self.datatype which in turn is set in
        # the super.

        if "initial_value" in kwargs:
            new_initial_value = kwargs.pop("initial_value")
        if not hasattr(self, '_initial_value'):
            # Initialise this attribute if we reach this point and this object
            # doesn't already have it (which may happen if this symbol has been
            # specialised from a Symbol).
            self._initial_value = None

        if "is_constant" in kwargs:
            new_is_constant_value = kwargs.pop("is_constant")
        if not hasattr(self, '_is_constant'):
            # At least initialise it if we reach this point and it doesn't
            # exist (which may happen if this symbol has been specialised from
            # a Symbol).
            self._is_constant = False

        # Record whether an explicit value has been supplied for 'interface'
        # (before it is consumed by the super method).
        interface_supplied = "interface" in kwargs

        super()._process_arguments(**kwargs)

        # Now that we have a datatype we can use initial_value setter
        # with proper error checking.
        if new_initial_value is not None:
            self.initial_value = new_initial_value

        # Now that we know whether or not we have an intial_value and an
        # interface, we can call the is_constant setter.
        if new_is_constant_value is not None:
            self.is_constant = new_is_constant_value

        if self.is_constant and not interface_supplied and self.is_automatic:
            # No explicit interface was supplied and this Symbol represents
            # a runtime constant so change its interface to be static if it
            # would otherwise default to Automatic.
            self.interface = StaticInterface()

    @property
    def is_constant(self):
        '''
        :returns: Whether the symbol is a compile-time constant (True) or
            not (False).
        :rtype: bool
        '''
        return self._is_constant

    @is_constant.setter
    def is_constant(self, value):
        '''
        :param bool value: whether or not this symbol is a compile-time
            constant.

        :raises ValueError: if `value` is True but this symbol does not have an
            initial value or an import or unresolved interface.

        '''
        if (value and not (self.is_import or self.is_unresolved) and
                self.initial_value is None):
            # A Symbol of UnsupportedType could have initialisation within its
            # original declaration.
            # pylint: disable=import-outside-toplevel
            from psyclone.psyir.symbols.datatypes import UnsupportedType
            if not isinstance(self.datatype, UnsupportedType):
                raise ValueError(
                    f"DataSymbol '{self.name}' cannot be a constant because it"
                    f" does not have an initial value or an import or "
                    f"unresolved interface.")
        self._is_constant = value

    @property
    def initial_value(self):
        '''
        :returns: the initial value associated with this symbol (if any).
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        return self._initial_value

    @initial_value.setter
    def initial_value(self, new_value):
        '''
        :param new_value: set or change the initial value associated
            with this DataSymbol. If the value is None then this symbol does
            not have an initial value (and cannot be a constant). Otherwise it
            can receive PSyIR expressions or Python intrinsic types available
            in the TYPE_MAP_TO_PYTHON map.
        :type new_value: Optional[item of TYPE_MAP_TO_PYTHON |
            :py:class:`psyclone.psyir.nodes.Node`]

        :raises ValueError: if a non-None value is provided and 1) this
            DataSymbol instance represents an argument, or 2) this
            DataSymbol instance is not a scalar (as the shape attribute is
            not empty), or 3) an initial value is provided but the type of
            the value is not supported, or 4) the type of the value
            provided is not compatible with the datatype of this DataSymbol
            instance, or 5) the provided PSyIR expression is unsupported.
        :raises ValueError: if a None value is provided and this DataSymbol
            represents a constant and is not imported.

        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes import (Node, Literal, Operation, Reference,
                                          CodeBlock, IntrinsicCall)
        from psyclone.psyir.symbols.datatypes import (ScalarType, ArrayType,
                                                      UnsupportedType)

        if new_value is not None:
            if self.is_argument:
                raise ValueError(
                    f"Error setting initial value for symbol '{self.name}'. "
                    f"A DataSymbol with an ArgumentInterface can not have an "
                    f"initial value.")
            if not isinstance(self.datatype,
                              (ScalarType, ArrayType, UnsupportedType)):
                raise ValueError(
                    f"Error setting initial value for symbol '{self.name}'. "
                    f"A DataSymbol with an initial value must be a scalar or "
                    f"an array or of UnsupportedType but found "
                    f"'{type(self.datatype).__name__}'.")

            if isinstance(new_value, Node):
                for node in new_value.walk(Node):
                    if not isinstance(node, (Literal, Operation, Reference,
                                             CodeBlock, IntrinsicCall)):
                        raise ValueError(
                            f"Error setting initial value for symbol "
                            f"'{self.name}'. PSyIR static expressions can only"
                            f" contain PSyIR Literal, Operation, Reference,"
                            f" IntrinsicCall or CodeBlock nodes but found: "
                            f"{node}")
                new_initial_value = new_value
            else:
                from psyclone.psyir.symbols.datatypes import TYPE_MAP_TO_PYTHON
                # No need to check that self.datatype has an intrinsic
                # attribute as we know it is a ScalarType or ArrayType
                # due to an earlier test.
                lookup = TYPE_MAP_TO_PYTHON[self.datatype.intrinsic]
                if not isinstance(new_value, lookup):
                    raise ValueError(
                        f"Error setting initial value for symbol "
                        f"'{self.name}'. This DataSymbol instance datatype is "
                        f"'{self.datatype}' meaning the initial value should "
                        f"be '{lookup}' but found '{type(new_value)}'.")
                if self.datatype.intrinsic == ScalarType.Intrinsic.BOOLEAN:
                    # In this case we know new_value is a Python boolean as it
                    # has passed the isinstance(new_value, lookup) check.
                    if new_value:
                        new_initial_value = Literal('true', self.datatype)
                    else:
                        new_initial_value = Literal('false', self.datatype)
                else:
                    # Otherwise we convert the Python intrinsic to a PSyIR
                    # Literal using its string representation.
                    new_initial_value = Literal(str(new_value), self.datatype)

            # Add it to a properly formed Assignment parent, this implicitly
            # guarantees that the node is not attached anywhere else (and is
            # unexpectedly modified) and also makes it similar to any other RHS
            # expression, enabling some functionality without special cases.
            # Note that the parent dangles on top of the init value, and is not
            # referenced directly from anywhere else.
            from psyclone.psyir.nodes import Assignment
            parent = Assignment()
            parent.addchild(Reference(self))
            parent.addchild(new_initial_value)
            self._initial_value = new_initial_value
        else:
            if self.is_constant and not self.is_import:
                raise ValueError(
                    f"DataSymbol '{self.name}' is a constant and not imported "
                    f"and therefore must have an initial value but got None")
            self._initial_value = None

    def __str__(self):
        ret = self.name + ": DataSymbol<" + str(self.datatype)
        ret += ", " + str(self._interface)
        if self.initial_value is not None:
            ret += f", initial_value={self.initial_value}"
        if self.is_constant:
            ret += ", constant=True"
        return ret + ">"

    def copy(self):
        '''Create and return a copy of this object. Any references to the
        original will not be affected so the copy will not be referred
        to by any other object.

        :returns: An object with the same properties as this symbol object.
        :rtype: :py:class:`psyclone.psyir.symbols.DataSymbol`

        '''
        if self.initial_value is not None:
            new_init_value = self.initial_value.copy()
        else:
            new_init_value = None
        return DataSymbol(self.name, self.datatype, visibility=self.visibility,
                          interface=self.interface,
                          is_constant=self.is_constant,
                          initial_value=new_init_value)

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
        super().copy_properties(symbol_in)
        self._is_constant = symbol_in.is_constant
        self._initial_value = symbol_in.initial_value
