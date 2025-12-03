# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2025, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the GenericInterfaceSymbol.'''

from __future__ import annotations
from dataclasses import dataclass
from typing import Union

from psyclone.psyir.symbols.data_type_symbol import DataTypeSymbol
from psyclone.psyir.symbols.datatypes import DataType, UnresolvedType
from psyclone.psyir.symbols.symbol import Symbol
from psyclone.psyir.symbols.routinesymbol import RoutineSymbol


class GenericInterfaceSymbol(RoutineSymbol):
    '''Symbol identifying a generic interface that maps to a number of
    different callable routines.

    Unlike the parent classes, the constructor for this class cannot be
    supplied with the 'is_pure', 'is_elemental' or 'datatype' properties as
    these are derived from the RoutineSymbols contained within this interface.

    :param str name: name of the interface.
    :param routines: the routines that this interface provides access
        to and whether or not each of them is a module procedure.
    :type routines: list[tuple[
                             :py:class:`psyclone.psyir.symbols.RoutineSymbol`,
                             bool]]
    :param kwargs: additional keyword arguments provided by
        :py:class:`psyclone.psyir.symbols.TypedSymbol`. Note that 'is_pure',
        'is_elemental' and 'datatype' are not supported.
    :type kwargs: unwrapped dict.

    :raises ValueError: if passed the 'is_pure', 'is_elemental' or 'datatype'
                        properties.
    '''
    @dataclass(frozen=True)
    class RoutineInfo:
        '''
        Holds information on a single routine member of an interface.

        :param symbol: the symbol representing the routine.
        :param from_container: whether or not this routine is from a Container
                               (i.e. a 'module procedure' in Fortran).
        '''
        symbol: RoutineSymbol
        from_container: bool

    def __init__(self, name, routines, **kwargs):
        for keyword in ["is_pure", "is_elemental", "datatype"]:
            if keyword in kwargs:
                raise ValueError(
                    f"The '{keyword}' property of GenericInterfaceSymbol "
                    f"cannot be supplied to the constructor - it is "
                    f"computed dynamically from its constituent RoutineSymbols"
                )
        super().__init__(name, **kwargs)
        self._routines = []
        self._process_arguments(routines=routines,
                                **kwargs)

    def _process_arguments(self, **kwargs):
        ''' Process the arguments for the constructor and the specialise
        methods. In this case the 'routines' argument.

        :param kwargs: keyword arguments which can be:\n
            :param routines: the routines that this interface provides access
                to and whether or not each of them is a module procedure.
            :type routines: list[tuple[
                 :py:class:`psyclone.psyir.symbols.RoutineSymbol`,
                 bool]]
        '''
        if "routines" in kwargs:
            # Use the setter for 'routines' as it performs checking.
            self.routines = kwargs.pop("routines")
        else:
            self._routines = []

        super()._process_arguments(**kwargs)

    @property
    def routines(self):
        '''
        :returns: information on all of the routines to which this interface
                  provides access.
        :rtype: list[tuple[:py:class:`psyclone.psyir.symbols.RoutineSymbol`,
                           bool]]
        '''
        return self._routines

    @routines.setter
    def routines(self, values):
        '''
        Setter for the list of routines to which this interface provides
        access.

        :param values: the RoutineSymbols and whether or not each of them
            is a module procedure.
        :type values: list[
            tuple[:py:class:`psyclone.psyir.symbols.RoutineSymbol`, bool]]

        :raises ValueError: if no (or an empty) list of values is provided.
        :raises TypeError: if `symbols` is not a list that consists only of
                           (RoutineSymbol, bool) tuples.
        '''
        if not values:
            raise ValueError("A GenericInterfaceSymbol requires a list of "
                             "RoutineSymbols but none were provided.")
        if not isinstance(values, list):
            raise TypeError(f"A GenericInterfaceSymbol requires a list of "
                            f"tuples describing its member routines but got: "
                            f"'{values}'")
        self._routines = []
        for item in values:
            if not isinstance(item, tuple):
                raise TypeError(
                    f"A GenericInterfaceSymbol ('{self.name}') requires a "
                    f"list of tuples but got: '{type(item).__name__}'")
            if (len(item) != 2 or not isinstance(item[0], RoutineSymbol) or
                    not isinstance(item[1], bool)):
                raise TypeError(
                    f"Each tuple used to define a routine within the "
                    f"GenericInterfaceSymbol '{self.name}' must consist of a "
                    f"RoutineSymbol and a bool but got: "
                    f"{[type(rt).__name__ for rt in item]}")
            self._routines.append(self.RoutineInfo(item[0], item[1]))

    @property
    def is_pure(self) -> Union[bool, None]:
        '''
        :returns: whether the routine represented by this Symbol has no side
            effects (guarantees that the routine always returns the same
            result for a given set of inputs).
        '''
        if self._routines:
            # If one Routine in an Interface is pure then they all must be.
            return self._routines[0].symbol.is_pure
        return None

    @is_pure.setter
    def is_pure(self, _):
        '''
        This property is computed dynamically from the RoutineSymbols that
        this GenericInterfaceSymbol maps to and therefore cannot be set.

        This method exists for compatibility with the RoutineSymbol superclass.
        '''

    @property
    def is_elemental(self) -> Union[bool, None]:
        '''
        :returns: whether the routine represented by this Symbol is elemental
            (acts element-by-element on supplied array arguments) or None if
            this is not known.
        '''
        if self._routines:
            # If one Routine is elemental then they all must be.
            return self._routines[0].symbol.is_elemental
        return None

    @is_elemental.setter
    def is_elemental(self, _):
        '''
        This property is computed dynamically from the RoutineSymbols that
        this GenericInterfaceSymbol maps to and therefore cannot be set.

        This method exists for compatibility with the RoutineSymbol superclass.
        '''

    @property
    def container_routines(self):
        '''
        :returns: those routines that are defined in a Container.
        :rtype: list[:py:class:`psyclone.psyir.symbols.RoutineSymbol`]
        '''
        result = []
        for value in self._routines:
            if value.from_container:
                result.append(value.symbol)
        return result

    @property
    def external_routines(self):
        '''
        :returns: those routines that are external procedures.
        :rtype: list[:py:class:`psyclone.psyir.symbols.RoutineSymbol`]
        '''
        result = []
        for value in self._routines:
            if not value.from_container:
                result.append(value.symbol)
        return result

    def __str__(self):
        return (f"{self.name}: {type(self).__name__}<{self.datatype}, "
                f"routines={[rt.symbol.name for rt in self.routines]}>")

    def copy(self):
        '''Create and return a copy of this object. Any references to the
        original will not be affected so the copy will not be referred
        to by any other object.

        :returns: A symbol object with the same properties as this
                  symbol object.
        :rtype: :py:class:`psyclone.psyir.symbols.GenericInterfaceSymbol`

        '''
        # The constructors for all Symbol-based classes have 'name' as the
        # first positional argument.
        rt_info = [(rt.symbol, rt.from_container) for rt in self.routines]
        return type(self)(self.name, rt_info,
                          visibility=self.visibility,
                          interface=self.interface.copy())

    @property
    def datatype(self) -> Union[DataType, DataTypeSymbol]:
        '''
        :returns: the datatype of this symbol if it can be determined.
        '''
        # Use the str representation of each type as that is hashable.
        dtypes = set(str(rinfo.symbol.datatype) for rinfo in self._routines)
        if len(dtypes) == 1:
            return self._routines[0].symbol.datatype
        # We have more than one possible datatype.
        return UnresolvedType()

    @datatype.setter
    def datatype(self, value):
        '''
        This property is computed dynamically from the RoutineSymbols that
        this GenericInterfaceSymbol maps to and therefore cannot be set.

        This method exists for compatibility with the RoutineSymbol superclass.
        '''

    def copy_properties(self,
                        symbol_in: RoutineSymbol,
                        exclude_interface: bool = False):
        '''
        Replace all properties in this object with the properties from
        symbol_in, apart from the name (which is immutable) and visibility.
        If `exclude_interface` is True, the interface is also not updated.

        :param symbol_in: the Symbol to copy properties from.
        :param exclude_interface: whether or not to copy the interface
            property of the provided Symbol (default is to include it).

        '''
        super().copy_properties(symbol_in,
                                exclude_interface=exclude_interface)
        # We must add information on the routines which this interface
        # can bind to.
        new_values = []
        for info in symbol_in.routines:
            new_sym = info.symbol.copy()
            if self.is_import:
                # If this interface symbol is imported then the Routines it
                # wraps must also be in scope in that same Container. Note that
                # they may (and probably will) be private to that Container.
                new_sym.interface = self.interface
            new_values.append((new_sym, info.from_container))
        self.routines = new_values

    def replace_symbols_using(self, table_or_symbol):
        '''
        Replace any Symbols referred to by this object with those in the
        supplied SymbolTable (or just the supplied Symbol instance) if they
        have matching names. If there is no match for a given Symbol then it
        is left unchanged.

        Before performing any replacement, the supplied symbol is specialised
        to a RoutineSymbol, if necessary.

        :param table_or_symbol: the symbol table from which to get replacement
            symbols or a single, replacement Symbol.
        :type table_or_symbol: :py:class:`psyclone.psyir.symbols.SymbolTable` |
            :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        # Construct a new list of RoutineSymbols.
        new_routines = []
        for routine in self.routines:
            if isinstance(table_or_symbol, Symbol):
                if table_or_symbol.name.lower() == routine.symbol.name.lower():
                    new_rt = table_or_symbol
                else:
                    new_rt = routine.symbol
            else:
                new_rt = table_or_symbol.lookup(routine.symbol.name,
                                                otherwise=routine.symbol)
            if not isinstance(new_rt, RoutineSymbol):
                new_rt.specialise(RoutineSymbol)
            new_routines.append((new_rt, routine.from_container))
        self.routines = new_routines

    def get_all_accessed_symbols(self) -> set[Symbol]:
        '''
        :returns: a set of all the symbols accessed inside this interface.
        '''
        symbols = super().get_all_accessed_symbols()

        for rt_info in self.routines:
            symbols.add(rt_info.symbol)
        return symbols


# For Sphinx AutoAPI documentation generation
__all__ = ["GenericInterfaceSymbol"]
