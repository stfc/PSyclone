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

from dataclasses import dataclass

from psyclone.psyir.symbols.symbol import Symbol
from psyclone.psyir.symbols.routinesymbol import RoutineSymbol


class GenericInterfaceSymbol(RoutineSymbol):
    '''Symbol identifying a generic interface that maps to a number of
    different callable routines.

    :param str name: name of the interface.
    :param routines: the routines that this interface provides access
        to and whether or not each of them is a module procedure.
    :type routines: list[tuple[
                             :py:class:`psyclone.psyir.symbols.RoutineSymbol`,
                             bool]]
    :param kwargs: additional keyword arguments provided by
                   :py:class:`psyclone.psyir.symbols.TypedSymbol`
    :type kwargs: unwrapped dict.

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
                          datatype=self.datatype.copy(),
                          visibility=self.visibility,
                          interface=self.interface.copy())

    def copy_properties(self, symbol_in: RoutineSymbol):
        '''
        Copies the properties of the supplied Symbol into this one.

        :param symbol_in: the Symbol to copy properties from.

        '''
        super().copy_properties(symbol_in)
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

    def reference_accesses(self):
        '''
        :returns: a map of all the symbol accessed inside this object, the
            keys are Signatures (unique identifiers to a symbol and its
            structure acccessors) and the values are SingleVariableAccessInfo
            (a sequence of AccessTypes).
        :rtype: :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        access_info = super().reference_accesses()

        # pylint: disable=import-outside-toplevel
        from psyclone.core import AccessType, Signature
        for rt_info in self.routines:
            access_info.add_access(Signature(rt_info.symbol.name),
                                   AccessType.TYPE_INFO, self)
        return access_info


# For Sphinx AutoAPI documentation generation
__all__ = ["GenericInterfaceSymbol"]
