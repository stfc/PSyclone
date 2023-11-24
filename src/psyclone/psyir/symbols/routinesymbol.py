# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2023, Science and Technology Facilities Council.
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
# Authors R. W. Ford, S. Siso and N. Nobre, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the RoutineSymbol.'''

from psyclone.psyir.symbols.datatypes import NoType, UnknownFortranType
from psyclone.psyir.symbols.symbol import SymbolError
from psyclone.psyir.symbols.typed_symbol import TypedSymbol


class RoutineSymbol(TypedSymbol):
    '''Symbol identifying a callable routine.

    :param str name: name of the symbol.
    :param datatype: data type of the symbol. Default to NoType().
    :type datatype: :py:class:`psyclone.psyir.symbols.DataType`
    :param kwargs: additional keyword arguments provided by \
                   :py:class:`psyclone.psyir.symbols.TypedSymbol`
    :type kwargs: unwrapped dict.

    '''
    def __init__(self, name, datatype=None, **kwargs):
        # In general all arguments are processed by the _process_arguments
        # but in the 'datatype' case it must be done here because it is a
        # mandatory argument for the super constructor. There is equivalent
        # logic in the _process_argument for when the RoutineSymbol is
        # specialised instead of constructed.
        if datatype is None:
            datatype = NoType()
        super().__init__(name, datatype)
        # Whether this Routine is 'elemental'. A value of None indicates that
        # this is unknown.
        self._is_elemental = None
        # Whether this Routine is 'pure' (has no side effects). A value of
        # None indicates that this is unknown.
        self._is_pure = None
        self._process_arguments(**kwargs)

    def _process_arguments(self, **kwargs):
        ''' Process the arguments for the constructor and the specialise
        methods. In this case it provides a default NoType datatype if
        none is found or provided. It also handles the 'is_pure' and
        'is_elemental' arguments since these are specific to RoutineSymbol.

        :param kwargs: keyword arguments which can be:\n
            the arguments in :py:class:`psyclone.psyir.symbols.TypedSymbol`
        :type kwargs: unwrapped dict.

        '''
        if "datatype" not in kwargs and \
           (not hasattr(self, '_datatype') or self.datatype is None):
            kwargs["datatype"] = NoType()
        # Use the setters as they perform type checking.
        self.is_elemental = kwargs.pop("is_elemental", None)
        self.is_pure = kwargs.pop("is_pure", None)

        super()._process_arguments(**kwargs)

    def __str__(self):
        is_pure = "unknown" if self.is_pure is None else f"{self.is_pure}"
        is_elemental = ("unknown" if self.is_elemental is None
                        else f"{self.is_elemental}")
        return (f"{self.name}: {type(self).__name__}<{self.datatype}, "
                f"pure={is_pure}, elemental={is_elemental}>")

    @property
    def is_pure(self):
        '''
        :returns: whether the routine represented by this Symbol has no side \
            effects (guarantees that the routine always returns the same \
            result for a given set of inputs).
        :rtype: bool | NoneType
        '''
        return self._is_pure

    @is_pure.setter
    def is_pure(self, value):
        '''
        Sets whether or not the Routine represented by this Symbol is \
        guaranteed not to have side effects (see the corresponding property \
        description).

        :param value: the new value for the is_pure property.
        :type value: NoneType | bool

        :raises TypeError: if the type of the supplied value is invalid.

        '''
        if value is not None and not isinstance(value, bool):
            raise TypeError(f"is_pure for a {type(self).__name__} must be a "
                            f"bool or None but got '{type(value).__name__}'")
        self._is_pure = value

    @property
    def is_elemental(self):
        '''
        :returns: whether the routine represented by this Symbol is elemental \
            (acts element-by-element on supplied array arguments) or None if \
            this is not known.
        :rtype: bool | NoneType
        '''
        return self._is_elemental

    @is_elemental.setter
    def is_elemental(self, value):
        '''
        Sets whether or not the Routine represented by this Symbol is
        elemental.

        :param value: the new value for the is_elemental property.
        :type value: NoneType | bool

        :raises TypeError: if the type of the supplied value is invalid.

        '''
        if value is not None and not isinstance(value, bool):
            raise TypeError(f"is_elemental for a {type(self).__name__} must "
                            f"be a bool or None but got "
                            f"'{type(value).__name__}'")
        self._is_elemental = value

    def get_routine(self, container=None):
        '''
        Recursively searches for the implementation of this RoutineSymbol by
        following Container imports.

        :param container: optional Container containing the RoutineSymbol we
                          are searching for.
        :type container: Optional[:py:class:`psyclone.psyir.nodes.Container`]

        :returns: the PSyIR of the implementation of this Routine.
        :rtype: :py:class:`psyclone.psyir.nodes.Routine`

        :raises NotImplementedError: if this symbol is unresolved or is of
            UnknownFortranType (which often means it is an Interface).
        :raises ValueError: if this symbol is not imported and no `container`
            has been supplied in which to search.
        :raises SymbolError: if the RoutineSymbol is not imported or unresolved
            but an associated Routine cannot be found in the Container.

        '''
        if self.is_unresolved:
            # TODO Use ModuleManager here?
            raise NotImplementedError(
                f"RoutineSymbol '{self.name}' is unresolved and searching for "
                f"its implementation is not yet supported.")

        if self.is_import:
            csym = self.interface.container_symbol
            # If necessary, this will search for and process the source file
            # defining the container.
            container = csym.container
            rsym = container.symbol_table.lookup(self.name)
            if not isinstance(rsym, RoutineSymbol):
                # We now know that this is a RoutineSymbol so specialise it
                # in place.
                rsym.specialise(RoutineSymbol)
            # This symbol is itself imported into the current Container so
            # we recurse.
            return rsym.get_routine(container)

        if not container:
            raise ValueError(
                f"RoutineSymbol '{self.name}' is not imported so a "
                f"Container node must be provided in order to search for "
                f"its Schedule.")

        if isinstance(self.datatype, UnknownFortranType):
            raise NotImplementedError(
                f"RoutineSymbol '{self.name}' exists in Container "
                f"'{container.name}' but is of UnknownFortranType:\n"
                f"{self.datatype.declaration}\n"
                f"Cannot currently module inline such a routine.")

        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes.routine import Routine
        for routine in container.walk(Routine, stop_type=Routine):
            if routine.name.lower() == self.name.lower():
                kernel_schedule = routine
                return kernel_schedule

        raise SymbolError(
            f"Routine '{self.name}' is imported from '{container.name}' "
            f"but failed to find a matching routine in that "
            f"container.")


# For Sphinx AutoAPI documentation generation
__all__ = ["RoutineSymbol"]
