# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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

from psyclone.psyir.symbols.routinesymbol import RoutineSymbol


class GenericInterfaceSymbol(RoutineSymbol):
    '''Symbol identifying a generic interface that maps to a number of
    different callable routines.

    :param str name: name of the interface.
    :param routines: the routines that this interface provides access to.
    :type routines: list[:py:class:`psyclone.psyir.symbols.RoutineSymbol`]
    :param kwargs: additional keyword arguments provided by
                   :py:class:`psyclone.psyir.symbols.TypedSymbol`
    :type kwargs: unwrapped dict.

    :raises ValueError: if no routines are provided.
    :raises TypeError: if `routines` is not a list of RoutineSymbols.

    '''
    def __init__(self, name, routines, **kwargs):
        super().__init__(name, **kwargs)
        if not routines:
            raise ValueError("A GenericInterfaceSymbol requires a list of "
                             "RoutineSymbols but none were provided.")
        if not all(isinstance(item, RoutineSymbol) for item in routines):
            raise TypeError(
                f"A GenericInterfaceSymbol must be provided with a list of "
                f"RoutineSymbols but got: "
                f"{[type(rt).__name__ for rt in routines]}")
        self._routines = routines

    @property
    def routines(self):
        '''
        :returns: the routines to which this interface provides access.
        :rtype: list[:py:class:`psyclone.psyir.symbols.RoutineSymbol`]
        '''
        return self._routines

    def __str__(self):
        is_pure = "unknown" if self.is_pure is None else f"{self.is_pure}"
        is_elemental = ("unknown" if self.is_elemental is None
                        else f"{self.is_elemental}")
        return (f"{self.name}: {type(self).__name__}<{self.datatype}, "
                f"pure={is_pure}, elemental={is_elemental}, "
                f"routines={[rt.name for rt in self.routines]}>")


# For Sphinx AutoAPI documentation generation
__all__ = ["RoutineSymbol"]
