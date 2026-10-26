# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the IntrinsicSymbol.'''

from __future__ import annotations

from psyclone.psyir.symbols.routinesymbol import RoutineSymbol


class IntrinsicSymbol(RoutineSymbol):
    '''Symbol identifying a callable intrinsic routine.

    :param str name: name of the symbol.
    :param intrinsic: the intrinsic enum describing this Symbol.
    :type intrinsic: :py:class:`psyclone.psyir.nodes.IntrinsicCall.Intrinsic`
    :param kwargs: additional keyword arguments provided by
                   :py:class:`psyclone.psyir.symbols.TypedSymbol`
    :type kwargs: unwrapped dict.

    # TODO #2541: Currently name and the intrinsic should match, we really
    # just need the name, and make all the Intrinsic signature information
    # live inside the IntrinsicSymbol class.

    '''
    def __init__(self, name, intrinsic, **kwargs):
        super().__init__(name, **kwargs)
        self._intrinsic = intrinsic

    @property
    def intrinsic(self):
        '''
        :returns: the intrinsic enum describing this Symbol.
        :rtype: :py:class:`psyclone.psyir.nodes.IntrinsicCall.Intrinsic`
        '''
        return self._intrinsic

    def copy(self) -> IntrinsicSymbol:
        '''Create and return a copy of this object. Any references to the
        original will not be affected so the copy will not be referred
        to by any other object.

        :returns: A symbol object with the same properties as this
                  symbol object.

        '''
        # The constructors for all Symbol-based classes have 'name' as the
        # first positional argument.
        return type(self)(self.name, self.intrinsic,
                          datatype=self.datatype.copy(),
                          visibility=self.visibility,
                          interface=self.interface.copy(),
                          is_pure=self.is_pure,
                          is_elemental=self.is_elemental)


# For Sphinx AutoAPI documentation generation
__all__ = ["IntrinsicSymbol"]
