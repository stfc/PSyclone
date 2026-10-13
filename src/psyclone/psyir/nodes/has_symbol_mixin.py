# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module contains the HasSymbolMixin implementation.'''

from __future__ import annotations
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from psyclone.psyir.symbols import Symbol


class HasSymbolMixin:
    '''Mixin for PSyIR nodes that store a Symbol.'''

    @property
    def symbol(self) -> Symbol:
        '''
        :returns: the Symbol associated with this node.
        '''
        return self._symbol

    @symbol.setter
    def symbol(self, symbol: Symbol):
        '''Set the Symbol associated with this node.

        :param symbol: the Symbol associated with this node.

        :raises TypeError: if symbol is not a Symbol.
        '''
        # Import locally to avoid a circular dependency.
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.symbols import Symbol
        if symbol and not isinstance(symbol, Symbol):
            raise TypeError(
                f"The {type(self).__name__} symbol setter expects a PSyIR "
                f"Symbol but found '{type(symbol).__name__}'.")
        self._symbol = symbol


__all__ = ["HasSymbolMixin"]
